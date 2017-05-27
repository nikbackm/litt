/** LITT - now for C++! ***********************************************************************************************

Changelog:
 * 2017-05-27: Now have column and list display modes working. File output format is utf-8 with bom (bom for V/VIEW).
 * 2017-05-26: Decided to use exceptions for error handling after all!
 * 2017-05-24: Decided to use WriteConsoleW for writing (converted) utf-8 output. Least problems and faster too.
 * 2017-05-22: Have now decided to finish it and no longer depend on TCC and the SQLite shell. Also for a faster LITT!
 * 2017-05-20: First tests started. Not using Modern C++ so far though, not for SQLite parts at least.
 * 2017-05-19: Got inpired by Kenny Kerr's PluralSight course "SQlite with Modern C++".
 * Previous:   Refer to the litt.btm changelog.

**********************************************************************************************************************/

// add parameters to list functions, at least re-usable ones!
// rest of selects!
// rest of display modes, html and csv
// error handling of all API calls!
// decide whether to add the actions that run defined VIEWs. If not remove from help. (Can be done in columns mode by setting col size by col name + 2 and also size of first row)
// input actions! maybe make SQLITE c++ wrapper for that.
// would be nice with litt state that does not change from query to query when reusing list methods for input actions!

#include "stdafx.h"

namespace Utils 
{
	bool toInt(std::string const & str, int& value)
	{
		char* endPtr;
		int v = strtol(str.c_str(), &endPtr, 10);
		if (errno == ERANGE || (endPtr != str.c_str() + str.length())) {
			return false;
		}
		value = v;
		return true;
	}

	std::wstring toWide(int codePage, const char *src, int len = 0)
	{
		if (int const nLen = (len > 0 ? len : lstrlenA(src))) {
			if (int const sizeNeeded = MultiByteToWideChar(codePage, 0, src, nLen, NULL, 0)) {
				std::wstring wstr(sizeNeeded, '\0');
				int const res = MultiByteToWideChar(codePage, 0, src, nLen, &wstr[0], sizeNeeded);
				if (res == sizeNeeded) {
					return wstr;
				}
			}
		}
		return std::wstring();
	}

	std::wstring toWide(int codePage, std::string const & str) 
	{ 
		return toWide(codePage, str.c_str(), str.length()); 
	}

	std::string toNarrow(int codePage, const wchar_t *src, int len = 0)
	{
		if (int const nLen = (len > 0 ? len : lstrlenW(src))) {
			if (int const sizeNeeded = WideCharToMultiByte(codePage, 0, src, nLen, NULL, 0, NULL, NULL)) {
				std::string str(sizeNeeded, '\0');
				int const res = WideCharToMultiByte(codePage, 0, src, nLen, &str[0], sizeNeeded, NULL, NULL);
				if (res == sizeNeeded) {
					return str;
				}
			}
		}
		return std::string();
	}
	
	std::wstring utf8ToWide(const char* utf8String, int len = 0)
	{
		return toWide(CP_UTF8, utf8String, len);
	}

	std::string toUtf8(int codePage, const char* str, int len = 0)
	{
		auto wstr = toWide(codePage, str, len);
		return toNarrow(CP_UTF8, wstr.c_str(), wstr.length());
	}

	std::string toUtf8(int codePage, std::string const & str)
	{
		auto wstr = toWide(codePage, str);
		return toNarrow(CP_UTF8, wstr.c_str(), wstr.length());
	}

	// Replace our wildcard "*" with SQL's wildcard "%".
	void makeLikeArg(std::string & str)
	{
		std::replace(str.begin(), str.end(), '*', '%');
	}
} // Utils
using namespace Utils;

namespace LittConstants 
{
	const char* DefDbName = "litt.sqlite";
	const char  OptDelim = '.';
	const char* LogOp_OR = " OR ";
	const char* LogOp_AND = " AND ";
}
using namespace LittConstants;

enum class DisplayMode {
	column,
	csv,
	html,
	line,
	list,
	tabs,
};

enum class ColumnType {
	text = 0,
	numeric = 1,
};

enum class ColumnSortOrder {
	Asc = 0,
	Desc = 1,
};

// Note: All member value types are chosen/designed so that zero-init will set the desired default.
struct ColumnInfo {
	// These values are pre-configured:
	char const* name;
	int         defWidth;
	ColumnType  type;
	char const* label; // optional
	bool        isGroupAggregate;

	// These values are set at runtime. Stored here for convenience.
	mutable int  overriddenWidth;
	mutable bool usedInQuery;

	std::string getLikeArg(std::string val) const {
		makeLikeArg(val);
		int ival;
		if (type != ColumnType::numeric || !toInt(val, ival)) {
			val = "'" + val + "'";
		}
		return val;
	}
};

// A collection of columns including some integer data (width, sortOrder).
using Columns = std::vector<std::pair<ColumnInfo const *, int>>; 

enum class ColumnsDataKind {
	none,
	width,
	sortOrder
};

struct OptionParser {
	std::stringstream ss;
	const char* type;
	OptionParser(std::string const & value, const char* type = "option")
		: ss(value), type(type)
	{}

	bool getNext(std::string& next)
	{
		return !!std::getline(ss, next, OptDelim);
	}
	
	__declspec(noreturn) void throwError() 
	{
		throw std::invalid_argument(std::string("Faulty ") + type + " value: " + ss.str());
	}

	std::string getNext()
	{
		std::string value;
		if (!getNext(value)) { throwError(); }
		return value;
	}

	int nextInt()
	{
		auto val = getNext();
		int ival; if (!toInt(val, ival)) { throwError(); }
		return ival;
	}

	std::string nextIntAsStr() { return std::to_string(nextInt()); }
};

struct Litt {
	// Maps short name to column info.
	std::map<std::string, ColumnInfo> columnInfos;

	bool headerOn = true;
	bool selectDistinct = false;
	bool showQuery = false;
	bool explainQuery = false;
	bool showNumberOfRows = false;
	DisplayMode displayMode = DisplayMode::column;
	std::string listSep = "|";
	std::string colSep = "  ";

	std::string dbPath; // Path to LITT db file

	Columns orderBy; // Overrides the default action order.
	Columns selectedColumns; // Overrides the default action columns.
	Columns additionalColumns; // Added to the action or overridden columns.
	mutable std::string whereCondition;

	std::string action;
	std::vector<std::string> actionArgs;
	std::string actionRightWildCard;
	std::string actionLeftWildCard;

	mutable int rowCount = 0; // The number of rows printed so far.

	Litt() :
		columnInfos({ // OBS! Don't use "desc" and "asc" and short names! :)
			{"ai",   {"Authors.AuthorID", 8, ColumnType::numeric}},
			{"beb",  {"\"Bought Ebook\"", 3, ColumnType::numeric}},
			{"bi",   {"Books.BookID", 6, ColumnType::numeric}},
			{"bt",   {"Title", 40 }},
			{"dr",   {"\"Date read\"", 10 }},
			{"dg",   {"\"Date(s)\"", 30, ColumnType::text, nullptr, true }},
			{"fn",   {"\"First Name\"",15 }},
			{"ge",   {"Genre", 25 }},
			{"gi",   {"GenreID", 8, ColumnType::numeric }},
			{"ln",   {"\"Last Name\"",15 }},
			{"ng",   {"\"Author(s)\"", 35, ColumnType::text, nullptr, true }},
			{"nn",   {"ltrim(\"First Name\"||' '||\"Last Name\")", 20, ColumnType::text, "Author" }},
			{"la",   {"Language", 4 }},
			{"own",  {"Owned", 3, ColumnType::numeric }},
			{"ot",   {"\"Original Title\"", 30 }},
			{"se",   {"Series", 27 }},
			{"si",   {"SeriesID", 8, ColumnType::numeric }},
			{"sp",   {"\"Part in Series\"", 4 }},
			{"st",   {"Story", 30 }},
			{"stid", {"StoryID", 7, ColumnType::numeric }},
			{"so",   {"Source", 40 }},
			{"soid", {"Sources.SourceID", 7, ColumnType::numeric }},
			{"dw",   {"strftime('%w',\"Date Read\")", 3, ColumnType::text, "DOW" }},
			{"btl",  {"length(Title)", 4, ColumnType::numeric }},
			{"lnl",  {"length(\"Last Name\")", 4, ColumnType::numeric }},
			{"fnl",  {"length(\"First Name\")", 4, ColumnType::numeric }},
			{"dwl",  {"case cast (strftime('%w',\"Date Read\") as integer)"
					  " when 0 then 'Sun' when 1 then 'Mon' when 2 then 'Tue' when 3 then 'Wed' when 4 then 'Thu' when 5 then 'Fri' when 6 then 'Sat' else '' end",
					  5, ColumnType::text, "DoW" }},
			{"ti",  {"time(\"Date Read\")", 5, ColumnType::text, "Time" }},

			// Some special-purpose virtual columns, these are not generally usable:

			// Intended for use in -w for booksReadPerPeriod. Will end up in the HAVING clause of Total sub-query.
			{"drc", {"Count(BookID)", 0, ColumnType::numeric, nullptr, true }},
			// Intended for use in (list)sametitle
			{"btc", {"TitleCount", 11, ColumnType::numeric, "\"Title count\"" }},
			// Intended for use in (list)rereads
			{"brc", {"ReadCount", 10, ColumnType::numeric, "\"Read count\"" }},
			// This is for the "number of books" column in listAuthorBookCounts
			{"abc", {"COUNT(Books.BookID)", 6, ColumnType::numeric, "Books" }},
			// This is for the "number of books" column in listGenreBookCounts
			{"gbc", {"COUNT(Books.BookID)", 6, ColumnType::numeric, "Books" }},
	})
	{
	}

	ColumnInfo const* getColumn(std::string const & sn) const
	{
		auto it = columnInfos.find(sn);
		if (it == columnInfos.end()) {
			throw std::invalid_argument("Invalid short column name: " + sn);
		}
		return &it->second;
	}

	Columns getColumns(std::string const & sns, ColumnsDataKind kind, bool usedInQuery) const
	{
		Columns res;
		OptionParser opts(sns);
		std::string sn;
		for (;;) {
			if (sn.empty()) {
				if (!opts.getNext(sn)) {
					break;
				}
			}
			auto column = getColumn(sn);
			if (usedInQuery) {
				column->usedInQuery = true;
			}

			// Now get the optional data (sortOrder/width).
			int data = -1;
			bool endOfInput = false;
			if (opts.getNext(sn)) {
				if (kind == ColumnsDataKind::sortOrder) {
					if (sn == "asc") {
						data = (int)ColumnSortOrder::Asc;
					}
					else if (sn == "desc") {
						data = (int)ColumnSortOrder::Desc;
					}
				}
				else if (kind == ColumnsDataKind::width) {
					if (toInt(sn, data)) {
					}
				}
				if (data >= 0) {
					sn.clear();
				}
				else {
					// lookup sn as column next iteration
				}
			}
			else {
				endOfInput = true;
			}

			if (data < 0) { // Provide default values
				switch (kind) {
				case ColumnsDataKind::width: data = column->defWidth; break;
				case ColumnsDataKind::sortOrder: data = (int)ColumnSortOrder::Asc; break;
				}
			}
			res.push_back(std::make_pair(column, data));
			if (endOfInput) {
				break;
			}
		} // for

		return res;
	}

	std::string getWherePredicate(std::string const & value) const
	{
		OptionParser opts(value, "where");
		std::string wcond;

		std::string sn;
		while (opts.getNext(sn)) {
			auto col = getColumn(sn);
			col->usedInQuery = true;

			auto val = opts.getNext(); // Either a value or an operation for the value coming up.
			std::string oper;
			if (val == "lt") oper = "<";
			else if (val == "gt") oper = ">";
			else if (val == "eq") oper = "=";
			else if (val == "nq" || val == "ne") oper = "notlike";
			else if (val == "isnull" || val == "isempty") oper = val;

			if (oper.empty()) {
				oper = "LIKE";
			}
			else {
				if (oper != "isnull" && oper != "isempty") {
					val = opts.getNext();
				}
			}

			val = col->getLikeArg(val);

			std::string snCond;
			std::string colName(col->name); // !!! use label instead if found!!??? test and see

			if (oper == "notlike") snCond = "ifnull(" + colName + ", '') NOT LIKE " + val;
			else if (oper == "isnull") snCond = colName + " IS NULL";
			else if (oper == "isempty") snCond = colName + " = ''";
			else snCond = colName + " " + oper + " " + val;

			if (wcond.empty()) {
				wcond = snCond;
			}
			else {
				wcond = wcond + LogOp_AND + snCond;
			}
		}
		return wcond;
	}

	std::string parseCountCondition(std::string const & name, std::string const & value) const
	{
		OptionParser opts(value, "count condition");
		auto val = opts.getNext(); 
		int iVal;
		if (toInt(val, iVal)) { return name + " >= " + val; }
		else if (val == "lt") { return name + " < " + opts.nextIntAsStr(); }
		else if (val == "gt") { return name + " > " + opts.nextIntAsStr(); }
		else if (val == "eq") { return name + " = " + opts.nextIntAsStr(); }
		else if (val == "range") { 
			auto r1 = opts.nextIntAsStr(); auto r2 = opts.nextIntAsStr();
			return r1 + " <= " + name + " AND " + name + " <= " + r2; }
		else {
			throw std::invalid_argument("Invalid operator " + val + " in count condition value: " + value);
		}
	}

	void appendToWhereCondition(const char* logicalOp, std::string const & predicate) const
	{
		if (!predicate.empty()) {
			if (whereCondition.empty()) {
				whereCondition = "(" + predicate + ")";
			}
			else {
				whereCondition = "(" + whereCondition + ")" + logicalOp + "(" + predicate + ")";
			}
		}
	}

	void addActionWhereCondition(const char* sn, unsigned actionArgIndex) const
	{
		if (actionArgIndex < actionArgs.size()) {
			auto val = actionLeftWildCard + actionArgs[actionArgIndex] + actionRightWildCard;
			auto col = getColumn(sn);
			col->usedInQuery = true;
			appendToWhereCondition(LogOp_AND, std::string(col->name) + " LIKE " + col->getLikeArg(val));
		}
	}

	std::string actionArg(unsigned index) const {
		return index < actionArgs.size() ? actionArgs[index] : "";
	}

	DWORD        consoleMode = 0;
	int    const consoleCodePage = GetConsoleCP();
	HANDLE const stdOutHandle    = GetStdHandle(STD_OUTPUT_HANDLE);
	bool   const stdOutIsConsole = stdOutHandle != NULL && GetConsoleMode(stdOutHandle, &consoleMode);

	// Got missing WriteConsole output with 32K buffer! 20K seems ok so far...
	// 32K seems to work at home with Win10 though, but not at work with Win7.
	static const int BufSize = 1000*20;
	mutable char buffer[BufSize];
	mutable int  bufPos = 0;

	void writeOutPut(const char* str, int len) const
	{
		if (BufSize < len) {
			flushOutput();
			doWriteOutPut(str, len);
		}
		else {
			if ((BufSize - bufPos) < len) {
				flushOutput();
			}
			_ASSERT(len <= (BufSize - bufPos));
			memcpy(&buffer[bufPos], str, len);
			bufPos += len;
		}
	}

	void writeOutPut(char c) const
	{
		if (BufSize == bufPos) {
			flushOutput();
		}
		buffer[bufPos++] = c;
	}

	void writeOutPut(std::string const & str) const 
	{
		writeOutPut(str.c_str(), str.length());
	}

	void writeOutPut(const char* str) const 
	{
		writeOutPut(str, strlen(str));
	}

	void writeUtf8Width(const char* str, unsigned width) const
	{ // PRE: str contains only complete utf-8 code points.
		unsigned writtenChars = 0;

		for (int i = 0; str[i] != '\0' && writtenChars < width; /*inc in body*/) {
			if ((str[i] & 0x80) == 0) { // 0xxx xxxx - Single utf8 byte
				writeOutPut(str[i]);
				i += 1;
			}
			else if ((str[i] & 0xC0) == 0xC0) { // 11xx xxxx - utf8 start byte
				int n = 1;
				while ((str[i + n] & 0xC0) == 0x80) { // 10xx xxxx - continuation byte
					++n;
				}
				if (n == 1) throw std::invalid_argument("No utf-8 continuation byte(s)!");
				if (4 < n) throw std::invalid_argument("Too many utf-8 continuation bytes!");
				writeOutPut(&str[i], n);
				i += n;
			}
			else {
				throw std::invalid_argument("Invalid utf-8 byte!");
			}
			// The above is little more complex that would be needed, as we must make sure
			// to write complete utf-8 code points. Would not do if the buffer were flushed while
			// we wrote a middle or starting utf-8 byte! Well, will work when writing to file,
			// but not when writing to the console.

			// Note: We assume every Unicode code point corresponds to an actual visible character.
			// Does not take combining characters and such into account.
			++writtenChars;
		}

		while (writtenChars < width) {
			writeOutPut(' ');
			++writtenChars;
		}
	}

	void doWriteOutPut(const char* str, int len) const
	{
		if (len == 0) return;

		if (stdOutIsConsole) {
			auto ws = utf8ToWide(str, len);
			DWORD written;
			if (!WriteConsole(stdOutHandle, ws.c_str(), ws.length(), &written, 0)) {
				throw std::runtime_error("WriteConsole failed with error code: " + std::to_string(GetLastError()));
			}
		}
		else {
			fwrite(str, len, 1, stdout);
		}
	}

	void flushOutput() const 
	{
		doWriteOutPut(buffer, bufPos);
		bufPos = 0;
	}

	void flushOutputNoThrow() const
	{
		try { 
			flushOutput(); 
		}
		catch (std::exception& ex) { 
			fprintf(stderr, "\nflushOutput failed: %s\n", ex.what()); 
		}
	}
};

void parseCommandLine(int argc, char **argv, Litt& litt)
{
	for (int i = 1; i < argc; ++i) {
		if (argv[i][0] == '-' && argv[i][1] != '\0') { // A stand-alone '-' can be used as an (action) argument.
			auto const opt = argv[i][1];
			auto const val = std::string(argv[i][2] != '\0' ? &argv[i][2] : "");
			switch (opt) {
			case 'd':
				if (val == "col" || val == "column") litt.displayMode = DisplayMode::column;
				else if (val == "csv") litt.displayMode = DisplayMode::csv;
				else if (val == "html") litt.displayMode = DisplayMode::html;
				else if (val == "line") litt.displayMode = DisplayMode::line;
				else if (val == "tabs") litt.displayMode = DisplayMode::tabs;
				else if (val.substr(0, 4) == "list") {
					litt.displayMode = DisplayMode::list;
					if (4 < val.length()) {
						if (val[4] == ':' && 5 < val.length()) {
							litt.listSep = toUtf8(litt.consoleCodePage, val.substr(5).c_str());
						}
						else {
							goto invalidDisplayMode;
						}
					}
				}
				else {
					invalidDisplayMode:
					throw std::invalid_argument("Invalid display mode: " + val);
				}
				break;
			case 'h':
				if (val == "on") litt.headerOn = true;
				else if (val == "off") litt.headerOn = false;
				else throw std::invalid_argument("Invalid header value: " + val);
				break;
			case 'o':
				litt.orderBy = litt.getColumns(val, ColumnsDataKind::sortOrder, true);
				break;
			case 'c': 
				litt.selectedColumns = litt.getColumns(val, ColumnsDataKind::width, true);
				break;
			case 'l': 
				litt.dbPath = val;
				break;
			case 'a': {
				auto a = litt.getColumns(val, ColumnsDataKind::width, true);
				litt.additionalColumns.insert(litt.additionalColumns.end(), a.begin(), a.end());
				}
				break;
			case 'w': 
				litt.appendToWhereCondition(LogOp_OR, litt.getWherePredicate(val));
				break;
			case 's':
				// Not necessarily included in the query, hence "false".
				for (auto& c : litt.getColumns(val, ColumnsDataKind::width, false)) { 
					c.first->overriddenWidth = c.second;
				}
				break;
			case 'q':
				litt.showQuery = true;
				break;
			case 'u':
				litt.selectDistinct = true;
				break;
			case 'x': 
				litt.explainQuery = true;
				break;
			case 'n': 
				litt.showNumberOfRows = true;
				break;
			default:
				throw std::invalid_argument(std::string("Invalid option: ") + argv[i]);
			}
		}
		else {
			if (litt.action.empty()) {
				litt.action = argv[i]; 
				if (litt.action.length() >= 2 && litt.action[0] == '*') {
					litt.actionLeftWildCard = "*"; 
					litt.action.erase(0, 1);
				}
				if (litt.action.length() >= 2 && litt.action.back() == '*') {
					litt.actionRightWildCard = "*"; 
					litt.action.pop_back();
				}
			}
			else {
				litt.actionArgs.push_back(argv[i]);
			}
		}
	}

	if (litt.action.empty()) {
		throw std::invalid_argument("Action argument(s) missing");
	}

	if (litt.dbPath.empty()) {
		char mydocs[MAX_PATH];
		if (GetEnvironmentVariableA("MYDOCS", mydocs, MAX_PATH)) {
			litt.dbPath = std::string(mydocs) + "\\litt\\" + DefDbName;
		}
		else {
			litt.dbPath = DefDbName;
		}
	}
	if (GetFileAttributesA(litt.dbPath.c_str()) == -1) {
		throw std::invalid_argument("Cannot find: " + litt.dbPath);
	}
}

int showHelp(bool showExtended = false)
{
	printf(
R"(Usage: LITT [<options>] action <action arguments>

Supported actions:
   a  [<last name>] [<first name>] (Lists authors with given last name (and first name)).
   aa [<last name>] [<first name>] (Same as above, but also includes all books by the authors).
   b  [<title>]                    (Lists all books matching the given title).
   bb [<title>]                    (Same as above, but includes more details)
   ot [<origTitle>]                (Only lists books with original titles)
   st [<story>]                    (Only lists books with separate stories)
   s  [<series>]                   (Lists series)
   g  [<genre>]                    (Lists genre)
   so [<source>]                   (Lists book sources - where a certain book "read" was gotten)
   soo [<source>]                  (Lists book sources WITH read books for the sources)

   add-a                           (Adds a new author)
   add-b                           (Adds a new book)
   add-s                           (Adds a new series)
   add-g                           (Adds a new genre)

   abc [<bookCountCond>] [<bRRs>]  (Lists the number of read books for each author, second param = 1 => re-reads included.
                                    Supports virtual column abc - book count - for selection and ordering, but not for WHERE.)
   gbc [<bookCountCond>] [<bRRs>]  (Lists the number of read books for each genre, similar to abc)
   brd [<booksReadCond>]           (Lists the dates and books where [cond] books where read.)
   brm/bry/brmy/brym/brwd [-h]     (Lists the number of books read per month/year/etc. Supports -wdrc in addition to date param.))"
	);
	if (showExtended) {
		printf(
R"(

       [<dateCondition>] {<column def (where condition)> <column name>}
       
       Can also use brp which is more generic, it takes [<period col def (strftime string)>] [<period col name]>
       BEFORE the parameters the previos ones do.
)"
		);
	}
	printf(
R"(
   two "brd lite"                  (Lists dates with two or more books read)
   rereads                         (Lists re-read books. Can use extra virtual column "brc" - Read Count)
   sametitle                       (Lists books with same title. Can use extra virtual column "btc" - Book Title Count)
   
   b2s <BookID> <SeriesID> <part>  (Adds a book to a series)
   
   set-dr <BookID>                 (Add/Change or Delete DateRead for a book)
   set-bs <BookID> <DateRead>      (Set/Change the book source for a read date of a book)
   set-bs-clean                    (Same as above - also params - but re-creates the temp file with book sources)
   set-g <BookID> [C|D CurGenreID] (Add, Change or Delete genre for a book. Need to specify current GenreID for C and D)
   
   all                             (Lists "everything")
   h                               (Show more extensive help)
   
NOTE: As wildcards in most match arguments "*" (any string) and "_" (any character) can be used. Wild-cards (*) around the first 8
      listing actions also supported for similar effect, e.g. *b* will list all books containing the given title string, while
      b* will list books starting with it instead.
      
Options:
    -d[DisplayMode] (Determines how the results are displayed)
    -h[on|off]      (Determines if a header row is shown or not)
    -c[selColumns]  (Determines the included colums, overrides default columns for the action)
    -a[addColumns]  (Include these columns in addition to the default ones for the action)
    -o[colOrder]    (Determines sort order for results, by default sorts by included columns starting from left)
    -w[whereCond]   (Adds a WHERE condition - will be AND:ed with the one specified by the action and arguments.
                     If several -w options are included their values will be OR:ed)
    -s[colSizes]    (Override the default column sizes)
    -q              (Debug - dumps the SQLITE commands instead of producing results)
    -u              (Makes sure the results only contain UNIQUE/DISTICT values)
    -l[dbPath]      (Can specify an alternate litt database file)
    
    Note: Not all options are meaningful to all actions. In those cases they are simply ignored.
)"
	);
	if (showExtended) {
		printf(
R"(
selColumns format: <shortName>[.<width>]{.<shortName>[.<width>]}
colOrder format: <shortName>[.asc|desc]{.<shortName>[.asc|desc]}
whereCond format: <shortName>[.<cmpOper>].<cmpArg>{.<shortName>[.<cmpOper>].<cmpArg>}
          cmpOper: lt,gt,eq,nq,isnull,isempty ("LIKE" if none is given, isnull & isempty take no cmpArg)
colSizes format: Same as selColumns format

bookCountCond and booksReadCond formats:
    <number>        Only includes authors with book count >= <number>
    lt.<number>     Only includes authors with book count < <number>
    gt.<number>     Only includes authors with book count > <number>
    eq.<number>     Only includes authors with book count = <number>
    range.<n1>.<n2> Only includes authors with book count in range [n1,n2]

DisplayMode values:
    col/column  Left-aligned columns (Specified or default widths are used)
    csv         Comma-separated values
    html        HTML <table> code
    line        One value per line
    list[:sep]  Values delimited by separator string, default is "|"
    tabs        Tab-separated values

Column short name values:
    bt,bi,btl       - Book title, BookID, length of book title
    ln,fn,lnl,fnl   - Last/First Name & length thereof
    nn/ng           - Full name/Aggregated full name(s) per book.
    gi,ge           - GenreID and Genre
    dr/dg,dw,dwl,ti - Date read/Aggregated dates, DOW for Date read, DOW string, Time
    own,la,beb      - Owned, Language, Bought Ebook
    st,stid         - Story,StoryID
    se,si,sp        - Series, SeriesID, Part in Series
    so,soid         - Source, SourceID

)"
		);
	}
	return 0;
}

enum InnerJoinFor : unsigned {
	IJF_DefaultsOnly = 0x00,
	IJF_Stories = 0x01,
	IJF_Series = 0x02,
	IJF_OrigTitle = 0x04,
};

enum class SelectOption {
	normal,
	distinct,
};

class SelectQuery {
	std::stringstream m_sstr;
	Columns m_orderBy;
public:
	Litt const & litt;
	std::vector<unsigned> columnWidths; // Only set for column mode.

	SelectQuery(Litt const & litt) 
		: litt(litt) 
	{
	}

	void initSelect(const char* defColumns, const char* from, const char* defOrderBy, SelectOption selectOption = SelectOption::normal)
	{
		m_sstr.clear();
		if (litt.explainQuery) {
			m_sstr << "EXPLAIN QUERY PLAN ";
		}
		m_sstr << "SELECT ";
		if (litt.selectDistinct || selectOption == SelectOption::distinct) {
			m_sstr << "DISTINCT ";
		}

		auto selCols = litt.selectedColumns.empty() ? litt.getColumns(defColumns, ColumnsDataKind::width, true) : litt.selectedColumns;
		selCols.insert(selCols.end(), litt.additionalColumns.begin(), litt.additionalColumns.end());
		for (unsigned i = 0; i < selCols.size(); ++i) {
			if (i != 0) {
				m_sstr << ",";
			}
			auto ci = selCols[i].first;
			m_sstr << ci->name;
			if (ci->label != nullptr) {
				m_sstr << " AS " << ci->label;
			}
			if (litt.displayMode == DisplayMode::column) {
				auto const width = ci->overriddenWidth > 0 ? ci->overriddenWidth : selCols[i].second;
				_ASSERT(width > 0);
				columnWidths.push_back(width);
			}
		}

		m_sstr << "\nFROM " << from;

		if (litt.explainQuery && litt.displayMode == DisplayMode::column) {
			columnWidths = { 10, 10, 10, 100 }; // !!!! check shell code?
		}

		// Not used here, but we must "run" it anyway in order to finalize all columns used in the query for later "addIfColumns" calls.
		auto asc = [](Columns cols) { for (auto& c : cols) { c.second = (int)ColumnSortOrder::Asc; } return cols; };
		m_orderBy = litt.orderBy.empty()
			? (litt.selectedColumns.empty()
				? litt.getColumns(defOrderBy, ColumnsDataKind::sortOrder, true)
				: asc(litt.selectedColumns))
			: litt.orderBy;
	}

	void addWhere(const char* additionalCond = nullptr)
	{
		std::string whereCond = litt.whereCondition;
		if (additionalCond != nullptr) {
			whereCond = "(" + whereCond + ")" + LogOp_AND + "(" + additionalCond + ")";
		}

		if (!whereCond.empty()) {
			m_sstr << "\nWHERE " << whereCond;
		}
	}

	void add(const char* line)
	{
		m_sstr << "\n" << line;
	}

	void add(std::string const & line)
	{
		add(line.c_str());
	}

	void addIf(bool cond, const char* line)
	{
		if (cond) add(line);
	}

	void addIfColumns(const char* columns, const char* line) 
	{
		for (auto& col : litt.getColumns(columns, ColumnsDataKind::none, false)) {
			if (col.first->usedInQuery) {
				add(line);
				return;
			}
		}
	}

	void addIfColumns(const char* columns, std::string const & line)
	{
		addIfColumns(columns, line.c_str());
	}

	void addAuxTables(InnerJoinFor ijf = IJF_DefaultsOnly)
	{
		#define ngSqlSelect "(SELECT BookID, ltrim(group_concat(\"First Name\"||' '||\"Last Name\",', ')) AS 'Author(s)' FROM Books INNER JOIN AuthorBooks USING(BookID) INNER JOIN Authors USING(AuthorID) GROUP BY BookID)"
		#define dgSqlSelect "(SELECT BookID, group_concat(\"Date read\",', ') AS 'Date(s)' FROM Books INNER JOIN DatesRead USING(BookID) GROUP BY BookID)"

		std::string serJoin = (ijf & IJF_Series)    ? "INNER" : "LEFT OUTER";
		std::string stoJoin = (ijf & IJF_Stories)   ? "INNER" : "LEFT OUTER";
		std::string ortJoin = (ijf & IJF_OrigTitle) ? "INNER" : "LEFT OUTER";

		addIfColumns("dr.so",       "INNER JOIN DatesRead USING(BookID)");
		addIfColumns("ng",          "INNER JOIN " ngSqlSelect " USING(BookID)");
		addIfColumns("dg",          "INNER JOIN " dgSqlSelect " USING(BookID)");
		addIfColumns("fn.ln.nn.st", "INNER JOIN AuthorBooks USING(BookID)");
		addIfColumns("fn.ln.nn",    "INNER JOIN Authors USING(AuthorID)");
		addIfColumns("ot",           ortJoin + " JOIN OriginalTitles USING(BookID)");
		addIfColumns("st",           stoJoin + " JOIN Stories USING(AuthorID, BookID)");
		addIfColumns("sp.se",        serJoin + " JOIN BookSeries USING(BookID)");
		addIfColumns("se",           serJoin + " JOIN Series USING(SeriesID)");
		addIfColumns("so",          "LEFT OUTER JOIN Sources USING(SourceID)");
		addIfColumns("ge.gi",       "LEFT OUTER JOIN BookGenres USING(BookID)");
		addIfColumns("ge",          "LEFT OUTER JOIN Genres USING(GenreID)");

		#undef ngSqlSelect
		#undef dgSqlSelect
	}

	void addOrderBy()
	{
		if (!m_orderBy.empty()) { 
			m_sstr << "\nORDER BY ";
			for (unsigned i = 0; i < m_orderBy.size(); ++i) {
				if (i != 0) {
					m_sstr << ",";
				}
				auto ci = m_orderBy[i].first;
				auto order = (ColumnSortOrder)m_orderBy[i].second;
				m_sstr << (ci->label != nullptr ? ci->label : ci->name);
				if (order == ColumnSortOrder::Desc) {
					m_sstr << " DESC"; // ASC is default.
				}
			}
		}
	}

	std::string getSql() const
	{
		return toUtf8(litt.consoleCodePage, m_sstr.str() + ";");
	}
};

namespace QueryOutput
{
	void outputRow(SelectQuery& query, int argc, char **argv)
	{
		auto& litt = query.litt;
		auto mode = litt.displayMode;

		for (int i = 0; i < argc; i++) {
			auto val = argv[i] ? argv[i] : "";
			switch (mode) {
			case DisplayMode::column:
				if (i != 0) litt.writeOutPut(litt.colSep);
				litt.writeUtf8Width(val, query.columnWidths[i]);
				break;
			case DisplayMode::list:
				if (i != 0) litt.writeOutPut(litt.listSep);
				litt.writeOutPut(val);
				break;
			}
		}

		litt.writeOutPut('\n');
	}

	int callBack(void *pArg, int argc, char **argv, char **azColName) 
	{
		auto& query = *static_cast<SelectQuery*>(pArg);
		auto& litt = query.litt;
		try {
			if (litt.rowCount++ == 0) {
				if (!litt.stdOutIsConsole && litt.displayMode == DisplayMode::column) {
					// Write the UTF-8 BOM, seems V/VIEW needs it to properly 
					// detect the utf-8 encoding depending on the actual output.
					// Seems to interfere with V:S CSV mode though!
					const unsigned char bom[] = { 0xEF, 0xBB, 0xBF };
					litt.writeOutPut((const char*)&bom[0], sizeof(bom));
				}
				outputRow(query, argc, azColName);
				if (litt.displayMode == DisplayMode::column) {
					for (int i = 0; i < argc; ++i) {
						if (i != 0) litt.writeOutPut(litt.colSep);
						std::string underLine(query.columnWidths[i], '-');
						litt.writeOutPut(underLine);
					}
					litt.writeOutPut('\n');
				}
			}
			outputRow(query, argc, argv);
			return 0;
		}
		catch (std::exception& ex) {
			litt.flushOutputNoThrow();
			fprintf(stderr, "\nCallback exception: %s\n", ex.what());
			return 1;
		}
	}
}

void runSelectQuery(SelectQuery& query)
{
	std::string sql = query.getSql();
	auto& litt = query.litt;

	if (litt.showQuery) {
		// !!! also print relevant options? maybe add some -q values for that..... 
		litt.writeOutPut(sql); litt.writeOutPut('\n');
		litt.flushOutput();
		return;
	}

	std::string errMsg;

	sqlite3 *db = nullptr;
	int res = sqlite3_open(litt.dbPath.c_str(), &db);
	if (res != SQLITE_OK) {
		errMsg = std::string("Cannot open database: ") + sqlite3_errmsg(db);
		goto out;
	}

	litt.rowCount = 0;
	char *zErrMsg = nullptr;
	res = sqlite3_exec(db, sql.c_str(), QueryOutput::callBack, &query, &zErrMsg);
	litt.flushOutputNoThrow();
	if (res != SQLITE_OK) {
		errMsg = std::string("SQL error: ") + zErrMsg;
		sqlite3_free(zErrMsg);
		goto out;
	}

	if (litt.showNumberOfRows) {
		printf("# = %i\n", litt.rowCount); // !!! also stderr output??? Need to sync console and stdio streams most likely!
	}

out:
	sqlite3_close(db);
	if (!errMsg.empty()) {
		throw std::runtime_error(errMsg);
	}
}

void runSingleTableOutputCmd(Litt const & litt, const char* defColumns, const char* table, const char* defOrderBy)
{
	SelectQuery query(litt);
	query.initSelect(defColumns, table, defOrderBy);
	query.addWhere();
	query.addOrderBy();
	runSelectQuery(query);
}

void runListData(Litt const & litt, const char* defColumns, const char* defOrderBy, InnerJoinFor ijf = IJF_DefaultsOnly)
{
	SelectQuery query(litt);
	query.initSelect(defColumns, "Books", defOrderBy);
	query.addAuxTables(ijf);
	query.addWhere();
	query.addOrderBy();
	runSelectQuery(query);
}

void listAuthors(Litt const & litt)
{
	litt.addActionWhereCondition("ln", 0);
	litt.addActionWhereCondition("fn", 1);
	if (litt.action == "a") {
		runSingleTableOutputCmd(litt, "ai.ln.fn", "Authors", "ai");
	}
	else {
		runListData(litt, "bi.ln.fn.bt.dr.so.ge", "ln.fn.dr.bi");
	}
}

void listBooks(Litt const & litt)
{
	litt.addActionWhereCondition("bt", 0);
	if (litt.action == "b") {
		runSingleTableOutputCmd(litt, "bi.bt.100", "Books", "bi");
	}
	else {
		runListData(litt, "bi.nn.bt.dr.so.ge", "dr.bi.ln.fn.bt");
	}
}

void listSeries(Litt const & litt)
{
	litt.addActionWhereCondition("se", 0);
	if (litt.action == "s") {
		runSingleTableOutputCmd(litt, "si.se", "Series", "si");
	}
	else {
		runListData(litt, "se.sp.bt.dr.bi.ln.fn", "dr.bi.bt", IJF_Series);
	}
}

void listGenres(Litt const & litt)
{
	litt.addActionWhereCondition("ge", 0);
	if (litt.action == "g") {
		runSingleTableOutputCmd(litt, "gi.ge", "Genres", "ge");
	}
	else {
		runListData(litt, "bi.bt.ge.dr.nn", "dr.bi.bt");
	}
}

void listOriginalTitles(Litt const & litt)
{
	litt.addActionWhereCondition("ot", 0);
	runListData(litt, "bi.nn.ot.bt.dr.so.20.ge", "ot.dr.bi.ln.fn.bt", IJF_OrigTitle);
}

void listStories(Litt const & litt)
{
	litt.addActionWhereCondition("st", 0);
	runListData(litt, "bi.bt.30.st.50.ln.fn.dr", "bt.bi", IJF_Stories);
}

void listSources(Litt const & litt)
{
	litt.addActionWhereCondition("so", 0);
	if (litt.action == "so") {
		runSingleTableOutputCmd(litt, "soid.so", "Sources", "so");
	}
	else {
		runListData(litt, "dr.so.50.bt.30.ln.fn", "so");
	}
}

void listRereads(Litt const & litt)
{
	SelectQuery query(litt);
	const char* from = "(SELECT BookID, Count(BookID) As ReadCount FROM DatesRead GROUP BY BookID HAVING Count(BookID) > 1)";
	query.initSelect("brc.bt.dr.ng", from, "brc.desc.ln.bt.dr", SelectOption::distinct);
	query.add("INNER JOIN Books USING(BookID)");
	query.addAuxTables();
	query.addWhere();
	query.addOrderBy();
	runSelectQuery(query);
}

void listSametitle(Litt const & litt)
{
	SelectQuery query(litt);
	const char* from = "(SELECT Title, Count(Title) As TitleCount FROM Books GROUP BY Title HAVING Count(Title) > 1)";
	query.initSelect("bi.bt.ng.btc", from, "bt.bi", SelectOption::distinct);
	query.add("INNER JOIN Books USING(Title)");
	query.addAuxTables();
	query.addWhere();
	query.addOrderBy();
	runSelectQuery(query);
}

void listAuthorBookCounts(Litt const & litt, std::string const & countCond, bool includeReReads) 
{
	SelectQuery query(litt);
	const char* from = "Books INNER JOIN AuthorBooks USING(BookID) INNER JOIN Authors USING(AuthorID)";
	query.initSelect("nn.abc", from, "abc.desc");
	query.addIf(includeReReads, "INNER JOIN DatesRead USING(BookID)");
	query.addWhere();
	query.add("GROUP BY AuthorID");
	if (!countCond.empty()) {
		query.add("HAVING " + litt.parseCountCondition(litt.getColumn("abc")->name, countCond));
	}
	query.addOrderBy();
	runSelectQuery(query);
}

void listGenreBookCounts(Litt const & litt, std::string const & countCond, bool includeReReads)
{
	SelectQuery query(litt);
	const char* from = "Books INNER JOIN BookGenres USING(BookID) INNER JOIN Genres USING(GenreID)";
	query.initSelect("ge.35.gbc", from, "gbc.desc");
	query.addIf(includeReReads, "INNER JOIN DatesRead USING(BookID)");
	query.addWhere();
	query.add("GROUP BY GenreID");
	if (!countCond.empty()) {
		query.add("HAVING " + litt.parseCountCondition(litt.getColumn("gbc")->name, countCond));
	}
	query.addOrderBy();
	runSelectQuery(query);
}

void listBooksReadPerDate(Litt const & litt, std::string countCond)
{
	if (countCond.empty()) countCond = "2";

	#define calcDRTimeWindow R"(case when (time("Date Read") > '00:00:00' and time("Date Read") < '06:00:00') then date("Date Read", '-6 hours') else date("Date Read") end)"

	SelectQuery query(litt);
	const char* from = "Books INNER JOIN DatesRead USING(BookID) INNER JOIN AuthorBooks USING(BookID) INNER JOIN Authors USING(AuthorID)";
	query.initSelect("dr.bt.ln.fn", from, "dr");
	query.add("WHERE " calcDRTimeWindow " IN");
	query.add(" (SELECT CalcDR FROM (SELECT " calcDRTimeWindow " as CalcDR FROM DatesRead WHERE \"Date Read\" > \"2001-10\")");
	query.add("  GROUP BY CalcDR");
	query.add("  HAVING " + litt.parseCountCondition("Count(CalcDR)", countCond) + ")");
	if (!litt.whereCondition.empty()) {
		query.add(" AND " + litt.whereCondition);
	}
	query.addOrderBy();
	runSelectQuery(query);

	#undef calcDRTimeWindow
}

int main(int argc, char **argv)
{
	if (argc <= 1) {
		return showHelp();
	}

	try {
		Litt litt;
		parseCommandLine(argc, argv, litt);

		auto const & action = litt.action;
		if (action == "h") {
			showHelp(true);
		}
		else if (action == "a" || action == "aa") {
			listAuthors(litt);
		}
		else if (action == "b" || action == "bb") {
			listBooks(litt);
		}
		else if (action == "s" || action == "ss") {
			listSeries(litt);
		}
		else if (action == "g" || action == "gg") {
			listGenres(litt);
		}
		else if (action == "ot") {
			listOriginalTitles(litt);
		}
		else if (action == "st") {
			listStories(litt);
		}
		else if (action == "so" || action == "soo") {
			listSources(litt);
		}
		else if (action == "rereads") {
			listRereads(litt);
		}
		else if (action == "sametitle") {
			listSametitle(litt);
		}
		else if (action == "abc") {
			listAuthorBookCounts(litt, litt.actionArg(0), litt.actionArg(1) == "1");
		}
		else if (action == "gbc") {
			listGenreBookCounts(litt, litt.actionArg(0), litt.actionArg(1) == "1");
		}
		else if (action == "brd") {
			listBooksReadPerDate(litt, litt.actionArg(0));
		}
		else {
			throw std::invalid_argument("Invalid action: " + action);
		}

		return 0;
	}
	catch (std::exception& ex) {
		fprintf(stderr, "%s\n", ex.what());
		return 1;
	}
}
