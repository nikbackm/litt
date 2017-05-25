/** LITT - now for C++! ***********************************************************************************************

Changelog:
 * 2017-05-24: Decided to use WriteConsoleW for writing (converted) utf-8 output. Least problems and faster too.
 * 2017-05-22: Have now decided to finish it and no longer depend on TCC and the SQLite shell. Also for a faster LITT!
 * 2017-05-20: First tests started. Not using Modern C++ so far though, not for SQLite parts at least.
 * 2017-05-19: Got inpired by Kenny Kerr's PluralSight course "SQlite with Modern C++".
 * Previous:   Refer to the litt.btm changelog.

**********************************************************************************************************************/

// !!!! error handling of all API calls!!!!

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
} // Utils
using namespace Utils;

namespace LittConstants 
{
	const char* DefDbName = "litt.sqlite";
	const char  OptDelim = '.';
	const int   MaxColumnWidth = 300;
	const char* LogOp_OR = "OR";
	const char* LogOp_AND = "AND";

	const char* dgSQL = "(SELECT BookID, group_concat(\"Date read\",', ') AS 'Date(s)' FROM Books INNER JOIN DatesRead USING(BookID) GROUP BY BookID)";
	const char* ngSQL = "(SELECT BookID, ltrim(group_concat(\"First Name\"||' '||\"Last Name\",', ')) AS 'Author(s)' FROM Books INNER JOIN AuthorBooks USING(BookID) INNER JOIN Authors USING(AuthorID) GROUP BY BookID)";
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
	int  overriddenWidth;
	bool usedInQuery;
};

// A collection of columns and their associated widths (Which can also be interpreted as sort order!)
using Columns = std::vector<std::pair<ColumnInfo*, int>>; 

enum class ColumnsDataKind {
	none,
	width,
	sortOrder
};

struct LittState {
	// Maps short name to column info.
	std::map<std::string, ColumnInfo> columnInfos;

	bool headerOn = true;
	bool selectDistinct = false;
	bool showQuery = false;
	bool explainQuery = false;
	bool showNumberOfRows = false;
	DisplayMode mode = DisplayMode::column;
	std::string listSep = "|";

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

	LittState() :
		columnInfos({
			{"ai",   {"Authors.AuthorID", 8, ColumnType::numeric}},
			{"beb",  {"Bought Ebook", 3, ColumnType::numeric}},
			{"bi",   {"Books.BookID", 6, ColumnType::numeric}},
			{"bt",   {"Title", 40 }},
			{"dr",   {"Date read", 10 }},
			{"dg",   {"Date(s)", 30, ColumnType::text, nullptr, true }},
			{"fn",   {"First Name",15 }},
			{"ge",   {"Genre", 25 }},
			{"gi",   {"GenreID", 8, ColumnType::numeric }},
			{"ln",   {"Last Name",15 }},
			{"ng",   {"Author(s)", 35, ColumnType::text, nullptr, true }},
			{"nn",   {"ltrim(\"First Name\"||' '||\"Last Name\")", 20, ColumnType::text, "Author" }},
			{"la",   {"Language", 4 }},
			{"own",  {"Owned", 3, ColumnType::numeric }},
			{"ot",   {"Original Title", 30 }},
			{"se",   {"Series", 27 }},
			{"si",   {"SeriesID", 8, ColumnType::numeric }},
			{"sp",   {"Part in Series", 4 }},
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
			{"btc", {"TitleCount", 11, ColumnType::numeric, "Title count" }},
			// Intended for use in (list)rereads
			{"brc", {"ReadCount", 10, ColumnType::numeric, "Read count" }},
			// This is for the "number of books" column in listAuthorBookCounts
			{"abc", {"COUNT(Books.BookID)", 6, ColumnType::numeric, "Books" }},
			// This is for the "number of books" column in listGenreBookCounts
			{"gbc", {"COUNT(Books.BookID)", 6, ColumnType::numeric, "Books" }},
	})
	{
	}

	ColumnInfo* getColumn(std::string const & sn)
	{
		auto it = columnInfos.find(sn);
		if (it != columnInfos.end()) {
			return &it->second;
		}
		else {
			fprintf(stderr, "Invalid short column name '%s'\n", sn.c_str());
			return nullptr;
		}
	}

	Columns getColumns(std::string const & sns, ColumnsDataKind kind, bool usedInQuery, bool& ok)
	{
		Columns res;
		std::stringstream ss(sns);
		std::string sn;
		for (;;) {
			if (sn.empty()) {
				if (!std::getline(ss, sn, OptDelim)) {
					break;
				}
			}
			auto column = getColumn(sn);
			if (column == nullptr) {
				goto error;
			}

			if (usedInQuery) {
				column->usedInQuery = true;
			}

			// Now get the optional sortOrder/width.
			int data = -1;
			bool endOfInput = false;
			if (std::getline(ss, sn, OptDelim)) {
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
						if (MaxColumnWidth < data) {
							fprintf(stderr, "Invalid column width '%i'\n", data);
							goto error;
						}
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

		ok = true;
		return res;
	error:
		ok = false;
		return Columns();
	}

	bool addToWhereCondition(const char* logicalOp, std::string const & predicate) const
	{
		std::string wcond;
		// !!!

		if (!wcond.empty()) {
			if (whereCondition.empty()) {
				whereCondition = "(" + wcond + ")";
			}
			else {
				whereCondition = "(" + whereCondition + ")" + logicalOp + "(" + wcond + ")";
			}
		}

		return true;
	}

	bool addActionWhereCondition(const char* sn, int actionArgIndex) const
	{
		if (actionArgIndex < actionArgs.size()) {
			auto value = actionLeftWildCard + actionArgs[actionArgIndex] + actionRightWildCard;
			return addToWhereCondition(LogOp_AND, std::string(sn) + "." + value);
		}
		else {
			return true;
		}
	}

	DWORD        consoleMode = 0;
	int    const consoleCodePage = GetConsoleCP();
	HANDLE const stdOutHandle    = GetStdHandle(STD_OUTPUT_HANDLE);
	bool   const stdOutIsConsole = stdOutHandle != NULL && GetConsoleMode(stdOutHandle, &consoleMode);

	static const int BufSize = 32000;
	mutable char  buffer[BufSize];
	mutable DWORD bufPos = 0;

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

	void writeOutPut(std::string const & str) const 
	{
		writeOutPut(str.c_str(), str.length());
	}

	void writeOutPut(const char* str) const 
	{
		writeOutPut(str, strlen(str));
	}

	void writeOutPut(char c) const 
	{
		writeOutPut(&c, 1);
	}

	void doWriteOutPut(const char* str, int len) const
	{
		if (stdOutIsConsole) {
			auto ws = utf8ToWide(str, len);
			DWORD written;
			WriteConsole(stdOutHandle, ws.c_str(), ws.length(), &written, 0);
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
};

struct QueryBuilder {
	LittState const & ls;
	QueryBuilder(LittState const & ls) : ls(ls) {
	}
};

bool parseCommandLine(int argc, char **argv, LittState& ls)
{
	bool ok = true;
	for (int i = 1; i < argc; ++i) {
		if (argv[i][0] == '-' && argv[i][1] != '\0') { // A stand-alone '-' can be used as an (action) argument.
			auto const opt = argv[i][1];
			auto const val = std::string(argv[i][2] != '\0' ? &argv[i][2] : "");
			switch (opt) {
			case 'd':
				if (val == "col" || val == "column") ls.mode = DisplayMode::column;
				else if (val == "csv") ls.mode = DisplayMode::csv;
				else if (val == "html") ls.mode = DisplayMode::html;
				else if (val == "line") ls.mode = DisplayMode::line;
				else if (val == "tabs") ls.mode = DisplayMode::tabs;
				else if (val.substr(0, 4) == "list") {
					ls.mode = DisplayMode::list;
					if (4 < val.length()) {
						if (val[4] == ':' && 5 < val.length()) {
							ls.listSep = toUtf8(ls.consoleCodePage, val.substr(5).c_str());
						}
						else {
							goto optionError;
						}
					}
				}
				else {
					goto optionError;
				}
				break;
			case 'h':
				if (val == "on") ls.headerOn = true;
				else if (val == "off") ls.headerOn = false;
				else goto optionError;
				break;
			case 'o':
				ls.orderBy = ls.getColumns(val, ColumnsDataKind::sortOrder, true, ok);
				if (!ok) { return false; }
				break;
			case 'c': 
				ls.selectedColumns = ls.getColumns(val, ColumnsDataKind::width, true, ok);
				if (!ok) { return false; }
				break;
			case 'l': 
				ls.dbPath = val;
				break;
			case 'a': {
				auto add = ls.getColumns(val, ColumnsDataKind::width, true, ok);
				if (!ok) { return false; }
				ls.additionalColumns.insert(ls.additionalColumns.end(), add.begin(), add.end());
				}
				break;
			case 'w': 
				ls.addToWhereCondition(LogOp_OR, val);
				break;
			case 's':
				// Note: The specified columns might actually not be used in the query. 
				// Depends on the other parameters.
				for (auto& c : ls.getColumns(val, ColumnsDataKind::width, false, ok)) { 
					c.first->overriddenWidth = c.second;
				}
				if (!ok) { return false; }
				break;
			case 'q':
				ls.showQuery = true;
				break;
			case 'u':
				ls.selectDistinct = true;
				break;
			case 'x': 
				ls.explainQuery = true;
				break;
			case 'n': 
				ls.showNumberOfRows = true;
				break;
			default: optionError:
				fprintf(stderr, "Invalid option '%s'\n", argv[i]);
				return false;
			}
		}
		else {
			if (ls.action.empty()) {
				ls.action = argv[i]; 
				if (ls.action.length() >= 2 && ls.action[0] == '*') {
					ls.actionLeftWildCard = "*"; 
					ls.action.erase(0, 1);
				}
				if (ls.action.length() >= 2 && ls.action.back() == '*') {
					ls.actionRightWildCard = "*"; 
					ls.action.pop_back();
				}
			}
			else {
				ls.actionArgs.push_back(argv[i]);
			}
		}
	}

	if (ls.action.empty()) {
		fprintf(stderr, "Action argument(s) missing\n");
		return false;
	}

	if (ls.dbPath.empty()) {
		char mydocs[MAX_PATH];
		if (GetEnvironmentVariableA("MYDOCS", mydocs, MAX_PATH)) {
			ls.dbPath = std::string(mydocs) + "\\litt\\" + DefDbName;
		}
		else {
			ls.dbPath = DefDbName;
		}
	}
	if (GetFileAttributesA(ls.dbPath.c_str()) == -1) {
		fprintf(stderr, "Cannot find '%s'\n", ls.dbPath.c_str());
		return false;
	}

	return true;
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
   dump                            (Dumps the database as SQL)
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

int runSelectQuery(LittState const & ls, std::string const & sql)
{
	auto sqlUtf8 = toUtf8(ls.consoleCodePage, sql); // !!! will be done by querybuilder.....

	if (ls.showQuery) {
		// !!! also print relevant options?
		ls.writeOutPut(sqlUtf8.c_str(), sqlUtf8.length());
		ls.flushOutput();
		return 0;
	}

	auto callback = [](void *pArg, int argc, char **argv, char **azColName) {
		auto ls = static_cast<LittState const*>(pArg);
		if (ls->rowCount++ == 0) {
			for (int i = 0; i < argc; i++) {
				ls->writeOutPut(azColName[i]);
				if (i + 1 != argc) ls->writeOutPut(ls->listSep);
			}
			ls->writeOutPut('\n');
		}
		for (int i = 0; i < argc; i++) {
			ls->writeOutPut(argv[i] ? argv[i] : "");
			if (i + 1 != argc) ls->writeOutPut(ls->listSep);
		}
		ls->writeOutPut('\n');

		return 0;
	};

	sqlite3 *db = nullptr;
	int res = sqlite3_open(ls.dbPath.c_str(), &db);
	if (res != SQLITE_OK) {
		fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
		goto out;
	}

	ls.rowCount = 0;
	char *zErrMsg = nullptr;
	res = sqlite3_exec(db, sqlUtf8.c_str(), callback, const_cast<LittState*>(&ls), &zErrMsg);
	ls.flushOutput();
	if (res != SQLITE_OK) {
		fprintf(stderr, "SQL error: %s\n", zErrMsg);
		sqlite3_free(zErrMsg);
	}

	if (ls.showNumberOfRows) {
		printf("# = %i\n", ls.rowCount); // !!! also stderr output??? Need to sync console and stdio streams most likely!
	}

out:
	sqlite3_close(db);
	return res;
}

int runSingleTableOutputCmd(LittState const & ls, const char* selectedColumns, const char* table, const char* orderBy)
{
	//`gosub extractSelectedColumns %1 & gosub createOrderBy %3 & ^
	// runSqlOutputCmd %SELECT% %outSelect% from %2 WHERE %@makeWhereCond[] ORDER BY %outOrderBy%;`

	return 1;
}

int runListData(LittState const & ls, const char* selectedColumns, const char* orderBy)
{
	// gosub extractSelectedColumns %1 & gosub createOrderBy %2 & gosub listdata`
	return 1;
}

int listAuthors(LittState const & ls)
{
	if (!ls.addActionWhereCondition("ln", 0) && ls.addActionWhereCondition("fn", 1)) {
		return false;
	}
	if (ls.action == "a") {
		return runSingleTableOutputCmd(ls, "ai.ln.fn", "Authors", "ai");
	}
	else {
		return runListData(ls, "bi.ln.fn.bt.dr.so.ge", "ln.fn.dr.bi");
	}
}

int listBooks(LittState const & ls)
{
	char *sql =
"SELECT Books.\"BookID\", ltrim(\"First Name\" || ' ' || \"Last Name\") AS \"Author\", \"Title\", \"Date read\", \"Source\", \"Genre\" FROM Books\n\
INNER JOIN DatesRead USING(BookID)\n\
INNER JOIN AuthorBooks USING(BookID)\n\
INNER JOIN Authors USING(AuthorID)\n\
LEFT OUTER JOIN Sources USING(SourceID)\n\
LEFT OUTER JOIN BookGenres USING(BookID)\n\
LEFT OUTER JOIN Genres USING(GenreID)\n\
WHERE(1 == 1)\n\
ORDER BY \"Date read\", Books.\"BookID\", \"Last Name\", \"First Name\", \"Title\";";

	return runSelectQuery(ls, sql);
}

// Making a query:
// Inputs: 
// - default col widths
// - defCols (can include widths) and defColOrder from the action, always there
// - selColumns, colOrder, addColumns, colWidths, main whereCond values, extra whereConds from the command line, all optional, will have precedense over def values
// Ouptputs:
// - an SQL query string
// - will set "active" widths according to inputs (should likely be an array that corresponds to the selected columns (in same order)), only needed for column mode
// Intermediate state:
// - will need access to all referenced columns (selected, where, order by) while building the query (calling Add* methods) as some additions will depend on this.
// NOTES:
// - Currently a bug in btmlitt to use selColumns as colOrder if no colOrder, as selColumns can include widths, and that will cause an error (no such short name!)

int main(int argc, char **argv)
{
	if (argc <= 1) {
		return showHelp();
	}

	LittState ls;
	if (!parseCommandLine(argc, argv, ls)) {
		return ERROR_BAD_ARGUMENTS;
	}

	auto const & action = ls.action;
	if (action == "h") {
		return showHelp(true);
	}
	else if (action == "a" || action == "aa") {
		return listAuthors(ls);
	}
	else if (action == "b" || action == "bb") {
		return listBooks(ls);
	}
	else {
		fprintf(stderr, "Invalid action '%s'\n", action.c_str());
		return ERROR_BAD_ARGUMENTS;
	}
}
