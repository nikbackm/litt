/** LITT - now for C++! ***********************************************************************************************

Changelog:
 * 2017-06-13: Speeded up ybca/g/s by using temporary memory tables instead of nested queries. Greatest speedup came from
               being able to use rowId:s for joining year tables instead of generating RowNumbers in year queries via 
               count(*) hack.
 * 2017-06-12: Now sorts aa by Date Read too, just like bb, and not via Last Name.
 * 2017-06-12: Added ybca/g/s; Yearly book counts (top lists) for authors, genres and sources.
 * 2017-06-11: bi har nu inte längre "Books." med i namnet => funkar med ansi och cons. Nyare SQLITE verkar inte 
               behöva det för "ambiguity".
 * 2017-06-08: Now uses regex_search instead of regex_match also for --cons (already used it for --ansi) => no need to 
               specify trailing and/or preceding ".*" to match the whole value.
               Converts regex values to utf-8 before using them.
 * 2017-06-08: Added ANSI color support via --ansi option.
 * 2017-06-07: Now uses blue text on input.
 * 2017-06-05: Can escape options value separators with the escape character '!'. Is also used to escape itself. ("re!" => "ren")
               Also made option parser more robust in general, will no longer allow empty option values (which end parsing!).
 * 2017-06-05: Bug fix: Should not ORDER BY the label either, will fail if the column with the label is not included in SELECT
               even if the column definition itself works as ORDER BY. So label should really only be used as the "AS" value
               in SELECT. Also when referring to cons columns since cons only see the returned column names as it's done now.
 * 2017-06-05: Allow to specify column width zero also for -s, already worked for -a and -c. Zero width useful for e.g. sec!
               Also avoids printing the column at all when width = 0 (don't want the column separator shown then either!)
               (Will still get an unneeded column separator if the first column is zero-width though)
 * 2017-06-04: Added time range (diff smaller/greater) to consecutive row matching.
               Added new sec (virtual) column containing "Date Read" as TotalSeconds (since 1970).
               Bug fix: Need to check for "dw.dwl.ti.sec" too when adding DatesRead in addAuxTables.
 * 2017-06-03: Added consecutive row matching according to user-selected criteria.
 * 2017-06-02: Added samestory and titlestory. Calculates column widths automatically if missing.
 * 2017-06-02: Can now input the same author several times for the same book (in case s/he contributes several stories).
               This also worked on oldlitt, but that was only because it ignored errors from individual rows (because
               it was running by piping to sqlite shell), and it still showed error messages.
 * 2017-06-01: Input methods now complete and somewhat tested too! Many improvements compared to oldlitt!
 * 2017-05-29: Added listSourceBookcounts (sbc)
 * 2017-05-29: abc and gbc now supports generic where conditions!
 * 2017-05-29: Added HTML and tabs display output modes. Decided to skip csv and line modes.
 * 2017-05-29: Added the "range" operator to -w options as well. 
 * 2017-05-29: Can now search for things containing "'"!
 * 2017-05-28: Listing of books per period now working and better than before, can add use WHERE in addition to HAVING 
               for the total. Also moved HAVING to the columns instead, more usable that way! 
 * 2017-05-27: Now have column and list display modes working. File output format is utf-8 with bom (bom for V/VIEW).
 * 2017-05-26: Decided to use exceptions for error handling after all!
 * 2017-05-24: Decided to use WriteConsoleW for writing (converted) utf-8 output. Least problems and faster too.
 * 2017-05-22: Have now decided to finish it and no longer depend on TCC and the SQLite shell. Also for a faster LITT!
 * 2017-05-20: First tests started. Not using Modern C++ so far though, not for SQLite parts at least.
 * 2017-05-19: Got inpired by Kenny Kerr's PluralSight course "SQlite with Modern C++".
 * Previous:   Refer to the litt.btm changelog.

**********************************************************************************************************************/

#include "stdafx.h"

void showHelp(bool showExtended = false)
{
	printf(
R"(Usage: LITT [<options>] action <action arguments>

Actions:
   a   [<last name>] [<first name>] (Lists authors with given last name (and first name)).
   aa  [<last name>] [<first name>] (Same as above, but also includes all books by the authors).
   b   [<title>]                    (Lists all books matching the given title).
   bb  [<title>]                    (Same as above, but includes more details)
   ot  [<origTitle>]                (Only lists books with original titles)
   st  [<story>]                    (Only lists books with separate stories)
   s   [<series>]                   (Lists series)
   g   [<genre>]                    (Lists genre)
   so  [<source>]                   (Lists book sources - where a certain book "read" was gotten)
   soo [<source>]                   (Lists book sources WITH read books for the sources)

   add-a                            (Adds a new author)
   add-b                            (Adds a new book)
   add-s                            (Adds a new series)
   add-g                            (Adds a new genre)

   abc [<bookCountCond>] [<bRRs>]   (Lists the number of read books for each author, second param = 1 => re-reads included.
                                     Supports virtual column bc - book count - for column selection, sorting and where)
   gbc [<bookCountCond>] [<bRRs>]   (Lists the number of read books for each genre, similar to abc)
   sbc [<bookCountCond>] [<bRRs>]   (Lists the number of read books for each book source, similar to abc)
   ybca/ybcg/ybcs [#] [fy] [ly]     (Lists yearly book counts for authors, genres and sources. Args are row count, first and last year) 
   brd [<booksReadCond>]            (Lists the dates and books where [cond] books where read.)
   brm/bry/brmy/brym/brwd [...]     (Lists the number of books read per month/year/etc. Supports extra virtual column prc in in -w))"
	);
	if (showExtended) {
		printf(
R"(

       [<dateCondition>] {<column def(where condition)> <column name>}
       
       Can also use brp which is more generic, it takes [<period col def(strftime string)>] [<period col name]>
       BEFORE the parameters the previos ones do.
)"
		);
	}
	printf(
R"(
   rereads                          (Lists re-read books. Can use extra virtual column "brc" - Read Count)
   sametitle                        (Lists books with same title. Can use extra virtual column "btc" - Book Title Count)
   samestory                        (Lists stories with same title - Shows duplicates)
   titlestory                       (Lists books with same title as a story - Shows duplicates)
   
   b2s <BookID> <SeriesID> <part>   (Adds a book to a series)
   
   set-dr <BookID> [C|D CurGenreID] (Add, Change or Delete 'date read' for a book. Need to specify current dr for C and D)
   set-g <BookID> [C|D CurGenreID]  (Add, Change or Delete genre for a book. Need to specify current GenreID for C and D)

   h                                (Show more extensive help)
   
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

    --cons:<minRowCount>:{<colSnOrName>[:re|ren:<regExValue>]|[:dlt|dgt:<diffValue>]}+
                    Specifies column conditions for consecutive output row matching.
                    If no explicit method is specified then matching is done by comparing against the
                    same column value of the previous row.
                    The dlt/dgt diff matching is only supported for the "sec" column.)

    --ansi[:off:<boolInt>][:defC:<ansiC>][:colC:<col>:<ansiC>][:valC:<colVal>:<regExValue>:col{.col}:<ansiC>}
                    Specifies ANSI colors for columns, rows and specific values. Only enabled in column display mode.
                    * off  : Turn off ANSI coloring. Default is on when --ansi is specified.
                    * defC : Specify default color for all values.
                    * colC : Specify ANSI color for given column (either given as short name or full name).
                    * valC : Specify ANSI color for the given columns when the value of the given value
                             columns matches the included regex.

    For escaping option separators the escape character '!' can be used. It's also used to escape itself.
)"
	);
	if (showExtended) {
		printf(
R"(
selColumns format: <shortName>[.<width>]{.<shortName>[.<width>]}
colOrder format: <shortName>[.asc|desc]{.<shortName>[.asc|desc]}
whereCond format: <shortName>[.<cmpOper>].<cmpArg>{.<shortName>[.<cmpOper>].<cmpArg>}
          cmpOper: lt,gt,eq,nq,isnull,isempty,range ("LIKE" if none is given, isnull & isempty take no cmpArg, range takes two)
colSizes format: Same as selColumns format

bookCountCond and booksReadCond formats:
    <number>        Only includes authors with book count >= <number>
    lt.<number>     Only includes authors with book count < <number>
    gt.<number>     Only includes authors with book count > <number>
    eq.<number>     Only includes authors with book count = <number>
    range.<n1>.<n2> Only includes authors with book count in range [n1,n2]

DisplayMode values:
    col/column  Left-aligned columns (Specified or default widths are used)
    html        HTML table code
    htmldoc     Full HTML document
    list[:sep]  Values delimited by separator string, default is "|"
    tabs        Tab-separated values

Column short name values:
    bt,bi,btl       - Book title, BookID, length of book title
    ln,fn,lnl,fnl   - Last/First Name & length thereof
    nn/ng           - Full name/Aggregated full name(s) per book.
    gi,ge           - GenreID and Genre
    dr/dg,dw,dwl,ti,sec
                    - Date read/Aggregated dates, DOW for Date read, DOW string, Time, TotalSeconds
    own,la,beb      - Owned, Language, Bought Ebook
    st,stid         - Story,StoryID
    se,si,sp        - Series, SeriesID, Part in Series
    so,soid         - Source, SourceID
)"
		);
	}
}

namespace std
{
	template<> struct default_delete<sqlite3> { void operator()(sqlite3* ptr) { sqlite3_close(ptr); } };
}

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

	bool toULongLong(std::string const & str, unsigned long long & value)
	{
		char* endPtr;
		auto v = strtoull(str.c_str(), &endPtr, 10);
		if (errno == ERANGE || (endPtr != str.c_str() + str.length())) {
			return false;
		}
		value = v;
		return true;
	}

	void replaceAll(std::string& str, const std::string& from, const std::string& to)
	{
		if (from.empty()) return;
		for (size_t pos = 0; (pos = str.find(from, pos)) != std::string::npos; pos += to.length()) {
			str.replace(pos, from.length(), to);
		}
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

	std::string fromUtf8(int codePage, std::string const & str)
	{
		auto wstr = toWide(CP_UTF8, str);
		return toNarrow(codePage, wstr.c_str(), wstr.length());
	}

	std::string fmt(_In_z_ _Printf_format_string_ const char* fmtStr, ...)
	{
		va_list ap;
		va_start(ap, fmtStr);
		size_t size = vsnprintf(nullptr, 0, fmtStr, ap) + 1; // Extra space for '\0'
		va_end(ap);
		std::string res(size, '\0');
		va_start(ap, fmtStr);
		vsnprintf(&res[0], size, fmtStr, ap);
		res.pop_back(); // Remove the trailing '\0'
		va_end(ap);
		return res;
	}

	std::string quote(std::string const & str)
	{
		return (!str.empty() && str.front() != '"') ? ("\"" + str + "\"") : str;
	}

	std::string unquote(std::string const & str)
	{
		auto res = str;
		if (res.length() >= 2 && res.front() == '"' && res.back() == '"') {
			res.erase(0, 1);
			res.pop_back();
		}
		return res;
	}

	// Escape the SQL value and add the SQL quotes (if needed).
	std::string escSqlVal(std::string str, bool tryToTreatAsNumeric = false)
	{
		int intVal;
		if (!(tryToTreatAsNumeric && toInt(str, intVal))) {
			replaceAll(str, "'", "''");
			str = "'" + str + "'";
		}
		return str;
	}

	void toLowerCase(std::string& str)
	{
		std::transform(str.begin(), str.end(), str.begin(), [](char c) { return static_cast<char>(tolower(c)); });
	}

	bool enableVTMode()
	{
		HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
		if (hOut == INVALID_HANDLE_VALUE) { return false; }
		DWORD dwMode = 0;
		if (!GetConsoleMode(hOut, &dwMode)) { return false; }
		return !!SetConsoleMode(hOut, dwMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING); // TODO: Only do this on supported Windows 10 editions?
	}

	SYSTEMTIME GetSystemTime()
	{
		SYSTEMTIME st{}; ::GetSystemTime(&st);
		return st;
	}
} // Utils
using namespace Utils;

namespace LittDefs
{
	using IdValue = unsigned long long;

	enum class DisplayMode {
		column,
		html,
		htmldoc,
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

	const char*   DefDbName = "litt.sqlite";
	const char    OptDelim = '.';
	const char    OptExtDelim = ':';
	const char    Wc = '*';
	const char*   WcS = "*";
	const char*   LogOp_OR = " OR ";
	const char*   LogOp_AND = " AND ";
	const IdValue EmptyId = 0;

	// Replace our wildcard with SQL's wildcard. Also escape and add SQL quoting if needed.
	std::string likeArg(std::string str, bool tryToTreatAsNumeric = false)
	{
		std::replace(str.begin(), str.end(), Wc, '%');
		return escSqlVal(str, tryToTreatAsNumeric);
	}

	unsigned colWidth(std::string const & str)
	{
		auto const w = str.length();
		return (w > 2) ? w - 2 : w; // Don't include quotes in column width, they will not be printed.
	}

	bool toSecondsValue(std::string const & str, unsigned long long & value)
	{
		if (str[0] == '-' || str[0] == '\0') {
			return false;
		}
		return toULongLong(str, value);
	}

	bool toIdValue(std::string const & str, IdValue& value) { return toULongLong(str, value); }

	// Note: All member value types are chosen/designed so that zero-init will set the desired default.
	struct ColumnInfo {
		// These values are pre-configured:
		char const* nameDef; // Name or definition for column
		int         defWidth;
		ColumnType  type;
		char const* label; // optional, used when name does not refer to a direct table column.
		bool        isGroupAggregate;

		// These values are set at runtime. Stored here for convenience.
		mutable int  overriddenWidth;
		mutable bool usedInQuery;

		const char* labelName() const { return label != nullptr ? label : nameDef; }

		std::string getLikeArg(std::string val) const
		{
			return likeArg(std::move(val), type == ColumnType::numeric);
		}
	};

	// A collection of columns including some integer data (width, sortOrder).
	using Columns = std::vector<std::pair<ColumnInfo const *, int>>;

	enum class ColumnsDataKind {
		none,
		width,
		sortOrder
	};
}
using namespace LittDefs;

namespace Input
{
	enum InputOptions : unsigned {
		none     = 0x00,
		optional = 0x00,
		required = 0x01,
	};

	std::string readLine() {
		CONSOLE_SCREEN_BUFFER_INFO info{};
		HANDLE const hOut = GetStdHandle(STD_OUTPUT_HANDLE);
		GetConsoleScreenBufferInfo(hOut, &info);
		DWORD const fgMask = FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_RED | FOREGROUND_INTENSITY;
		DWORD const bgMask = BACKGROUND_BLUE | BACKGROUND_GREEN | BACKGROUND_RED | BACKGROUND_INTENSITY;
		std::string str;
		// Set text color to blue, keep background.
		SetConsoleTextAttribute(hOut, FOREGROUND_BLUE | FOREGROUND_INTENSITY | (bgMask & info.wAttributes));
		std::getline(std::cin, str);
		// Restore the previous text color.
		SetConsoleTextAttribute(hOut, info.wAttributes & (fgMask | bgMask));
		return str;
	}

	void prefillInput(std::string const& str)
	{
		std::vector<INPUT_RECORD> recs(str.size());
		for (size_t i = 0; i < str.size(); ++i) {
			recs[i].EventType = KEY_EVENT;
			recs[i].Event.KeyEvent.bKeyDown = true;
			recs[i].Event.KeyEvent.wRepeatCount = 1;
			recs[i].Event.KeyEvent.dwControlKeyState = 0;
			recs[i].Event.KeyEvent.wVirtualKeyCode = 0;
			recs[i].Event.KeyEvent.wVirtualScanCode = 0;
			recs[i].Event.KeyEvent.uChar.AsciiChar = str[i];
		}
		DWORD out = 0;
		if (!WriteConsoleInput(GetStdHandle(STD_INPUT_HANDLE), recs.data(), recs.size(), &out)) {
			fprintf(stderr, "Failed to prefill input! Error code = %i", GetLastError());
		}
	}

	std::string input(const char* prompt)
	{
		printf("%s: ", prompt);
		return readLine();
	}

	void input(std::string& value, const char* prompt, InputOptions options = none, const char* regex = nullptr)
	{
		if (!value.empty()) { prefillInput(value); }
	retry:
		value = input(prompt);
		if (value.empty() && (required & (unsigned)options)) {
			goto retry;
		}
		if (!value.empty() && regex != nullptr) {
			if (!std::regex_match(value, std::regex(regex))) {
				prefillInput(value);
				goto retry;
			}
		}
	}

	void input(
		IdValue& value, 
		const char* prompt,
		std::function<std::string(IdValue)> checkId,
		InputOptions options = none, 
		std::function<void(std::string const &)> onInvalidInput = nullptr)
	{
		if (value != EmptyId) { prefillInput(std::to_string(value)); }
	retry:
		auto str = input(prompt);
		value = EmptyId;
		if (!str.empty()) {
			toIdValue(str, value);
			if (value == EmptyId && onInvalidInput) {
				onInvalidInput(str);
				goto retry;
			}
			else try { checkId(value); } catch (std::exception&) {
				printf("Invalid id for this column, please try again.\n");
				goto retry;
			}
		}
		if (value == EmptyId && (required & (unsigned)options)) {
			goto retry;
		}
	}

	int ask(const char* validAnswers, std::string const & question, int& defAndRes )
	{
		bool foundDefault = false;
		int const len = strlen(validAnswers);
		printf("%s? (", question.c_str());
		for (int i = 0; i < len; ++i) {
			if (i != 0) putc('/', stdout);
			int c = tolower(validAnswers[i]);
			if (defAndRes == c) {
				foundDefault = true;
				c = toupper(c);
			}
			putc(c, stdout);
		}
		printf(") ");

		int answer;
		for (;;) {
			answer = tolower(_getch());
			if (answer == '\r' && foundDefault) {
				answer = defAndRes;
				goto done;
			}
			for (int i = 0; i < len; ++i) {
				if (answer == tolower(validAnswers[i])) {
					defAndRes = answer;
					goto done;
				}
			}
		}
	done:
		printf("\n"); _ASSERT(answer == defAndRes);
		return answer;
	}

	int ask(const char* validAnswers, std::string const & question)
	{
		int def = 0; return ask(validAnswers, question, def);
	}

	bool confirm(std::string const & question)
	{
		int def = 0; return 'y' == ask("yn", question, def);
	}
}
using namespace Input;

struct OptionParser {
	std::stringstream m_ss;
	const char* const m_type;
	const char        m_delim;
	OptionParser(std::string const & value, const char* type = "option", char delim = OptDelim)
		: m_ss(value), m_type(type), m_delim(delim)
	{}

	bool empty() const
	{
		return m_ss.eof();
	}

	bool getNext(std::string& next)
	{
		if (m_ss.eof()) { return false; }

		const int EscapeChar = '!';
		bool escOn = false;

		std::string str; char c;
		while (m_ss.get(c)) {
			if (c == EscapeChar) {
				if (escOn) { str.push_back(EscapeChar); }
				escOn = !escOn;
			}
			else if (c == m_delim) {
				if (escOn) {
					str.push_back(m_delim);
					escOn = false;
				}
				else {
					break; // delim found
				}
			}
			else {
				if (escOn) { throw std::invalid_argument("Illegal escape sequence"); }
				str.push_back(c);
			}
		}
		if (escOn) { throw std::invalid_argument("Incomplete escape sequence"); }
		if (str.empty()) { throw std::invalid_argument("Empty option values not allowed"); }

		next = std::move(str);
		return true;
	}
	
	__declspec(noreturn) void throwError() 
	{
		throw std::invalid_argument(fmt("Faulty %s value: %s", m_type, m_ss.str().c_str()));
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

class Output {
	DWORD        m_consoleMode = 0;
	int    const m_consoleCodePage = GetConsoleCP();
	HANDLE const m_stdOutHandle    = GetStdHandle(STD_OUTPUT_HANDLE);
	bool   const m_stdOutIsConsole = m_stdOutHandle != NULL && GetConsoleMode(m_stdOutHandle, &m_consoleMode);
	// Got missing WriteConsole output with 32K buffer! 20K seems ok so far... but uses 10K to avoid analyse warning..
	// 32K seems to work at home with Win10 though, but not at work with Win7.
	static const int BufSize = 1000*10;
	mutable char m_buffer[BufSize];
	mutable int  m_bufPos = 0;
public:
	bool stdOutIsConsole() const { return m_stdOutIsConsole; }

	void write(const char* str, int len) const
	{
		if (BufSize < len) {
			flush();
			doWrite(str, len);
		}
		else {
			if ((BufSize - m_bufPos) < len) {
				flush();
			}
			_ASSERT(len <= (BufSize - m_bufPos));
			memcpy(&m_buffer[m_bufPos], str, len);
			m_bufPos += len;
		}
	}

	void write(char c) const
	{
		if (BufSize == m_bufPos) {
			flush(); Assert(m_bufPos == 0);
		}
		m_buffer[m_bufPos++] = c;
	}

	void write(std::string const & str) const 
	{
		write(str.c_str(), str.length());
	}

	void write(const char* str) const 
	{
		write(str, strlen(str));
	}

	void writeUtf8Width(const char* str, unsigned width) const
	{ // PRE: str contains only complete utf-8 code points.
		unsigned writtenChars = 0;

		for (int i = 0; str[i] != '\0' && writtenChars < width; /*inc in body*/) {
			if ((str[i] & 0x80) == 0) { // 0xxx xxxx - Single utf8 byte
				write(str[i]);
				i += 1;
			}
			else if ((str[i] & 0xC0) == 0xC0) { // 11xx xxxx - utf8 start byte
				int n = 1;
				while ((str[i + n] & 0xC0) == 0x80) { // 10xx xxxx - continuation byte
					++n;
				}
				if (n == 1) throw std::invalid_argument("No utf-8 continuation byte(s)!");
				if (4 < n) throw std::invalid_argument("Too many utf-8 continuation bytes!");
				write(&str[i], n);
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
			write(' ');
			++writtenChars;
		}
	}

	void writeHtml (const char* z) const 
	{
		int i;
		while( *z ) {
			for (i = 0; z[i] !='\0'
					 && z[i] !='<'
					 && z[i] !='&'
					 && z[i] !='>'
					 && z[i] !='\"'
					 && z[i] !='\'';
				i++) {}

			if (i > 0) {
				write(z, i);
			}

			if      (z[i]=='<')  { write("&lt;");   } 
			else if (z[i]=='&')  { write("&amp;");  }
			else if (z[i]=='>')  { write("&gt;");   } 
			else if (z[i]=='\"') { write("&quot;"); } 
			else if (z[i]=='\'') { write("&#39;");  } 
			else                 { break;                 }

			z += i + 1;
		}
	}

	void doWrite(const char* str, int len) const
	{
		if (len == 0) return;
		if (m_stdOutIsConsole) {
			auto ws = utf8ToWide(str, len);
			DWORD written;
			if (!WriteConsole(m_stdOutHandle, ws.c_str(), ws.length(), &written, 0)) {
				throw std::runtime_error("WriteConsole failed with error code: " + std::to_string(GetLastError()));
			}
		}
		else {
			if (fwrite(str, len, 1, stdout) != 1) {
				throw std::runtime_error("fwrite failed to write all data, errno: " + std::to_string(errno));
			}
		}
	}

	void flush() const 
	{
		doWrite(m_buffer, m_bufPos);
		m_bufPos = 0;
	}

	void flushNoThrow() const
	{
		try { 
			flush(); 
		}
		catch (std::exception& ex) { 
			fprintf(stderr, "\nflushOutput failed: %s\n", ex.what()); 
		}
	}
};

class Litt {
	// Maps short name to column info.
	std::map<std::string, ColumnInfo> m_columnInfos;
	std::unique_ptr<sqlite3> m_conn;
	Output m_output;
public:
	int const consoleCodePage = GetConsoleCP();
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

	mutable std::string m_whereCondition;
	mutable std::string m_havingCondition;

	std::string m_action;
	std::vector<std::string> m_actionArgs;
	std::string m_actionRightWildCard;
	std::string m_actionLeftWildCard;

	mutable int m_rowCount = 0; // The number of rows printed so far.

	Litt(int argc, char** argv) :
		m_columnInfos({ // OBS! As a sn, don't use "desc", "asc" and any other name that may appear after one in the command line options!
			{"ai",   {"Authors.AuthorID", 8, ColumnType::numeric}},
			{"beb",  {"\"Bought Ebook\"", 3, ColumnType::numeric}},
			{"bi",   {"BookID", 6, ColumnType::numeric}},
			{"bt",   {"Title", 40 }},
			{"dr",   {"\"Date Read\"", 10 }},
			{"dg",   {"\"Date(s)\"", 30, ColumnType::text, nullptr, false }},
			{"fn",   {"\"First Name\"",15 }},
			{"ge",   {"Genre", 25 }},
			{"gi",   {"GenreID", 8, ColumnType::numeric }},
			{"ln",   {"\"Last Name\"",15 }},
			{"ng",   {"\"Author(s)\"", 35, ColumnType::text, nullptr, false }},
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
			{"ti",   {"time(\"Date Read\")", 5, ColumnType::text, "Time" }},
			{"sec",  {"strftime('%s',\"Date Read\")", 11, ColumnType::numeric, "Timestamp" }},

			// Some special-purpose virtual columns, these are not generally usable:

			// Intended for use in -w for listBooksReadPerPeriod. Will end up in the HAVING clause of Total sub-query.
			{"prc", {"Count(BookID)", 0, ColumnType::numeric, nullptr, true }},

			// Intended for use in listSametitle
			{"btc", {"TitleCount", 11, ColumnType::numeric, "\"Title count\"", false }},
			// Intended for use in listRereads
			{"brc", {"ReadCount", 10, ColumnType::numeric, "\"Read count\"", false }},

			// This is for the "number of books" column in list*BookCounts.
			{"bc",  {"COUNT(Books.BookID)", 6, ColumnType::numeric, "Books", true }},
	})
	{
		for (auto& ci : m_columnInfos) { ci.second.overriddenWidth = -1; }

		for (int i = 1; i < argc; ++i) {
			if (argv[i][0] == '-' && argv[i][1] != '\0') { // A stand-alone '-' can be used as an (action) argument.
				auto const opt = argv[i][1];
				auto const val = std::string(argv[i][2] != '\0' ? &argv[i][2] : "");
				switch (opt) {
				case 'd':
					if (val == "col" || val == "column") displayMode = DisplayMode::column;
					else if (val == "html")    displayMode = DisplayMode::html;
					else if (val == "htmldoc") displayMode = DisplayMode::htmldoc;
					else if (val == "tabs")    displayMode = DisplayMode::tabs;
					else if (val.substr(0, 4) == "list") {
						displayMode = DisplayMode::list;
						if (4 < val.length()) {
							if (val[4] == ':' && 5 < val.length()) {
								listSep = toUtf8(val.substr(5).c_str());
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
					if (val == "on") headerOn = true;
					else if (val == "off") headerOn = false;
					else throw std::invalid_argument("Invalid header value: " + val);
					break;
				case 'o':
					orderBy = getColumns(val, ColumnsDataKind::sortOrder, true);
					break;
				case 'c': 
					selectedColumns = getColumns(val, ColumnsDataKind::width, true);
					break;
				case 'l': 
					dbPath = val;
					break;
				case 'a': {
					auto a = getColumns(val, ColumnsDataKind::width, true);
					additionalColumns.insert(additionalColumns.end(), a.begin(), a.end());
					}
					break;
				case 'w': 
					appendToWhereCondition(LogOp_OR, getWhereCondition(val));
					break;
				case 's':
					// Not necessarily included in the query, hence "false".
					for (auto& c : getColumns(val, ColumnsDataKind::width, false)) { 
						c.first->overriddenWidth = c.second;
					}
					break;
				case 'q':
					showQuery = true;
					break;
				case 'u':
					selectDistinct = true;
					break;
				case 'x': 
					explainQuery = true;
					break;
				case 'n': 
					showNumberOfRows = true;
					break;
				case '-': { // Extended option 
					OptionParser extVal(val, "extended option", OptExtDelim);
					auto const extName = extVal.getNext();
					if (extName == "cons") {
						m_consRowMinCount = extVal.nextInt();
						for (std::string colName = extVal.getNext(); ; ) {
							colName = getColumnName(colName);
							ConsRowColumnInfo col;
							col.name = colName; colName.clear();
							col.index = -1; // Will be set when we get the first callback
							col.matchMethod = ConsRowMatchMethod::columnValue;
							std::string mm;
							if (extVal.getNext(mm)) {
								if (mm == "regex" || mm == "re" || mm == "ren") {
									col.matchMethod = (mm == "ren")
										? ConsRowMatchMethod::regExNot : ConsRowMatchMethod::regEx;
									col.re = getRegex(extVal.getNext());
									extVal.getNext(colName);
								}
								else if (mm == "dlt" || mm == "dgt") {
									if (col.name != getColumn("sec")->labelName()) {
										throw std::invalid_argument("Diff match not valid for column " + col.name);
									}
									col.matchMethod = (mm == "dlt")
										? ConsRowMatchMethod::diffLt : ConsRowMatchMethod::diffGt;
									col.diff = extVal.nextInt();
									extVal.getNext(colName);
								}
								else {
									colName = mm; // use as name next iteration
								}
							}
							m_consRowColumns.push_back(col);
							if (colName.empty()) break;
						};
					}
					else if (extName == "ansi") {
						m_ansiEnabled = true; // On by default when using ansi option
						std::string subOpt;

						auto nextColor = [&]() 
						{
							auto val = extVal.getNext();
							return (val[0] == '\x1b') ? val : "\x1b[" + val;
						};

						while (extVal.getNext(subOpt)) {
							toLowerCase(subOpt);
							if (subOpt == "off") {
								m_ansiEnabled = (extVal.nextInt() == 0);
							}
							else if (subOpt == "defc") {
								m_ansiDefColor = nextColor();
							}
							else if (subOpt == "colc") {
								AnsiColumnColor acc;
								acc.colName = getColumnName(extVal.getNext());
								acc.ansiColor = nextColor();
								m_ansiColColors.push_back(acc);
							}
							else if (subOpt == "valc") {
								AnsiValueColor avc;
								avc.colName = getColumnName(extVal.getNext());
								avc.rowValueRegEx = getRegex(extVal.getNext());
								auto coloredCols = OptionParser(extVal.getNext(), "column", OptDelim);
								do {
									avc.coloredColumns.push_back(getColumnName(coloredCols.getNext()));
								} while(!coloredCols.empty());
								avc.ansiColor = nextColor();
								m_ansiValueColors.push_back(avc);
							}
							else {
								throw std::invalid_argument("Unrecognized ansi sub-option: " + subOpt);
							}
							// TODO: header colors/style? just use defColor and bold?
						}
					}
					else throw std::invalid_argument("Unrecognized extended option: " + extName);
					}
					break;
				default:
					throw std::invalid_argument(std::string("Unrecognized option: ") + argv[i]);
				}
			}
			else {
				if (m_action.empty()) {
					m_action = argv[i];
					if (m_action.length() >= 2 && m_action[0] == Wc) {
						m_actionLeftWildCard = WcS;
						m_action.erase(0, 1);
					}
					if (m_action.length() >= 2 && m_action.back() == Wc) {
						m_actionRightWildCard = WcS;
						m_action.pop_back();
					}
				}
				else {
					m_actionArgs.push_back(argv[i]);
				}
			}
		}

		if (m_action.empty()) {
			throw std::invalid_argument("Action argument(s) missing");
		}

		if (dbPath.empty()) {
			char mydocs[MAX_PATH];
			if (GetEnvironmentVariableA("MYDOCS", mydocs, MAX_PATH)) {
				dbPath = std::string(mydocs) + "\\litt\\" + DefDbName;
			}
			else {
				dbPath = DefDbName;
			}
		}
		if (GetFileAttributesA(dbPath.c_str()) == -1) {
			throw std::invalid_argument("Cannot find: " + dbPath);
		}
			
		sqlite3* conn = nullptr;
		int res = sqlite3_open(dbPath.c_str(), &conn); m_conn.reset(conn);
		if (res != SQLITE_OK) {
			throw std::runtime_error(fmt("Cannot open database: %s", sqlite3_errmsg(conn)));
		}
		executeSql("PRAGMA foreign_keys = ON", nullptr, nullptr, false);
	}

	ColumnInfo const* getColumn(std::string const & sn) const
	{
		auto it = m_columnInfos.find(sn);
		if (it == m_columnInfos.end()) {
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

	std::string getColumnName(std::string const snOrName)
	{
		auto colInfo = m_columnInfos.find(snOrName);
		return (colInfo != m_columnInfos.end())
			? unquote(colInfo->second.labelName()) 
			: snOrName;
	}

	std::regex getRegex(std::string const & reVal)
	{
		return std::regex(
			toUtf8(reVal),
			std::regex_constants::ECMAScript |
			std::regex_constants::optimize |
			std::regex_constants::nosubs);
	}

	std::string getWhereCondition(std::string const & value) const // Will also update included columns!
	{
		OptionParser opts(value, "where");
		std::string wcond;
		std::string hcond;

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
			else if (val == "isnull" || val == "isempty" || val == "range") oper = val;

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
			std::string colName(col->nameDef); 

			if      (oper == "notlike") snCond = "ifnull(" + colName + ", '') NOT LIKE " + val;
			else if (oper == "isnull")  snCond = colName + " IS NULL";
			else if (oper == "isempty") snCond = colName + " = ''";
			else if (oper == "range")   snCond = val + " <= " + colName + " AND " + colName + " <= " + col->getLikeArg(opts.getNext());
			else                        snCond = colName + " " + oper + " " + val;

			auto appendSnCondTo = [&](std::string& c) { c = ((c.empty()) ? snCond : c + LogOp_AND + snCond); };
			appendSnCondTo(col->isGroupAggregate ? hcond : wcond);
		}

		appendToHavingCondition(LogOp_OR, hcond); // HACK: always merged, not returned. Works for now though!
		return wcond;
	}

	static std::string parseCountCondition(std::string const & name, std::string const & value)
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

	static std::string appendConditions(const char* logicalOp, std::string const & cond, std::string const & cond2)
	{
		if (cond.empty()) return cond2;
		if (cond2.empty()) return cond;
		return "(" + cond + ")" + logicalOp + "(" + cond2 + ")";
	}

	void appendToHavingCondition(const char* logicalOp, std::string const & condition) const
	{
		m_havingCondition = appendConditions(logicalOp, m_havingCondition, condition);
	}

	void addCountCondToHavingCondition(const char* sn, std::string const & countCond) const
	{
		appendToHavingCondition(LogOp_OR, parseCountCondition(getColumn(sn)->nameDef, countCond));
	}

	void appendToWhereCondition(const char* logicalOp, std::string const & condition) const
	{
		m_whereCondition = appendConditions(logicalOp, m_whereCondition, condition);
	}

	void resetWhere() const
	{
		m_whereCondition.clear();
	}

	void addActionWhereCondition(const char* sn, std::string const & cond) const
	{
		if (!cond.empty()) {
			auto val = m_actionLeftWildCard + cond + m_actionRightWildCard;
			auto col = getColumn(sn);
			col->usedInQuery = true;
			appendToWhereCondition(LogOp_AND, std::string(col->nameDef) + " LIKE " + col->getLikeArg(val));
		}
	}

	void addActionWhereCondition(const char* sn, unsigned actionArgIndex) const
	{
		addActionWhereCondition(sn, arg(actionArgIndex)); 
	}

	std::string arg(unsigned index, const char* def = "") const 
	{
		return index < m_actionArgs.size() ? m_actionArgs[index] : def;
	}

	IdValue idarg(unsigned index, const char* name) const 
	{
		IdValue val;
		return index < m_actionArgs.size()
			? (toIdValue(m_actionArgs[index], val)
				? val
				: throw std::invalid_argument(fmt("Invalid %s value!", name)))
			: throw std::invalid_argument(fmt("%s argument missing!", name));
	}

	int intarg(unsigned index, const char* name) const 
	{
		int val;
		return index < m_actionArgs.size()
			? (toInt(m_actionArgs[index], val)
				? val
				: throw std::invalid_argument(fmt("Invalid %s value!", name)))
			: throw std::invalid_argument(fmt("%s argument missing!", name));
	}

	int intarg(unsigned index, const char* name, int def) const 
	{
		return index < m_actionArgs.size()
			? intarg(index, name) 
			: def;
	}

	std::string argm(unsigned index, const char* name) const 
	{
		return index < m_actionArgs.size()
			? m_actionArgs[index]
			: throw std::invalid_argument(fmt("%s argument missing!", name));
	}

	std::string toUtf8(std::string const & str) const   { return Utils::toUtf8(consoleCodePage, str); }
	std::string fromUtf8(std::string const & str) const { return Utils::fromUtf8(consoleCodePage, str); }

	std::string encodeSqlFromInput(std::string const& sql) const { return toUtf8(sql); }

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

	class OutputQuery {
		std::stringstream m_sstr;
		Columns m_orderBy;
	public:
		Litt const & litt;
		std::vector<unsigned> columnWidths; // Only set for column mode.

		OutputQuery(Litt const & litt) 
			: litt(litt) 
		{}

		void initSelectBare(SelectOption selectOption = SelectOption::normal)
		{
			if (litt.explainQuery) {
				m_sstr << "EXPLAIN QUERY PLAN ";
			}
			m_sstr << "SELECT ";
			if (litt.selectDistinct || selectOption == SelectOption::distinct) {
				m_sstr << "DISTINCT ";
			}
		}

		void initColumnWidths()
		{
			if (litt.explainQuery && litt.displayMode == DisplayMode::column) {
				columnWidths = { 10, 10, 10, 100 }; 
			} // else assumes columnWidths are properly set!
		}

		void addCol(ColumnInfo const * ci)
		{
			m_sstr << ci->nameDef;
			if (ci->label != nullptr) {
				m_sstr << " AS " << ci->label;
			}
		}

		void initSelect(const char* defColumns, const char* from, const char* defOrderBy, SelectOption selectOption = SelectOption::normal)
		{
			initSelectBare(selectOption);

			auto selCols = litt.selectedColumns.empty() ? litt.getColumns(defColumns, ColumnsDataKind::width, true) : litt.selectedColumns;
			selCols.insert(selCols.end(), litt.additionalColumns.begin(), litt.additionalColumns.end());
			for (unsigned i = 0; i < selCols.size(); ++i) {
				if (i != 0) {
					m_sstr << ",";
				}
				auto ci = selCols[i].first;
				addCol(ci);
				if (litt.displayMode == DisplayMode::column) {
					auto const width = ci->overriddenWidth >= 0 ? ci->overriddenWidth : selCols[i].second;
					_ASSERT(width >= 0);
					columnWidths.push_back(width);
				}
			}
			initColumnWidths();

			m_sstr << "\nFROM " << from;

			// Not used here, but we must "run" it anyway in order to finalize all columns used in the query for later "addIfColumns" calls.
			auto asc = [](Columns cols) { for (auto& c : cols) { c.second = (int)ColumnSortOrder::Asc; } return cols; };
			m_orderBy = litt.orderBy.empty()
				? (litt.selectedColumns.empty()
					? litt.getColumns(defOrderBy, ColumnsDataKind::sortOrder, true)
					: asc(litt.selectedColumns))
				: litt.orderBy;
		}

		void addWhere()
		{
			if (!litt.m_whereCondition.empty()) {
				m_sstr << "\nWHERE " << litt.m_whereCondition;
			}
		}

		void addHaving()
		{
			if (!litt.m_havingCondition.empty()) {
				m_sstr << "\nHAVING " << litt.m_havingCondition;
			}
		}

		void a(const char* str)
		{
			m_sstr << str;
		}

		void a(std::string const & str)
		{
			m_sstr << str;
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

		void addAuxTables(InnerJoinFor ijf = IJF_DefaultsOnly, unsigned indentSize = 0)
		{
			std::string indent(indentSize, ' ');

			std::string serJoin = (ijf & IJF_Series)    ? "INNER" : "LEFT OUTER";
			std::string stoJoin = (ijf & IJF_Stories)   ? "INNER" : "LEFT OUTER";
			std::string ortJoin = (ijf & IJF_OrigTitle) ? "INNER" : "LEFT OUTER";
			
			#define ngSqlSelect "(SELECT BookID, ltrim(group_concat(\"First Name\"||' '||\"Last Name\",', ')) AS 'Author(s)' FROM Books INNER JOIN AuthorBooks USING(BookID) INNER JOIN Authors USING(AuthorID) GROUP BY BookID)"
			#define dgSqlSelect "(SELECT BookID, group_concat(\"Date read\",', ') AS 'Date(s)' FROM Books INNER JOIN DatesRead USING(BookID) GROUP BY BookID)"

			addIfColumns("dr.dw.dwl.ti.sec.so", indent + "INNER JOIN DatesRead USING(BookID)");
			addIfColumns("ng",          indent + "INNER JOIN " ngSqlSelect " USING(BookID)");
			addIfColumns("dg",          indent + "INNER JOIN " dgSqlSelect " USING(BookID)");
			addIfColumns("fn.ln.nn.st", indent + "INNER JOIN AuthorBooks USING(BookID)");
			addIfColumns("fn.ln.nn",    indent + "INNER JOIN Authors USING(AuthorID)");
			addIfColumns("ot",          indent +  ortJoin + " JOIN OriginalTitles USING(BookID)");
			addIfColumns("st",          indent +  stoJoin + " JOIN Stories USING(AuthorID, BookID)");
			addIfColumns("sp.se",       indent +  serJoin + " JOIN BookSeries USING(BookID)");
			addIfColumns("se",          indent +  serJoin + " JOIN Series USING(SeriesID)");
			addIfColumns("so",          indent +  "LEFT OUTER JOIN Sources USING(SourceID)");
			addIfColumns("ge.gi",       indent +  "LEFT OUTER JOIN BookGenres USING(BookID)");
			addIfColumns("ge",          indent +  "LEFT OUTER JOIN Genres USING(GenreID)");

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
					m_sstr << (ci->nameDef);
					if (order == ColumnSortOrder::Desc) {
						m_sstr << " DESC"; // ASC is default.
					}
				}
			}
		}

		std::string getSql() const
		{
			return litt.encodeSqlFromInput(m_sstr.str());
		}
	}; // OutputQuery

	static const char* rowValue(const char* val)  // Guard against nullptr:s coming from NULL db values.
	{ 
		return (val != nullptr) ? val : "";
	}

	void outputRow(OutputQuery& query, bool isHeader, int argc, char **argv) const
	{
		switch (displayMode) {
		case DisplayMode::column:
			if (m_ansiEnabled) ansiSetRowColors(isHeader, argc, argv);
			break;
		case DisplayMode::htmldoc:
		case DisplayMode::html:
			m_output.write("<tr>");
			break;
		}

		for (int i = 0; i < argc; i++) {
			auto val = rowValue(argv[i]);
			switch (displayMode) {
			case DisplayMode::column:
				if (query.columnWidths[i] > 0) {
					if (i != 0) m_output.write(colSep);
					if (m_ansiEnabled) m_output.write(m_ansiRowColors[i].get());
					m_output.writeUtf8Width(val, query.columnWidths[i]);
					if (m_ansiEnabled && i == argc - 1) m_output.write(m_ansiDefColor);
				}
				break;
			case DisplayMode::list:
				if (i != 0) m_output.write(listSep);
				m_output.write(val);
				break;
			case DisplayMode::tabs:
				if (i != 0) m_output.write('\t');
				m_output.write(val);
				break;
			case DisplayMode::html:
			case DisplayMode::htmldoc:
				m_output.write(isHeader ? "<th>" : "<td>");
				m_output.writeHtml(val);
				m_output.write(isHeader ? "</th>" : "</td>");
				m_output.write('\n');
				break;
			}
		}

		switch (displayMode) {
		case DisplayMode::htmldoc:
		case DisplayMode::html:
			m_output.write("</tr>");
			break;
		}
		m_output.write('\n');
	}

	struct AnsiColumnColor {
		std::string colName;
		std::string ansiColor;
	};

	struct AnsiValueColor {
		std::string colName; // For this column,
		std::regex  rowValueRegEx; // matching this row value,
		std::string ansiColor; // apply this row color,
		std::vector<std::string> coloredColumns; // to these columns.
	};

	struct AnsiValueColorIndexed {
		int colIndex;
		std::regex  rowValueRegEx;
		std::string ansiColor;
		std::vector<int> colIndexes;
	};

	bool m_ansiEnabled = false;
	std::string m_ansiDefColor = "\x1b[30m"; // TODO: Make it use current console text color by default.

	std::vector<AnsiColumnColor> m_ansiColColors;
	mutable std::vector<std::string> m_ansiColColorsIndexed;

	std::vector<AnsiValueColor> m_ansiValueColors;
	mutable std::vector<AnsiValueColorIndexed> m_ansiValueColorsIndexed;

	mutable std::vector<std::reference_wrapper<std::string const>> m_ansiRowColors;

	void ansiInit(int argc, char **azColName) const
	{
		enableVTMode();

		for (int i = 0; i < argc; ++i) { 
			m_ansiRowColors.emplace_back(m_ansiDefColor); // Just to init the size, will need to reset for each row.
			m_ansiColColorsIndexed.push_back(m_ansiDefColor); // Init to defaults
		}

		for (auto const& acc : m_ansiColColors) {
			for (int i = 0; i < argc; ++i) {
				if (acc.colName == azColName[i]) {
					m_ansiColColorsIndexed[i] = acc.ansiColor;
					break;
				}
			}
		}

		for (auto& avc : m_ansiValueColors) {
			for (int i = 0; i < argc; ++i) {
				if (avc.colName == azColName[i]) {
					AnsiValueColorIndexed indexed;
					indexed.colIndex = i;
					indexed.rowValueRegEx = avc.rowValueRegEx;
					indexed.ansiColor = avc.ansiColor;
					for (auto const& cc : avc.coloredColumns) {
						for (int ii = 0; ii < argc; ++ii) {
							if (cc == azColName[ii]) {
								indexed.colIndexes.push_back(ii);
								break;
							}
						}
					}
					if (!indexed.colIndexes.empty()) {
						m_ansiValueColorsIndexed.push_back(indexed);
					}
					break;
				}
			}
		}

	}

	void ansiSetRowColors(bool isHeader, int argc, char** argv) const
	{
		for (int i = 0; i < argc; ++i) {
			m_ansiRowColors[i] = m_ansiColColorsIndexed[i];
		}

		for (auto const& avc : m_ansiValueColorsIndexed) {
			auto val = rowValue(argv[avc.colIndex]);
			if (std::regex_search(val, avc.rowValueRegEx)) {
				for (auto index : avc.colIndexes) {
					m_ansiRowColors[index] = avc.ansiColor;
				}
			}
		}
	}

	enum class ConsRowMatchMethod {
		columnValue,
		regEx,
		regExNot,
		diffLt,
		diffGt,
	};

	struct ConsRowColumnInfo {
		std::string        name;
		ConsRowMatchMethod matchMethod;
		std::regex         re;
		int                diff;
		mutable int        index;
	};

	int m_consRowMinCount = 0;
	std::vector<ConsRowColumnInfo> m_consRowColumns;
	mutable std::vector<std::vector<std::string>> m_consRowBuffer;
	mutable int m_consMatched = 0;

	bool consEnabled() const { return m_consRowMinCount > 0; }

	void consInit(int argc, char **argv, char **azColName) const
	{ _ASSERT(consEnabled());
		m_consRowBuffer.resize(std::max(1, m_consRowMinCount - 1));
		for (auto& row : m_consRowBuffer) {
			row.resize(argc);
		}

		for (auto& col : m_consRowColumns) {
			int j = 0;
			for (; j < argc; ++j) {
				if (col.name == azColName[j]) {
					col.index = j;
					break;
				}
			}
			if (j == argc) {
				throw std::invalid_argument("Could not find cons column " 
					+ col.name + " in the output columns");
			}
		}
		consSetBufferRow(0, argv);
		m_consMatched = 0;
	}

	void consSetBufferRow(int index, char **argv) const
	{
		std::transform(argv, argv + m_consRowBuffer[index].size(), m_consRowBuffer[index].begin(), rowValue);
	}

	void consOutputMatchedCount() const // OBS! This is mainly intended for use with column display mode.
	{ 
		if (m_consMatched >= m_consRowMinCount) {
			m_output.write(fmt("\n# = %i\n\n", m_consMatched));
		}
	}

	void consProcessRow(OutputQuery& query, int argc, char **argv) const
	{ _ASSERT(consEnabled());
		bool cvMatch = true, reMatch = true;
		for (auto const& col : m_consRowColumns) {
			auto val = rowValue(argv[col.index]);
			switch (col.matchMethod) {
			case ConsRowMatchMethod::columnValue:
				cvMatch = cvMatch && (m_consRowBuffer[0][col.index] == val);
				break;
			case ConsRowMatchMethod::regEx:
				reMatch = reMatch && std::regex_search(val, col.re);
				break;
			case ConsRowMatchMethod::regExNot:
				reMatch = reMatch && !std::regex_search(val, col.re);
				break;
			case ConsRowMatchMethod::diffLt:
			case ConsRowMatchMethod::diffGt:
				if (cvMatch) {
					int prevIndex = std::min(std::max(0, m_consMatched - 1), std::max(0, m_consRowMinCount - 2));
					auto& prevVal = m_consRowBuffer[prevIndex][col.index];
					unsigned long long cur=0, prev=0;
					if (toSecondsValue(val, cur) && toSecondsValue(prevVal, prev)) {
						if (col.matchMethod == ConsRowMatchMethod::diffLt) {
							cvMatch = ((cur - prev) < col.diff);
						}
						else {
							cvMatch = ((cur - prev) > col.diff);
						}
					}
					else {
						cvMatch = false;
					}
				}
				break;
			}
		}
		bool const consMatch = cvMatch && reMatch;

		if (consMatch) {
			++m_consMatched;
			if (m_consMatched == m_consRowMinCount && !(m_consRowMinCount == 1)) {
				std::vector<char*> rChar(m_consRowBuffer[0].size());
				for (auto const& r : m_consRowBuffer) {
					std::transform(r.begin(), r.end(), rChar.begin(),
						[](std::string const& str) { return const_cast<char*>(str.c_str()); });
					outputRow(query, false, rChar.size(), rChar.data());
				}
			}
			consSetBufferRow(std::min(m_consMatched - 1, std::max(0, m_consRowMinCount - 2)), argv);
			if (m_consMatched >= m_consRowMinCount) {
				outputRow(query, false, argc, argv);
			}
		}
		else {
			consOutputMatchedCount();
			consSetBufferRow(0, argv);
			m_consMatched = reMatch ? 1 : 0;
			if (m_consRowMinCount == 1 && reMatch) {
				outputRow(query, false, argc, argv);
			}
		}
	}

	static int outputQueryCallBack(void *pArg, int argc, char **argv, char **azColName) 
	{
		auto& query = *static_cast<OutputQuery*>(pArg);
		auto& litt = query.litt;
		auto& output = litt.m_output;
		try {
			if (litt.m_rowCount == 0) {
				if (litt.displayMode == DisplayMode::column) {
					for (int i = query.columnWidths.size(); i < argc; ++i) {
						query.columnWidths.push_back(std::min(size_t{30},
							std::max(strlen(azColName[i]), strlen(rowValue(argv[i])))));
					}
					if (litt.m_ansiEnabled) {
						litt.ansiInit(argc, azColName);
					}
				}

				if (!output.stdOutIsConsole() && litt.displayMode == DisplayMode::column) {
					// HACK: Write the UTF-8 BOM, seems V/VIEW needs it to properly 
					// detect the utf-8 encoding depending on the actual output.
					// Seems to interfere with V:S CSV mode though!
					const unsigned char bom[] = { 0xEF, 0xBB, 0xBF };
					output.write((const char*)&bom[0], sizeof(bom));
				}
				if (litt.displayMode == DisplayMode::htmldoc) {
					auto docStart = 
						"<!DOCTYPE html>\n" 
						"<html>\n"
						"<head>\n"
						"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n"
						"<title>" + litt.m_action + "</title>\n"
						"</head>\n"
						"<body>\n"
						"<table>\n";
					output.write(docStart);
				}
				if (litt.headerOn) {
					litt.outputRow(query, true, argc, azColName);
					if (litt.displayMode == DisplayMode::column) {
						for (int i = 0; i < argc; ++i) {
							if (query.columnWidths[i] > 0) {
								if (i != 0) output.write(litt.colSep);
								std::string underLine(query.columnWidths[i], '-');
								output.write(underLine);
							}
						}
						output.write('\n');
					}
				}
				if (litt.consEnabled()) {
					litt.consInit(argc, argv, azColName);
				}
			} // if (litt.m_rowCount == 0) {

			if (litt.consEnabled()) {
				litt.consProcessRow(query, argc, argv);
			}
			else {
				litt.outputRow(query, false, argc, argv);
			}
			++litt.m_rowCount;
			return 0;
		}
		catch (std::exception& ex) {
			output.flushNoThrow();
			fprintf(stderr, "\nCallback exception: %s\n", ex.what());
			return 1;
		}
	}

	void runOutputQuery(OutputQuery& query)
	{
		std::string sql = query.getSql();
		auto& litt = query.litt;
		auto& output = litt.m_output;

		if (litt.showQuery) {
			output.write(sql); output.write('\n');
			output.flush();
			return;
		}

		litt.m_rowCount = 0;
		int res = sqlite3_exec(litt.m_conn.get(), sql.c_str(), outputQueryCallBack, &query, nullptr);
		if (res == SQLITE_OK) {
			if (litt.displayMode == DisplayMode::htmldoc) {
				output.write("</table>\n</body>\n</html>\n");
			}
			if (consEnabled()) {
				consOutputMatchedCount(); // In case matching was still ongoing at the last row.
			}
		}
		if (m_ansiEnabled && litt.displayMode == DisplayMode::column && m_rowCount > 0) {
			m_output.write(m_ansiDefColor);
		}
		output.flushNoThrow();
		if (res != SQLITE_OK) {
			throw std::runtime_error(fmt("SQL error: %s", sqlite3_errmsg(litt.m_conn.get())));
		}

		if (litt.showNumberOfRows) {
			printf("\n# = %i\n", litt.m_rowCount);
		}
	}

	void runSingleTableOutputCmd(const char* defColumns, const char* table, const char* defOrderBy)
	{
		OutputQuery query(*this);
		query.initSelect(defColumns, table, defOrderBy);
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void runListData(const char* defColumns, const char* defOrderBy, InnerJoinFor ijf = IJF_DefaultsOnly)
	{
		OutputQuery query(*this);
		query.initSelect(defColumns, "Books", defOrderBy);
		query.addAuxTables(ijf);
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listAuthors(std::string const & action, std::string const & ln, std::string const & fn)
	{
		addActionWhereCondition("ln", ln);
		addActionWhereCondition("fn", fn);
		if (action == "a") {
			runSingleTableOutputCmd("ai.ln.fn", "Authors", "ai");
		}
		else {
			runListData("bi.ln.fn.bt.dr.so.ge", "ai.dr.bi");
		}
	}

	void listBooks()
	{
		addActionWhereCondition("bt", 0);
		if (m_action == "b") {
			runSingleTableOutputCmd("bi.bt.100", "Books", "bi");
		}
		else {
			runListData("bi.nn.bt.dr.so.ge", "dr.bi.ln.fn.bt");
		}
	}

	void listSeries(std::string const & action, std::string const & series)
	{
		addActionWhereCondition("se", series);
		if (action == "s") {
			runSingleTableOutputCmd("si.se", "Series", "si");
		}
		else {
			runListData("se.sp.bt.dr.bi.ln.fn", "se.sp.dr.bi", IJF_Series);
		}
	}

	void listGenres(std::string const & action, std::string const & genre)
	{
		addActionWhereCondition("ge", genre);
		if (action == "g") {
			runSingleTableOutputCmd("gi.ge", "Genres", "ge");
		}
		else {
			runListData("bi.bt.ge.dr.nn", "dr.bi.bt");
		}
	}

	void listOriginalTitles()
	{
		addActionWhereCondition("ot", 0);
		runListData("bi.nn.ot.bt.dr.so.20.ge", "ot.dr.bi.ln.fn.bt", IJF_OrigTitle);
	}

	void listStories()
	{
		addActionWhereCondition("st", 0);
		runListData("bi.bt.30.st.50.ln.fn.dr", "bt.bi", IJF_Stories);
	}

	void listSources(std::string const & action, std::string const & sourceName)
	{
		addActionWhereCondition("so", sourceName);
		if (action == "so") {
			runSingleTableOutputCmd("soid.so", "Sources", "so");
		}
		else {
			runListData("dr.so.50.bt.30.ln.fn", "so");
		}
	}

	void listRereads()
	{
		OutputQuery query(*this);
		const char* from = "(SELECT BookID, Count(BookID) As ReadCount FROM DatesRead GROUP BY BookID HAVING Count(BookID) > 1)";
		query.initSelect("brc.bt.dr.ng", from, "brc.desc.ln.bt.dr", SelectOption::distinct);
		query.add("INNER JOIN Books USING(BookID)");
		query.addAuxTables();
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listSametitle()
	{
		OutputQuery query(*this);
		const char* from = "(SELECT Title, Count(Title) As TitleCount FROM Books GROUP BY Title HAVING Count(Title) > 1)";
		query.initSelect("bi.bt.ng.btc", from, "bt.bi", SelectOption::distinct);
		query.add("INNER JOIN Books USING(Title)");
		query.addAuxTables();
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listSamestory() 
	{
		OutputQuery query(*this);
		//query.columnWidths = { 25,4,5,5,20,20,15,15,10,10,10,10 };
		query.initColumnWidths();
		query.initSelectBare();
		query.a(
R"(S1.Story as Story,
case when S1.AuthorID = S2.AuthorID then 'YES' else '-' end as Dupe, 
S1.BookID AS 'BID 1', S2.BookID as 'BID 2',
S1.Title as Title1, S2.Title as Title2,
S1."First Name" || ' ' || S1."Last Name" as 'Author 1', 
case when S1.AuthorID <> S2.AuthorID then  S2."First Name" || ' ' || S2."Last Name" else '* see 1 *' end as 'Author 2',
S1."Date read" as 'Date read 1', S2."Date read" as 'Date read 2',
S1.Source as 'Source 1', S2.Source as 'Source 2'
FROM (Stories
	INNER JOIN Books USING(BookID)
	INNER JOIN Authors USING(AuthorID)
	INNER JOIN DatesRead USING(BookID)
	INNER JOIN Sources USING(SourceID)
) AS S1 
JOIN (Stories
	INNER JOIN Books USING(BookID)
	INNER JOIN Authors USING(AuthorID)
	INNER JOIN DatesRead USING(BookID)
	INNER JOIN Sources USING(SourceID)
) as S2
WHERE S1.Story = S2.Story AND S1.StoryID <> S2.StoryID 
	AND S1.BookID <> S2.BookID              -- Avoid stories with more than one author (River of Souls)
	AND S1.BookID NOT Between 1274 AND 1276 -- Avoid Forever Yours, that's the book not the stories!
GROUP BY S1.Story
ORDER BY Dupe DESC)");
		runOutputQuery(query);
	}

	void listTitlestory()
	{
		OutputQuery query(*this);
		query.columnWidths = { 6,4,20,10,15,20,8,15,20,10,15 };
		query.initColumnWidths();
		query.initSelectBare(SelectOption::distinct);
		query.a(
R"(B.BookID as BookID, case when B.AuthorID = S.AuthorID then 'YES' else '-' end as Dupe, B.Title as Title, 
B."Date read" as 'Book read', B.Source as 'Book source', 
case when B.AuthorID <> S.AuthorID then B."First Name" || ' ' || B."Last Name" else '* see story *' end as 'Book Author',
S.BookID as 'S BookID', S."First Name" || ' ' || S."Last Name" as 'Story Author', S.Title as 'Story book title',  
S."Date read" as 'Story read', S.Source as 'Story source'
FROM (Books 
	INNER JOIN AuthorBooks USING(BookID)
	INNER JOIN Authors USING(AuthorID)
	INNER JOIN DatesRead USING(BookID)
	INNER JOIN Sources USING(SourceID)
) AS B 
JOIN (Stories 
	INNER JOIN AuthorBooks USING(AuthorID)
	INNER JOIN Authors USING(AuthorID)
	INNER JOIN Books USING(BookID)
	INNER JOIN DatesRead USING(BookID)
	INNER JOIN Sources USING(SourceID)
) as S 
WHERE B.Title = S.Story
ORDER BY Dupe DESC, B."Date read")");
		runOutputQuery(query);
	}

	void listBookCounts(std::string const & countCond, bool includeReReads, const char* columns, const char* snGroupBy)
	{
		auto selCols = columns + std::string(".bc");
		if (!countCond.empty()) { addCountCondToHavingCondition("bc", countCond); }
		if (includeReReads) { getColumn("dr")->usedInQuery = true; }
		getColumns(selCols, ColumnsDataKind::width, true);  // In case -c option is used!

		OutputQuery query(*this);
		query.initSelect(selCols.c_str(), "Books", "bc.desc");
		query.addAuxTables();
		query.addWhere();
		query.add("GROUP BY " + std::string(getColumn(snGroupBy)->nameDef));
		query.addHaving();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listAuthorBookCounts(std::string const & countCond, bool includeReReads) 
	{
		listBookCounts(countCond, includeReReads, "nn.35", "ai");
	}

	void listGenreBookCounts(std::string const & countCond, bool includeReReads)
	{
		listBookCounts(countCond, includeReReads, "ge.35", "gi");
	}

	void listSourceBookCounts(std::string const & countCond, bool includeReReads)
	{
		listBookCounts(countCond, includeReReads, "so.35", "soid");
	}

	void listYearlyBooksCounts(int count, int firstYear, int lastYear, const char* snColSelect, const char* snColGroupBy)
	{
		auto col = getColumn(snColSelect);  col->usedInQuery = true;
		auto bc  = getColumn("bc");         bc->usedInQuery = true;
		auto gby = getColumn(snColGroupBy); gby->usedInQuery = true;

		OutputQuery q(*this);
		q.columnWidths = { 3 }; for (int y = firstYear; y <= lastYear; ++y) q.columnWidths.push_back(30);
		q.initColumnWidths();
		q.add("ATTACH DATABASE ':memory:' AS memdb;");
		for (int year = firstYear; year <= lastYear; ++year) {
			auto ycond = appendConditions(LogOp_AND, m_whereCondition, getWhereCondition(fmt("dr.%i-*", year)));
			q.add(fmt("CREATE TABLE memdb.year%i AS", year));
			q.add(fmt("SELECT printf('%%3i - %%s', %s, %s) as \"%i\"", bc->nameDef, col->nameDef, year));
			q.add    ("FROM BOOKS");
			q.addAuxTables();
			q.add(fmt("WHERE %s", ycond.c_str()));
			q.add(fmt("GROUP BY %s", gby->nameDef));
			q.addHaving();
			q.add(fmt("ORDER BY %s DESC, %s", bc->nameDef, col->nameDef));
			q.add(fmt("LIMIT %i;", count));
		} q.a("\n");
		q.initSelectBare(); q.a("* FROM");
		q.add(fmt("(WITH RECURSIVE Pos(\"#\") AS (SELECT 1 UNION ALL SELECT \"#\" + 1 FROM Pos WHERE \"#\" < %i) SELECT * FROM Pos)", count));
		for (int year = firstYear; year <= lastYear; ++year) {
			q.add(fmt("LEFT OUTER JOIN memdb.year%i ON \"#\" == memdb.year%i.rowId", year, year));
		}
		q.add("ORDER BY \"#\";");
		q.add("DETACH DATABASE memdb");
		runOutputQuery(q);
	}

	void listBooksReadPerDate(std::string countCond)
	{
		if (countCond.empty()) countCond = "2";

		// We count dates between time 00:00 to 06:00 as the previous day (was up late reading, so want them counted to prev day).
		#define calcDRTimeWindow "case when (time(\"Date Read\") > '00:00:00' and time(\"Date Read\") < '06:00:00') then date(\"Date Read\", '-6 hours') else date(\"Date Read\") end"

		OutputQuery query(*this);
		query.initSelect("dr.bt.ln.fn", "Books", "dr.bt"); 
		getColumn("dr")->usedInQuery = true; // in case of -c!
		query.addAuxTables();
		query.add("WHERE " calcDRTimeWindow " IN");
		query.add(" (SELECT CalcDR FROM (SELECT " calcDRTimeWindow " as CalcDR FROM DatesRead WHERE \"Date Read\" > \"2001-10\")");
		query.add("  GROUP BY CalcDR");
		query.add("  HAVING " + parseCountCondition("Count(CalcDR)", countCond) + ")");
		if (!m_whereCondition.empty()) {
		query.add(" AND " + m_whereCondition);
		}
		query.addOrderBy();
		runOutputQuery(query);

		#undef calcDRTimeWindow
	}

	struct PeriodColumn {
		std::string const definition;
		std::string const name; // Used in SQL so need to be quoted in case it contains spaces, is a number etc.

		PeriodColumn(std::string d, std::string const & n) : 
			definition(std::move(d)), name(quote(n))
		{
		}

		unsigned colWidth() const { return LittDefs::colWidth(name); }
	};

	std::vector<PeriodColumn> getPeriodColumns(int fromActionArgIndex)
	{
		std::vector<PeriodColumn> res;
		size_t width = 0;
		for (int i = fromActionArgIndex; ; ) {
			auto def = arg(i++);
			if (def.empty()) {
				break;
			}
			auto name = arg(i++);
			if (name.empty()) {
				throw std::invalid_argument(std::string("No name for def: ") + def);
			}
			res.push_back({ def, name });
			width = std::max(width, res.back().name.length());
		}
		for (auto const & pc : res) {
			printf("%-*s : %s\n", width, pc.name.c_str(), pc.definition.c_str());
		}
		printf("\n");
		return res;
	}

	void listBooksReadPerPeriod(
		std::string const& periodDef,
		std::string        period,
		std::string const& cond,
		std::vector<PeriodColumn> const& columns)
	{
		period = quote(period);
		OutputQuery q(*this);
		q.columnWidths.push_back(colWidth(period)); q.columnWidths.push_back(strlen("Total"));
		for (auto& c : columns) { q.columnWidths.push_back(std::max(4u, c.colWidth())); }
		q.initColumnWidths();
		appendToWhereCondition(LogOp_AND, getWhereCondition("dr.gt.2002"));

		q.initSelectBare(); q.a("Main." + period + " AS " + period + ", Total"); for (auto& c : columns) { q.a(", " + c.name); }; q.a(" FROM");
		q.add(" (SELECT " + period + ", Count(BookID) as Total FROM");
		q.add("   (SELECT BookID, strftime('" + periodDef + "', \"Date Read\") AS " + period);
		q.add("    FROM Books");
		q.addAuxTables(IJF_DefaultsOnly, 4);
		q.add("    WHERE " + m_whereCondition + ")");
		q.add("  GROUP BY " + period);
		q.add(" ) Main");

		for (auto& c : columns) {
		// We don't update the having condition, it should be same for all columns and not included in col defs.
		auto ccond = appendConditions(LogOp_AND, m_whereCondition, getWhereCondition(c.definition));
		q.add(" LEFT OUTER JOIN");
		q.add(" (SELECT " + period + ", Count(BookID) AS " + c.name + " FROM");
		q.add("    (SELECT BookID, strftime('" + periodDef + "', \"Date Read\") AS " + period);
		q.add("     FROM Books");
		q.addAuxTables(IJF_DefaultsOnly, 5);
		q.add("     WHERE " + ccond + ")");
		q.add("  GROUP BY " + period);
		if (!m_havingCondition.empty()) { 
		q.add("  HAVING " + m_havingCondition);
		}
		q.add(" )");
		q.add(" USING(" + period + ")");
		}

		if (!cond.empty() && cond != WcS) {
		q.add(" WHERE Main." + period + " LIKE " + likeArg(cond + WcS));
		}
		q.add(" ORDER BY " + period);
		runOutputQuery(q);
	}

	int executeSql(std::string const& userSql, int (*callback)(void*,int,char**,char**) = nullptr, void* callBackData = nullptr, bool enableShowQuery = true) const
	{
		auto encSql = encodeSqlFromInput(userSql);
		if (enableShowQuery && showQuery) {
			m_output.write(encSql); m_output.write('\n'); m_output.flush();
			return 0;
		}

		int res = sqlite3_exec(m_conn.get(), encSql.c_str(), callback, callBackData, nullptr);
		if (res != SQLITE_OK) {
			throw std::runtime_error(fmt("SQL error: %s", sqlite3_errmsg(m_conn.get())));
		}

		return sqlite3_changes(m_conn.get());
	}

	IdValue executeInsert(std::string const& userSql) const
	{
		executeSql(userSql);
		return sqlite3_last_insert_rowid(m_conn.get());
	}

	void executeInsert(std::string const& userSql, const char* idName) const
	{
		auto id = executeInsert(userSql);
		if (id != EmptyId) {
			printf("Added with %s %llu\n", idName, id);
		}
	}

	std::vector<std::string> selectRowValue(std::string const& userSql) const
	{
		std::vector<std::string> res;
		auto callback = [](void *pArg, int argc, char** argv, char** /*azColName*/)
		{
			auto& res = *static_cast<std::vector<std::string>*>(pArg);
			res.resize(argc);
			std::transform(argv, argv + argc, res.begin(), rowValue);
			return 0;
		};
		executeSql(userSql, callback, &res, false);
		return res;
	}

	std::string selectSingleValue(std::string const& userSql, const char* valueName) const
	{
		auto res = selectRowValue(userSql);
		if (res.empty()) {
			throw std::runtime_error(fmt("Could not find %s", valueName));
		}
		return res[0];
	}

	std::string selDV(std::string const& sql, const char* idName) const
	{
		return fromUtf8(selectSingleValue(sql, idName)); // Convert to console code page, it will be displayed there.
	}
	std::string selTitle(IdValue id) { return selDV(fmt("SELECT Title FROM Books WHERE BookID=%llu", id), "BookID"); }
	std::string selSeries(IdValue id) { return selDV(fmt("SELECT Series FROM Series WHERE SeriesID=%llu", id), "SeriesID"); }
	std::string selSource(IdValue id) { return selDV(fmt("SELECT Source FROM Sources WHERE SourceID=%llu", id), "SourceID"); }
	std::string selGenre(IdValue id) { return selDV(fmt("SELECT Genre FROM Genres WHERE GenreID=%llu", id), "GenreID"); }
	std::string selAuthor(IdValue id) { return selDV(fmt("SELECT \"First Name\" || ' ' || \"Last Name\" FROM Authors WHERE AuthorID=%llu", id), "AuthorID"); }

	#define SEL_F(selFunc) [&](IdValue id) { return selFunc(id); }
	#define LIST_F(listCodeUsingStringArg) [&](std::string const & s) { resetWhere(); listCodeUsingStringArg; }
	#define ESC_S(str) escSqlVal(str).c_str()

	void addAuthor()
	{
		auto ln = input("Enter the last name"); if (ln.empty()) return;
		auto fn = input("Enter the first name");
		if (confirm(fmt("Add author '%s, %s'", ln.c_str(), fn.c_str()))) {
			executeInsert(fmt("INSERT INTO Authors (\"Last Name\",\"First Name\") VALUES(%s,%s)",
				ESC_S(ln), ESC_S(fn)), "AuthorID");
		}
	}

	void addGenre()
	{
		auto name = input("Enter genre name"); if (name.empty()) return;
		if (confirm(fmt("Add genre '%s'", name.c_str()))) {
			executeInsert(fmt("INSERT INTO Genres (Genre) VALUES(%s)", 
				ESC_S(name)), "GenreID");
		}
	}

	void addSeries()
	{
		auto name = input("Enter series name"); if (name.empty()) return;
		if (confirm(fmt("Add series '%s'", name.c_str()))) {
			executeInsert(fmt("INSERT INTO Series (Series) VALUES(%s)", 
				ESC_S(name)), "SeriesID");
		}
	}

	void addBook()
	{
		auto st = GetSystemTime();
		auto authors     = std::vector<std::tuple<IdValue, std::string>>(); // AuthorId + Story
		auto title       = std::string();
		auto dateRead    = fmt("%04d-%02d-%02d", st.wYear, st.wMonth, st.wDay);
		auto sourceId    = EmptyId;
		auto genreId     = EmptyId;
		auto origtitle   = std::string(); 
		auto lang        = int('e');
		auto owns        = int('n');
		auto boughtEbook = int('n');
		auto seriesId    = EmptyId;
		auto seriesPart  = std::string();
	enterBook:
		for (size_t i = 0;;) {
			auto aid = (i < authors.size()) ? std::get<0>(authors[i]) : EmptyId;
			input(aid, "AuthorID",  SEL_F(selAuthor), optional, LIST_F(listAuthors("a", s + WcS, "")));
			if (aid == EmptyId) {
				if (i < authors.size()) { authors.erase(authors.begin() + i); }
				if (i < authors.size() || (i == 0 && !title.empty())) continue; else break;
			}
			auto story = (i < authors.size()) ? std::get<1>(authors[i]) : std::string();
			input(story, "Story name (optional)");
			if (authors.size() <= i) { authors.reserve((i+1)*2); authors.resize(i + 1); }
			authors[i++] = std::make_tuple(aid, story);
		}
		if (authors.empty() && title.empty()) {
			return;
		}
		input(title, "Book title", required);
		input(dateRead, "Date read", required, R"(\d\d\d\d-\d\d-\d\d \d\d\:\d\d)");
		input(sourceId, "Book SourceID", SEL_F(selSource), required, LIST_F(listSources("so", WcS + s + WcS))); 
		input(genreId, "Book GenreID", SEL_F(selGenre), required, LIST_F(listGenres("g", WcS + s + WcS))); 
		input(origtitle, "Original title (optional)", optional);
		ask("es", "Language", lang);
		ask("yn", "Own book", owns);
		ask("yn", "Bought ebook", boughtEbook);
		input(seriesId, "SeriesID (optional)", SEL_F(selSeries), optional, LIST_F(listSeries("s", WcS + s + WcS))); 
		if (seriesId != EmptyId) {
			input(seriesPart, "Part in series", required);
		}

		auto langStr = [](int l) { return l == 'e' ? "en" : "sv"; };
		auto ynStr = [](int yn) { return yn == 'y' ? "yes" : "no"; };
		auto ynInt = [](int yn) { return yn == 'y' ? 1     : 0;    };

		printf("\n");
		bool hasWidth = false; size_t width = 0; again:
		for (auto const& a : authors) {
			auto const& aid = std::get<0>(a);
			auto const& story = std::get<1>(a);
			auto name = selAuthor(aid);
			width = std::max(width, name.length());
			if (hasWidth) printf("%-4llu - %-*s%s%s\n", aid, width, name.c_str(), (story.empty() ?  "" : "  :  "), story.c_str());
		}
		if (!hasWidth) { hasWidth = true; goto again; }
		printf("\n");
		printf("Title          : %s\n", title.c_str());
		printf("Date read      : %s\n", dateRead.c_str());
		printf("Genre          : %s\n", selGenre(genreId).c_str());
		printf("Source         : %s\n", selSource(sourceId).c_str());
		printf("Language       : %s\n", langStr(lang));
		printf("Owned          : %s\n", ynStr(owns));
		printf("Bought e-book  : %s\n", ynStr(boughtEbook));
		if (!origtitle.empty()) {
		printf("Original title : %s\n", origtitle.c_str()); }
		if (seriesId != EmptyId) {
		printf("Series         : Part %s of %s\n", seriesPart.c_str(), selSeries(seriesId).c_str()); }
		printf("\n");

		switch (ask("yne", "Add book")) {
			case 'y': break;
			case 'n': return;
			case 'e': default: goto enterBook;
		}
		// Add it!
		auto bookId = selectSingleValue("SELECT max(BookId) + 1 FROM Books", "BookId"); auto bid = bookId.c_str();
		auto addedAuthorBooks = std::set<std::string>();

		std::string sql = "BEGIN TRANSACTION;\n";
		sql.append(fmt("INSERT INTO Books (BookID,Title,Language,Owned,\"Bought Ebook\") VALUES(%s,%s,'%s',%i,%i);\n",
			bid, ESC_S(title), langStr(lang), ynInt(owns), ynInt(boughtEbook)));
		sql.append(fmt("INSERT INTO DatesRead (BookID,\"Date Read\",SourceID) VALUES(%s,%s,%llu);\n",
			bid, ESC_S(dateRead), sourceId));
		sql.append(fmt("INSERT INTO BookGenres (BookID,GenreID) VALUES(%s,%llu);\n", bid, genreId));
		for (auto const& a : authors) {
			auto const aid = std::get<0>(a);
			auto abKey = std::to_string(aid) + "_" + bookId;
			if (addedAuthorBooks.find(abKey) == addedAuthorBooks.end()) {
				addedAuthorBooks.insert(abKey);
				sql.append(fmt("INSERT INTO AuthorBooks (AuthorID,BookID) VALUES(%llu,%s);\n", aid, bid));
			}
			auto const& story = std::get<1>(a);
			if (!story.empty()) {
				sql.append(fmt("INSERT INTO Stories (StoryID,AuthorID,BookID,Story) VALUES(NULL,%llu,%s,%s);\n",
					aid, bid, ESC_S(story)));
			}
		}
		if (!origtitle.empty()) {
			sql.append(fmt("INSERT INTO OriginalTitles (BookID,\"Original Title\") VALUES(%s,%s);\n",
				bid, ESC_S(origtitle)));
		}
		if (seriesId != EmptyId) {
			sql.append(fmt("INSERT INTO BookSeries (BookID,SeriesID,\"Part in Series\") VALUES(%s,%llu,%s);\n",
				bid, seriesId, escSqlVal(seriesPart, true).c_str()));
		}
		sql.append("COMMIT TRANSACTION");
		
		try {
			executeSql(sql);
			printf("Added %i rows.\n", sqlite3_total_changes(m_conn.get()));
		}
		catch (std::exception& ex) {
			printf("Failed to add book: %s\n\nSQL command was:\n\n%s\n\n", ex.what(), sql.c_str());
			executeSql("ROLLBACK TRANSACTION");
			if (confirm("Retry")) {
				goto enterBook;
			}
		}
	}

	void addBookToSeries(IdValue bookId, IdValue seriesId, int part)
	{
		if (confirm(fmt("Add '%s [%llu]' to '%s [%llu]' as part %i", 
				selTitle(bookId).c_str(), bookId, 
				selSeries(seriesId).c_str(), seriesId, part))) {
			executeInsert(fmt("INSERT INTO BookSeries (BookID,SeriesID,\"Part in Series\") VALUES(%llu,%llu,%i)", 
				bookId, seriesId, part), "BookSeries");
		}
	}

	void setBookGenre(IdValue bookId, std::string const& cmd, IdValue genreId)
	{
		auto const bt = selTitle(bookId);
		auto const genre = (cmd == "c" || cmd == "d") ? selGenre(genreId) : "";
		int changes = -1;

		if (cmd == "a" || cmd == "c") {
			IdValue newGenreId = EmptyId;
			input(newGenreId, "New GenreID", SEL_F(selGenre), optional, LIST_F(listGenres("g", WcS + s + WcS)));
			if (newGenreId == EmptyId) return;
			auto const newG = selGenre(newGenreId);
			if (cmd == "c") {
				if (confirm(fmt("Change '%s' => '%s' for '%s'", genre.c_str(), newG.c_str(), bt.c_str()))) {
					changes = executeSql(fmt("UPDATE BookGenres SET GenreID=%llu WHERE BookID=%llu AND GenreID=%llu", 
						newGenreId, bookId, genreId));
				}
			}
			else {
				if (confirm(fmt("Add '%s' to '%s'", newG.c_str(), bt.c_str()))) {
					changes = executeSql(fmt("INSERT INTO BookGenres (BookID,GenreID) VALUES(%llu,%llu)",
						bookId, newGenreId));
				}
			}
		}
		else if (cmd == "d") {
			if (confirm(fmt("Remove '%s' from '%s'", genre.c_str(), bt.c_str()))) {
				changes = executeSql(fmt("DELETE FROM BookGenres WHERE BookID=%llu AND GenreID=%llu",
					bookId, genreId));
			}
		}
		else {
			throw std::invalid_argument("Invalid genre-cmd: " + cmd);
		}

		if (changes != -1) { printf("Updated %i rows\n", changes); }
	}

	void setBookDateRead(IdValue bookId, std::string const& cmd, std::string const& dr)
	{
		auto const bt = selTitle(bookId);
		int changes = -1;

		if (cmd == "c" || cmd == "d") { // Check that is exists.
			selectSingleValue(fmt("SELECT BookID FROM DatesRead WHERE BookID=%llu AND \"Date Read\"=%s", 
				bookId, ESC_S(dr)), fmt("date read %s for bookId %llu", dr.c_str(), bookId).c_str());
		}

		if (cmd == "a" || cmd == "c") {
			std::string newDr;
			input(newDr, "New date read", optional); // Don't check regex here, might want to add old date.
			if (newDr.empty()) return;
			if (cmd == "c") {
				if (confirm(fmt("Change date read '%s' => '%s' for '%s'", dr.c_str(), newDr.c_str(), bt.c_str()))) {
					changes = executeSql(fmt("UPDATE DatesRead SET \"Date Read\"=%s WHERE BookID=%llu AND \"Date Read\"=%s",
						ESC_S(newDr), bookId, ESC_S(dr)));
				}
			}
			else { // add
				IdValue newSourceId = EmptyId;
				input(newSourceId, "SourceID", SEL_F(selSource), required, LIST_F(listSources("so", WcS + s + WcS)));
				auto source = selSource(newSourceId);
				if (confirm(fmt("Add date read '%s' with source '%s' to '%s'", newDr.c_str(), source.c_str(), bt.c_str()))) {
					changes = executeSql(fmt("INSERT INTO DatesRead (BookID,\"Date Read\",SourceID) VALUES(%llu,%s,%llu)",
						bookId, ESC_S(newDr), newSourceId));
				}
			}
		}
		else if (cmd == "d") {
			if (confirm(fmt("Remove date read '%s' from '%s'", dr.c_str(), bt.c_str()))) {
				changes = executeSql(fmt("DELETE FROM DatesRead WHERE BookID=%llu AND \"Date Read\"=%s",
					bookId, ESC_S(dr)));
			}
		}
		else {
			throw std::invalid_argument("Invalid dateRead-cmd: " + cmd);
		}

		if (changes != -1) { printf("Updated %i rows\n", changes); }
	}

	void executeAction() 
	{
		auto const& action = m_action;
		if (action == "h") {
			showHelp(true);
		}
		else if (action == "a" || action == "aa") {
			listAuthors(action, arg(0), arg(1));
		}
		else if (action == "b" || action == "bb") {
			listBooks();
		}
		else if (action == "s" || action == "ss") {
			listSeries(action, arg(0));
		}
		else if (action == "g" || action == "gg") {
			listGenres(action, arg(0));
		}
		else if (action == "ot") {
			listOriginalTitles();
		}
		else if (action == "st") {
			listStories();
		}
		else if (action == "so" || action == "soo") {
			listSources(action, arg(0));
		}
		else if (action == "rereads") {
			listRereads();
		}
		else if (action == "sametitle") {
			listSametitle();
		}
		else if (action == "titlestory") {
			listTitlestory();
		}
		else if (action == "samestory") {
			listSamestory();
		}
		else if (action == "abc") {
			listAuthorBookCounts(arg(0), arg(1) == "1");
		}
		else if (action == "gbc") {
			listGenreBookCounts(arg(0), arg(1) == "1");
		}
		else if (action == "sbc") {
			listSourceBookCounts(arg(0), arg(1) == "1");
		}
		else if (action == "ybca" || action == "ybcg" || action == "ybcs") {
			auto count = intarg(0, "count", 10);
			auto firstYear = intarg(1, "firstYear", GetSystemTime().wYear - 4);
			auto lastYear = intarg(2, "lastYear", firstYear + 4);
			auto snSel = "nn"; auto snGby = "ai"; // 'a' by default.
			switch (action[3]) {
				case 'g': snSel = "ge"; snGby = "gi"; break;
				case 's': snSel = "so"; snGby = "soid"; break;
			}
			listYearlyBooksCounts(count, firstYear, lastYear, snSel, snGby);
		}
		else if (action == "brd") {
			listBooksReadPerDate(arg(0));
		}
		else if (action == "brwd") {
			listBooksReadPerPeriod("%w", "Weekday", arg(0, WcS), getPeriodColumns(1));
		}
		else if (action == "brm") {
			listBooksReadPerPeriod("%Y-%m", "Year-Month", arg(0, WcS), getPeriodColumns(1));
		}
		else if (action == "bry") {
			listBooksReadPerPeriod("%Y", "Year", arg(0, WcS), getPeriodColumns(1));
		}
		else if (action == "brp") {
			listBooksReadPerPeriod(arg(0), arg(1), arg(2, WcS), getPeriodColumns(3));
		}
		else if (action == "brym") {
			const char* months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
			std::vector<PeriodColumn> monthColumns;
			for (int m = 1; m <= 12; ++m) {
				char def[10]; sprintf_s(def, "dr.*-%02d-*", m);
				monthColumns.push_back({ std::string(def), std::string(months[m-1]) });
			}
			listBooksReadPerPeriod("%Y", "Year", arg(0, WcS), monthColumns);
		}
		else if (action == "brmy") {
			auto st = GetSystemTime();
			std::vector<PeriodColumn> yearColumns;
			for (int y = 2002; y <= st.wYear; ++y) {
				char def[10]; sprintf_s(def, "dr.%04d-*", y);
				yearColumns.push_back({ def, std::to_string(y) });
			}
			listBooksReadPerPeriod("%m", "Month", arg(0, WcS), yearColumns);
		}
		else if (action == "add-a" || action == "adda") {
			addAuthor();
		}
		else if (action == "add-g" || action == "addg") {
			addGenre();
		}
		else if (action == "add-s" || action == "adds") {
			addSeries();
		}
		else if (action == "add-b" || action == "addb") {
			addBook();
		}
		else if (action == "b2s" || action == "btos") {
			auto bid  = idarg(0, "bookId");
			auto sid  = idarg(1, "seriesId");
			auto part = intarg(2, "part");
			addBookToSeries(bid, sid, part);
		}
		else if (action == "set-g" || action == "setg") {
			auto bid = idarg(0, "bookId");
			auto cmd = arg(1, "a");
			auto gid = (cmd == "a" ? EmptyId : idarg(2, "genreId"));
			setBookGenre(bid, cmd, gid);
		}
		else if (action == "set-dr" || action == "setdr") {
			auto bid = idarg(0, "bookId");
			auto cmd = arg(1, "a");
			auto dr = (cmd == "a" ? "" : argm(2, "dateRead"));
			setBookDateRead(bid, cmd, dr);
		}
		else {
			throw std::invalid_argument("Invalid action: " + action);
		}
	}
}; // Litt

int main(int argc, char **argv)
{
	try {
		if (argc <= 1) {
			showHelp();
		}
		else {
			Litt litt(argc, argv);
			litt.executeAction();
		}
		return 0;
	}
	catch (std::exception& ex) {
		fprintf(stderr, "%s\n", ex.what());
		return 1;
	}
}
