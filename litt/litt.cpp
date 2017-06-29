/** LITT - now for C++! ***********************************************************************************************

Changelog:
 * 2017-06-29: Added "add-dr", removed "add" from set-dr and set-g and "delete" from set-g.
 * 2017-06-29: add-b now allows for more flexible input (DR mainly). Will also ask before discarding edits.
 * 2017-06-28: Added "add-st" and "add-so". Harmonized and extended the simple add actions (a,g,s,so).
 * 2017-06-22: Re-organized and fixed the online help again. Action ybc* => *bcy to match *bc action.
 * 2017-06-22: Added length short names for rest of the text columns. Makes sure the needed tables are joined for length sns.
 * 2017-06-21: Re-organized the online help.
 * 2017-06-21: Added option -e<encoding> to specify output encoding. Will be used when stdout is redirected.
 * 2017-06-21: Can now use cons-dlt/dgt also with other than "sec" column, useful for ID columns at least. Also now works to
               compare with negative difference values.
 * 2017-06-19: Added "fit width" option -f[on|off|auto|<widthValue>] for column display mode, where default is auto, 
               meaning will be used where appropriate. Specifying a custom widthValue is same as "on", but will use specified
               width instead of the console window width or the default value for when there is no console.
 * 2017-06-19: Made listing output more consistent. Uses nn instead of ln.fn by default, changed some default column widths and
               default columns sizes used by runSingleTableOutputCmd.
 * 2017-06-17: b2s now uses OR REPLACE so it's easier to fix wrong series part! 
               Also made it possible to use non-int part values.
 * 2017-06-16: Added set-ot (to tiresome to use DB Browser!)
 * 2017-06-16: Removed artifical 2001-10 start in brd. (Will now get a few "fake" hits from 1998-2001, but fine.)
 * 2017-06-16: Fixed output from getPeriodColumns; does not print a newline if there are no columns, takes BOM and utf-8 into
               account, just like output from the query.
 * 2017-06-16: Now listBooksReadPerPeriod supports all dates instead of limiting to 2002 and newer.
               Done by simply using substr instead of strftime where possible. Requires YYYY-MM-DD [HH:MM] 
               date format though so less general now, cannot have dates as timestamps. Not an issue of course.
 * 2017-06-16: brmy action now takes firstYear and lastYear as parameters instead of the pretty useless date condition used
               to select a specific month.
 * 2017-06-16: Can now give max number of characters to compare in cons when comparing with prev row value.
 * 2017-06-14: Simplified samestory. Removed hacks for "Forever Yours"!
               Removed unnecessary SELECT DISTINCT by default in many places.
               Simplified titlestory a little, removed unneeded AuthorBooks, so no need for distinct.
 * 2017-06-14: Added the id values to addAuxTables, might want to select those too! Removed all table names from column names in
               in columnInfo:s. (Not only Books.BookID as last time).
 * 2017-06-13: Speeded up ybca/g/s by using temporary memory tables instead of nested queries. Greatest speedup came from
               being able to use rowId:s for joining year tables instead of generating RowNumbers in year queries via 
               count(*) hack.
               No longer needs to specify a value for --ansi off sub-option, always sets to disabled.
               ybca/g/s now calculates lastYear from firstYear instead of calculating both from current year.
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
	puts(
R"(Usage: LITT {options} <action with arguments> {options}

Basic list actions:
   h                              Show full help.
   a/aa   [lastName] [firstName]  List authors - without/with books.
   b/bb   [title]                 List books - with minimum/full details.
   ot     [origTitle]             List original titles for books.
   st     [story]                 List (anthology) stories for books.
   s/ss   [series]                List series - without/with books.
   g/gg   [genre]                 List genre - without/with books.
   so/soo [source]                List book sources where a certain book "read" was gotten - without/with books.

   rereads                        List re-read books. Can use virtual column "brc" - Book Read Count.
   sametitle                      List books with same title. Can use virtual column "btc" - Book Title Count.
   samestory                      List stories with same title.
   titlestory                     List books with same title as a story - Duplicates shown.
   brd [booksReadCond]            List the dates and books where [cond] (default 2) books where read.

List number of books read for author, genre and source. Can use virtual column "bc":
   abc/gbc        [bookCountCond] [bRRs]             For author and genre. bRRs=1 => include re-reads.
   sbc            [bookCountCond]                    For source. Re-reads are always included.
   abcy/gbcy/sbcy [rowCount] [firstYear] [lastYear]  Yearly book counts for author, genre and source.

List number of books read for specific periods along with a total count. Can use virtual column "prc":
   brmy [firstYear] [lastYear]    Total over month/year.
   brym [yearCondition]           Total over year/month.

   brm/bry/brwd [periodCondition] {<columnWhereCond> <columnName>}
                                  - Total over year-months, years and weekdays with optional extra columns.

   brp <periodColumn-strftime-def> <periodColumnName> [periodCondition] {<columnWhereCond> <columnName>}
                                  - Generalization of brm/bry/brwd, can customize the period and its name.

Adding and modifying data:
   add-a     [lastName] [firstName]       Add an author.
   add[f]-b                               Add a book. (addf-b variant allows less strict date values)
   add-st    [BookID] [AuthorID] [story]  Add a story for a book.
   add-s     [series]                     Add a series.
   add-g     [genre]                      Add a genre.
   add-so    [source]                     Add a book source.
   add[f]-dr [BookID] [dr] [SourceId]     Add a 'date read' for a book with given source.
   
   set-dr    [BookID] [dr] [newDr|delete] Change or delete 'date read' for a book.
   set-g     [BookID] [GenreID] [newGID]  Change genre for a book.
   set-ot    [BookID] [origTitle]         Set the original title for a book. Will update current one if already set.
   b2s       [BookID] [SeriesID] [part]   Add a book to a series, will update current part if already set.
)"
	); if (showExtended) puts(
R"(
NOTE: As wildcards in most match arguments and options "*" (any string) and "_" (any character) can be used. Wild-cards "*" 
      around the listing actions also gives a similar effect, e.g. *b* will list all books containing the given title 
      string, while b* will only list books starting with it instead.
      
Options:
    -d[DisplayMode]   Display mode. Default is column.
    -h[on|off]        Header row on/off. Default is on.
    -c[selColumns]    Override the default columns of the action.
    -a[addColumns]    Include additional columns.
    -o[colOrder]      Override sort order. By default sorts by used (-c or default) columns starting from left.
    -w[whereCond]     Add a WHERE condition - will be AND:ed with the one specified by the action and arguments.
                      If several -w options are included their values will be OR:ed together.
    -s[colSizes]      Override the default column sizes.
    -q                Use debug mode - dump the SQL query/command instead of executing it.
    -u                Make sure the result only contain DISTINCT (UNIQUE) values.
    -l[dbPath]        Specify litt/sqlite database file. Uses "litt.sqlite" by default. Either from the
                      current directory or from "%MYDOCS%\litt\" if MYDOCS is set.
    -n                Print number of output rows at the end.
    -e[encoding]      Output encoding for pipes and redirection. Default is utf8.
    -f[on|off|auto|w] Fit width mode. Default is auto; used where most fitting. Specifying
                      an explicit width value implies mode "on". If no value is specified then the width
                      of the console is used. If there is no console then a hard-coded value is used.

    --cons:<minRowCount>:{<colSnOrName>[:charCmpCount]|[:re|ren:<regExValue>]|[:dlt|dgt:<diffValue>]}+
                     Specify column conditions for consecutive output row matching.
                     If no explicit method is specified then matching is done by comparing against the
                     same column value of the previous row. Can optionally specify the max number of characters
                     to compare.
                     The dlt/dgt diff matching is only supported for the "sec" column.

    --ansi[:off][:defC:<ansiC>][:colC:<col>:<ansiC>][:valC:<colVal>:<regExValue>:col{.col}:<ansiC>}
                     Specify ANSI colors for columns, rows and specific values. Only enabled in column display mode.
                     * off  : Turn off ANSI coloring. Default is on when --ansi option is included.
                     * defC : Specify default color for all values.
                     * colC : Specify ANSI color for given column (either given as short name or full name).
                     * valC : Specify ANSI color for the given columns when the value of the given value
                              columns matches the included regex.

    For escaping option separators the escape character '!' can be used. It's also used to escape itself.
    Note that if an option is included several times, then the last one will normally be the effective one.
    Some options like -a and -w are additive though and all option instances of those will be used.

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
    bt, bi, ot      - Book title, BookID, Original title
    ln, fn, ai      - Author last and first name, AuthorID
    nn, ng          - Author full name, Aggregated full name(s) per book.
    ge, gi          - Genre, GenreID
    dr, dg          - Date read, Aggregated dates
    dw, dwl         - Day of week numeral and Day of week string for Date read
    ti, sec         - Time of day and TotalSeconds for Date read
    own, la, beb    - Owned, Language, Bought Ebook
    st, stid        - Story, StoryID
    se, si, sp      - Series, SeriesID, Part in Series
    so, soid        - Source, SourceID
 
    To get the length of column values "l" can be appended to the short name for:
     - bt, ot, ln, fn, nn, ng, ge, dr, dg, st, se, so   
)"
	);
}

namespace Utils
{
	#define std_string_fmt_impl(fmtStr,resVar) \
		va_list ap; \
		va_start(ap, fmtStr); \
		size_t size = vsnprintf(nullptr, 0, fmtStr, ap) + 1; /* Extra space for '\0' */ \
		va_end(ap); \
		std::string resVar(size, '\0'); \
		va_start(ap, fmtStr); \
		vsnprintf(&res[0], size, fmtStr, ap); \
		res.pop_back(); /* Remove the trailing '\0' */ \
		va_end(ap)

	std::string fmt(_In_z_ _Printf_format_string_ const char* fmtStr, ...)
	{
		std_string_fmt_impl(fmtStr,res);
		return res;
	}

	template <typename T>
	void appendTo(std::vector<T>& a, const std::vector<T>& b)
	{
	    a.reserve(a.size() + b.size());
	    a.insert(a.end(), b.begin(), b.end());
	}

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
		int const nLen = (len > 0 ? len : lstrlenA(src));
		if (nLen == 0) return std::wstring();
		if (int const sizeNeeded = MultiByteToWideChar(codePage, 0, src, nLen, NULL, 0)) {
			std::wstring wstr(sizeNeeded, '\0');
			int const res = MultiByteToWideChar(codePage, 0, src, nLen, &wstr[0], sizeNeeded);
			if (res == sizeNeeded) {
				return wstr;
			}
		}
		throw std::runtime_error(fmt("MultiByteToWideChar for code page %i failed", codePage));
	}

	std::wstring toWide(int codePage, std::string const & str)
	{
		return toWide(codePage, str.c_str(), str.length());
	}

	std::string toNarrow(int codePage, const wchar_t *src, int len = 0)
	{
		int const nLen = (len > 0 ? len : lstrlenW(src));
		if (nLen == 0) return std::string();
		if (int const sizeNeeded = WideCharToMultiByte(codePage, 0, src, nLen, NULL, 0, NULL, NULL)) {
			std::string str(sizeNeeded, '\0');
			int const res = WideCharToMultiByte(codePage, 0, src, nLen, &str[0], sizeNeeded, NULL, NULL);
			if (res == sizeNeeded) {
				return str;
			}
		}
		throw std::runtime_error(fmt("WideCharToMultiByte for code page %i failed", codePage));
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
		std::string const nameDef; // Name or definition for column
		int         const defWidth;
		ColumnType  const type;
		std::string const label; // optional, used when name does not refer to a direct table column.
		bool        const isGroupAggregate;
		ColumnInfo const* lengthColumn; // Will be set only if the column has a length column added.

		// These values are set at runtime. Stored here for convenience.
		mutable int  overriddenWidth;
		mutable bool usedInQuery;

		std::string const& labelName() const { return label.empty() ? nameDef : label; }

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

	std::string input(const char* prompt, InputOptions options = required)
	{
	retry:
		printf("%s: ", prompt);
		auto value = readLine();
		if (value.empty() && (required & (unsigned)options)) {
			goto retry;
		}
		return value;
	}

	std::string input(const char* prompt, const char* regEx, InputOptions options = required)
	{
	retry:
		auto value = input(prompt, options);
		if (!value.empty() && !std::regex_match(value, std::regex(regEx))) {
			goto retry;
		}
		return value;
	}

	void input(std::string& value, const char* prompt, InputOptions options = required)
	{
		if (!value.empty()) { prefillInput(value); }
		value = input(prompt, options);
	}

	void input(std::string& value, const char* prompt, const char* regex, InputOptions options = required)
	{
	retry:
		input(value, prompt, options);
		if (!value.empty() && !std::regex_match(value, std::regex(regex))) {
			goto retry;
		}
	}

	using InputCheckIdFunction = std::function<void(IdValue)>;
	using InputListFunction = std::function<void(std::string const &)>;

	void input(
		IdValue& value, 
		const char* prompt,
		InputCheckIdFunction const& checkId,
		InputListFunction const& listValues,
		InputOptions options = required)
	{
		if (value != EmptyId) { prefillInput(std::to_string(value)); }
	retry:
		auto str = input(prompt, options);
		value = EmptyId;
		if (!str.empty()) {
			toIdValue(str, value);
			if (value == EmptyId && listValues) {
				listValues(str);
				goto retry;
			}
			else try { checkId(value); } catch (std::exception&) {
				printf("Invalid ID for this column, please try again.\n");
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

class OptionParser {
	std::stringstream m_ss;
	const char* const m_type;
	const char        m_delim;
public:
	OptionParser(std::string const & value, const char* type = "option", char delim = OptDelim)
		: m_ss(value), m_type(type), m_delim(delim)
	{}

	bool empty() const
	{
		return m_ss.eof() || m_ss.str().empty();
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
	const int Utf8Encoding = 65001;

	DWORD        m_consoleMode = 0;
	int    const m_consoleCodePage = GetConsoleCP();
	HANDLE const m_stdOutHandle    = GetStdHandle(STD_OUTPUT_HANDLE);
	bool   const m_stdOutIsConsole = m_stdOutHandle != NULL && GetConsoleMode(m_stdOutHandle, &m_consoleMode);
	int          m_encoding = Utf8Encoding; 
	// Got missing WriteConsole output with 32K buffer! 20K seems ok so far... but uses 10K to avoid analyse warning..
	// 32K seems to work at home with Win10 though, but not at work with Win7.
	static const int BufSize = 1000*10;
	mutable char m_buffer[BufSize];
	mutable int  m_bufPos = 0;
public:
	bool stdOutIsConsole() const { return m_stdOutIsConsole; }

	void setEncoding(int encoding) { m_encoding = encoding; }

	void writeUtf8Bom() const
	{
		if (m_encoding == Utf8Encoding && !m_stdOutIsConsole) {
			const unsigned char bom[] = { 0xEF, 0xBB, 0xBF };
			write((const char*)&bom[0], sizeof(bom));
		}
	}

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
			int res;
			if (m_encoding == Utf8Encoding) {
				res = fwrite(str, len, 1, stdout);
			}
			else {
				auto wstr = Utils::utf8ToWide(str, len);
				auto encStr = Utils::toNarrow(m_encoding, wstr.c_str(), wstr.length());
				res = fwrite(encStr.c_str(), encStr.length(), 1, stdout);
			}
			if (res != 1) {
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

struct SqliteCloser { void operator()(sqlite3* p) const { sqlite3_close(p); } };

class Litt {
	std::map<std::string, ColumnInfo> m_columnInfos; // Maps short name to column info.
	std::unique_ptr<sqlite3, SqliteCloser> m_conn;
	Output m_output;

	int const consoleCodePage = GetConsoleCP();
	bool m_headerOn = true;
	bool m_fitWidthOn = false;
	bool m_fitWidthAuto = true;
	int  m_fitWidthValue = 200;
	bool m_selectDistinct = false;
	bool m_showQuery = false;
	bool m_explainQuery = false;
	bool m_showNumberOfRows = false;
	DisplayMode m_displayMode = DisplayMode::column;
	std::string m_listSep = "|";
	std::string m_colSep = "  ";
	std::string m_dbPath; // Path to LITT db file

	Columns m_orderBy; // Overrides the default action order.
	Columns m_selectedColumns; // Overrides the default action columns.
	Columns m_additionalColumns; // Added to the action or overridden columns.

	mutable std::string m_whereCondition;
	mutable std::string m_havingCondition;

	std::string m_action;
	std::vector<std::string> m_actionArgs;
	std::string m_actionRightWildCard;
	std::string m_actionLeftWildCard;

	mutable int m_rowCount = 0; // The number of rows printed so far.

	ColumnInfo& addColumn(std::string const& sn, std::string const& nameDef, int defWidth, ColumnType type, std::string const& label = "", bool isGroupAgg = false)
	{
		auto res = m_columnInfos.emplace(std::make_pair(sn, ColumnInfo{ nameDef, defWidth, type, label, isGroupAgg })); _ASSERT(res.second);
		res.first->second.overriddenWidth = -1;
		return res.first->second;
	}

	ColumnInfo& addColumnText(std::string const & sn, std::string const & nameDef, int defWidth, std::string const & label = "")
	{
		return addColumn(sn, nameDef, defWidth, ColumnType::text, label);
	}

	ColumnInfo& addColumnNumeric(std::string const & sn, std::string const & nameDef, int defWidth, std::string const & label = "")
	{
		return addColumn(sn, nameDef, defWidth, ColumnType::numeric, label);
	}

	ColumnInfo& addColumnTextWithLength(std::string const & sn, std::string const & nameDef, int defWidth, std::string const & label = "" )
	{
		auto& ci = addColumnText(sn, nameDef, defWidth, label);
		auto const labelLength = "l_" + sn;
		auto & ciLength = addColumnNumeric(sn + "l", "length(" + nameDef + ")", labelLength.length(), labelLength);
		ci.lengthColumn = &ciLength;
		return ci;
	}

	ColumnInfo& addColumnNumericAggregate(std::string const & sn, std::string const & nameDef, int defWidth, std::string const & label = "")
	{
		return addColumn(sn, nameDef, defWidth, ColumnType::numeric, label, true); 
	}

public:
	Litt(int argc, char** argv)
	{
		// OBS! As a sn, don't use "desc", "asc" and any other name that may appear after one in the command line options!
		addColumnNumeric("ai", "AuthorID", 8);
		addColumnNumeric("beb", "\"Bought Ebook\"", 3);
		addColumnNumeric("bi", "BookID", 6);
		addColumnTextWithLength("bt", "Title", 45);
		addColumnTextWithLength("dr", "\"Date Read\"", 10);
		addColumnTextWithLength("dg", "\"Date(s)\"", 30);
		addColumnTextWithLength("fn", "\"First Name\"", 15);
		addColumnTextWithLength("ge", "Genre", 30);
		addColumnNumeric("gi", "GenreID", 8);
		addColumnTextWithLength("ln", "\"Last Name\"", 20); 
		addColumnTextWithLength("ng", "\"Author(s)\"", 50);
		addColumnTextWithLength("nn", "ltrim(\"First Name\"||' '||\"Last Name\")", 25, "Author");
		addColumnText("la", "Language", 4);
		addColumnNumeric("own", "Owned", 3);
		addColumnTextWithLength("ot", "\"Original Title\"", 45);
		addColumnTextWithLength("se", "Series", 40);
		addColumnNumeric("si", "SeriesID", 8);
		addColumnText("sp", "\"Part in Series\"", 4);
		addColumnTextWithLength("st", "Story", 45);
		addColumnNumeric("stid", "StoryID", 7);
		addColumnTextWithLength("so", "Source", 35);
		addColumnNumeric("soid", "SourceID", 8);

		// Columns for more formats of Date Read:
		addColumnNumeric("dw", "CAST(strftime('%w',\"Date Read\") AS INTEGER)", 3, "DOW");
		addColumnText("dwl", "CASE CAST(strftime('%w',\"Date Read\") AS INTEGER)"
					  " WHEN 0 THEN 'Sun' WHEN 1 THEN 'Mon' WHEN 2 THEN 'Tue' WHEN 3 THEN 'Wed' WHEN 4 THEN 'Thu' WHEN 5 THEN 'Fri' WHEN 6 THEN 'Sat' ELSE '' END",
					  5, "DoW");
		addColumnText("ti", "time(\"Date Read\")", 5, "Time");
		addColumnNumeric("sec", "CAST(strftime('%s',\"Date Read\") AS INTEGER)", 11, "Timestamp");

		// Special-purpose "virtual" columns, these are not generally usable:
		// Intended for use in -w for listBooksReadPerPeriod. Will end up in the HAVING clause of Total sub-query.
		addColumnNumericAggregate("prc", "Count(BookID)", 0);
		// Intended for use in listSametitle
		addColumnNumeric("btc", "TitleCount", 11, "\"Title count\"");
		// Intended for use in listRereads
		addColumnNumeric("brc", "ReadCount", 10, "\"Read count\"");
		// This is for the "number of books" column in list*BookCounts.
		addColumnNumericAggregate("bc",  "COUNT(Books.BookID)", 6, "Books");

		if (m_output.stdOutIsConsole()) {
			CONSOLE_SCREEN_BUFFER_INFO csbi{}; GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
			m_fitWidthValue = csbi.srWindow.Right - csbi.srWindow.Left;
		}

		for (int i = 1; i < argc; ++i) {
			if (argv[i][0] == '-' && argv[i][1] != '\0') { // A stand-alone '-' can be used as an (action) argument.
				auto const opt = argv[i][1];
				auto const val = std::string(argv[i][2] != '\0' ? &argv[i][2] : "");
				switch (opt) {
				case 'd':
					if (val == "col" || val == "column") m_displayMode = DisplayMode::column;
					else if (val == "html")    m_displayMode = DisplayMode::html;
					else if (val == "htmldoc") m_displayMode = DisplayMode::htmldoc;
					else if (val == "tabs")    m_displayMode = DisplayMode::tabs;
					else if (val.substr(0, 4) == "list") {
						m_displayMode = DisplayMode::list;
						if (4 < val.length()) {
							if (val[4] == ':' && 5 < val.length()) {
								m_listSep = toUtf8(val.substr(5).c_str());
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
				case 'e':
					int encoding;
					if (toInt(val, encoding)) {
						m_output.setEncoding(encoding);
					}
					else throw std::invalid_argument("Invalid encoding value: " + val);
					break;
				case 'f': {
					OptionParser fval(val);
					auto v = fval.empty() ? "on" : fval.getNext();
					if      (v == "on" || toInt(v, m_fitWidthValue)) { m_fitWidthOn = true;  m_fitWidthAuto = true; }
					else if (v == "off")                             { m_fitWidthOn = false; m_fitWidthAuto = false; }
					else if (v == "auto")                            { m_fitWidthOn = false; m_fitWidthAuto = true; }
					else throw std::invalid_argument("Invalid fit width value: " + val);
					break;
				}
				case 'h':
					if (val == "on"  || val == "") m_headerOn = true;
					else if (val == "off")         m_headerOn = false;
					else throw std::invalid_argument("Invalid header value: " + val);
					break;
				case 'o':
					m_orderBy = getColumns(val, ColumnsDataKind::sortOrder, true);
					break;
				case 'c': 
					m_selectedColumns = getColumns(val, ColumnsDataKind::width, true);
					break;
				case 'l': 
					m_dbPath = val;
					break;
				case 'a': 
					appendTo(m_additionalColumns, getColumns(val, ColumnsDataKind::width, true));
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
					m_showQuery = true;
					break;
				case 'u':
					m_selectDistinct = true;
					break;
				case 'x': 
					m_explainQuery = true;
					break;
				case 'n': 
					m_showNumberOfRows = true;
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
							col.charCmpCount = 0;
							std::string mm;
							if (extVal.getNext(mm)) {
								if (mm == "regex" || mm == "re" || mm == "ren") {
									col.matchMethod = (mm == "ren")
										? ConsRowMatchMethod::regExNot : ConsRowMatchMethod::regEx;
									col.re = getRegex(extVal.getNext());
									extVal.getNext(colName);
								}
								else if (mm == "dlt" || mm == "dgt") {
									col.matchMethod = (mm == "dlt")
										? ConsRowMatchMethod::diffLt : ConsRowMatchMethod::diffGt;
									col.diff = extVal.nextInt();
									extVal.getNext(colName);
								}
								else if (toInt(mm, col.charCmpCount)) {
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
								m_ansiEnabled = false;
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

		if (m_dbPath.empty()) {
			char mydocs[MAX_PATH];
			if (GetEnvironmentVariableA("MYDOCS", mydocs, MAX_PATH)) {
				m_dbPath = std::string(mydocs) + "\\litt\\" + DefDbName;
			}
			else {
				m_dbPath = DefDbName;
			}
		}
		if (GetFileAttributesA(m_dbPath.c_str()) == -1) {
			throw std::invalid_argument("Cannot find: " + m_dbPath);
		}
			
		sqlite3* conn = nullptr;
		int res = sqlite3_open(m_dbPath.c_str(), &conn); m_conn.reset(conn);
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
			appendToWhereCondition(LogOp_AND, col->nameDef + " LIKE " + col->getLikeArg(val));
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

	IdValue idargi(unsigned index, const char* name, 
		InputCheckIdFunction const& checkFunc, 
		InputListFunction const& listFunc,
		InputOptions iopt = Input::required) const
	{
		if (index < m_actionArgs.size()) return idarg(index, name);
		IdValue val = EmptyId;
		input(val, fmt("Enter %s", name).c_str(), checkFunc, listFunc, iopt);
		return val;
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

	std::string argi(unsigned index, const char* name, InputOptions iopt = Input::required) const 
	{
		return index < m_actionArgs.size()
			? m_actionArgs[index]
			: input(fmt("Enter %s", name).c_str(), iopt);
	}

	std::string argi(unsigned index, const char* name, const char* regEx, InputOptions iopt = Input::required) const
	{
		return index < m_actionArgs.size()
			? (std::regex_match(m_actionArgs[index], std::regex(regEx)) 
				? m_actionArgs[index] 
				: throw std::invalid_argument("Invalid date value: " + m_actionArgs[index]))
			: input(fmt("Enter %s", name).c_str(), regEx, iopt);
	}

	std::string toUtf8(std::string const & str) const   { return Utils::toUtf8(consoleCodePage, str); }
	std::string fromUtf8(std::string const & str) const { return Utils::fromUtf8(consoleCodePage, str); }

	std::string encodeSqlFromInput(std::string const& sql) const { return toUtf8(sql); }

	enum AuxTableOptions : unsigned {
		IJF_DefaultsOnly = 0x00,
		IJF_Stories = 0x01,
		IJF_Series = 0x02,
		IJF_OrigTitle = 0x04,
		Skip_AuthorBooks = 0x08,
		Skip_Stories = 0x10,
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
			if (litt.m_explainQuery) {
				m_sstr << "EXPLAIN QUERY PLAN ";
			}
			m_sstr << "SELECT ";
			if (litt.m_selectDistinct || selectOption == SelectOption::distinct) {
				m_sstr << "DISTINCT ";
			}
		}

		void initColumnWidths()
		{
			if (litt.m_explainQuery && litt.m_displayMode == DisplayMode::column) {
				columnWidths = { 10, 10, 10, 100 }; 
			} // else assumes columnWidths are properly set!
		}

		void addCol(ColumnInfo const * ci)
		{
			m_sstr << ci->nameDef;
			if (!ci->label.empty()) {
				m_sstr << " AS " << ci->label;
			}
		}

		void initSelect(const char* defColumns, const char* from, const char* defOrderBy, SelectOption selectOption = SelectOption::normal)
		{
			initSelectBare(selectOption);

			auto selCols = litt.m_selectedColumns.empty() ? litt.getColumns(defColumns, ColumnsDataKind::width, true) : litt.m_selectedColumns;
			appendTo(selCols, litt.m_additionalColumns);
			for (unsigned i = 0; i < selCols.size(); ++i) {
				if (i != 0) {
					m_sstr << ",";
				}
				auto ci = selCols[i].first;
				addCol(ci);
				if (litt.m_displayMode == DisplayMode::column) {
					auto const width = ci->overriddenWidth >= 0 ? ci->overriddenWidth : selCols[i].second;
					_ASSERT(width >= 0);
					columnWidths.push_back(width);
				}
			}
			initColumnWidths();

			m_sstr << "\nFROM " << from;

			// Not used here, but we must "run" it anyway in order to finalize all columns used in the query for later "addIfColumns" calls.
			auto asc = [](Columns cols) { for (auto& c : cols) { c.second = (int)ColumnSortOrder::Asc; } return cols; };
			m_orderBy = litt.m_orderBy.empty()
				? (litt.m_selectedColumns.empty()
					? litt.getColumns(defOrderBy, ColumnsDataKind::sortOrder, true)
					: asc(litt.m_selectedColumns))
				: litt.m_orderBy;
		}

		void addWhere(int indentSize = 0)
		{
			if (!litt.m_whereCondition.empty()) {
				m_sstr << "\n" << std::string(indentSize, ' ') << "WHERE " << litt.m_whereCondition;
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

		void adf(_In_z_ _Printf_format_string_ const char* fmtStr, ...)
		{
			std_string_fmt_impl(fmtStr, res);
			add(res);
		}

		void addIf(bool cond, const char* line)
		{
			if (cond) add(line);
		}

		void addIfColumns(const char* columns, const char* line) 
		{
			std::string sn;
			for (const char* c = columns; ; ++c) {
				if (*c == '.' || *c == '\0') {
					auto ci = litt.getColumn(sn);
					if (ci->usedInQuery || (ci->lengthColumn && ci->lengthColumn->usedInQuery)) { 
						add(line); 
						break;
					}
					if (*c == '\0') { break; }
					sn.clear();
				}
				else {
					sn.push_back(*c);
				}
			}
		}

		void addIfColumns(const char* columns, std::string const & line)
		{
			addIfColumns(columns, line.c_str());
		}

		void addAuxTables(AuxTableOptions opt = IJF_DefaultsOnly, unsigned indentSize = 0)
		{
			std::string indent(indentSize, ' ');
			auto serJoin = (opt & IJF_Series)    ? "INNER" : "LEFT OUTER";
			auto stoJoin = (opt & IJF_Stories)   ? "INNER" : "LEFT OUTER";
			auto ortJoin = (opt & IJF_OrigTitle) ? "INNER" : "LEFT OUTER";
			auto ng = "(SELECT BookID, ltrim(group_concat(\"First Name\"||' '||\"Last Name\",', ')) AS 'Author(s)' FROM Books INNER JOIN AuthorBooks USING(BookID) INNER JOIN Authors USING(AuthorID) GROUP BY BookID)";
			auto dg = "(SELECT BookID, group_concat(\"Date read\",', ') AS 'Date(s)' FROM Books INNER JOIN DatesRead USING(BookID) GROUP BY BookID)";

			addIfColumns("dr.dw.dwl.ti.sec.soid.so", indent + "INNER JOIN DatesRead USING(BookID)");
			addIfColumns("ng",                       indent + "INNER JOIN " + ng + " USING(BookID)");
			addIfColumns("dg",                       indent + "INNER JOIN " + dg + " USING(BookID)");
			if ((opt & Skip_AuthorBooks) == 0)
			addIfColumns("ai.fn.ln.nn.st.stid",      indent + "INNER JOIN AuthorBooks USING(BookID)");
			addIfColumns("fn.ln.nn",                 indent + "INNER JOIN Authors USING(AuthorID)");
			addIfColumns("ot",                       indent +  ortJoin + " JOIN OriginalTitles USING(BookID)");
			if ((opt & Skip_Stories) == 0)
			addIfColumns("stid.st",                  indent +  stoJoin + " JOIN Stories USING(AuthorID, BookID)");
			addIfColumns("si.sp.se",                 indent +  serJoin + " JOIN BookSeries USING(BookID)");
			addIfColumns("se",                       indent +  serJoin + " JOIN Series USING(SeriesID)");
			addIfColumns("so",                       indent +  "LEFT OUTER JOIN Sources USING(SourceID)");
			addIfColumns("gi.ge",                    indent +  "LEFT OUTER JOIN BookGenres USING(BookID)");
			addIfColumns("ge",                       indent +  "LEFT OUTER JOIN Genres USING(GenreID)");
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
					m_sstr << ci->nameDef;
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
		switch (m_displayMode) {
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
			switch (m_displayMode) {
			case DisplayMode::column:
				if (query.columnWidths[i] > 0) {
					if (i != 0) m_output.write(m_colSep);
					if (m_ansiEnabled) m_output.write(m_ansiRowColors[i].get());
					m_output.writeUtf8Width(val, query.columnWidths[i]);
					if (m_ansiEnabled && i == argc - 1) m_output.write(m_ansiDefColor);
				}
				break;
			case DisplayMode::list:
				if (i != 0) m_output.write(m_listSep);
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

		switch (m_displayMode) {
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

	void ansiSetRowColors(bool /*isHeader*/, int argc, char** argv) const
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
		int                charCmpCount;
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
				if (cvMatch) {
					auto const & prevVal = m_consRowBuffer[0][col.index];
					if (col.charCmpCount <= 0) {
						cvMatch = (prevVal == val);
					}
					else {
						auto const maxCmp  = (size_t)col.charCmpCount;
						auto const prevLen = prevVal.size();
						auto const len     = strlen(val);
						if (len != prevLen && (len < maxCmp || prevLen < maxCmp)) {
							// If one (or both) of them is less than maxCmp and they have different lengths, then they are not equal.
							cvMatch = false; 
						}
						else {
							auto const cmpCnt = std::min({maxCmp, prevLen, len});
							for (size_t i = 0; i < cmpCnt; ++i) {
								if (prevVal[i] != val[i]) {
									cvMatch = false;
									break;
								}
							}
						}
					}
				}
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
							cvMatch = (((long long)cur - (long long)prev) < col.diff);
						}
						else {
							cvMatch = (((long long)cur - (long long)prev) > col.diff);
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

	void writeBomIfNeeded() const
	{
		static bool wroteBom = false;
		if (wroteBom) return;

		if (m_displayMode == DisplayMode::column) {
			// HACK: Write the UTF-8 BOM, seems V/VIEW needs it to properly 
			// detect the utf-8 encoding depending on the actual output.
			// Seems to interfere with V:s CSV mode though!
			m_output.writeUtf8Bom();
			wroteBom = true;
		}
	}

	static int outputQueryCallBack(void *pArg, int argc, char **argv, char **azColName)
	{
		auto query = static_cast<OutputQuery*>(pArg);
		return query->litt.outputQueryCallBack(*query, argc, argv, azColName);
	}

	int outputQueryCallBack(OutputQuery& query, int argc, char **argv, char **azColName) const
	{
		try {
			if (m_rowCount == 0) {
				if (m_displayMode == DisplayMode::column) {
					for (int i = query.columnWidths.size(); i < argc; ++i) {
						query.columnWidths.push_back(std::min(size_t{30},
							std::max(strlen(azColName[i]), strlen(rowValue(argv[i])))));
					}

					if (m_fitWidthOn) {
						size_t const target = m_fitWidthValue;
						for (auto& w : query.columnWidths) { if (w > target) w = target; } // Guard against HUGE sizes, can be slow to inc!
						size_t current = 0; for (auto w : query.columnWidths) { current += (w + 2); } current -= 2;
						if (current != target) {
							int const inc = (target > current) ? 1 : -1;
							size_t diff = abs((long)(target - current));
							for (;;) {
								size_t changed = 0;
								for (auto& w : query.columnWidths) {
									// Don't touch columns with smallish widths (IDs, dates)
									// Also don't make arbitrarily small and large. 
									// Currently widest value in any column (not ng!) is 59.
									if (10 < w && (inc < 0 || w < 60)) {
										w += inc;
										changed += abs(inc);
										if (changed >= diff) goto done;
									}
								}
								if (changed == 0) goto done;
								diff -= changed;
							}
							done:;
						}
					}

					if (m_ansiEnabled) {
						ansiInit(argc, azColName);
					}
				}

				writeBomIfNeeded();

				if (m_displayMode == DisplayMode::htmldoc) {
					auto docStart = 
						"<!DOCTYPE html>\n" 
						"<html>\n"
						"<head>\n"
						"<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n"
						"<title>" + m_action + "</title>\n"
						"</head>\n"
						"<body>\n"
						"<table>\n";
					m_output.write(docStart);
				}
				if (m_headerOn) {
					outputRow(query, true, argc, azColName);
					if (m_displayMode == DisplayMode::column) {
						for (int i = 0; i < argc; ++i) {
							if (query.columnWidths[i] > 0) {
								if (i != 0) m_output.write(m_colSep);
								std::string underLine(query.columnWidths[i], '-');
								m_output.write(underLine);
							}
						}
						m_output.write('\n');
					}
				}
				if (consEnabled()) {
					consInit(argc, argv, azColName);
				}
			} // if (litt.m_rowCount == 0) {

			if (consEnabled()) {
				consProcessRow(query, argc, argv);
			}
			else {
				outputRow(query, false, argc, argv);
			}
			++m_rowCount;
			return 0;
		}
		catch (std::exception& ex) {
			m_output.flushNoThrow();
			fprintf(stderr, "\nCallback exception: %s\n", ex.what());
			return 1;
		}
	}

	void runOutputQuery(OutputQuery& query)
	{
		std::string sql = query.getSql();

		if (m_showQuery) {
			m_output.write(sql); m_output.write('\n');
			m_output.flush();
			return;
		}

		m_rowCount = 0;
		int res = sqlite3_exec(m_conn.get(), sql.c_str(), outputQueryCallBack, &query, nullptr);
		if (res == SQLITE_OK) {
			if (m_displayMode == DisplayMode::htmldoc) {
				m_output.write("</table>\n</body>\n</html>\n");
			}
			if (consEnabled()) {
				consOutputMatchedCount(); // In case matching was still ongoing at the last row.
			}
		}
		if (m_ansiEnabled && m_displayMode == DisplayMode::column && m_rowCount > 0) {
			m_output.write(m_ansiDefColor);
		}
		m_output.flushNoThrow();
		if (res != SQLITE_OK) {
			throw std::runtime_error(fmt("SQL error: %s", sqlite3_errmsg(m_conn.get())));
		}

		if (m_showNumberOfRows) {
			printf("\n# = %i\n", m_rowCount);
		}
	}

	void runSingleTableOutputCmd(const char* defColumns, const char* table, const char* defOrderBy)
	{
		m_fitWidthOn = m_fitWidthAuto;
		OutputQuery query(*this);
		query.initSelect(defColumns, table, defOrderBy);
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void runListData(const char* defColumns, const char* defOrderBy, AuxTableOptions opt = IJF_DefaultsOnly)
	{
		m_fitWidthOn = m_fitWidthAuto;
		OutputQuery query(*this);
		query.initSelect(defColumns, "Books", defOrderBy);
		query.addAuxTables(opt);
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listAuthors(std::string const & action, std::string const & ln, std::string const & fn)
	{
		addActionWhereCondition("ln", ln);
		addActionWhereCondition("fn", fn);
		if (action == "a") {
			runSingleTableOutputCmd("ai.nn.50", "Authors", "ai");
		}
		else {
			runListData("bi.nn.bt.dr.so.ge", "ai.dr.bi");
		}
	}

	void listBooks(std::string const & action, std::string const & title)
	{
		addActionWhereCondition("bt", title);
		if (action == "b") {
			runSingleTableOutputCmd("bi.bt.70", "Books", "bi");
		}
		else {
			runListData("bi.nn.bt.dr.so.ge", "dr.bi.ln.fn");
		}
	}

	void listSeries(std::string const & action, std::string const & series)
	{
		addActionWhereCondition("se", series);
		if (action == "s") {
			runSingleTableOutputCmd("si.se.70", "Series", "si");
		}
		else {
			runListData("se.sp.bt.dr.bi.nn", "se.sp.dr.bi.ln.fn", IJF_Series);
		}
	}

	void listGenres(std::string const & action, std::string const & genre)
	{
		addActionWhereCondition("ge", genre);
		if (action == "g") {
			runSingleTableOutputCmd("gi.ge.50", "Genres", "ge");
		}
		else {
			runListData("ge.bi.bt.dr.nn", "ge.dr.bi.ln.fn");
		}
	}

	void listOriginalTitles()
	{
		addActionWhereCondition("ot", 0);
		runListData("bi.nn.ot.bt.dr.so.ge", "ot.dr.bi.ln.fn", IJF_OrigTitle);
	}

	void listStories()
	{
		addActionWhereCondition("st", 0);
		runListData("bi.bt.st.nn.dr", "bt.bi.st.ln.fn", IJF_Stories);
	}

	void listSources(std::string const & action, std::string const & sourceName)
	{
		addActionWhereCondition("so", sourceName);
		if (action == "so") {
			runSingleTableOutputCmd("soid.so.50", "Sources", "so");
		}
		else {
			runListData("so.bi.bt.dr.nn", "so.dr.bi.ln.fn");
		}
	}

	void listRereads()
	{
		OutputQuery query(*this);
		const char* from = "(SELECT BookID, Count(BookID) As ReadCount FROM DatesRead GROUP BY BookID HAVING Count(BookID) > 1)";
		query.initSelect("brc.bt.dr.ng", from, "brc.desc.ln.bt.dr");
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
		query.initSelect("bi.bt.ng.btc", from, "bt.bi");
		query.add("INNER JOIN Books USING(Title)");
		query.addAuxTables();
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listSamestory() 
	{
		m_fitWidthOn = m_fitWidthAuto;
		OutputQuery query(*this);
		const char* from = 
			"(SELECT DISTINCT S1.* FROM Stories AS S1 JOIN Stories as S2"
			" WHERE S1.Story = S2.Story AND S1.StoryID <> S2.StoryID AND S1.BookID <> S2.BookID)";
		query.initSelect("st.nn.bi.bt.dr.so", from, "st.nn.dr");
		query.add("INNER JOIN Books USING(BookID)");
		query.addAuxTables(AuxTableOptions(
			Skip_AuthorBooks | // Already have AuthorID from S1 so skip to avoid need for SELECT DISTINCT.
			Skip_Stories));    // Already have Stories content from S1 so skip to avoid ambigous column error.
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listTitlestory()
	{
		m_fitWidthOn = m_fitWidthAuto;
		OutputQuery query(*this);
		query.columnWidths = { 6,4,20,10,15,20,8,15,20,10,15 };
		query.initColumnWidths();
		query.initSelectBare();
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
	INNER JOIN Books USING(BookID)
	INNER JOIN Authors USING(AuthorID)
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
		query.add("GROUP BY " + getColumn(snGroupBy)->nameDef);
		query.addHaving();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listYearlyBooksCounts(int count, int firstYear, int lastYear, const char* snColSelect, const char* snColGroupBy)
	{
		m_fitWidthOn = m_fitWidthAuto;

		auto col = getColumn(snColSelect);  col->usedInQuery = true;
		auto bc  = getColumn("bc");         bc->usedInQuery = true;
		auto gby = getColumn(snColGroupBy); gby->usedInQuery = true;

		OutputQuery q(*this);
		q.columnWidths = { 3 }; for (int y = firstYear; y <= lastYear; ++y) q.columnWidths.push_back(30);
		q.initColumnWidths();
		q.add("ATTACH DATABASE ':memory:' AS mdb;");
		q.add("CREATE TABLE mdb.Res (\"#\" INTEGER PRIMARY KEY"); // Create result set in-place due to SQLite's 64-way join limit.
		for (int year = firstYear; year <= lastYear; ++year) q.adf(",\"%i\" TEXT", year);
		q.add(");");
		q.adf("INSERT INTO mdb.Res (\"#\") WITH RECURSIVE c(x) AS (SELECT 1 UNION ALL SELECT x+1 FROM c WHERE x<%i) SELECT * FROM c;\n", count);
		for (int year = firstYear; year <= lastYear; ++year) {
			auto ycond = appendConditions(LogOp_AND, m_whereCondition, getWhereCondition(fmt("dr.%i-*", year)));
			q.adf("CREATE TABLE mdb.Year%i AS SELECT printf('%%3i - %%s',%s,%s) AS \"%i\"", year, bc->nameDef.c_str(), col->nameDef.c_str(), year);
			q.add("FROM BOOKS");
			q.addAuxTables();
			q.adf("WHERE %s", ycond.c_str());
			q.adf("GROUP BY %s", gby->nameDef.c_str());
			q.addHaving();
			q.adf("ORDER BY %s DESC, %s", bc->nameDef.c_str(), col->nameDef.c_str());
			q.adf("LIMIT %i;", count);
			q.adf("UPDATE mdb.Res SET \"%i\" = (SELECT \"%i\" FROM mdb.Year%i WHERE rowId=mdb.Res.\"#\");\n", year, year, year);
		} q.a("\n");
		q.initSelectBare(); q.a("* FROM mdb.Res ORDER BY \"#\";");
		q.add("DETACH DATABASE mdb");
		runOutputQuery(q);
	}

	void listBooksReadPerDate(std::string countCond)
	{
		if (countCond.empty()) countCond = "2";

		// We count dates between time 00:00 to 06:00 as the previous day (was up late reading, so want them counted to prev day).
		std::string calcDRTimeWindow = "case when (time(\"Date Read\") > '00:00:00' and time(\"Date Read\") < '06:00:00') then date(\"Date Read\", '-6 hours') else date(\"Date Read\") end";

		OutputQuery query(*this);
		query.initSelect("dr.bt.nn", "Books", "dr.bt"); 
		getColumn("dr")->usedInQuery = true; // in case of -c!
		query.addAuxTables();
		query.add("WHERE " + calcDRTimeWindow + " IN");
		query.add(" (SELECT CalcDR FROM (SELECT " + calcDRTimeWindow + " as CalcDR FROM DatesRead)");
		query.add("  GROUP BY CalcDR");
		query.add("  HAVING " + parseCountCondition("Count(CalcDR)", countCond) + ")");
		if (!m_whereCondition.empty()) {
		query.add(" AND " + m_whereCondition);
		}
		query.addOrderBy();
		runOutputQuery(query);
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
		if (!res.empty()) {
			writeBomIfNeeded();
			for (auto const & pc : res) {
				m_output.writeUtf8Width(toUtf8(pc.name).c_str(), width);
				m_output.write(" : ");
				m_output.write(toUtf8(pc.definition));
				m_output.write("\n");
			}
			m_output.write("\n");
		}
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
		getColumn("dr")->usedInQuery = true; // Need to JOIN with DatesRead also when not used in WHERE.
		std::string periodFunc;
		if      (periodDef == "%Y")    periodFunc = "substr(\"Date Read\",1,4)";
		else if (periodDef == "%m")    periodFunc = "substr(\"Date Read\",6,2)";
		else if (periodDef == "%Y-%m") periodFunc = "substr(\"Date Read\",1,7)";
		else                           periodFunc = fmt("strftime('%s', \"Date Read\")", periodDef.c_str());

		q.initSelectBare(); q.a("Main." + period + " AS " + period + ", Total"); for (auto& c : columns) { q.a(", " + c.name); }; q.a(" FROM");
		q.add(" (SELECT " + period + ", Count(BookID) as Total FROM");
		q.add("   (SELECT BookID, " + periodFunc + " AS " + period);
		q.add("    FROM Books");
		q.addAuxTables(IJF_DefaultsOnly, 4);
		q.addWhere(4);
		q.add("   )");
		q.add("  GROUP BY " + period);
		q.add(" ) Main");

		for (auto& c : columns) {
		// We don't update the having condition, it should be same for all columns and not included in col defs.
		auto ccond = appendConditions(LogOp_AND, m_whereCondition, getWhereCondition(c.definition));
		q.add(" LEFT OUTER JOIN");
		q.add(" (SELECT " + period + ", Count(BookID) AS " + c.name + " FROM");
		q.add("    (SELECT BookID, " + periodFunc + " AS " + period);
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
		if (enableShowQuery && m_showQuery) {
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
	std::string selTitle(IdValue id) const { return selDV(fmt("SELECT Title FROM Books WHERE BookID=%llu", id), "BookID"); }
	std::string selSeries(IdValue id) const { return selDV(fmt("SELECT Series FROM Series WHERE SeriesID=%llu", id), "SeriesID"); }
	std::string selSource(IdValue id) const { return selDV(fmt("SELECT Source FROM Sources WHERE SourceID=%llu", id), "SourceID"); }
	std::string selGenre(IdValue id) const { return selDV(fmt("SELECT Genre FROM Genres WHERE GenreID=%llu", id), "GenreID"); }
	std::string selAuthor(IdValue id) const { return selDV(fmt("SELECT \"First Name\" || ' ' || \"Last Name\" FROM Authors WHERE AuthorID=%llu", id), "AuthorID"); }

	InputCheckIdFunction cf(std::string (Litt::*selMethod)(IdValue id) const) const
	{
		return [=](IdValue id) { (this->*selMethod)(id); };
	}

	#define LIST_F(listCode) [&](std::string const & s) { resetWhere(); listCode; }
	InputListFunction getListBook()   { return LIST_F(listBooks  ("b",  s + WcS));       }
	InputListFunction getListAuthor() { return LIST_F(listAuthors("a",  s + WcS, ""));   }
	InputListFunction getListSource() { return LIST_F(listSources("so", WcS + s + WcS)); }
	InputListFunction getListGenre()  { return LIST_F(listGenres ("g",  WcS + s + WcS)); }
	InputListFunction getListSeries() { return LIST_F(listSeries ("s",  WcS + s + WcS)); }
	#undef LIST_F

	#define ESC_S(str) escSqlVal(str).c_str()

	void addAuthor(std::string const & ln, std::string const & fn)
	{
		executeInsert(fmt("INSERT INTO Authors (\"Last Name\",\"First Name\") VALUES(%s,%s)", ESC_S(ln), ESC_S(fn)), "AuthorID");
	}

	void addGenre(std::string const & name)
	{
		executeInsert(fmt("INSERT INTO Genres (Genre) VALUES(%s)", ESC_S(name)), "GenreID");
	}

	void addSeries(std::string const & name)
	{
		executeInsert(fmt("INSERT INTO Series (Series) VALUES(%s)",  ESC_S(name)), "SeriesID");
	}

	void addSource(std::string const & name)
	{
		executeInsert(fmt("INSERT INTO Sources (Source) VALUES(%s)",  ESC_S(name)), "SourceID");
	}

	void addBook(const char* drRegEx)
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
			input(aid, "AuthorID", cf(&Litt::selAuthor), getListAuthor(), optional);
			if (aid == EmptyId) {
				if (i < authors.size()) { authors.erase(authors.begin() + i); }
				if (i < authors.size() || (i == 0 && !title.empty())) continue; else break;
			}
			auto story = (i < authors.size()) ? std::get<1>(authors[i]) : std::string();
			input(story, "Story name (optional)", optional);
			if (authors.size() <= i) { authors.reserve((i+1)*2); authors.resize(i + 1); }
			authors[i++] = std::make_tuple(aid, story);
		}
		if (authors.empty() && title.empty()) {
			return;
		}
		input(title, "Book title");
		input(dateRead, "Date read", drRegEx);
		input(sourceId, "Book SourceID", cf(&Litt::selSource), getListSource());
		input(genreId, "Book GenreID", cf(&Litt::selGenre), getListGenre());
		input(origtitle, "Original title (optional)", optional);
		ask("es", "Language", lang);
		ask("yn", "Own book", owns);
		ask("yn", "Bought ebook", boughtEbook);
		input(seriesId, "SeriesID (optional)", cf(&Litt::selSeries), getListSeries(), optional);
		if (seriesId != EmptyId) {
			input(seriesPart, "Part in series");
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
			case 'n': if (confirm("Discard entered values")) return; else goto enterBook;
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

	void addStory(IdValue bookId, IdValue authorId, std::string const & story)
	{
		if (confirm(fmt("Add story '%s' to '%s [%llu]' for author %s [%llu]", 
			story.c_str(), selTitle(bookId).c_str(), bookId, selAuthor(authorId).c_str(), authorId))) {
			auto sql = fmt("INSERT OR IGNORE INTO AuthorBooks (AuthorID,BookID) VALUES(%llu,%llu);", 
				authorId, bookId);
			sql.append(fmt("INSERT INTO Stories (StoryID,AuthorID,BookID,Story) VALUES(NULL,%llu,%llu,%s)",
				authorId, bookId, ESC_S(story)));
			executeInsert(sql, "StoryID");
		}
	}

	void addBookToSeries(IdValue bookId, IdValue seriesId, std::string const & part)
	{
		if (confirm(fmt("Add '%s [%llu]' to '%s [%llu]' as part %s", 
			selTitle(bookId).c_str(), bookId, selSeries(seriesId).c_str(), seriesId, part.c_str()))) {
			executeInsert(fmt("INSERT OR REPLACE INTO BookSeries (BookID,SeriesID,\"Part in Series\") VALUES(%llu,%llu,%s)", 
				bookId, seriesId, escSqlVal(part, true).c_str()), "BookSeries");
		}
	}

	void setBookGenre(IdValue bookId, IdValue genreId, IdValue newGenreId)
	{
		if (newGenreId != EmptyId) {
			if (confirm(fmt("Change '%s' => '%s' for '%s'", 
				selGenre(genreId).c_str(), selGenre(newGenreId).c_str(), selTitle(bookId).c_str()))) {
				int changes = executeSql(fmt("UPDATE BookGenres SET GenreID=%llu WHERE BookID=%llu AND GenreID=%llu", 
					newGenreId, bookId, genreId));
				printf("Updated %i rows\n", changes);
			}
		}
		else {
			if (confirm(fmt("Remove '%s' from '%s'", selGenre(genreId).c_str(), selTitle(bookId).c_str()))) {
				int changes = executeSql(fmt("DELETE FROM BookGenres WHERE BookID=%llu AND GenreID=%llu",
					bookId, genreId));
				printf("Deleted %i rows\n", changes);
			}
		}
	}

	void addDateRead(IdValue bookId, std::string const& dr, IdValue sourceId)
	{
		if (confirm(fmt("Add date read '%s' with source '%s' to '%s'", 
			dr.c_str(), selSource(sourceId).c_str(), selTitle(bookId).c_str()))) {
			int changes = executeSql(fmt("INSERT INTO DatesRead (BookID,\"Date Read\",SourceID) VALUES(%llu,%s,%llu)", 
				bookId, ESC_S(dr), sourceId));
			printf("Added %i rows\n", changes);
		}
	}

	void setBookDateRead(IdValue bookId, std::string const& dr, std::string const& newDr)
	{
		if (newDr != "delete") {
			if (confirm(fmt("Change date read '%s' => '%s' for '%s'", dr.c_str(), newDr.c_str(), selTitle(bookId).c_str()))) {
				int changes = executeSql(fmt("UPDATE DatesRead SET \"Date Read\"=%s WHERE BookID=%llu AND \"Date Read\"=%s",
					ESC_S(newDr), bookId, ESC_S(dr)));
				printf("Updated %i rows\n", changes);
			}
		}
		else {
			if (confirm(fmt("Remove date read '%s' from '%s'", dr.c_str(), selTitle(bookId).c_str()))) {
				int changes = executeSql(fmt("DELETE FROM DatesRead WHERE BookID=%llu AND \"Date Read\"=%s",
					bookId, ESC_S(dr)));
				printf("Deleted %i rows\n", changes);
			}
		}
	}

	void setOriginalTitle(IdValue bookId, std::string const & originalTitle)
	{
		if (confirm(fmt("Set original title of '%s' => '%s'", selTitle(bookId).c_str(), originalTitle.c_str()))) {
			int changes = executeSql(fmt("INSERT OR REPLACE INTO OriginalTitles (BookID, \"Original Title\") VALUES (%llu, %s)", 
				bookId, ESC_S(originalTitle)));
			printf("Updated %i rows\n", changes);
		}
	}

	void executeSimpleAddAction(const char* name, void (Litt::*addMethod)(std::string const&), unsigned argIndex = 0)
	{
		auto arg = argi(argIndex, name, optional);
		if (!arg.empty() && confirm(fmt("Add %s '%s'", name, arg.c_str()))) {
			(this->*addMethod)(arg);
		}
	}

	static const char* getDateReadRegEx(bool flexible)
	{
		return flexible ? R"(\d\d\d\d-.*)" : R"(\d\d\d\d-\d\d-\d\d \d\d\:\d\d)";
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
			listBooks(action, arg(0));
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
			listBookCounts(arg(0), arg(1) == "1", "nn.35", "ai");
		}
		else if (action == "gbc") {
			listBookCounts(arg(0), arg(1) == "1", "ge.35", "gi");
		}
		else if (action == "sbc") {
			listBookCounts(arg(0), true, "so.35", "soid"); // DR always included when SO is.
		}
		else if (action == "abcy" || action == "gbcy" || action == "sbcy") {
			auto count = intarg(0, "count", 10);
			auto firstYear = intarg(1, "firstYear", GetSystemTime().wYear - 4);
			auto lastYear = intarg(2, "lastYear", firstYear + 4);
			auto snSel = "nn"; auto snGby = "ai"; // assume 'a' by default.
			switch (action[0]) {
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
			auto def = argm(0, "periodDef");
			auto name = argm(1, "periodName");
			listBooksReadPerPeriod(def, name, arg(2, WcS), getPeriodColumns(3));
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
			auto firstYear = intarg(0, "firstYear", st.wYear);
			auto lastYear  = intarg(1, "lastYear", st.wYear);
			std::vector<PeriodColumn> yearColumns;
			for (int y = firstYear; y <= lastYear; ++y) {
				char def[10]; sprintf_s(def, "dr.%04d-*", y);
				yearColumns.push_back({ def, std::to_string(y) });
			}
			appendToWhereCondition(LogOp_AND, getWhereCondition(fmt("dr.range.%i-01-01.%i-12-31", firstYear, lastYear)));
			listBooksReadPerPeriod("%m", "Month", WcS, yearColumns);
		}
		else if (action == "add-a" || action == "adda") {
			auto lastName = argi(0, "last name", optional); if (lastName.empty()) return;
			auto firstName = argi(1, "first name", optional); // May be empty.
			if (confirm(fmt("Add author '%s, %s'", lastName.c_str(), firstName.c_str()))) {
				addAuthor(lastName, firstName);
			}
		}
		else if (action == "add-g" || action == "addg") {
			executeSimpleAddAction("genre", &Litt::addGenre);
		}
		else if (action == "add-s" || action == "adds") {
			executeSimpleAddAction("series", &Litt::addSeries);
		}
		else if (action == "add-so" || action == "addso") {
			executeSimpleAddAction("book source", &Litt::addSource);
		}
		else if (action == "add-b" || action == "addf-b" || action == "addb") {
			addBook(getDateReadRegEx(action == "addf-b"));
		}
		else if (action == "add-st" || action == "addst") {
			if (auto bid = idargi(0, "BookId", cf(&Litt::selTitle), getListBook(), optional)) {
				auto aid = idargi(1, "AuthorId", cf(&Litt::selAuthor), getListAuthor());
				auto story = argi(2, "Story");
				addStory(bid, aid, story);
			}
		}
		else if (action == "b2s" || action == "btos") {
			if (auto bid = idargi(0, "BookId", cf(&Litt::selTitle), getListBook(), optional)) {
				auto sid  = idargi(1, "SeriesID", cf(&Litt::selSeries), getListSeries());
				auto part = argi(2, "Part");
				addBookToSeries(bid, sid, part);
			}
		}
		else if (action == "set-g" || action == "setg") {
			if (auto bid = idargi(0, "BookId", cf(&Litt::selTitle), getListBook(), optional)) {
				auto genreId = idargi(1, "GenreId", cf(&Litt::selGenre), getListGenre());
				// Check that genreId exists for book.
				selectSingleValue(fmt("SELECT GenreId FROM BookGenres WHERE BookID=%llu AND GenreID=%llu", bid, genreId),
					fmt("GenreId %llu for bookId %llu", genreId, bid).c_str());
				auto newGenreId = idargi(2, "New GenreId", cf(&Litt::selGenre), getListGenre());
				setBookGenre(bid, genreId, newGenreId);
			}
		}
		else if (action == "add-dr" || action == "addf-dr") {
			if (auto bid = idargi(0, "BookId", cf(&Litt::selTitle), getListBook(), optional)) {
				auto dateRead = argi(1, "Date read", getDateReadRegEx(action == "addf-dr"));
				auto sourceId = idargi(2, "SourceID", cf(&Litt::selSource), getListSource());
				addDateRead(bid, dateRead, sourceId);
			}
		}
		else if (action == "set-dr" || action == "setdr") {
			if (auto bid = idargi(0, "BookId", cf(&Litt::selTitle), getListBook(), optional)) {
				auto dr = argi(1, "Current date read");
				// Check that dr exists for book.
				selectSingleValue(fmt("SELECT BookID FROM DatesRead WHERE BookID=%llu AND \"Date Read\"=%s", bid, ESC_S(dr)), 
					fmt("date read %s for bookId %llu", dr.c_str(), bid).c_str());
				auto newDr = argi(2, "New date read or 'delete' to remove");
				setBookDateRead(bid, dr, newDr);
			}
		}
		else if (action == "set-ot" || action == "setot") {
			if (auto bid = idargi(0, "BookId", cf(&Litt::selTitle), getListBook(), optional)) {
				auto ot = argi(1, "Original title");
				setOriginalTitle(bid, ot);
			}
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
