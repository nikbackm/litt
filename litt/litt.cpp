// LITT - now for C++! 
//
#include "stdafx.h"

void showHelp(int level = 0)
{
	fputs(
R"(Usage: LITT {options} <action with arguments> {options}

Basic list actions:
   h[0|1|2]                       Show help, level 0..2, level 2 is default.
   a|aa   [lastName] [firstName]  List authors - without/with books.
   ps     [lastName] [firstName]  List pseudonyms.
   b|bb   [title]                 List books - with minimum/full details.
   ot     [origTitle]             List original titles for books.
   st|stt [story]                 List (anthology) stories for books.
   s|ss   [series]                List series - without/with books.
   g|gg   [genre]                 List genre - without/with books.
   so|soo [source]                List book sources where a certain book "read" was gotten - without/with books.
   c|cc   [bookCategory]          List book category - without/with books.
   l|ll   [language]              List language - without/with books.

   rereads                        List re-read books. Can use virtual column "brc" - Book Read Count.
   reot                           List re-read original titles; translated books also read as original title books.
   sametitle                      List books with same title. Can use virtual column "btc" - Book Title Count.
   sameisbn                       List books with same ISBN.
   samestory                      List stories with same title.
   titlestory                     List books with same title as a story - Duplicates shown.
   brd [booksReadCond]            List the dates and books where [cond] (default 2) books where read.

List book counts or sums as determined by --cnt option. Can use virtual columns bc, bcp, bcw and bckw:
   abc|gbc|cbc [bookCountCond] [bRRs]  - For author, genre, category. bRRs=1 => include re-reads.
   lbc|obc     [bookCountCond] [bRRs]  - For language and original title language. bRRs=1 => include re-reads.
   sbc         [bookCountCond]         - For source. Re-reads are always included.

   abcy|gbcy|sbcy|cbcy|lbcy|obcy [rowCount] [firstYear] [lastYear]
                                       - Yearly book counts for author, genre, source, category, 
                                         language and original title language.

   These listings additionally support the virtual columns bca, bcg, bcst, bcso and bcc paired with
   the corresponding --cnt option values.
    
   brmy [firstYear] [lastYear]         - Over month/year plus Total.
   brym [yearCondition]                - Over year/month plus Total.

   brm|bry|brwd [periodCondition] {<columnWhereCond> <columnName>}
                                       - Total over year-months, years and weekdays with optional extra columns.

   brp <periodColumn-strftime-def> <periodColumnName> [periodCondition] {<columnWhereCond> <columnName>}
                                       - Generalization of brm,bry,brwd, can customize the period and its name.
)", stdout); if (1 <= level) fputs(
R"(
Adding and modifying data:
   add-b                                   Add a book.
   add-dr   [BookID] [dr] [SourceId]       Add a 'date read' for a book with given source.
   add-a    [lastName] [firstName]         Add an author.
   add-s    [series]                       Add a series.
   add-g    [genre]                        Add a genre.
   add-so   [source]                       Add a book source.
   add-c    [category]                     Add a book category.
   add-st   [BookID] [AID] [story] [rat]   Add a story for a book.
   add-bg   [BookID] [GenreID]             Add a genre for a book.
   add-stg  [StoryID] [GenreID]            Add a genre for a story.
   
   set-r    [BookID] [rating]              Set rating for a book.
   set-str  [StoryID] [rating]             Set rating for a story.
   set-dr   [BookID] [newDr|delete] [dr|i] Change or delete 'date read' for a book. Dr/Index optional if #dr = 1.
   set-so   [BookID] [SourceId] [dr|i]     Change source for a 'date read'. Dr/Index optional if #dr = 1.
   set-g    [BookID] [GenreID] [newGID]    Change genre for a book. (Specify newGID=0 to delete)
   set-stg  [StoryID] [GenreID] [newGID]   Change genre for a story. (see above)
   set-ot   [BookID] [origTitle|delete]    Set or delete the original title for a book.
   set-s    [BookID] [SID] [part|delete]   Set or delete series for a book.
   set-bd   [BookID] [pubdate]             Set first publication date for a book.
   set-otd  [BookID] [otPubdate]           Set first publication date of the original title for a book.
   set-own  [BookID] [owned]               Set owned for a book.

   execute   [sqlString]                   Execute the given SQL string. Use with CAUTION!
)", stdout); if (2 <= level) fputs(
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
    -q[d]             Use debug mode - dump the SQL query/command instead of executing it. 
                      Adding 'd' also lists default columns for the action.
    -u                Make sure the result only contain DISTINCT (UNIQUE) values.
    -l[dbPath]        Specify litt-sqlite database file. Uses "litt.sqlite" by default. Either from the
                      current directory or from "%MYDOCS%\litt\" if MYDOCS is set.
    -n                Print number of output rows at the end.
    -e[encoding]      Output encoding for pipes and redirection. Default is utf8.
    -f[on|off|auto|w] Fit column width mode. Default is auto => fit only when console is too narrow.
                      Specifying an explicit width value implies mode "on". If no value is specified then 
                      the width of the console is used. If there is no console then a hard-coded value is used.
    -y[on|off]        Automatic YES to all confirm prompts. Default is off.
    -x[1|2|3]         Explains the query plan. x/x1 = graph EQP output, x2 = VM code output, x3 = raw EQP output.

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

    --cnt:[b|p|w|kw] Specify what to count in book count listings. Default (b) is books.
                     Can also use p for pages, w for words or kw for kilo-words.
                     For the br* listings these are also supported:
                     * a  = authors
                     * g  = genres
                     * st = stories
                     * so = sources
                     * c  = categories

    --drr:[f|l|m|r]  Specify which date in a date range value (yyyy-mm-dd..yyyy-mm-dd) that will be
                     used in book count listings for date periods.
                     Default is (f)irst, can also use (l)ast, (m)iddle or (r)andom.

    --limit:<n>      LIMIT value.
    --offset:<n>     OFFSET value.

    --colsep:<str>   The column separator used in columns display mode. Default = "  "
    
    For escaping option separators the escape character '!' can be used. It's also used to escape itself.
    Note that if an option is included several times, then the last one will normally be the effective one.
    Some options like -a and -w are additive though and all option instances of those will be used.

selColumns format: <shortName>[.<width>]{.<shortName>[.<width>]}
addColumns format: Same as selColumns format.
colOrder format: <shortName|actualName>[.asc|desc]{.<shortName|actualName>[.asc|desc]}
whereCond format: <shortName>[.<cmpOper>].<cmpArg>{.<shortName>[.<cmpOper>].<cmpArg>}
          cmpOper: lt,lte,gt,gte,eq,neq,glob,nglob,isnull,notnull,isempty ("LIKE" if none is given, isnull, notnull & isempty take no cmpArg)
          cmpOper: range,nrange - These take two cmpArgs, for start and stop of range (both inclusive)
          cmpOper: and,or,nand,nor - These will consume the rest of the whereCond terms and AND/OR/NAND/NOR them using LIKE/=.
colSizes format: Same as selColumns format

bookCountCond and booksReadCond formats:
    <number>        Only includes authors with book count >= <number>
    lt.<number>     Only includes authors with book count < <number>
    gt.<number>     Only includes authors with book count > <number>
    eq.<number>     Only includes authors with book count = <number>
    range.<n1>.<n2> Only includes authors with book count in range [n1,n2]

DisplayMode values:
    col|column  Left-aligned columns (Specified or default widths are used)
    html        HTML table code
    htmldoc     Full HTML document
    list[:sep]  Values delimited by separator string, default is "|"
    tabs        Tab-separated values

Column short name values:
    bt, bi           - Book title, BookID
    cat, catid       - Book category, CategoryID
    ra, own, beb     - Rating, Owned, Bought Ebook
    la, laid         - Language, LangID
    bd, by           - First publication date and year
    isbn             - ISBN for book. May also be other ID like ASIN in case there is no ISBN
    is10, is13       - ISBN value in ISBN10 and ISBN13 formats. (NULL if check sum wrong)
    ot, otla, otli   - Original title, its Language and LangID
    otd, oty         - First publication date and year for the original title
    otis, oi10, oi13 - Original title ISBN, and in ISBN10 and ISBN13 formats (NULL...)
    pgs, wds         - Book pages and words
    wpp, kw          - Words per page and kilo-words
    ln, fn, nn, ai   - Author last and first name, author full name, AuthorID
    nc, ng           - Author count and author(s) for book
    ge, gi, gg       - Genre, GenreID, Genre(s) for book
    dr, dc, dg       - Date read, Date read count, Date read(s).
    drrf, drrl, drrm - First, last and middle date of a DR range
    drrr, drrd       - A random date in a DR range and the number of days in it
    dw, dwl          - Day of week numeral and Day of week string for Date read
    dy,dm, dym,dymd  - Year, Month and yyyy-MM, yyyy-MM-dd for Date read
    ti, sec          - Time of day and TotalSeconds for Date read
    drbd             - Difference in days between Date read and first publication date
    bdod             - Difference in days between book and original title first publication dates
    bdo              - otd if exists else bd (i.e. always the first publication date)
    st, stid, stra   - Story, StoryID, Story rating
    ar, gr, sor, ser - Average rating for Author, Genre, Source and Series
    btst             - Title combined with story (if there is one)
    stbc, stbg       - Book count and book(s) for story
    stnc, stng       - Author count and author(s) for story
    stge, stgg       - Genre and Genre(s) for story
    bsra, bsge, bsgg - Rating, Genre and Genre(s) for story or book (if there is no story)
    astc, astg       - Story count and stories for author
    bstc, bstg       - Story count and stories for book
    bastc, bastg     - Story count and stories for book+author.
    btbastg          - Title combined with stories for book+author
    bstng            - Authors for book+story
    se, si, pa, sep  - Series, SeriesID, Part in Series, Series + Part
    so, soid         - Source, SourceID
    ps, psf, psmid   - Pseudonym(s), Pseudonym For, Pseudonym MainID
    abc, abcp, abcr  - Author book count, with pseudonyms counted (for MainID) and with re-reads
    agg, aggp        - Author genre(s) and with pseudonyms included (for MainID)
    agc, agcp        - Author genre count and count with pseudonyms counted (for MainID)
    gbc, gac         - Genre book count and author count
    cbc, sobc, sebc  - Category, source and series book count
    lbc, obc         - Language and original language book count

    To get the length of column values "l" can be appended to the short name for non-numeric/ID columns.
    E.g. "btl" will provide the lengths of the "bt" column values.

Window function columns:
    lag, lagi        - Lag in days (real and int) from the previous book
    dind, mind, yind - Index/ordinal of book for day, month and year
    ap, al, ac       - Period, lag and count for author
    gp, gl, gc       - Same for genre
    sp, sl, sc       - Same for source
)", stdout);
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
		vsnprintf(&resVar[0], size, fmtStr, ap); \
		resVar.pop_back(); /* Remove the trailing '\0' */ \
		va_end(ap)

	std::string fmt(_In_z_ _Printf_format_string_ const char* fmtStr, ...)
	{
		std_string_fmt_impl(fmtStr,res);
		return res;
	}

	template <typename T>
	void operator+=(std::vector<T>& a, const std::vector<T>& b)
	{
	    a.insert(a.end(), b.begin(), b.end());
	}

	template <typename T, typename CFunc>
	bool toIntType(std::string const& str, T& value, CFunc cfunc)
	{
		errno = 0;
		char* endPtr;
		T v = cfunc(str.c_str(), &endPtr, 10);
		if (endPtr == str.c_str() || *endPtr != '\0' || errno == ERANGE) {
			return false;
		}
		value = v;
		return true;
	}

	bool toInt(std::string const & str, int& value)
	{
		return toIntType(str, value, strtol);
	}

	bool toULongLong(std::string const & str, unsigned long long & value)
	{
		return toIntType(str, value, strtoull);
	}

	void replaceAll(std::string& str, const std::string& from, const std::string& to)
	{
		if (from.empty()) return;
		for (size_t pos = 0; (pos = str.find(from, pos)) != std::string::npos; pos += to.length()) {
			str.replace(pos, from.length(), to);
		}
	}

	std::wstring toWide(int codePage, const char *src, int const len)
	{
		if (len == 0) return std::wstring();
		if (int const cw = MultiByteToWideChar(codePage, 0, src, len, NULL, 0)) {
			std::wstring wstr(cw, '\0');
			if (int const res = MultiByteToWideChar(codePage, 0, src, len, &wstr[0], cw); res == cw)
				return wstr;
		}
		throw std::runtime_error(fmt("MultiByteToWideChar for code page %i failed", codePage));
	}

	std::string toNarrow(int codePage, const wchar_t *src, int const len)
	{
		if (len == 0) return std::string();
		if (int const cb = WideCharToMultiByte(codePage, 0, src, len, NULL, 0, NULL, NULL)) {
			std::string str(cb, '\0');
			if (int const res = WideCharToMultiByte(codePage, 0, src, len, &str[0], cb, NULL, NULL); res == cb)
				return str;
		}
		throw std::runtime_error(fmt("WideCharToMultiByte for code page %i failed", codePage));
	}

	std::wstring utf8ToWide(const char* utf8String, int len) // Used during output.
	{
		return toWide(CP_UTF8, utf8String, len);
	}

	std::string toUtf8(int codePage, std::string const & str)
	{
		auto wstr = toWide(codePage, str.c_str(), str.length() );
		return toNarrow(CP_UTF8, wstr.c_str(), wstr.length());
	}

	std::string fromUtf8(int codePage, std::string const & str)
	{
		auto wstr = toWide(CP_UTF8, str.c_str(), str.length());
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
		if (int intVal; !(tryToTreatAsNumeric && toInt(str, intVal))) {
			replaceAll(str, "'", "''");
			str = "'" + str + "'";
		}
		return str;
	}

	void toLowerCase(std::string& str)
	{
		std::transform(str.begin(), str.end(), str.begin(), [](char c) { return static_cast<char>(tolower(c)); });
	}

	int calcIsbn10CheckDigit(const char* isbn)
	{
		int sum = 0;
		for (int i = 0; i < 9; ++i) {
			int const d = isbn[i] - '0'; if (d < 0 || 9 < d) return 0;
			sum += (10 - i) * d;
		}
		int const rem = sum % 11;
		int const chkVal = (rem == 0) ? 0 : 11 - rem;
		return (chkVal == 10) ? 'X' : chkVal + '0';
	}

	int calcIsbn13CheckDigit(const char* isbn)
	{
		int sum = 0, weight = 1;
		for (int i = 0; i < 12; ++i) {
			int const d = isbn[i] - '0'; if (d < 0 || 9 < d) return 0;
			sum += weight * d;
			weight = (weight == 1) ? 3 : 1;
		}
		int const rem = sum % 10;
		int const chkVal = (rem == 0) ? 0 : 10 - rem;
		return chkVal + '0';
	}

	bool checkIsbn(const char* isbn, int len)
	{
		if (len == 10) return calcIsbn10CheckDigit(isbn) == isbn[9];
		if (len == 13) return calcIsbn13CheckDigit(isbn) == isbn[12];
		return false;
	}

	bool enableVTMode() // We assume a Windows 10 edition that supports ANSI.
	{
		if (HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE); hOut != INVALID_HANDLE_VALUE)
			if (DWORD dwMode=0; GetConsoleMode(hOut, &dwMode))
				return SetConsoleMode(hOut, dwMode|ENABLE_VIRTUAL_TERMINAL_PROCESSING);
		return false;
	}

	SYSTEMTIME getLocalTime()
	{
		SYSTEMTIME st{}; ::GetLocalTime(&st); return st;
	}
} // Utils
using namespace Utils;

#define S(str) (str).c_str()
#define ESC_S(str) escSqlVal(str).c_str()

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

	enum class Count {
		books,
		pages,
		words,
		kwords,
		authors,
		genres,
		stories,
		sources,
		categories,
	};

	enum class FitWidth {
		off,
		on,
		automatic,
	};

	enum class DRRange {
		first,
		last,
		middle,
		random
	};

	const char*   DefDbName = "litt.sqlite";
	const char    OptDelim = '.';
	const char    OptExtDelim = ':';
	const char    Wc = '*';
	const char*   WcS = "*";
	const char*   LogOp_OR = " OR ";
	const char*   LogOp_AND = " AND ";
	const IdValue EmptyId = 0;
	const char*   RatingRegEx = R"x([+-]?((\d+(\.\d*)?)|(\.\d+)))x";
	const char*   DateReadRegEx = R"x(\d{4}-\d\d-\d\d( [0-5]\d:[0-5]\d|~|\.\.\d{4}-\d\d-\d\d)?)x";
	const char*   PubDateRegEx = R"x(\d{4}(-\d\d(-\d\d)?)?)x";
	const unsigned EmptyUnsigned = unsigned(UINT_MAX);

	// Replace our wildcard with SQL's wildcard. Also escape and add SQL quoting if needed.
	std::string likeArg(std::string str, bool tryToTreatAsNumeric = false, bool glob = false)
	{
		if (!glob) std::replace(str.begin(), str.end(), Wc, '%');
		return escSqlVal(str, tryToTreatAsNumeric);
	}

	unsigned colWidth(std::string const & str)
	{
		auto const w = str.length();
		return (w > 2) ? w - 2 : w; // Don't include quotes in column width, they will not be printed.
	}

	constexpr auto& toSecondsValue = toULongLong;
	constexpr auto& toIdValue = toULongLong;

	struct TableInfo {
		TableInfo* parent = nullptr; // Parent table, i.e. table that will need to be included in the current query if this table is. May be null.
		bool used = false;           // Is the table used in the (output) query?
		bool included = false;       // Is the table already included in the query?

		void reset()
		{
			parent = nullptr;
			used = false;
			included = false;
		}
	};

	struct Tables {
		enum : int { MaxSize = 2};
		TableInfo* tis[MaxSize];

		Tables(TableInfo* ti1 = nullptr, TableInfo* ti2 = nullptr)
		{
			tis[0] = ti1;
			tis[1] = ti2;
		}

		void reset(TableInfo* ti1 = nullptr, TableInfo* ti2 = nullptr)
		{
			tis[0] = ti1;
			tis[1] = ti2;
		}

		TableInfo** begin() { return tis;}

		TableInfo** end() 
		{ 
			int i = 0;
			while (i < MaxSize && tis[i] != nullptr) { ++i; }
			return tis + i;
		}
	};

	struct TableInfos {
		TableInfos() {};

		union {
			TableInfo arrView[11 + 7 + 2 + 38] = {};
			#pragma warning(disable : 4201) // nameless struct extension.
			struct {
				// Start tables
				TableInfo books;
				TableInfo authors;
				TableInfo originalTitles;
				TableInfo series;
				TableInfo sources;
				TableInfo gbook;
				TableInfo gstory;
				TableInfo stories;
				TableInfo bookCategory;
				TableInfo l_book;
				TableInfo l_ot;

				// Link tables
				TableInfo authorbooks;
				TableInfo bookGenres;
				TableInfo bookSeries;
				TableInfo bookStories;
				TableInfo datesRead;
				TableInfo pseudonyms;
				TableInfo storyGenres;

				// Virtual link tables for stories
				TableInfo storyBooks;
				TableInfo storyAuthors;

				// Virtual tables
				TableInfo nc;
				TableInfo ng;
				TableInfo dc;
				TableInfo dg;
				TableInfo gg;
				TableInfo ar;
				TableInfo gr;
				TableInfo sor;
				TableInfo ser;
				TableInfo abc;
				TableInfo abcp;
				TableInfo abcr;
				TableInfo agg;
				TableInfo aggp;
				TableInfo agc;
				TableInfo agcp;
				TableInfo gbc;
				TableInfo gac;
				TableInfo cbc;
				TableInfo lbc;
				TableInfo obc;
				TableInfo sobc;
				TableInfo sebc;
				TableInfo stgg;
				TableInfo stnc;
				TableInfo stng;
				TableInfo stbc;
				TableInfo stbg;
				TableInfo bastc;
				TableInfo bastg;
				TableInfo astc;
				TableInfo astg;
				TableInfo bstc;
				TableInfo bstg;
				TableInfo bstng;
				TableInfo psmid;
				TableInfo ps;
				TableInfo psf;
			};
		};
	};
	static_assert(sizeof(TableInfos::arrView) == sizeof(TableInfos), "check arrView size!");

	struct ColumnInfo {
		// These values are pre-configured:
		std::string const nameDef; // Column name or SQL expression (def) for column
		int         const width; // Default width of column, can be overridden.
		ColumnType  const type;
		std::string const label; // optional, used when name does not refer to a direct table column.
		bool        const aggr; // Is a group aggregate column?
		bool        const justifyRight;

		// Also pre-configured, but have default values:
		ColumnInfo const* lengthColumn = nullptr; // Will be set only if the column has a length column added.
		const char*       collation = nullptr; // Only set if not using the default (usually BINARY)

		mutable Tables tables; // The tables referenced by the column definition.

		// These values are set at runtime. Stored here for convenience.
		mutable int  overriddenWidth = -1;
		mutable bool usedInQuery = false;
		mutable bool usedInResult = false;

		ColumnInfo(std::string const& nameDef, int width, ColumnType ct, std::string const& label, bool aggr, bool justifyRight, Tables t) :
			nameDef(nameDef),
			width(width),
			type(ct),
			label(label),
			aggr(aggr),
			justifyRight(justifyRight),
			tables(t)
		{}

		std::string const& labelName() const { return label.empty() ? nameDef : label; }

		std::string getLikeArg(std::string val, bool glob = false) const
		{
			return likeArg(std::move(val), type == ColumnType::numeric, glob);
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
		optional = 0x00,
		required = 0x01,
	};

	std::string readLine() {
		CONSOLE_SCREEN_BUFFER_INFO info{};
		HANDLE const hOut = GetStdHandle(STD_OUTPUT_HANDLE);
		GetConsoleScreenBufferInfo(hOut, &info);
		DWORD const fgMask = FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_RED | FOREGROUND_INTENSITY;
		DWORD const bgMask = BACKGROUND_BLUE | BACKGROUND_GREEN | BACKGROUND_RED | BACKGROUND_INTENSITY;

		WORD textColor = 0;
		if (auto strColor = getenv("LITT_INPUT_COLOR"); strColor != nullptr) {
			if (auto color = atoi(strColor); 0 < color && color <= 0xFFFF) {
				textColor = (WORD)color;
			}
		}

		SetConsoleTextAttribute(hOut, (textColor != 0) ? textColor  // else text color cyan, background as-is.
			: FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_INTENSITY | (bgMask & info.wAttributes));
		std::string str; std::getline(std::cin, str);
		SetConsoleTextAttribute(hOut, info.wAttributes & (fgMask | bgMask)); // Restore previous attributes.
		return str;
	}

	void prefillInput(std::string const& str)
	{
		if (str.empty()) return;
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
		if (DWORD n; !WriteConsoleInput(GetStdHandle(STD_INPUT_HANDLE), recs.data(), recs.size(), &n)) {
			fprintf(stderr, "Failed to prefill input! Error code = %i", GetLastError());
		}
	}

	std::string input(const char* prompt, InputOptions options = required)
	{
		for (;;) {
			printf("%s: ", prompt);
			auto value = readLine();
			if (!(value.empty() && (required & options)))
				return value;
		}
	}

	std::string input(const char* prompt, const char* regEx, InputOptions options = required)
	{
		for (;;) {
			auto value = input(prompt, options);
			if (value.empty() || std::regex_match(value, std::regex(regEx))) 
				return value;
			prefillInput(value);
		}
	}
	
	void input(std::string& value, const char* prompt, InputOptions options = required)
	{
		prefillInput(value);
		value = input(prompt, options);
	}

	void input(std::string& value, const char* prompt, const char* regex, InputOptions options = required)
	{
		prefillInput(value);
		value = input(prompt, regex, options);
	}

	void input(unsigned& value, const char* prompt, InputOptions options = required)
	{
		std::string ustr = (value != EmptyUnsigned) ? std::to_string(value) : "";
		input(ustr, prompt, R"([1-9]\d*)", options);
		value = std::stoul(ustr);
	}

	using InputCheckIdFunction = std::function<void(IdValue)>;
	using InputListFunction = std::function<void(std::string const &)>;

	void input(
		IdValue& value, 
		const char* prompt,
		InputCheckIdFunction const& checkId,
		InputListFunction const& listValues,
		InputOptions const options = required)
	{
		if (value != EmptyId) prefillInput(std::to_string(value));
		for (;;) {
			auto str = input(prompt, options);
			value = EmptyId; if (str.empty()) break; // Done, empty id allowed and inputted.
			toIdValue(str, value);
			if (value == EmptyId) {
				if (listValues) listValues(str);
			}
			else if (checkId) try { checkId(value); } catch (std::exception&) {
				printf("Invalid ID for this column, please try again.\n");
				value = EmptyId;
			}
			if (value != EmptyId) break; // Done, got a valid id.
		}
	}

	int askInput(const char* validAnswers, std::string const & question, int& defAndRes )
	{
		bool foundDefault = false;
		int const len = strlen(validAnswers);
		printf("%s? (", S(question));
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

	int ask(const char* validAnswers, std::string const & question, int def = 0)
	{
		return askInput(validAnswers, question, def);
	}

	void inputIsbn(std::string& value, const char* prompt, InputOptions options = required)
	{
		do {
			input(value, prompt, options);
		} while (!checkIsbn(value.c_str(), value.length()) &&
			ask("yn", fmt("Use invalid %s '%s'", prompt, value.c_str()), 'n') == 'n');
	}

	bool confirmEnabled = true;

	bool confirm(std::string const & question)
	{
		return confirmEnabled ? (ask("yn", question) == 'y') : true;
	}

	bool confirmf(_In_z_ _Printf_format_string_ const char* qftmstr, ...)
	{
		std_string_fmt_impl(qftmstr, question);
		return confirm(question);
	}
}
using namespace Input;

class OptionParser {
	std::string       m_option;
	unsigned          m_optionIndex = 0;
	const char* const m_type;
	const char        m_delim;
public:
	OptionParser(std::string const & value, const char* type = "option", char delim = OptDelim)
		: m_option(value), m_type(type), m_delim(delim)
	{}

	bool empty() const
	{
		return m_option.length() <= m_optionIndex;
	}

	bool getNext(std::string& next)
	{
		if (empty()) { return false; }

		const int EscapeChar = '!';
		bool escOn = false;

		std::string str;
		do {
			char const c = m_option[m_optionIndex++];
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
		} while (!empty());

		if (escOn) { throw std::invalid_argument("Incomplete escape sequence"); }
		if (str.empty()) { throw std::invalid_argument("Empty option values not allowed"); }

		next = std::move(str);
		return true;
	}
	
	__declspec(noreturn) void throwError() 
	{
		throw std::invalid_argument(fmt("Faulty %s value: %s", m_type, S(m_option)));
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
		if (int ival; toInt(val, ival)) return ival;
		throwError();
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
	mutable bool m_error = false;

public:
	bool stdOutIsConsole() const { return m_stdOutIsConsole; }

	void setEncoding(int encoding) { m_encoding = encoding; }

	bool error() const { return m_error; }

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

	void writeUtf8Width(const char* str, int width, bool justifyRight) const
	{ // PRE: str contains only complete utf-8 code points.
		int writtenChars = 0;

		if (justifyRight) { // OBS! Does not work for utf-8 strings with multibyte chars! Mainly with numeric columns for now!
			for (int i = strlen(str); i < width; ++i)
			{
				write(' ');
				++writtenChars;
			}
		}

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

	void doWrite(const char* const str, int const len) const
	{
		if (len == 0) return;
		if (m_stdOutIsConsole) {
			auto ws = utf8ToWide(str, len);
			if (DWORD nw; !WriteConsole(m_stdOutHandle, ws.c_str(), ws.length(), &nw, 0)) {
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
				m_error = true;
				throw std::runtime_error("fwrite failed to write all data, errno: " + std::to_string(errno));
			}
		}
	}

	void flush() const 
	{
		// Reset buffer before calling doWrite so we don't end up with further write errors during
		// error propagation in case it fails. Only the first failure is relevant.
		auto len = m_bufPos; m_bufPos = 0;
		doWrite(m_buffer, len);
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

enum class ExQ { None = 0, Graph = 1, VMCode = 2, Raw = 3 };

class Litt {
	TableInfos m_tableInfos;
	std::map<std::string, ColumnInfo> m_columnInfos; // Maps short name to column info.
	std::unique_ptr<sqlite3, SqliteCloser> m_conn;
	Output m_output;

	int const consoleCodePage = GetConsoleCP();
	bool m_headerOn = true;
	FitWidth m_fitWidth = FitWidth::automatic;
	int  m_fitWidthValue = 230;
	bool m_selectDistinct = false;
	bool m_showQuery = false;
	bool m_showDefaults = false;
	ExQ m_explainQuery = ExQ::None; 
	bool m_showNumberOfRows = false;
	DisplayMode m_displayMode = DisplayMode::column;
	std::string m_listSep = "|";
	std::string m_colSep = "  ";
	int m_colSepSize = 2; // Avoid recalculating during output since contains character count and not code-point count.
	std::string m_dbPath; // Path to LITT db file

	Columns m_orderBy; // Overrides the default action order.
	int m_limit = 0;
	int m_offset = 0;
	Columns m_selectedColumns; // Overrides the default action columns.
	Columns m_additionalColumns; // Added to the action or overridden columns.

	mutable std::string m_whereCondition;
	mutable std::string m_havingCondition;

	std::string m_action;
	std::vector<std::string> m_actionArgs;
	std::string m_actionRightWildCard;
	std::string m_actionLeftWildCard;

	Count m_count = Count::books;

	DRRange m_drRange = DRRange::first; // Which DRR option to use when counting.

	mutable int m_rowCount = 0; // The number of rows printed so far.

	ColumnInfo& ci(std::string const& sn, std::string const& nameDef, int width, ColumnType ct, Tables t, std::string const& label, bool aggr = false)
	{
		bool const justifyRight = (width < 0);
		if (auto res = m_columnInfos.try_emplace(sn, nameDef, abs(width), ct, label, aggr, justifyRight, t); res.second)
			return res.first->second;
		throw std::logic_error("Duplicate short name: " + sn);
	}

	ColumnInfo& ciText(std::string const & sn, std::string const & nameDef, int width, Tables tables, std::string const & label = "")
	{
		return ci(sn, nameDef, width, ColumnType::text, tables, label);
	}

	ColumnInfo& ciNum(std::string const & sn, std::string const & nameDef, int width, Tables tables, std::string const & label = "")
	{
		return ci(sn, nameDef, width, ColumnType::numeric, tables, label);
	}

	ColumnInfo& ciTextL(std::string const & sn, std::string const & nameDef, int width, Tables tables, const char* collation, std::string const & label = "")
	{
		auto& ci = ciText(sn, nameDef, width, tables, label);
		auto const labelLength = "l_" + sn;
		ci.lengthColumn = &ciNum(sn + "l", "length(" + nameDef + ")", labelLength.length(), tables, labelLength);
		ci.collation = collation;
		return ci;
	}

	ColumnInfo& ciAggr(std::string const & sn, std::string const & def, int width, Tables tables, std::string const & label)
	{
		return ci(sn, def, width, ColumnType::numeric, tables, label, true);
	}
/*
#define ADJUST_IGNORE(strlit,n,s) \
	if ((n > sizeof(strlit)-1) && sqlite3_strnicmp(strlit, s, std::min((int)(sizeof(strlit)-1), n)) == 0) \
		{ n -= (sizeof(strlit)-1); s += (sizeof(strlit)-1); return; }

	static void adjustLastName(int& n, const char*& s)
	{
		ADJUST_IGNORE("de ", n, s);
		ADJUST_IGNORE("van ", n, s);
		ADJUST_IGNORE("von ", n, s);
		ADJUST_IGNORE("lord ", n, s);
	}

	static int cmpLastName(void*, int n1, const void* p1, int n2, const void* p2)
	{
		auto s1 = static_cast<const char *>(p1);
		auto s2 = static_cast<const char *>(p2);
		adjustLastName(n1, s1);
		adjustLastName(n2, s2);
		int r = sqlite3_strnicmp(s1, s2, std::min(n1, n2));
		return r == 0 ? n1 - n2 : r;
	}

	static void adjustTitle(int& n, const char*& s)
	{
		ADJUST_IGNORE("the ", n, s);
		ADJUST_IGNORE("an ", n, s);
		ADJUST_IGNORE("a ", n, s);
	}

	static int cmpTitle(void*, int n1, const void* p1, int n2, const void* p2)
	{
		auto s1 = static_cast<const char *>(p1);
		auto s2 = static_cast<const char *>(p2);
		adjustTitle(n1, s1);
		adjustTitle(n2, s2);
		int r = sqlite3_strnicmp(s1, s2, std::min(n1, n2));
		return r == 0 ? n1 - n2 : r;
	}
*/
	static void isbn10(sqlite3_context* context, int argc, sqlite3_value** argv)
	{
		if (argc == 1) {
			if (auto isbn = reinterpret_cast<const char*>(sqlite3_value_text(argv[0])); isbn && isbn[0]) {
				if (int const len = strlen(isbn); checkIsbn(isbn, len)) {
					if (len == 10) {
						sqlite3_result_text(context, isbn, 10, SQLITE_TRANSIENT);
						return;
					}
					else if (len == 13 && isbn[0] == '9' && isbn[1] == '7' && isbn[2] == '8') {
						char i10[10];
						strncpy(i10, isbn+3, 9);
						i10[9] = (char)calcIsbn10CheckDigit(i10);
						sqlite3_result_text(context, i10, 10, SQLITE_TRANSIENT);
						return;
					}
				}
			}
		}
		sqlite3_result_null(context);
	}

	static void isbn13(sqlite3_context *context, int argc, sqlite3_value **argv)
	{
		if (argc == 1) {
			if (auto isbn = reinterpret_cast<const char*>(sqlite3_value_text(argv[0])); isbn && isbn[0]) {
				if (int const len = strlen(isbn); checkIsbn(isbn, len)) {
					if (len == 13) {
						sqlite3_result_text(context, isbn, 13, SQLITE_TRANSIENT);
						return;
					}
					else if (len == 10) {
						char i13[13] = { '9', '7', '8' };
						strncpy(i13 + 3, isbn, 9);
						i13[12] = (char)calcIsbn13CheckDigit(i13);
						sqlite3_result_text(context, i13, 13, SQLITE_TRANSIENT);
						return;
					}
				}
			}
		}
		sqlite3_result_null(context);
	}

#define A_NAME  "ltrim(\"First Name\"||' '||\"Last Name\")"
#define A_NAMES "group_concat(" A_NAME ",', ')"
#define APPEND_OPT_COL(mand, opt) mand " || CASE WHEN " opt " IS NULL THEN '' ELSE (' [' || " opt " || ']') END"

public:
	Litt(int argc, char** argv)
	{
		const char* const CDefault = nullptr;
		const char* const CNoCase = "NOCASE";
		const char* const CTitle = "NOCASE";
		const char* const CLastName = "NOCASE";

		auto&t = m_tableInfos;

		// OBS! As a sn, don't use "desc", "asc" and any other name that may appear after one in the command line options!
		ciNum("ai", "AuthorID", -8, Tables());
		ciNum("beb", "\"Bought Ebook\"", 3, Tables(&t.books));
		ciNum("bi", "BookID", -4, Tables());
		ciTextL("bt", "Title", 45, Tables(&t.books), CTitle);
		ciTextL("bd", "Date", 10, Tables(&t.books), CDefault);
		ciTextL("otd", "otDate", 10, Tables(&t.originalTitles), CDefault);
		ciTextL("bdo", "ifnull(otDate, Date)", 10, Tables(&t.books, &t.originalTitles), CDefault, "oDate");
		ciNum("by", "CAST(substr(Date,1,4) AS INTEGER)", 5, Tables(&t.books), "BYear");
		ciNum("oty", "CAST(substr(otDate,1,4) AS INTEGER)", 6, Tables(&t.originalTitles), "otYear");
		ciTextL("dr", "\"Date Read\"", 10, Tables(&t.datesRead), CDefault);
		ciNum("dc", "DRCnt", 5, Tables(&t.dc));
		ciTextL("dg", "\"Date(s)\"", 30, Tables(&t.dg), CDefault);
		ciTextL("fn", "\"First Name\"", 15, Tables(&t.authors), CNoCase);
		ciTextL("ge", "GBook.Genre", 30, Tables(&t.gbook), CNoCase);
		ciTextL("gg", "\"Genre(s)\"", 30, Tables(&t.gg), CNoCase);
		ciNum("gi", "BookGenres.GenreID", -7, Tables(&t.bookGenres));
		ciNum("gi_n", "GenreID", -7, Tables());
		ciTextL("ln", "\"Last Name\"", 20, Tables(&t.authors), CLastName); 
		ciNum("nc", "AuthorCnt", 6, Tables(&t.nc));
		ciTextL("ng", "\"Author(s)\"", 50, Tables(&t.ng), CNoCase);
		ciTextL("nn", A_NAME, 25, Tables(&t.authors), CNoCase, "Author");
		ciNum("laid", "Books.LangID", 6, Tables(&t.books));
		ciNum("laid_n", "LangID", 6, Tables());
		ciNum("otli", "OriginalTitles.LangID", 8, Tables(&t.originalTitles), "otLangID");
		ciText("la", "L_book.Language", 4, Tables(&t.l_book), "Lang");
		ciText("otla", "L_ot.Language", 4, Tables(&t.l_ot), "otLa");
		ciNum("ra", "Books.Rating", 3, Tables(&t.books), "Rating");
		ciNum("ar", "ARating", 3, Tables(&t.ar));
		ciNum("gr", "GRating", 3, Tables(&t.gr));
		ciNum("sor", "SORating", 3, Tables(&t.sor));
		ciNum("ser", "SERating", 3, Tables(&t.ser));
		ciNum("own", "Owned", 3, Tables(&t.books));
		ciTextL("isbn", "ISBN", 13, Tables(&t.books), CDefault);
		ciTextL("is10", "isbn10(ISBN)", 10, Tables(&t.books), CDefault);
		ciTextL("is13", "isbn13(ISBN)", 13, Tables(&t.books), CDefault);
		ciTextL("otis", "otISBN", 13, Tables(&t.originalTitles), CDefault);
		ciTextL("oi10", "isbn10(otISBN)", 10, Tables(&t.originalTitles), CDefault);
		ciTextL("oi13", "isbn13(otISBN)", 13, Tables(&t.originalTitles), CDefault);
		ciNum("catid", "CategoryID", 3, Tables());
		ciTextL("cat", "Category", 11, Tables(&t.bookCategory), CNoCase);
		ciNum("pgs", "Pages", 5, Tables(&t.books));
		ciNum("wds", "Words", 6, Tables(&t.books));
		ciNum("wpp", "Words / Pages", 4, Tables(&t.books), "WPP");
		ciNum("kw", "(Words + 500) / 1000", 4, Tables(&t.books), "Kwords");
		ciTextL("ot", "\"Original Title\"", 45, Tables(&t.originalTitles), CTitle);
		ciTextL("se", "Series", 40, Tables(&t.series), CTitle);
		ciNum("si", "SeriesID", -8, Tables());
		ciText("pa", "\"Part in Series\"", -4, Tables(&t.bookSeries));
		ciText("sep", "Series||' '||\"Part in Series\"", 40, Tables(&t.series, &t.bookSeries), "\"Series #\"");
		ciTextL("st", "Story", 45, Tables(&t.stories), CTitle);
		ciNum("stid", "StoryID", -7, Tables());
		ciNum("stra", "Stories.Rating", 3, Tables(&t.stories), "SRating");
		ciTextL("stge", "GStory.Genre", 30, Tables(&t.gstory), CNoCase, "SGenre");
		ciTextL("stgg", "\"StoryGenre(s)\"", 30, Tables(&t.stgg), CNoCase);
		ciNum("stbc", "\"BCnt\"", 4, Tables(&t.stbc));
		ciTextL("stbg", "\"StoryBooks(s)\"", 50, Tables(&t.stbg), CNoCase);
		ciNum("stnc", "\"ACnt\"", 4, Tables(&t.stnc));
		ciTextL("stng", "\"Story author(s)\"", 50, Tables(&t.stng), CNoCase);
		ciTextL("btst", APPEND_OPT_COL("Title", "Story"), 60, Tables(&t.books, &t.stories), CTitle, "\"Title [Story]\"");
		ciNum("bsra", "ifnull(Stories.Rating, Books.Rating)", 3, Tables(&t.books, &t.stories), "BSRating");
		ciTextL("bsge", "ifnull(GStory.Genre, GBook.Genre)", 30, Tables(&t.gbook, &t.gstory), CNoCase, "BSGenre");
		ciTextL("bsgg", "ifnull(\"StoryGenre(s)\", \"Genre(s)\")", 30, Tables(&t.gg, &t.stgg), CNoCase, "\"BSGenre(s)\"");
		ciNum("bastc", "SCnt", 4, Tables(&t.bastc));
		ciTextL("bastg", "Stories", 45, Tables(&t.bastg), CNoCase);
		ciTextL("btbastg", APPEND_OPT_COL("Title", "Stories"), 60, Tables(&t.books, &t.bastg), CTitle, "\"Title [Stories]\"");
		ciNum("astc", "AStoryCnt", 5, Tables(&t.astc));
		ciTextL("astg", "\"Author Stories\"", 100, Tables(&t.astg), CNoCase);
		ciNum("bstc", "BStoryCnt", 5, Tables(&t.bstc));
		ciTextL("bstg", "\"Book Stories\"", 100, Tables(&t.bstg), CNoCase);
		ciTextL("bstng", "\"Book+Story author(s)\"", 50, Tables(&t.bstng), CNoCase);
		ciTextL("so", "Source", 35, Tables(&t.sources), CNoCase);
		ciNum("soid", "SourceID", -8, Tables());
		ciTextL("ps", "ps.Pseudonyms", 25, Tables(&t.ps), CNoCase);
		ciTextL("psf", "psf.\"Pseudonym For\"", 25, Tables(&t.psf), CNoCase);
		ciNum("psmid", "PSMainID", -9, Tables(&t.psmid), "PSMainID");
		ciNum("abc", "ABC", 4, Tables(&t.abc));
		ciNum("abcp","ABCP",4, Tables(&t.abcp));
		ciNum("abcr","ABCR",4, Tables(&t.abcr));
		ciTextL("agg", "\"Author Genres\"",    135, Tables(&t.agg),  CNoCase);
		ciTextL("aggp","\"Author(p) Genres\"", 135, Tables(&t.aggp), CNoCase);
		ciNum("agc", "AGC", 4, Tables(&t.agc));
		ciNum("agcp","AGCP",4, Tables(&t.agcp));
		ciNum("gbc", "GBC", 4, Tables(&t.gbc));
		ciNum("gac", "GAC", 4, Tables(&t.gac));
		ciNum("cbc", "CBC", 4, Tables(&t.cbc));
		ciNum("lbc", "LBC", 4, Tables(&t.lbc));
		ciNum("obc", "OBC", 4, Tables(&t.obc));
		ciNum("sobc","SOBC",4, Tables(&t.sobc));
		ciNum("sebc","SEBC",4, Tables(&t.sebc));

#define ROUND_TO_INT(strExpr) "CAST(round(" strExpr ",0) AS INTEGER)"

#define DR_FIXED "substr(\"Date Read\",1,10)"
#define DR_SECS "CAST(ifnull(strftime('%s',\"Date Read\"), strftime('%s'," DR_FIXED ")) AS INTEGER)"
#define DR_DAYS "CAST(ifnull(strftime('%J',\"Date Read\"), strftime('%J'," DR_FIXED ")) AS REAL)"

#define BD_FIXED "CASE WHEN length(Date) = 10 THEN Date ELSE substr(Date||'-01-01', 1,10) END"
#define BD_DAYS "CAST(strftime('%J'," BD_FIXED ") AS REAL)"

#define OTD_FIXED "CASE WHEN length(otDate) = 10 THEN otDate ELSE substr(otDate||'-01-01', 1,10) END"
#define OTD_DAYS "CAST(strftime('%J'," OTD_FIXED ") AS REAL)"

		// Columns for more formats of Date Read:
		ciNum("dw", "CAST(strftime('%w',\"Date Read\") AS INTEGER)", -3, Tables(&t.datesRead), "DOW");
		ciText("dwl", "CASE CAST(strftime('%w',\"Date Read\") AS INTEGER)"
					  " WHEN 0 THEN 'Sun' WHEN 1 THEN 'Mon' WHEN 2 THEN 'Tue' WHEN 3 THEN 'Wed' WHEN 4 THEN 'Thu' WHEN 5 THEN 'Fri' WHEN 6 THEN 'Sat' ELSE NULL END",
					  5, Tables(&t.datesRead), "DoW");
		ciNum("dm", "CAST(strftime('%m',\"Date Read\") AS INTEGER)", -3, Tables(&t.datesRead), "Month");
		ciNum("dy", "CAST(substr(\"Date Read\",1,4) AS INTEGER)", -4, Tables(&t.datesRead), "Year");
		ciText("dym", "substr(\"Date Read\",1,7)", 7, Tables(&t.datesRead), "YMonth");
		ciText("dymd", "substr(\"Date Read\",1,10)", 10, Tables(&t.datesRead), "YMDay");
		ciText("ti", "ifnull(time(\"Date Read\"), time(" DR_FIXED "))", 5, Tables(&t.datesRead), "Time");
		ciNum("sec", DR_SECS, -11, Tables(&t.datesRead), "Timestamp");
		ciNum("drbd", ROUND_TO_INT(DR_DAYS " - " BD_DAYS), -6, Tables(&t.datesRead, &t.books), "RDelay");
		ciNum("bdod", ROUND_TO_INT(BD_DAYS " - " OTD_DAYS), -6, Tables(&t.books, &t.originalTitles), "TDelay");

		// Some columns for displaying the DR-range values with format "yyyy-mm-dd..yyyy-mm-dd".
		// Will also display sensible values for all other supported formats.

#define IS_DRR     "substr(\"Date Read\",11,2) = '..'"
#define DRR_1      "substr(\"Date Read\",1,10)"
#define DRR_2      "substr(\"Date Read\",13)"
#define DRR_IDAYS  "(julianday(" DRR_2 ") - julianday(" DRR_1 "))"

#define DRR_FIRST DRR_1
#define DRR_LAST "CASE WHEN " IS_DRR " THEN " DRR_2 " ELSE " DRR_1 " END"
#define DRR_MID  "CASE WHEN " IS_DRR " THEN date(" DRR_1 ", (" DRR_IDAYS " / 2) || ' days') ELSE " DRR_1 " END"
#define DRR_RAND "CASE WHEN " IS_DRR " THEN date(" DRR_1 ", abs(random() % " DRR_IDAYS ") || ' days') ELSE " DRR_1 " END"
#define DRR_DAYS "CASE WHEN " IS_DRR " THEN " DRR_IDAYS " ELSE 0.0 END"
// We keep these macros defined so they can be used elsewhere as well.

		ciText("drrf", DRR_FIRST, 10, Tables(&t.datesRead), "DRRFirst");
		ciText("drrl", DRR_LAST,  10, Tables(&t.datesRead), "DRRLast");
		ciText("drrm", DRR_MID,   10, Tables(&t.datesRead), "DRRMiddle");
		ciText("drrr", DRR_RAND,  10, Tables(&t.datesRead), "DRRRandom");
		ciNum("drrd", DRR_DAYS, -7, Tables(&t.datesRead), "DRRDays");

		// Some window function columns: 
		// Note: Cannot be used in WHERE!
		// Results may depend on what tables are joined in the query, as some tables (e.g. genre, authors, dates) might add more rows when joined.

#define LAG "(" DR_SECS " - lag(" DR_SECS ") OVER (ORDER BY \"Date Read\" ROWS BETWEEN 1 PRECEDING AND CURRENT ROW)) / 86400.0"
		ciNum("lag", "round(" LAG ", 1)", -5, Tables(&t.datesRead), "Lag");
		ciNum("lagi", ROUND_TO_INT(LAG),  -4, Tables(&t.datesRead), "Lag");
#undef LAG

#define XIND(part) "dense_rank() OVER(PARTITION BY " part " ORDER BY BookID)"
		ciNum("dind", XIND("date(" DR_FIXED ")"),          -4, Tables(&t.datesRead), "DInd");
		ciNum("mind", XIND("substr(\"Date Read\",1,7)"),   -4, Tables(&t.datesRead), "MInd");
		ciNum("yind", XIND("strftime('%Y'," DR_FIXED ")"), -4, Tables(&t.datesRead), "YInd");
#undef XIND

#define XPERIOD(part) ROUND_TO_INT("(max(" DR_SECS ") OVER (PARTITION BY " part ") - min(" DR_SECS ") OVER (PARTITION BY " part ")) / 86400.0")
#define XLAG(part) ROUND_TO_INT("(" DR_SECS " - lag(" DR_SECS ") OVER (PARTITION BY " part " ORDER BY \"Date Read\" ROWS BETWEEN 1 PRECEDING AND CURRENT ROW)) / 86400.0")
#define XCOUNT(part) "count(*) OVER (PARTITION BY " part ")"
		ciNum("ap", XPERIOD("AuthorID"),-7, Tables(), "APeriod");
		ciNum("al", XLAG("AuthorID"),   -5, Tables(), "ALag");
		ciNum("ac", XCOUNT("AuthorID"), -4, Tables(), "ACnt");

		ciNum("gp", XPERIOD("BookGenres.GenreID"), -7, Tables(&t.datesRead, &t.bookGenres), "GPeriod");
		ciNum("gl", XLAG("BookGenres.GenreID"),    -5, Tables(&t.datesRead, &t.bookGenres), "GLag");
		ciNum("gc", XCOUNT("BookGenres.GenreID"),  -4, Tables(&t.bookGenres), "GCnt");

		ciNum("sp", XPERIOD("SourceID"),-7, Tables(&t.datesRead), "SPeriod");
		ciNum("sl", XLAG("SourceID"),   -5, Tables(&t.datesRead), "SLag");
		ciNum("sc", XCOUNT("SourceID"), -4, Tables(), "SCnt");
#undef XCOUNT
#undef XLAG
#undef XPERIOD

#undef ROUND_TO_INT
#undef DR_SECS
#undef DR_FIXED
		
		// Special-purpose "virtual" columns, these are not generally usable:
		//
		ciNum("btc", "TitleCount", -5, Tables(), "Count"); // for listSametitle
		ciNum("brc", "ReadCount", -5, Tables(), "Reads");  // for listRereads
		// These are for the book count actions.
		ciAggr("bc",  "COUNT(BookID)", -6, Tables(), "Books");
		ciAggr("bcp", "SUM(Pages)", -7, Tables(), "Pages");
		ciAggr("bcw", "SUM(Words)", -9, Tables(), "Words");
		ciAggr("bckw", "(SUM(Words)+500)/1000", -6, Tables(), "Kwords");
		ciAggr("bca", "COUNT(AuthorID)", -6, Tables(), "Authors");
		ciAggr("bcg", "COUNT(GenreID)", -6, Tables(), "Genres");
		ciAggr("bcst", "COUNT(StoryID)", -6, Tables(), "Stories");
		ciAggr("bcso", "COUNT(SourceID)", -6, Tables(), "Sources");
		ciAggr("bcc", "COUNT(CategoryID)", -6, Tables(), "Categories");

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
								m_listSep = toUtf8(val.substr(5));
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
					if (int enc; toInt(val, enc)) m_output.setEncoding(enc);
					else throw std::invalid_argument("Invalid encoding value: " + val);
					break;
				case 'f': {
					OptionParser fval(val);
					auto v = fval.empty() ? "on" : fval.getNext();
					if      (v == "on" || toInt(v, m_fitWidthValue)) m_fitWidth = FitWidth::on;
					else if (v == "off")                             m_fitWidth = FitWidth::off;
					else if (v == "auto")                            m_fitWidth = FitWidth::automatic;
					else throw std::invalid_argument("Invalid fit width value: " + val);
					break;
				}
				case 'h':
					if (val == "on"  || val == "") m_headerOn = true;
					else if (val == "off")         m_headerOn = false;
					else throw std::invalid_argument("Invalid header value: " + val);
					break;
				case 'o':
					m_orderBy = getColumns(val, ColumnsDataKind::sortOrder, true, true);
					break;
				case 'c': 
					m_selectedColumns = getColumns(val, ColumnsDataKind::width, true);
					break;
				case 'l': 
					m_dbPath = val;
					break;
				case 'a': 
					m_additionalColumns += getColumns(val, ColumnsDataKind::width, true);
					break;
				case 'w': 
					appendToWhereCondition(LogOp_OR, getWhereCondition(val));
					break;
				case 's':
					for (auto& c : getColumns(val, ColumnsDataKind::width, /*usedInQuery*/false))
						c.first->overriddenWidth = c.second;
					break;
				case 'q':
					m_showQuery = true;
					m_showDefaults = (val == "d");
					Input::confirmEnabled = false;
					break;
				case 'u':
					m_selectDistinct = true;
					break;
				case 'x':
					if (!val.empty() && (val[0] < '0' || '3' < val[0])) 
						throw std::invalid_argument("Invalid explain value: " + val);
					m_explainQuery = (val.empty() ? ExQ::Graph : ExQ(val[0] - '0'));
					Input::confirmEnabled = false;
					break;
				case 'n': 
					m_showNumberOfRows = true;
					break;
				case 'y':
					if (val == "on"  || val == "") Input::confirmEnabled = false;
					else if (val == "off")         Input::confirmEnabled = true;
					else throw std::invalid_argument("Invalid yes value: " + val);
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

						auto nextColor = [&]() 
						{
							auto val = extVal.getNext();
							return (val[0] == '\x1b') ? val : "\x1b[" + val;
						};

						for (std::string subOpt; extVal.getNext(subOpt); ) {
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
						}
					}
					else if (extName == "cnt") {
						for (std::string what; extVal.getNext(what); ) {
							if      (what == "b") m_count = Count::books;
							else if (what == "p") m_count = Count::pages;
							else if (what == "w") m_count = Count::words;
							else if (what == "kw") m_count = Count::kwords;
							else if (what == "a") m_count = Count::authors;
							else if (what == "g") m_count = Count::genres;
							else if (what == "st") m_count = Count::stories;
							else if (what == "so") m_count = Count::sources;
							else if (what == "c") m_count = Count::categories;
							else throw std::invalid_argument("Unrecognized cnt value: " + what);
						}
					}
					else if (extName == "drr") {
						for (std::string what; extVal.getNext(what); ) {
							if      (what == "f") m_drRange = DRRange::first;
							else if (what == "l") m_drRange = DRRange::last;
							else if (what == "m") m_drRange = DRRange::middle;
							else if (what == "r") m_drRange = DRRange::random;
							else throw std::invalid_argument("Unrecognized drr value: " + what);
						}
					}
					else if (extName == "limit") {
						m_limit = extVal.nextInt();
					}
					else if (extName == "offset") {
						m_offset = extVal.nextInt();
					}
					else if (extName == "colsep") {
						m_colSep = toUtf8(extVal.empty() ? std::string() : extVal.getNext());
						m_colSepSize = Utils::utf8ToWide(m_colSep.c_str(), m_colSep.length()).length(); // Assumes UTF-16 length = #glyphs!
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

		/*auto createCollation = [conn](const char* name, int(*xCompare)(void*, int, const void*, int, const void*)) {
			if (sqlite3_create_collation(conn, name, SQLITE_UTF8, nullptr, xCompare) != SQLITE_OK)
				throw std::runtime_error(fmt("Failed to create collation %s: %s", name, sqlite3_errmsg(conn)));
		};
		createCollation(CLastName, &cmpLastName);
		createCollation(CTitle, &cmpTitle);*/

		using SqliteFunc = void (*)(sqlite3_context *context, int argc, sqlite3_value **argv);
		auto createScalarFunc = [conn](const char* name, int nArg, SqliteFunc func) {
			if (sqlite3_create_function(conn, name, nArg, SQLITE_UTF8, nullptr, func, nullptr, nullptr) != SQLITE_OK)
				throw std::runtime_error(fmt("Failed to create sqlite function %s: %s", name, sqlite3_errmsg(conn)));
		};

		createScalarFunc("isbn10", 1, isbn10);
		createScalarFunc("isbn13", 1, isbn13);
	}

	mutable decltype(m_columnInfos) m_ancis; // Holds ColumnInfo:s for "actual name" columns.

	ColumnInfo const* getColumn(std::string const & sn, bool allowActualName = false) const
	{
		if (auto it = m_columnInfos.find(sn); it != m_columnInfos.end()) return &it->second;
		if (allowActualName) return &m_ancis.try_emplace(sn, sn, (int)sn.length(), ColumnType::numeric, sn, false, false, Tables()).first->second;
		throw std::invalid_argument("Invalid short column name: " + sn);
	}

	Columns getColumns(std::string const & sns, ColumnsDataKind kind, bool usedInQuery, bool allowActualName = false) const
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
			auto column = getColumn(sn, allowActualName);
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
					toInt(sn, data);
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
				case ColumnsDataKind::width: data = column->width; break;
				case ColumnsDataKind::sortOrder: data = (int)ColumnSortOrder::Asc; break;
				}
			}
			res.emplace_back(column, data);
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

		for (std::string sn; opts.getNext(sn); ) {
			auto col = getColumn(sn);
			col->usedInQuery = true;
			bool glob = false; // Must not replace wildcard if globbing.

			auto val = opts.getNext(); // Either a value or an operation for the value coming up.
			std::string oper;
			if      (val == "lt")  oper = "<";
			else if (val == "lte") oper = "<=";
			else if (val == "gt")  oper = ">";
			else if (val == "gte") oper = ">=";
			else if (val == "eq")  oper = "=";
			else if (val == "neq") oper = "notlike";
			else if (val == "glob") oper = (glob = true, "GLOB");
			else if (val == "nglob") oper = (glob = true, "NOT GLOB");
			else if (val == "isnull" || val == "notnull" || val == "isempty" || val == "range" || val == "nrange") oper = val; 
			else if (val == "and" || val == "or" || val == "nand" || val == "nor") oper = val;
			
			if (oper.empty()) {
				oper = "LIKE";
			}
			else {
				if (oper != "isnull" && oper != "notnull" && oper != "isempty") {
					val = opts.getNext();
				}
			}

			auto getOperand = [&col, this, glob](std::string val) -> std::string {
				if (auto valCol = m_columnInfos.find(val); valCol != m_columnInfos.end()) {
					valCol->second.usedInQuery = true;
					return valCol->second.nameDef;
				}
				return col->getLikeArg(std::move(val), glob);
			};

			val = getOperand(val);

			std::string snCond;
			std::string colName(col->nameDef);
						
			if (oper == "notlike") snCond = "ifnull(" + colName + ", '') NOT LIKE " + val;
			else if (oper == "isnull")  snCond = colName + " IS NULL";
			else if (oper == "notnull") snCond = colName + " IS NOT NULL";
			else if (oper == "isempty") snCond = colName + " = ''";
			else if (oper == "range")   snCond = val + " <= " + colName + " AND " + colName + " <= " + getOperand(opts.getNext());
			else if (oper == "nrange")  snCond = "NOT (" + val + " <= " + colName + " AND " + colName + " <= " + getOperand(opts.getNext()) + ")";
			else if (oper == "and" || oper == "or" || oper == "nand" || oper == "nor") {
				snCond = "(";
				bool not = (oper[0] == 'n');
				for (;;) {
					snCond.append("ifnull(" + colName + ", '')"
						+ (val[0] == '\'' ? (not ? " NOT LIKE " : " LIKE ") : (not ? " <> " : " = "))
						+ val);
					if (!opts.getNext(val)) break;
					snCond.append(oper.back() == 'd' ? LogOp_AND : LogOp_OR);
					val = getOperand(val);
				}
				snCond.append(")");
			}
			else {
				snCond = colName + " " + oper + " " + val;
			}

			auto appendSnCondTo = [&](std::string& c) { c = ((c.empty()) ? snCond : c + LogOp_AND + snCond); };
			appendSnCondTo(col->aggr ? hcond : wcond);
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

	// Needed when listing output multiple times in same LITT session (like when listing items for book input!)
	// Note that usedInResult is used/checked and reset in addOrderBy. Cannot do that here since addAuxTables
	// is called multiple times to generate a single query in some cases.
	void resetTablesAndColumns()
	{
		for (auto& ti : m_tableInfos.arrView) ti.reset();
		for (auto& node : m_columnInfos) node.second.usedInQuery = false;
	}

	void resetListingData(std::string const& whereCondition) // For use from listings during add actions.
	{
		m_selectedColumns.clear(); m_orderBy.clear(); m_additionalColumns.clear();
		resetTablesAndColumns();
		m_whereCondition = getWhereCondition(whereCondition);
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

	bool hasArg(unsigned index)
	{
		return index < m_actionArgs.size();
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
				: throw std::invalid_argument(fmt("Invalid %s value: %s", name, S(m_actionArgs[index]))))
			: input(fmt("Enter %s", name).c_str(), regEx, iopt);
	}

	int intargi(unsigned index, const char* name, InputOptions iopt = Input::required) const 
	{
		return std::stoi(index < m_actionArgs.size()
			? m_actionArgs[index]
			: input(fmt("Enter %s", name).c_str(), R"x(\d+)x", iopt).c_str());
	}

	std::string toUtf8(std::string const & str) const   { return Utils::toUtf8(consoleCodePage, str); }
	std::string fromUtf8(std::string const & str) const { return Utils::fromUtf8(consoleCodePage, str); }

	std::string encodeSqlFromInput(std::string const& sql) const { return toUtf8(sql); }

	enum class Table { // The main tables, used to select listing source.
		books,
		authors,
		originalTitles,
		genres,
		series,
		sources,
		bookCategory,
		language,
		stories,
	};

	struct TableData {
		const char* tableName;
		TableInfo&  tableInfo;
	};
	
	TableData m_tableData[9] = { // Same order as Table!
		{ "Books", m_tableInfos.books },
		{ "Authors", m_tableInfos.authors },
		{ "OriginalTitles", m_tableInfos.originalTitles },
		{ "Genres GBook", m_tableInfos.gbook },
		{ "Series", m_tableInfos.series },
		{ "Sources", m_tableInfos.sources },
		{ "BookCategory", m_tableInfos.bookCategory },
		{ "Language L_book", m_tableInfos.l_book },
		{ "Stories", m_tableInfos.stories },
	};

	const char* getTableName(Table table) const
	{
		return m_tableData[(int)table].tableName;
	}

	TableInfo& getTableInfo(Table table)
	{
		return m_tableData[(int)table].tableInfo;
	}

	enum class SelectOption {
		normal,
		distinct,
	};

	struct QueryBuilder {
		std::string m_query;
		
		QueryBuilder(const char* sql = nullptr)
		{ 
			m_query.reserve(5000);
			if (sql != nullptr) m_query.append(sql);
		}

		void a(const char* str)
		{
			m_query.append(str);
		}

		void a(std::string const & str)
		{
			m_query.append(str);
		}

		void add(const char* line)
		{
			m_query.append("\n").append(line);
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

		void aIf(std::string const & line, bool cond)
		{
			if (cond) add(line);
		}
	};
	
	struct OutputQuery : QueryBuilder {
		Columns m_orderBy;
		Litt& litt;
		std::vector<int> columnWidths; // Only set for column mode.
		std::vector<bool> columnRight; // Only set for column mode.

		OutputQuery(Litt& litt) : litt(litt) {}

		void addExplain()
		{
			if (litt.m_explainQuery != ExQ::None) {
				m_query += "EXPLAIN ";
				if (litt.m_explainQuery != ExQ::VMCode) m_query += "QUERY PLAN ";
			}
		}

		void init()
		{
			addExplain();
		}

		void addDistinct(SelectOption selectOption)
		{
			if (litt.m_selectDistinct || selectOption == SelectOption::distinct) {
				m_query += "DISTINCT ";
			}
		}

		void initSelectBare(SelectOption selectOption = SelectOption::normal)
		{
			init();
			m_query += "SELECT ";
			addDistinct(selectOption);
		}

		void initWithSelectBare(const char* with, SelectOption selectOption = SelectOption::normal)
		{
			init();
			m_query.append("WITH\n").append(with).append("\n");
			m_query += "SELECT ";
			addDistinct(selectOption);
		}

		void initColumnWidths()
		{
			if (litt.m_displayMode == DisplayMode::column && litt.m_explainQuery != ExQ::None) {
				if (litt.m_explainQuery == ExQ::VMCode)   columnWidths = { 5, 20, 6, 6, 6, 30, 6, 7/*comment, not in LITT*/ };
				else if (litt.m_explainQuery == ExQ::Raw) columnWidths = { 5/*id*/, 6/*parent*/, 0/*not used*/, 100/*detail*/ };
				columnRight.clear();
			} // else assumes columnWidths are properly set!
		}

		void addResultColums(const char* defColumns)
		{
			auto selCols = litt.m_selectedColumns.empty() ? litt.getColumns(defColumns, ColumnsDataKind::width, true) : litt.m_selectedColumns;
			selCols += litt.m_additionalColumns;
			for (unsigned i = 0; i < selCols.size(); ++i) {
				auto ci = selCols[i].first;
				ci->usedInResult = true;
				if (i != 0) m_query.append(",");
				m_query.append(ci->nameDef);
				if (!ci->label.empty()) m_query.append(" AS ").append(ci->label);
				if (litt.m_displayMode == DisplayMode::column) {
					auto const width = ci->overriddenWidth >= 0 ? ci->overriddenWidth : selCols[i].second;
					_ASSERT(width >= 0);
					columnWidths.push_back(width);
					columnRight.push_back(ci->justifyRight);
				}
			}
			initColumnWidths();
		}

		void showDefaultColumns(const char* defColumns, const char* defOrderBy)
		{
			if (litt.m_showDefaults) {
				printf("defColumns: %s\ndefOrderBy: %s\n\n", defColumns, defOrderBy);
			}
		}

		void initSelect(const char* defColumns, const char* from, const char* defOrderBy, SelectOption selectOption = SelectOption::normal)
		{
			showDefaultColumns(defColumns, defOrderBy);
			initSelectBare(selectOption);
			addResultColums(defColumns);
			m_query.append("\nFROM ").append(from);
			// Not used here, but we must "run" it anyway in order to finalize all columns used in the query for later "addIfColumns" calls.
			initOrderBy(defOrderBy);
		}

		void initWithSelect(const char* defColumns, const char* with, const char* from, const char* defOrderBy, SelectOption selectOption = SelectOption::normal)
		{
			showDefaultColumns(defColumns, defOrderBy);
			initWithSelectBare(with, selectOption);
			addResultColums(defColumns);
			m_query.append("\nFROM ").append(from);
			// Not used here, but we must "run" it anyway in order to finalize all columns used in the query for later "addIfColumns" calls.
			initOrderBy(defOrderBy);
		}

		void initOrderBy(const char* defOrderBy, bool allowActualNames = false)
		{
			auto asc = [](Columns cols) { for (auto& c : cols) { c.second = (int)ColumnSortOrder::Asc; } return cols; };
			m_orderBy = litt.m_orderBy.empty()
				? (litt.m_selectedColumns.empty()
					? litt.getColumns(defOrderBy, ColumnsDataKind::sortOrder, true, allowActualNames)
					: asc(litt.m_selectedColumns))
				: litt.m_orderBy;
		}

		void addWhere(int indentSize = 0)
		{
			if (!litt.m_whereCondition.empty()) {
				m_query.append("\n").append(std::string(indentSize, ' ')).append("WHERE ").append(litt.m_whereCondition);
			}
		}

		void addHaving()
		{
			if (!litt.m_havingCondition.empty()) {
				m_query.append("\nHAVING ").append(litt.m_havingCondition);
			}
		}

		void xad(std::string const& line)
		{
			m_query.append("\n"); addExplain(); m_query.append(line);
		}

		void xaf(_In_z_ _Printf_format_string_ const char* fmtStr, ...)
		{
			std_string_fmt_impl(fmtStr, res);
			xad(res);
		}

		// NOTE: We assume the tableInfos are reset whenever startTable is changed to something else than the last call.
		void initTablesAndColumns(Table const startTable)
		{
			// First determine tables relationships according to start table and which table link/id fields are taken from
			//
			auto& t = litt.m_tableInfos;
			auto& bi = litt.getColumn("bi")->tables;
			auto& ai = litt.getColumn("ai")->tables;

			// Call AFTER setAuthorParent to avoid that nulling one of the table parents set here.
			// In case book and author has different parents.
			auto setBookParent = [&](TableInfo* bookTi) { 
				bi.reset(bookTi);
				t.books.parent = bookTi;
				t.originalTitles.parent = bookTi;
				t.authorbooks.parent = bookTi;
				t.bookGenres.parent = bookTi;
				t.datesRead.parent = bookTi;
				t.bookSeries.parent = bookTi;
				t.nc.parent = bookTi;
				t.ng.parent = bookTi;
				t.dc.parent = bookTi;
				t.dg.parent = bookTi;
				t.gg.parent = bookTi;
				t.bstc.parent = bookTi;
				t.bstg.parent = bookTi;
				bookTi->parent = nullptr; // No parent, might cause loop.
			};

			auto setAuthorParent = [&](TableInfo* authorTi) {
				ai.reset(authorTi);
				t.authors.parent = authorTi;
				t.ar.parent = authorTi;
				t.astc.parent = authorTi;
				t.astg.parent = authorTi;
				t.abc.parent = authorTi;
				t.abcp.parent = authorTi;
				t.abcr.parent = authorTi;
				t.agg.parent = authorTi;
				t.aggp.parent = authorTi;
				t.agc.parent = authorTi;
				t.agcp.parent = authorTi;
				authorTi->parent = nullptr; // No parent, might cause loop.
			};

			auto setBookAuthorParent = [&t](TableInfo* baTi) {
				t.bastc.parent = baTi;
				t.bastg.parent = baTi;
			};

			auto setGenreParent = [&t](TableInfo* baTi) {
				t.gr.parent = baTi;
				t.gbc.parent = baTi;
				t.gac.parent = baTi;
			};

			t.l_ot.parent = &t.originalTitles;
			t.obc.parent = &t.originalTitles;
			t.gstory.parent = &t.storyGenres;
			t.psmid.parent = &t.authors;
			t.ps.parent = &t.authors;
			t.psf.parent = &t.authors;
			t.bstng.parent = &t.bookStories;

			auto const authorBooksVal = (startTable != Table::authors) ? &t.authorbooks : nullptr;
			litt.getColumn("ap")->tables.reset(&t.datesRead, authorBooksVal);
			litt.getColumn("al")->tables.reset(&t.datesRead, authorBooksVal);
			litt.getColumn("ac")->tables.reset(authorBooksVal);

			t.l_book.parent = (startTable != Table::language) ? &t.books : nullptr;
			t.lbc.parent = t.l_book.parent;

			t.bookCategory.parent = (startTable != Table::bookCategory) ? &t.books : nullptr;
			t.cbc.parent = t.bookCategory.parent;
			litt.getColumn("catid")->tables.reset(t.bookCategory.parent);

			t.gbook.parent = (startTable != Table::genres) ? &t.bookGenres : nullptr;

			t.series.parent = (startTable != Table::series) ? &t.bookSeries : nullptr;
			t.ser.parent = t.series.parent;
			t.sebc.parent = t.series.parent;
			litt.getColumn("si")->tables.reset(t.series.parent);

			t.sources.parent = (startTable != Table::sources) ? &t.datesRead : nullptr;
			t.sor.parent = t.sources.parent;
			t.sobc.parent = t.sources.parent;
			litt.getColumn("soid")->tables.reset(t.sources.parent);
			litt.getColumn("sc")->tables.reset(t.sources.parent);

			_ASSERT(t.stories.parent == nullptr || t.stories.parent == &t.bookStories);
			if (startTable != Table::stories) {
				setBookAuthorParent(&t.authorbooks);
				t.bookStories.parent = &t.authorbooks;
				t.stories.parent = &t.bookStories;
				t.storyGenres.parent = &t.bookStories;
				t.stgg.parent = &t.bookStories;
				t.stnc.parent = &t.bookStories;
				t.stng.parent = &t.bookStories;
				t.stbc.parent = &t.bookStories;
				t.stbg.parent = &t.bookStories;
				if (startTable != Table::genres) { 
					setGenreParent(&t.bookGenres);
				}
				if (startTable != Table::authors) { 
					setAuthorParent(&t.authorbooks);
				}
			}
			litt.getColumn("stid")->tables.reset(t.stories.parent);

			switch (startTable) {
			case Table::books:
			case Table::originalTitles:
				bi.reset();
				break;
			case Table::bookCategory:
			case Table::language:
				setBookParent(&t.books);
				break;
			case Table::authors:
				setBookParent(&t.authorbooks);
				ai.reset();
				break;
			case Table::series:
				setBookParent(&t.bookSeries);
				break;
			case Table::genres:
				setBookParent(&t.bookGenres);
				break;
			case Table::sources:
				setBookParent(&t.datesRead);
				break;
			case Table::stories:
				setBookAuthorParent(&t.bookStories);
				setAuthorParent(&t.storyAuthors);
				setBookParent(&t.storyBooks); 
				setGenreParent(&t.storyGenres);
				t.authorbooks.parent = &t.bookStories; // storyBooks will not do.
				break;
			default:
				throw std::logic_error("initTablesAndColumns: Invalid startUpTable");
			}

			// Then apply used columns to the tables.
			//
			for (auto& node : litt.m_columnInfos) {
				ColumnInfo& ci = node.second;
				if (ci.usedInQuery) {
					for (TableInfo* ti : ci.tables) {
						for (TableInfo* cur = ti; cur != nullptr; cur = cur->parent) {
							cur->used = true;
						}
					}
				}
			}
		}

		void addAuxTablesRaw(Table startTable = Table::books, unsigned indentSize = 0)
		{
			litt.getTableInfo(startTable).included = true; // Do this here so it's done also if addAuxTablesMultipleCalls is called.
			std::string const indent(indentSize, ' ');

			// Virtual table queries.

			#define NC "(SELECT BookID, count(AuthorID) AS AuthorCnt FROM AuthorBooks GROUP BY BookID)"
			#define NG "(SELECT BookID, " A_NAMES " AS 'Author(s)' FROM AuthorBooks JOIN Authors USING(AuthorID) GROUP BY BookID)"

			#define DC "(SELECT BookID, count(\"Date read\") AS 'DRCnt' FROM DatesRead GROUP BY BookID)"
			#define DG "(SELECT BookID, group_concat(\"Date read\",', ') AS 'Date(s)' FROM DatesRead GROUP BY BookID)"

			#define GG "(SELECT BookID, group_concat(Genre,', ') AS 'Genre(s)' FROM BookGenres JOIN Genres USING(GenreID) GROUP BY BookID)"

			#define AR  "(SELECT AuthorID, avg(Rating) AS ARating  FROM AuthorBooks JOIN Books USING(BookID) GROUP BY AuthorID)"
			#define GR  "(SELECT GenreID,  avg(Rating) AS GRating  FROM BookGenres  JOIN Books USING(BookID) GROUP BY GenreID)"
			#define SOR "(SELECT SourceID, avg(Rating) AS SORating FROM DatesRead   JOIN Books USING(BookID) GROUP BY SourceID)"
			#define SER "(SELECT SeriesID, avg(Rating) AS SERating FROM BookSeries  JOIN Books USING(BookID) GROUP BY SeriesID)"

			#define STORY_BOOKS "(SELECT DISTINCT StoryID,BookID FROM BookStories)"
			#define STORY_AUTHORS "(SELECT DISTINCT StoryID,AuthorID FROM BookStories)"

			#define STGG "(SELECT StoryID, group_concat(Genre,', ') AS 'StoryGenre(s)' FROM Stories JOIN StoryGenres USING(StoryID) JOIN Genres USING(GenreID) GROUP BY StoryID)"
			#define STNC "(SELECT StoryID, count(AuthorID) AS ACnt FROM " STORY_AUTHORS " GROUP BY StoryID)"
			#define STNG "(SELECT StoryID, " A_NAMES " AS 'Story author(s)' FROM " STORY_AUTHORS " JOIN Authors USING(AuthorID) GROUP BY StoryID)"
			#define STBC "(SELECT StoryID, count(StoryID) AS BCnt FROM " STORY_BOOKS " GROUP BY StoryID)"
			#define STBG "(SELECT StoryID, group_concat(Title ,'; ') AS 'StoryBooks(s)' FROM " STORY_BOOKS " JOIN Books USING(BookID) GROUP BY StoryID)"

			#define STORYTABLES "Stories JOIN BookStories USING(StoryID)"
			#define BASTC "(SELECT AuthorID, BookID, count(StoryID) AS SCnt FROM BookStories GROUP BY AuthorID, BookID)"
			#define BASTG "(SELECT AuthorID, BookID, group_concat(Story,'; ') AS 'Stories' FROM " STORYTABLES " GROUP BY AuthorID, BookID)"
			#define ASTC "(SELECT AuthorID, count(StoryID) AS AStoryCnt FROM " STORY_AUTHORS " GROUP BY AuthorID)"
			#define ASTG "(SELECT AuthorID, group_concat(Story,'; ') AS 'Author Stories' FROM Stories JOIN " STORY_AUTHORS "USING(StoryID) GROUP BY AuthorID)"
			#define BSTC "(SELECT BookID, count(StoryID) AS BStoryCnt FROM " STORY_BOOKS " GROUP BY BookID)"
			#define BSTG "(SELECT BookID, group_concat(Story,'; ') AS 'Book Stories' FROM Stories JOIN " STORY_BOOKS "USING(StoryID) GROUP BY BookID)"
			#define BSTNG "(SELECT BookID, StoryID, " A_NAMES " AS 'Book+Story author(s)' FROM BookStories JOIN Authors USING(AuthorID) GROUP BY BookID, StoryID)"

			#define PSMID "(SELECT AuthorID AS psmAID, group_concat(psmain, ',') AS PSMainID FROM" \
				          "  (SELECT AuthorID, CASE AuthorID WHEN psp.PseudonymID THEN psp.MainID WHEN psm.MainID THEN psm.MainID ELSE NULL END AS psmain" \
				          "   FROM Authors LEFT JOIN Pseudonyms psm ON(psm.MainID = Authors.AuthorID) LEFT JOIN Pseudonyms psp ON(psp.PseudonymID = Authors.AuthorID)" \
				          "   WHERE psmain IS NOT NULL)" \
				          "GROUP BY AuthorID)"
			#define PS "(SELECT MainID, " A_NAMES " AS Pseudonyms FROM Authors JOIN Pseudonyms ON (AuthorID = PseudonymID) GROUP BY MainID)"
			#define PSF "(SELECT PseudonymID, " A_NAMES " AS \"Pseudonym For\" FROM Authors JOIN Pseudonyms ON (AuthorID = MainID) GROUP BY PseudonymID)"

			#define AB_PSE "(SELECT * FROM AuthorBooks UNION ALL SELECT BookID, MainID AS AuthorID FROM AuthorBooks JOIN Pseudonyms ON AuthorBooks.AuthorID = Pseudonyms.PseudonymID)"
			#define BS_PSE "(SELECT * FROM BookStories UNION ALL SELECT BookID, MainID AS AuthorID, StoryID FROM BookStories JOIN Pseudonyms ON BookStories.AuthorID = Pseudonyms.PseudonymID)"

			#define ABC  "(SELECT AuthorID, count(BookID) AS ABC FROM AuthorBooks GROUP BY AuthorID)"
			#define ABCP "(SELECT AuthorID, count(BookID) AS ABCP FROM " AB_PSE " GROUP BY AuthorID)"
			#define ABCR "(SELECT AuthorID, count(BookID) AS ABCR FROM AuthorBooks JOIN DatesRead USING(BookID) GROUP BY AuthorID)"

			#define AB_NOS "(SELECT * FROM AuthorBooks ab WHERE NOT EXISTS (SELECT 1 FROM BookStories bs WHERE bs.BookID = ab.BookID))"
			#define AG_B "(SELECT DISTINCT AuthorID, GenreID FROM " AB_NOS " JOIN BookGenres USING(BookID))"
			#define AG_S "(SELECT DISTINCT AuthorID, GenreID FROM BookStories JOIN StoryGenres USING(StoryID))"
			#define AG   "(SELECT * FROM " AG_B " UNION SELECT * FROM " AG_S ")"
			#define AGG  "(SELECT AuthorID, group_concat(Genre, ', ') AS \"Author Genres\" FROM " AG " JOIN Genres USING(GenreID) GROUP BY AuthorID)"
			#define AGC  "(SELECT AuthorID, count(GenreID) AS AGC FROM " AG " GROUP BY AuthorID)"

			#define AB_PSE_NOS "(SELECT * FROM " AB_PSE " ab WHERE NOT EXISTS (SELECT 1 FROM BookStories bs WHERE bs.BookID = ab.BookID))"
			#define AG_PSE_B "(SELECT DISTINCT AuthorID, GenreID FROM " AB_PSE_NOS " JOIN BookGenres USING(BookID))"
			#define AG_PSE_S "(SELECT DISTINCT AuthorID, GenreID FROM " BS_PSE " JOIN StoryGenres USING(StoryID))"
			#define AG_PSE   "(SELECT * FROM " AG_PSE_B " UNION SELECT * FROM " AG_PSE_S ")"
			#define AGGP "(SELECT AuthorID, group_concat(Genre, ', ') AS \"Author(p) Genres\" FROM " AG_PSE " JOIN Genres USING(GenreID) GROUP BY AuthorID)"
			#define AGCP "(SELECT AuthorID, count(GenreID) AS AGCP FROM " AG_PSE " GROUP BY AuthorID)"

			#define GBC "(SELECT GenreID, count(BookID) AS GBC FROM BookGenres GROUP BY GenreID)"
			#define GAC "(SELECT GenreID, count(AuthorID) AS GAC FROM " AG " GROUP BY GenreID)"

			#define CBC "(SELECT CategoryID, count(BookID) AS CBC FROM Books GROUP BY CategoryID)"
			#define LBC "(SELECT LangID, count(BookID) AS LBC FROM Books GROUP BY LangID)"
			#define OBC "(SELECT LangID, count(BookID) AS OBC FROM OriginalTitles GROUP BY LangID)"
			#define SOBC "(SELECT SourceID, count(BookID) AS SOBC FROM (SELECT DISTINCT SourceID, BookID FROM DatesRead) GROUP BY SourceID)"
			#define SEBC "(SELECT SeriesID, count(BookID) AS SEBC FROM BookSeries GROUP BY SeriesID)"

			auto includeImpl = [&](TableInfo& table, const char* join, const char* sql) {
				if (table.used && !table.included) {
					add(indent); a(join); a(" "); a(sql);
					table.included = true;
				}
			};
			auto include = [&](TableInfo& table, const char* sql) { 
				includeImpl(table, "JOIN", sql); 
			};
			auto includeLeft = [&](TableInfo& table, const char* sql) { 
				includeImpl(table, "LEFT JOIN", sql); 
			};
			auto includeLeftIf = [&](TableInfo& table, const char* sql, Table leftJoinUnlessThis) { 
				includeImpl(table, startTable == leftJoinUnlessThis ? "JOIN" : "LEFT JOIN", sql); 
			};

			auto& t = litt.m_tableInfos;

			switch (startTable) {
			case Table::bookCategory:
				include(t.books, "Books USING(CategoryID)");
				break;
			case Table::language:
				include(t.books, "Books USING(LangID)");
				break;
			case Table::sources:
				include(t.datesRead, "DatesRead USING(SourceID)");
				break;
			case Table::genres:
				include(t.bookGenres, "BookGenres USING(GenreID)");
				break;
			case Table::stories:
				if (t.storyBooks.used && t.storyAuthors.used) {
					// Faster to just use bookStories directly in this case.
					t.bookStories.used = true;
				}
				include(t.bookStories, "BookStories USING(StoryID)");
				if (!t.bookStories.included) {
					include(t.storyBooks, STORY_BOOKS " USING(StoryID)");
					include(t.storyAuthors, STORY_AUTHORS " USING(StoryID)");
				}
				include(t.authorbooks, "AuthorBooks USING(BookID,AuthorID)");
				break;
			case Table::authors:
				include(t.authorbooks, "AuthorBooks USING(AuthorID)");
				break;
			case Table::series:
				include(t.bookSeries, "BookSeries USING(SeriesID)");
				break;
			}

			include(t.books, "Books USING(BookID)");
			include(t.cbc, CBC " USING(CategoryID)");
			include(t.authorbooks, "AuthorBooks USING(BookID)");
			include(t.authors, "Authors USING(AuthorID)");
			include(t.bookCategory, "BookCategory USING(CategoryID)");
			include(t.l_book, "Language L_book ON(Books.LangID = L_book.LangID)");
			include(t.lbc, LBC " USING(LangID)");

			include(t.nc,  NC " USING(BookID)");
			include(t.ng,  NG " USING(BookID)");

			include(t.ar,   AR   " USING(AuthorID)");
			include(t.abc,  ABC  " USING(AuthorID)");
			include(t.abcp, ABCP " USING(AuthorID)");
			include(t.abcr, ABCR " USING(AuthorID)");
			include(t.agg,  AGG  " USING(AuthorID)");
			include(t.aggp, AGGP " USING(AuthorID)");
			include(t.agc,  AGC  " USING(AuthorID)");
			include(t.agcp, AGCP " USING(AuthorID)");

			include(t.datesRead, "DatesRead USING(BookID)");
			include(t.dc,        DC       " USING(BookID)");
			include(t.dg,        DG       " USING(BookID)");

			include(t.sources, "Sources USING(SourceID)");
			include(t.sor,     SOR    " USING(SourceID)");
			include(t.sobc,    SOBC   " USING(SourceID)");

			include(t.bookGenres, "BookGenres USING(BookID)");
			include(t.gbook,      "Genres GBook ON(BookGenres.GenreID = GBook.GenreID)");
			include(t.gg,         GG " USING(BookID)");

			includeLeft(t.originalTitles, "OriginalTitles USING(BookID)");
			includeLeftIf(t.l_ot,         "Language L_ot ON(OriginalTitles.LangID = L_ot.LangID)", Table::originalTitles);
			includeLeftIf(t.obc,          OBC      " obc ON OriginalTitles.LangID = obc.LangID", Table::originalTitles);

			includeLeftIf(t.bookSeries, "BookSeries USING(BookID)", Table::series);
			includeLeft(t.series,       "Series USING(SeriesID)");
			includeLeftIf(t.ser,        SER " USING(SeriesID)", Table::series);
			includeLeftIf(t.sebc,       SEBC " USING(SeriesID)", Table::series);

			includeLeftIf(t.bookStories, "BookStories USING(BookID,AuthorID)", Table::stories);
			includeLeft(t.stories,       "Stories USING(StoryID)");
			includeLeftIf(t.storyGenres, "StoryGenres USING(StoryID)", Table::stories);
			includeLeftIf(t.gstory,      "Genres GStory ON(StoryGenres.GenreID = GStory.GenreID)", Table::stories);
			includeLeftIf(t.stgg,        STGG " USING(StoryID)", Table::stories);
			includeLeftIf(t.stnc,        STNC " USING(StoryID)", Table::stories);
			includeLeftIf(t.stng,        STNG " USING(StoryID)", Table::stories);
			includeLeftIf(t.stbc,        STBC " USING(StoryID)", Table::stories);
			includeLeftIf(t.stbg,        STBG " USING(StoryID)", Table::stories);
			includeLeftIf(t.bstng,       BSTNG " USING(BookID,StoryID)", Table::stories);
			includeLeftIf(t.bastc,       BASTC " USING(AuthorID,BookID)", Table::stories);
			includeLeftIf(t.bastg,       BASTG " USING(AuthorID,BookID)", Table::stories);
			includeLeftIf(t.astc,        ASTC " USING(AuthorID)", Table::stories);
			includeLeftIf(t.astg,        ASTG " USING(AuthorID)", Table::stories);
			includeLeftIf(t.bstc,        BSTC " USING(BookID)", Table::stories);
			includeLeftIf(t.bstg,        BSTG " USING(BookID)", Table::stories);

			// Would be after GG above, but need to be after StoryGenres for stories listing.
			include(t.gr,  GR  " USING(GenreID)");
			include(t.gbc, GBC " USING(GenreID)");
			include(t.gac, GAC " USING(GenreID)");

			includeLeft(t.psmid, PSMID " psmid ON (psmAID = Authors.AuthorID)");
			includeLeft(t.ps,    PS    " ps ON (ps.MainID = Authors.AuthorID)");
			includeLeft(t.psf,   PSF   " psf ON (psf.PseudonymID = Authors.AuthorID)");
		}

		void addAuxTables(Table startTable = Table::books, unsigned indentSize = 0)
		{
			initTablesAndColumns(startTable);
			addAuxTablesRaw(startTable, indentSize);
			litt.resetTablesAndColumns();
		}

		void addAuxTablesMultipleCalls(Table startTable = Table::books, unsigned indentSize = 0)
		{
			addAuxTablesRaw(startTable, indentSize);
			for (auto& ti : litt.m_tableInfos.arrView) ti.included = false;
		}

		void addOrderBy()
		{
			if (!m_orderBy.empty()) { 
				m_query += "\nORDER BY ";
				for (unsigned i = 0; i < m_orderBy.size(); ++i) {
					if (i != 0) {
						m_query += ",";
					}
					auto ci = m_orderBy[i].first;
					auto order = (ColumnSortOrder)m_orderBy[i].second;
					// Use label if column is used in result(select), otherwise, have to use the name/def.
					// For window function based columns the latter may cause an out of memory error for some reason!
					// But only if the column is also included in the result, if the column is only used in order by it works!
					m_query += (ci->usedInResult ? ci->labelName() : ci->nameDef);
					if (ci->collation != nullptr) {
						m_query.append(" COLLATE ").append(ci->collation);
					}
					if (order == ColumnSortOrder::Desc) {
						m_query.append(" DESC"); // ASC is default.
					}
				}
			}
			if (litt.m_limit > 0) {
				m_query.append("\nLIMIT ").append(std::to_string(litt.m_limit));
				if (litt.m_offset > 0) {
					m_query.append(" OFFSET ").append(std::to_string(litt.m_offset));
				}
			}

			// Needed when called multiple times in same LITT session (like when listing items for book input!)
			for (auto& node : litt.m_columnInfos) node.second.usedInResult = false;
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
				if ((size_t)i == query.columnWidths.size()) {
					// This will happen if "execute" is used to execute two (or more) different SQL 
					// queries where the latter one contains more columns that the former. 
					// Just add a suitable value to avoid crash. No need to support this use case further.
					query.columnWidths.push_back(30);
					query.columnRight.push_back(false);
				}
				if (query.columnWidths[i] > 0) {
					if (i != 0) m_output.write(m_colSep);
					if (m_ansiEnabled && m_ansiRowColors[i].get() != m_ansiDefColor) m_output.write(m_ansiRowColors[i].get());
					m_output.writeUtf8Width(val, query.columnWidths[i], query.columnRight[i]);
					if (m_ansiEnabled && m_ansiRowColors[i].get() != m_ansiDefColor) m_output.write(m_ansiDefColor);
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
	std::string m_ansiDefColor = "\x1b[0m";

	std::vector<AnsiColumnColor> m_ansiColColors;
	mutable std::vector<std::string> m_ansiColColorsIndexed;

	std::vector<AnsiValueColor> m_ansiValueColors;
	mutable std::vector<AnsiValueColorIndexed> m_ansiValueColorsIndexed;

	mutable std::vector<std::reference_wrapper<std::string const>> m_ansiRowColors;

	void ansiInit(int argc, char **azColName) const
	{
		enableVTMode();
		m_output.write(m_ansiDefColor);

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

	bool consEnabled() const { return m_explainQuery == ExQ::None && m_consRowMinCount > 0; }

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

	struct EQPGraphRow {
		int iEqpId;        /* ID for this row */
		int iParentId;     /* ID of the parent row */
		std::string text;  /* Text to display for this row */
	};

	class EQPGraph {
		Output const& m_output;
		std::vector<EQPGraphRow> rows;
		std::string prefix;

		bool indexValid(int index) const { return 0 <= index && index < (int)rows.size(); }

		int nextRowIndex(int iEqpId, int oldIndex)
		{
			int row = indexValid(oldIndex) ? (oldIndex + 1) : 0;
			while (indexValid(row) && rows[row].iParentId != iEqpId)
				++row;
			return row;
		}

		void renderLevel(int iEqpId) {
			int next = -1;
			for (int row = nextRowIndex(iEqpId, -1); indexValid(row); row = next) {
				next = nextRowIndex(iEqpId, row);
				m_output.write(prefix);
				m_output.write(indexValid(next) ? "|--" : "`--");
				m_output.write(rows[row].text);
				m_output.write("\n");
				prefix += (indexValid(next) ? "|  " : "   "); // len = 3
				renderLevel(rows[row].iEqpId);
				prefix.resize(prefix.size() - 3);
			}
		}
	public:
		EQPGraph(Output const& output) :m_output(output) {}

		void appendRow(int iEqpId, int parentId, const char *zText)
		{
			if (!rows.empty() && rows.back().iEqpId > iEqpId) { // => New query starts
				render();
				rows.clear();
			}
			rows.push_back(EQPGraphRow{ iEqpId, parentId, zText });
		}

		void render() 
		{
			if (rows.empty()) return;
			m_output.write("QUERY PLAN\n");
			prefix.clear();
			renderLevel(0);
		}
	};

	mutable std::unique_ptr<EQPGraph> m_eqpGraph;

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
				writeBomIfNeeded();

				if (m_explainQuery == ExQ::Graph) {
					m_eqpGraph = std::make_unique<EQPGraph>(m_output);
				}
				else if (m_displayMode == DisplayMode::column) {
					for (int i = query.columnWidths.size(); i < argc; ++i) {
						query.columnWidths.push_back(std::min(size_t{30}, std::max(strlen(azColName[i]), strlen(rowValue(argv[i])))));
					}
					for (int i = query.columnRight.size(); i < argc; ++i) {
						query.columnRight.push_back(false);
					}

					bool fitW = (m_fitWidth == FitWidth::on);
					bool const autoFit = (m_fitWidth == FitWidth::automatic && m_output.stdOutIsConsole());
					if (fitW || autoFit) {
						int requiredWidth = 0;
						for (auto& w : query.columnWidths) { 
							if (w > m_fitWidthValue) w = m_fitWidthValue; // Guard against HUGE sizes, can be slow to inc!
							requiredWidth += w;
						}
						if (query.columnWidths.size() > 1) {
							requiredWidth += (m_colSepSize * (query.columnWidths.size() - 1));
						}
						if (autoFit) {
							fitW = (requiredWidth > m_fitWidthValue);
						}
					
						if (fitW) {
							int const target = m_fitWidthValue;
							int const current = requiredWidth;
							if (current != target) {
								int const inc = (target > current) ? 1 : -1;
								int diff = abs(target - current);
								for (;;) {
									int changed = 0;
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
					}

					if (m_ansiEnabled) {
						ansiInit(argc, azColName);
					}
				}
				else if (m_displayMode == DisplayMode::htmldoc) {
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

				if (!m_eqpGraph) {
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
				}
			} // if (m_rowCount == 0) {

			if (m_eqpGraph && argc == 4) {
				m_eqpGraph->appendRow(atoi(argv[0]), atoi(argv[1]), argv[3]);
			}
			else {
				if (consEnabled()) {
					consProcessRow(query, argc, argv);
				}
				else {
					outputRow(query, false, argc, argv);
				}
			}
			++m_rowCount;
			return 0;
		}
		catch (std::exception& ex) {
			if (! m_output.error()) {
				m_output.flushNoThrow();
				fprintf(stderr, "\nCallback exception: %s\n", ex.what());
			}
			return 1;
		}
	}

	void runOutputQuery(OutputQuery& query)
	{
		std::string sql = encodeSqlFromInput(query.m_query);

		if (m_showQuery) {
			m_output.write(sql); m_output.write('\n');
			m_output.flush();
			return;
		}

		m_rowCount = 0;
		if (int const res = sqlite3_exec(m_conn.get(), sql.c_str(), outputQueryCallBack, &query, nullptr); res == SQLITE_OK) {
			if (m_eqpGraph) {
				m_eqpGraph->render();
				m_eqpGraph.reset();
			}
			else {
				if (m_displayMode == DisplayMode::htmldoc) {
					m_output.write("</table>\n</body>\n</html>\n");
				}
				if (consEnabled()) {
					consOutputMatchedCount(); // In case matching was still ongoing at the last row.
				}
			}
			m_output.flush();
			if (m_showNumberOfRows) {
				printf("\n# = %i\n", m_rowCount);
			}
		}
		else {
			if (! m_output.error()) {
				throw std::runtime_error(fmt("%s\n\nSQL error: %s", S(sql), sqlite3_errmsg(m_conn.get())));
			}
		}
	}
	
	void runListData(const char* defColumns, const char* defOrderBy, Table startTable = Table::books, SelectOption selectOption = SelectOption::normal)
	{
		OutputQuery query(*this);
		query.initSelect(defColumns, getTableName(startTable), defOrderBy, selectOption);
		query.addAuxTables(startTable);
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listAuthors(std::string const & action, std::string const & ln, std::string const & fn)
	{
		addActionWhereCondition("ln", ln);
		addActionWhereCondition("fn", fn);
		if (action == "a") {
			runListData("ai.nn.30", "ai", Table::authors); 
		}
		else {
			runListData("bi.nn.bsra.btst.dr.so.bsgg", "ai.dr.bi", Table::authors);
		}
	}

	void listPseudonyms(std::string const & ln, std::string const & fn)
	{
		appendToWhereCondition(LogOp_AND, (getWhereCondition("psf.*") + LogOp_OR + getWhereCondition("ps.*"))); 
		addActionWhereCondition("ln", ln);
		addActionWhereCondition("fn", fn);
		runListData("psmid.ai.nn.ps.psf", "psmid.ps.desc.ln", Table::authors);
	}

	void listBooks(std::string const & action, std::string const & title)
	{
		if (action == "b") {
			addActionWhereCondition("bt", title);
			runListData("bi.bt.60", "bi", Table::books);
		}
		else {
			addActionWhereCondition("btst", title);
			runListData("bi.nn.bsra.btst.dr.so.bsgg", "dr.bi.stid.ln.fn", Table::books);
		}
	}

	void listSeries(std::string const & action, std::string const & series)
	{
		addActionWhereCondition("se", series);
		if (action == "s") {
			runListData("si.se.70", "si", Table::series);
		}
		else {
			runListData("bi.sep.nn.ra.bt.dr", "dr.bi.ln.fn", Table::series);
		}
	}

	void listGenres(std::string const & action, std::string const & genre)
	{
		addActionWhereCondition("ge", genre);
		if (action == "g") {
			runListData("gi_n.ge.50", "ge", Table::genres);
		}
		else {
			runListData("ge.bi.bt.ra.dr.ng.gg", "ge.dr.bi", Table::genres);
		}
	}

	void listOriginalTitles()
	{
		addActionWhereCondition("ot", 0);
		runListData("bi.ng.otla.ot.bt.dr.so.gg", "dr.bi", Table::originalTitles);
	}

	void listStories(std::string const & action, std::string const & story)
	{
		addActionWhereCondition("st", story);
		if (action == "st") {
			runListData("stid.st", "stid.st", Table::stories);
		}
		else {
			runListData("bi.bt.ra.dr.stid.st.stra.stng.stgg", "dr.bi.stid", Table::stories);
		}
	}

	void listSources(std::string const & action, std::string const & sourceName)
	{
		addActionWhereCondition("so", sourceName);
		if (action == "so") {
			runListData("soid.so.50", "so", Table::sources);
		}
		else {
			runListData("so.bi.bt.dr.nn", "so.dr.bi.ln.fn", Table::sources);
		}
	}

	void listBookCategories(std::string const & action, std::string const & catName)
	{
		addActionWhereCondition("cat", catName);
		if (action == "c") {
			runListData("catid.cat.30", "catid", Table::bookCategory);
		}
		else {
			runListData("cat.bi.bt.dr.nn", "dr.bi.ln.fn", Table::bookCategory);
		}
	}

	void listBookLanguages(std::string const & action, std::string const & catName)
	{
		addActionWhereCondition("la", catName);
		if (action == "l") {
			runListData("laid_n.la", "laid_n", Table::language);
		}
		else {
			runListData("la.bi.bt.dr.nn", "dr.bi.ln.fn", Table::language);
		}
	}

	void listRereads()
	{
		OutputQuery query(*this);
		const char* from = "(SELECT BookID, Count(BookID) As ReadCount FROM DatesRead GROUP BY BookID HAVING Count(BookID) > 1)";
		query.initSelect("brc.bt.bi.dr.ng", from, "dr.bi");
		query.add("JOIN Books USING(BookID)");
		query.addAuxTables();
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listReot()
	{
		OutputQuery query(*this);
		const char* with = 
R"r(	ag AS (SELECT BookID, group_concat(AuthorID,', ') AS ais FROM AuthorBooks GROUP BY BookID),
	qbt AS (SELECT BookID, ais, Title FROM Books JOIN ag USING(BookID) WHERE BookID NOT IN (SELECT BookID FROM OriginalTitles)),
	qot AS (SELECT BookID, ais, "Original Title" as ot, Books.LangID as bli FROM OriginalTitles JOIN Books USING(BookID) JOIN ag USING(BookID)),
	reot AS (SELECT qbt.BookID FROM qbt JOIN qot ON (qbt.ais = qot.ais AND qbt.BookID <> qot.BookID AND qbt.Title = qot.ot) UNION 
	         SELECT qot.BookID FROM qbt JOIN qot ON (qbt.ais = qot.ais AND qbt.BookID <> qot.BookID AND qbt.Title = qot.ot) UNION
	         SELECT q1.BookID FROM qot q1 JOIN qot q2 ON (q1.ais = q2.ais AND q1.ot = q2.ot AND q1.BookID <> q2.BookID AND q1.bli <> q2.bli)))r";
		const char* from = "reot JOIN Books USING(BookID)";
		query.initWithSelect("ng.ra.bt.dr.so.gg", with, from, "dr.bi");
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
		query.add("JOIN Books USING(Title)");
		query.addAuxTables();
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listSameISBN()
	{
		OutputQuery query(*this);
		const char* from = "(SELECT ISBN, Count(ISBN) As ISBNCount FROM Books GROUP BY ISBN HAVING Count(ISBN) > 1)";
		query.initSelect("bi.bt.isbn.dr.ng", from, "isbn.dr.bi");
		query.add("JOIN Books USING(ISBN)");
		query.addAuxTables();
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listSamestory() 
	{
		OutputQuery query(*this);
		auto from = "(SELECT DISTINCT a.* FROM Stories AS a JOIN Stories AS b WHERE a.Story = b.Story AND a.StoryID <> b.StoryID)";
		query.initSelect("stid.st.nn.bi.bt.dr.so", from, "st.nn.dr");
		query.add("JOIN BookStories USING(StoryID)");
		query.add("JOIN Books USING(BookID)");
		m_tableInfos.authorbooks.included = true; // Already have AuthorID from above so skip to avoid need for SELECT DISTINCT.
		m_tableInfos.stories.included = true; // Already have Stories content from above so skip to avoid ambigous column error.
		m_tableInfos.bookStories.included = true; // See above.
		query.addAuxTables();
		query.addWhere();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listTitlestory()
	{
		OutputQuery query(*this);
		query.columnWidths = { 6,4,20,15,10,15,20,10,15,20,10,15 };
		query.initColumnWidths();
		query.initSelectBare();
		query.a(
R"(B.BookID AS BookID, CASE WHEN B.AuthorID = S.AuthorID THEN 'YES' ELSE '-' END AS Dupe, B.Title AS Title, 
BRating||'/'||SRating||'/'||SBRating AS "B/S/SB Rating", "Book read", "Book source", 
CASE WHEN B.AuthorID <> S.AuthorID THEN BookAuthor ELSE '* see story *' END AS 'Book Author',
S.BookID||'/'||S.StoryID AS 'B/StoryID', "Story Author", "Story book title",  
"Story read", "Story source"
FROM (SELECT BookID, AuthorID, Title, Books.Rating AS BRating, "Date read" AS "Book read", Source AS "Book source", 
      ltrim("First Name"||' '||"Last Name") AS BookAuthor FROM Books
	JOIN AuthorBooks USING(BookID)
	JOIN Authors USING(AuthorID)
	JOIN DatesRead USING(BookID)
	JOIN Sources USING(SourceID)
) AS B
JOIN (SELECT BookID, AuthorID, Title AS "Story book title", Books.Rating AS SBRating,
      StoryID, Story, Stories.Rating AS SRating, 
      "Date read" AS "Story read", Source AS "Story source", 
      ltrim("First Name"||' '||"Last Name") AS "Story Author" FROM Stories JOIN BookStories USING(StoryID)
	JOIN Books USING(BookID)
	JOIN Authors USING(AuthorID)
	JOIN DatesRead USING(BookID)
	JOIN Sources USING(SourceID)
) as S 
WHERE B.Title = S.Story AND B.BookID <> S.BookID
ORDER BY Dupe DESC, "Book read")");
		runOutputQuery(query);
	}

	std::string getCountColumn()
	{
		switch (m_count) {
		case Count::books: return "bc";
		case Count::pages: return "bcp";
		case Count::words: return "bcw";
		case Count::kwords: return "bckw";
		case Count::authors: return "bca";
		case Count::genres: return "bcg";
		case Count::stories: return "bcst";
		case Count::sources: return "bcso";
		case Count::categories: return "bcc";
		default: throw std::logic_error("Invalid Count value: " + std::to_string((int)m_count));
		}
	}

	std::string getDrRangeColumn()
	{
		switch (m_drRange) {
		case DRRange::first: return DRR_FIRST;
		case DRRange::last: return DRR_LAST;
		case DRRange::middle: return DRR_MID;
		case DRRange::random: return DRR_RAND;
		default: throw std::logic_error("Invalid DRRange value: " + std::to_string((int)m_drRange));
		}
	}

	void listBookCounts(std::string const & countCond, bool includeReReads, const char* columns, const char* snGroupBy, Table startTable = Table::books)
	{
		std::string ccol = getCountColumn();
		auto selCols = columns + std::string(".") + ccol;
		if (!countCond.empty()) { addCountCondToHavingCondition(ccol.c_str(), countCond); }
		if (includeReReads) { getColumn("dr")->usedInQuery = true; }
		getColumn(snGroupBy)->usedInQuery = true;

		OutputQuery query(*this);
		query.initSelect(selCols.c_str(), getTableName(startTable), (ccol + ".desc").c_str());
		query.addAuxTables(startTable);
		query.addWhere();
		query.add("GROUP BY " + getColumn(snGroupBy)->nameDef);
		query.addHaving();
		query.addOrderBy();
		runOutputQuery(query);
	}

	void listYearlyBooksCounts(int count, int firstYear, int lastYear, const char* snColSelect, const char* snColGroupBy, Table startTable = Table::books)
	{
		unsigned cwidth = 0u;
		switch (m_count) {
		case Count::books: cwidth = 3u; break;
		case Count::pages: cwidth = 5u; break;
		case Count::words: cwidth = 8u; break;
		case Count::kwords: cwidth = 5u; break;
		}

		auto col = getColumn(snColSelect);  col->usedInQuery = true;
		auto bc  = getColumn(getCountColumn()); bc->usedInQuery = true;
		auto gby = getColumn(snColGroupBy); gby->usedInQuery = true;

		OutputQuery q(*this);
		q.columnWidths = { 3 }; for (int y = firstYear; y <= lastYear; ++y) q.columnWidths.push_back(30);
		q.initColumnWidths();
		q.add("ATTACH DATABASE ':memory:' AS mdb; BEGIN TRANSACTION;");
		q.add("CREATE TABLE mdb.Res (\n\"#\" INTEGER PRIMARY KEY"); // Create result set in-place due to SQLite's 64-way join limit.
		for (int year = firstYear; year <= lastYear; ++year) q.adf(",\"%i\" TEXT", year);
		q.add(");");
		q.xaf("INSERT INTO mdb.Res (\"#\") WITH RECURSIVE c(x) AS (SELECT 1 UNION ALL SELECT x+1 FROM c WHERE x<%i) SELECT * FROM c;\n", count);
		for (int year = firstYear; year <= lastYear; ++year) {
			auto ycond = appendConditions(LogOp_AND, m_whereCondition, getWhereCondition(fmt("dr.%i-*", year)));
			if (year == firstYear) q.initTablesAndColumns(startTable); // Need to call after getWhereCondition, once is enough since similar cond for whole loop.
			q.xaf("CREATE TABLE mdb.Year%i AS SELECT printf('%%%ui - %%s',%s,%s) AS \"%i\"", year, cwidth, S(bc->nameDef), S(col->nameDef), year);
			q.adf("FROM %s", getTableName(startTable));
			q.addAuxTablesMultipleCalls(startTable);
			q.adf("WHERE %s", S(ycond));
			q.adf("GROUP BY %s", S(gby->nameDef));
			q.addHaving();
			q.adf("ORDER BY %s DESC, %s", S(bc->nameDef), S(col->nameDef));
			q.adf("LIMIT %i;", count);
			if (m_explainQuery == ExQ::None)
			q.adf("UPDATE mdb.Res SET \"%i\" = (SELECT \"%i\" FROM mdb.Year%i WHERE rowId=mdb.Res.\"#\");", year, year, year);
			q.a("\n");
		}
		resetTablesAndColumns();
		q.a("\n"); q.initSelectBare(); q.a("* FROM mdb.Res ORDER BY \"#\";");
		q.add("END TRANSACTION; DETACH DATABASE mdb");
		runOutputQuery(q);
	}

	void listBooksReadPerDate(std::string countCond)
	{
		if (countCond.empty()) countCond = "2";

		// We count dates between time 00:00 to 06:00 as the previous day (was up late reading, so want them counted to prev day).
		std::string calcDRTimeWindow = "case when (time(\"Date Read\") > '00:00:00' and time(\"Date Read\") < '06:00:00') then date(\"Date Read\", '-6 hours') else date(\"Date Read\") end";

		OutputQuery query(*this);
		query.initSelect("dr.bt.nn", "Books", "dr.bt.nn"); 
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
		std::string def;
		std::string name; // Used in SQL so need to be quoted in case it contains spaces, is a number etc.

		PeriodColumn(std::string d, std::string const & n) : 
			def(std::move(d)), name(quote(n)) {}

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
			res.emplace_back(def, name);
			width = std::max(width, res.back().name.length());
		}
		if (!res.empty()) {
			writeBomIfNeeded();
			for (auto const & pc : res) {
				m_output.writeUtf8Width(toUtf8(pc.name).c_str(), width, false);
				m_output.write(" : ");
				m_output.write(toUtf8(pc.def));
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
		std::vector<PeriodColumn>&& columns) // Take ownership so we can modify it
	{
		// Total with empty def (i.e. include all) as first column. Avoids duplicating code this way!
		columns.emplace(columns.begin(), "", "Total");

		std::string whatCol; unsigned colWidths = 4u; std::string nonBookSn;
		switch (m_count) {
		case Count::books: whatCol = "BookID"; break;
		case Count::pages: whatCol = "Pages"; colWidths = 5u; break;
		case Count::words: whatCol = "Words"; colWidths = 8u; break;
		case Count::kwords: whatCol = "Words"; colWidths = 5u; break;
		case Count::authors: nonBookSn = "ai"; break;
		case Count::genres: nonBookSn = "gi"; break;
		case Count::stories: nonBookSn = "stid"; break;
		case Count::sources: nonBookSn = "soid";  break;
		case Count::categories: nonBookSn = "catid"; break;
		}
		std::string distinct;
		if (!nonBookSn.empty()) {
			distinct = "DISTINCT ";
			auto nonBookCol = getColumn(nonBookSn);
			nonBookCol->usedInQuery = true;
			whatCol = nonBookCol->nameDef;
		}

		std::string sn = getCountColumn();
		std::string colOp = getColumn(sn)->nameDef;

		period = quote(period);
		OutputQuery q(*this);
		q.columnWidths.push_back(colWidth(period)); q.columnRight.push_back(false);
		for (auto& c : columns) { q.columnWidths.push_back(std::max(colWidths, c.colWidth())); q.columnRight.push_back(true); }
		q.initColumnWidths();
		std::string periodFunc;
		if      (periodDef == "%Y")    periodFunc = "substr(\"Date Read\",1,4)";
		else if (periodDef == "%m")    periodFunc = "substr(\"Date Read\",6,2)";
		else if (periodDef == "%Y-%m") periodFunc = "substr(\"Date Read\",1,7)";
		else                           periodFunc = fmt("strftime('%s', \"Date Read\")", S(periodDef));

		q.add("ATTACH DATABASE ':memory:' AS mdb; BEGIN TRANSACTION;");
		// Use a table for DR instead of WITH/VIEW/subquery to ensure random() in DRRR is only evaluated once per DR-value.
		q.add("CREATE TABLE mdb.DR AS SELECT BookID, " + getDrRangeColumn() + " AS \"Date Read\", SourceID FROM DatesRead;");
		q.add("CREATE TABLE mdb.Res (\n" + period + " TEXT PRIMARY KEY");
		for (auto& c : columns) q.adf(",%s INTEGER", S(c.name));
		q.add(");\n");

		for (auto& c : columns) {
			auto ccond = c.def.empty() ? m_whereCondition : appendConditions(LogOp_AND, m_whereCondition, getWhereCondition(c.def));
			q.xad("INSERT INTO mdb.Res (" + period + ", " + c.name + ") SELECT " + period + ", " + colOp + " FROM");
			q.add(" (SELECT " + distinct + whatCol + ", " + periodFunc + " AS " + period);
			q.add("  FROM Books JOIN mdb.DR USING(BookID)");
			q.initTablesAndColumns(Table::books); // Call here to pick up new tables from c.def, same startTable => many calls safe.
			m_tableInfos.datesRead.included = true;
			q.addAuxTablesMultipleCalls(Table::books, 2);
			q.aIf("  WHERE " + ccond, !ccond.empty());
			q.add(" )");
			q.add(" GROUP BY " + period);
			q.aIf(" HAVING " + m_havingCondition, !m_havingCondition.empty());
			q.add(" ON CONFLICT(" + period  + ") DO UPDATE SET " + c.name + "=excluded." + c.name + ";\n");
		}

		q.a("\n"); q.initSelectBare(); q.a("* FROM mdb.Res");
		q.initOrderBy(period.c_str(), true);
		q.aIf("WHERE " + period + " LIKE " + likeArg(cond + WcS), !cond.empty() && cond != WcS);
		q.addOrderBy();
		q.add("; END TRANSACTION; DETACH DATABASE mdb");
		resetTablesAndColumns();
		runOutputQuery(q);
	}

	int executeSql(std::string const& userSql, int (*callback)(void*,int,char**,char**) = nullptr, void* callBackData = nullptr, bool enableShowQuery = true) const
	{
		auto encSql = encodeSqlFromInput(userSql);
		if (enableShowQuery && m_showQuery) {
			m_output.write(encSql); m_output.write('\n'); m_output.flush();
			return 0;
		}

		if (int res = sqlite3_exec(m_conn.get(), encSql.c_str(), callback, callBackData, nullptr); res != SQLITE_OK)
			throw std::runtime_error(fmt("SQL error: %s", sqlite3_errmsg(m_conn.get())));
		else
			return sqlite3_changes(m_conn.get());
	}

	int executeWriteSql(std::string const& userSql)
	{
		if (m_explainQuery != ExQ::None) {
			OutputQuery q(*this); 
			q.initColumnWidths();
			q.init();
			q.a(userSql);
			runOutputQuery(q);
			return 0;
		}
		else {
			int const changes = executeSql(userSql);
			if (!m_showQuery) {
				const char* verb = "Modified";
				if (userSql.find("UPDATE") != std::string::npos && userSql.find("INSERT") == std::string::npos) verb = "Updated";
				else if (userSql.find("DELETE")  != std::string::npos) verb = "Deleted";
				else if (userSql.find("REPLACE") != std::string::npos) verb = "Added/Updated";
				else if (userSql.find("UPDATE")  != std::string::npos) verb = "Added/Updated";
				else if (userSql.find("INSERT")  != std::string::npos) verb = "Added";
				printf("%s %i rows\n", verb, changes);
			}
			return changes;
		}
	}

	int executeWriteSqlf(_In_z_ _Printf_format_string_ const char* sqlFmtStr, ...)
	{
		std_string_fmt_impl(sqlFmtStr, userSql);
		return executeWriteSql(userSql);
	}

	void executeInsert(const char* idName, _In_z_ _Printf_format_string_ const char* sqlFmtStr, ...)
	{
		std_string_fmt_impl(sqlFmtStr, userSql);
		if (executeWriteSql(userSql) > 0) {
			if (auto const id = sqlite3_last_insert_rowid(m_conn.get()); id != EmptyId) {
				printf("Added with %s %llu\n", idName, id);
			}
		}
	}

	std::vector<std::vector<std::string>> selectRowValues(std::string const& userSql) const
	{
		std::vector<std::vector<std::string>> res;
		auto callback = [](void *pArg, int argc, char** argv, char** /*azColName*/)
		{
			std::vector<std::string> row;
			row.resize(argc);
			std::transform(argv, argv + argc, row.begin(), rowValue);
			auto& res = *static_cast<std::vector<std::vector<std::string>>*>(pArg);
			res.push_back(std::move(row));
			return 0;
		};
		executeSql(userSql, callback, &res, false);
		return res;
	}

	std::vector<IdValue> selectRowIdValues(std::string const& userSql) const
	{
		auto rows = selectRowValues(userSql);
		std::vector<IdValue> res;
		for (auto const& row : rows)
		{
			if (IdValue id; !toIdValue(row.at(0), id))
				throw std::logic_error("Invalid id value: '" + row.at(0) + "' from query " + userSql);
			else res.push_back(id);
		}
		return res;
	}

	bool hasRowValue(std::string const& userSql)
	{
		auto res = selectRowValues(userSql);
		return !res.empty();
	}

	std::string selectSingleValue(std::string const& userSql, const char* valueName) const
	{
		auto res = selectRowValues(userSql);
		if (res.empty()) throw std::runtime_error(fmt("Could not find %s", valueName));
		return res[0].at(0);
	}

	std::string selDV(const char* valCol, Table table, const char* idCol, IdValue id) const
	{
		auto sql = fmt("SELECT %s FROM %s WHERE %s=%llu", valCol, getTableName(table), idCol, id);
		return fromUtf8(selectSingleValue(sql, idCol)); // Convert to console code page, it will be displayed there.
	}
	std::string selTitle(IdValue id) const        { return selDV("Title", Table::books, "BookID", id); }
	std::string selSeries(IdValue id) const       { return selDV("Series", Table::series, "SeriesID", id); }
	std::string selSource(IdValue id) const       { return selDV("Source", Table::sources, "SourceID", id); }
	std::string selGenre(IdValue id) const        { return selDV("Genre", Table::genres, "GenreID", id); }
	std::string selAuthor(IdValue id) const       { return selDV(A_NAME, Table::authors, "AuthorID", id); }
	std::string selStory(IdValue id) const        { return selDV("Story", Table::stories, "StoryID", id); }
	std::string selBookCategory(IdValue id) const { return selDV("Category", Table::bookCategory, "CategoryID", id); }
	std::string selLanguage(IdValue id) const     { return selDV("Language", Table::language, "LangID", id); }
	
	InputCheckIdFunction cf(std::string (Litt::*selMethod)(IdValue id) const) const
	{
		return [=](IdValue id) { (this->*selMethod)(id); };
	}

	#define LIST_F(listCode) [&](std::string const & s) { resetListingData(""); listCode; }
	InputListFunction getListBook()   { return LIST_F(listBooks  ("b",  s + WcS));       }
	InputListFunction getListAuthor() { return LIST_F(listAuthors("a",  s + WcS, ""));   }
	InputListFunction getListSource() { return LIST_F(listSources("so", WcS + s + WcS)); }
	InputListFunction getListGenre()  { return LIST_F(listGenres ("g",  WcS + s + WcS)); }
	InputListFunction getListSeries() { return LIST_F(listSeries ("s",  WcS + s + WcS)); }
	InputListFunction getListStory()  { return LIST_F(listStories("st", s + WcS));       }
	InputListFunction getListBookCategory() { return LIST_F(listBookCategories("c", s + WcS)); }
	InputListFunction getListLanguage() { return LIST_F(listBookLanguages("l", s + WcS)); }
	
	#undef LIST_F

	IdValue getNextIdValue(const char* idCol, const char* table)
	{
		auto idStr = selectSingleValue(fmt("SELECT ifnull(max(%s), 0) + 1 FROM %s", idCol, table), idCol);
		IdValue id = 1; toIdValue(idStr, id); return id;
	}

	void addAuthor()
	{
		auto ln = argi(0, "last name", optional); if (ln.empty()) return;
		auto fn = argi(1, "first name", optional); // May be empty.
		if (confirmf("Add author '%s, %s'", S(ln), S(fn))) {
			executeInsert("AuthorID", "INSERT INTO Authors (\"Last Name\",\"First Name\") VALUES(%s,%s)", ESC_S(ln), ESC_S(fn));
		}
	}

	void addGenre(std::string const & name)
	{
		executeInsert("GenreID", "INSERT INTO Genres (Genre) VALUES(%s)", ESC_S(name));
	}

	void addSeries(std::string const & name)
	{
		executeInsert("SeriesID", "INSERT INTO Series (Series) VALUES(%s)",  ESC_S(name));
	}

	void addSource(std::string const & name)
	{
		executeInsert("SourceID", "INSERT INTO Sources (Source) VALUES(%s)",  ESC_S(name));
	}

	void addBookCategory(std::string const & name)
	{
		executeInsert("CategoryID", "INSERT INTO BookCategory (Category) VALUES(%s)",  ESC_S(name));
	}
	
	using GenreIds = std::vector<IdValue>;

	void inputGenres(GenreIds& genres, const char* prompt)
	{
		for (size_t i = 0;;) {
			auto gi = (i < genres.size()) ? genres[i] : EmptyId;
			input(gi, prompt, cf(&Litt::selGenre), getListGenre(), optional);
			if (gi == EmptyId) {
				if (i < genres.size()) { genres.erase(genres.begin() + i); }
				if (i < genres.size() || (i == 0)) continue; else break;
			}
			if (genres.size() <= i) { genres.reserve((i + 1) * 2); genres.resize(i + 1); }
			genres[i++] = gi;
		}
	};

	void printGenres(GenreIds const& genres)
	{
		bool first = true;
		for (auto gi : genres) {
			if (!first) printf(", "); first = false;
			printf("%s", S(selGenre(gi)));
		}
	}

	void explainNotSupported()
	{
		if (m_explainQuery != ExQ::None)
			throw std::invalid_argument("-x option is not supported for this action");
	}

	void printTotalChanges()
	{
		if (auto const changes = sqlite3_total_changes(m_conn.get()); changes > 0)
			printf("Added %i rows.\n", changes);
	}

	void addBook()
	{
		explainNotSupported();

		struct AData { IdValue authorId; std::string story; IdValue storyId; std::string storyRating; GenreIds storyGenres; };
		auto st = getLocalTime();
		auto authors     = std::vector<AData>();
		auto title       = std::string();
		auto dateRead    = fmt("%04d-%02d-%02d ", st.wYear, st.wMonth, st.wDay); // extra space to remind adding hh:mm.
		auto sourceId    = EmptyId;
		auto genreIds    = GenreIds();
		auto origtitle   = std::string();
		auto otLangId    = EmptyId;
		auto otIsbn      = std::string();
		auto otDate      = std::string();
		auto rating      = std::string();
		auto isbn        = std::string();
		auto catId       = EmptyId;
		auto pages       = EmptyUnsigned;
		auto words       = EmptyUnsigned;
		auto date        = std::string();
		auto langId      = IdValue{2}; // English
		auto owns        = int('n');
		auto boughtEbook = int('n');
		auto seriesId    = EmptyId;
		auto seriesPart  = std::string();
	enterBook:
		auto nextStoryId = getNextIdValue("StoryID", "Stories");
		bool hasStories = false;
		for (size_t i = 0;;) {
			AData ad = (i < authors.size()) ? authors[i] : AData{};
			input(ad.authorId, "AuthorID", cf(&Litt::selAuthor), getListAuthor(), optional);
			if (ad.authorId == EmptyId) {
				if (i < authors.size()) { authors.erase(authors.begin() + i); }
				if (i < authors.size() || (i == 0 && !title.empty())) continue; else break;
			}
			input(ad.story, "Story name (optional)", optional);
			if (!ad.story.empty()) {
				hasStories = true;
				if (i > 0 && ad.story == authors[i - 1].story && ask("yn", "Same as previous", 'y') == 'y') {
					ad.storyId     = authors[i - 1].storyId;
					ad.storyRating = authors[i - 1].storyRating;
					ad.storyGenres = authors[i - 1].storyGenres;
				}
				else {
					if (getStoryId(ad.storyId, ad.story)) {
						ad.storyRating.clear(); // no rating => existing story, so don't add to Stories or StoryGenres
						// Lookup existing genres so they can be added to the book.
						ad.storyGenres = selectRowIdValues("SELECT GenreID from StoryGenres WHERE StoryID=" + std::to_string(ad.storyId));
					}
					else {
						ad.storyId = nextStoryId++;
						input(ad.storyRating, "Story rating", RatingRegEx);
						inputGenres(ad.storyGenres, "Story GenreID");
					}
				}
			}
			if (authors.size() <= i) { authors.reserve((i+1)*2); authors.resize(i + 1); }
			authors[i++] = std::move(ad);
		}
		if (authors.empty() && title.empty()) {
			return;
		}
		input(title, "Book title");
		input(dateRead, "Date read", DateReadRegEx);
		input(rating, "Rating", RatingRegEx);
		input(sourceId, "Book SourceID", cf(&Litt::selSource), getListSource());
		inputIsbn(isbn, "ISBN");
		input(date, "First publication date", PubDateRegEx);
		input(catId, "Book CategoryID", cf(&Litt::selBookCategory), getListBookCategory());
		input(pages, "Pages");
		input(words, "Words");
		if (hasStories) {
			genreIds.clear();
			for (auto const& ad : authors) for (auto const gi : ad.storyGenres) 
				if (std::find(genreIds.begin(), genreIds.end(), gi) == genreIds.end())
					genreIds.push_back(gi);
		}
		else {
			inputGenres(genreIds, "Book GenreID");
		}
		input(langId, "LangID", cf(&Litt::selLanguage), getListLanguage());
		input(origtitle, "Original title (optional)", optional);
		if (!origtitle.empty()) {
			inputIsbn(otIsbn, "OT ISBN");
			input(otDate, "OT first publication date", PubDateRegEx);
			input(otLangId, "OT LangID", cf(&Litt::selLanguage), getListLanguage());
		}
		askInput("yn", "Own book", owns);
		askInput("yn", "Bought ebook", boughtEbook);
		input(seriesId, "SeriesID (optional)", cf(&Litt::selSeries), getListSeries(), optional);
		if (seriesId != EmptyId) {
			input(seriesPart, "Part in series");
		}

		auto ynStr = [](int yn) { return yn == 'y' ? "yes" : "no"; };
		auto ynInt = [](int yn) { return yn == 'y' ? 1     : 0;    };

		printf("\n");
		bool hasWidth = false; size_t width = 0; again:
		for (auto const& a : authors) {
			auto name = selAuthor(a.authorId);
			width = std::max(width, name.length());
			if (hasWidth) {
				printf("%-4llu - %-*s", a.authorId, width, S(name));
				if (!a.story.empty()) {
					printf("  :  %s [%llu] [Rating=%s]\n", S(a.story), a.storyId, S(a.storyRating));
					printf("        %-*s :  ", width, ""); printGenres(a.storyGenres);
				}
				printf("\n");
			}
		}
		if (!hasWidth) { hasWidth = true; goto again; }
		printf("\n");
		printf("Title          : %s\n", S(title));
		printf("Date read      : %s\n", S(dateRead));
		printf("Rating         : %s\n", S(rating));
		printf("ISBN           : %s\n", S(isbn));
		printf("Date           : %s\n", S(date));
		printf("Category       : %s\n", S(selBookCategory(catId)));
		printf("Pages / Words  : %u / %u\n", pages, words);
		printf("Genre(s)       : ");    printGenres(genreIds); printf("\n");
		printf("Source         : %s\n", S(selSource(sourceId)));
		printf("Language       : %s\n", S(selLanguage(langId)));
		printf("Owned          : %s\n", ynStr(owns));
		printf("Bought e-book  : %s\n", ynStr(boughtEbook));
		if (!origtitle.empty()) {
		printf("Original title : %s\n", S(origtitle));
		printf("OT ISBN        : %s\n", S(otIsbn));
		printf("OT Date        : %s\n", S(otDate));
		printf("OT Language    : %s\n", S(selLanguage(otLangId))); }
		if (seriesId != EmptyId) {
		printf("Series         : Part %s of %s\n", S(seriesPart), S(selSeries(seriesId))); }
		printf("\n");

		switch (ask("yne", "Add book")) {
			case 'y': break;
			case 'n': if (confirm("Discard entered values")) return; else goto enterBook;
			case 'e': default: goto enterBook;
		}
		// Add it!
		auto bid = getNextIdValue("BookID", "Books");

		QueryBuilder qb("BEGIN TRANSACTION;");
		qb.adf("INSERT INTO Books (BookID,Title,LangID,Owned,\"Bought Ebook\",Rating,ISBN,CategoryID,Pages,Words,Date)"
		                          " VALUES(%llu,%s,%llu,%i,%i,%s,%s,%llu,%u,%u,%s);",
			bid, ESC_S(title), langId, ynInt(owns), ynInt(boughtEbook), S(rating), ESC_S(isbn), catId, pages, words, ESC_S(date));
		qb.adf("INSERT INTO DatesRead (BookID,\"Date Read\",SourceID) VALUES(%llu,%s,%llu);",
			bid, ESC_S(dateRead), sourceId);
		for (auto gi : genreIds) {
			qb.adf("INSERT OR IGNORE INTO BookGenres (BookID,GenreID) VALUES(%llu,%llu);", bid, gi);
		}
		for (auto const& a : authors) {
			qb.adf("INSERT OR IGNORE INTO AuthorBooks (BookID,AuthorID) VALUES(%llu,%llu);", bid, a.authorId);
			if (!a.story.empty()) {
				if (!a.storyRating.empty()) {
					qb.adf("INSERT OR IGNORE INTO Stories (StoryID,Story,Rating) VALUES(%llu,%s,%s);",
						a.storyId, ESC_S(a.story), S(a.storyRating));
					for (auto gi : a.storyGenres) {
						qb.adf("INSERT OR IGNORE INTO StoryGenres (StoryID,GenreID) VALUES(%llu,%llu);", a.storyId, gi);
					}
				}
				qb.adf("INSERT INTO BookStories (BookID,AuthorID,StoryID) VALUES(%llu,%llu,%llu);",
					bid, a.authorId, a.storyId);
			}
		}
		if (!origtitle.empty()) {
			qb.adf("INSERT INTO OriginalTitles (BookID,\"Original Title\",LangID,otISBN,otDate) VALUES(%llu,%s,%llu,%s,%s);",
				bid, ESC_S(origtitle), otLangId, ESC_S(otIsbn), ESC_S(otDate));
		}
		if (seriesId != EmptyId) {
			qb.adf("INSERT INTO BookSeries (BookID,SeriesID,\"Part in Series\") VALUES(%llu,%llu,%s);",
				bid, seriesId, S(escSqlVal(seriesPart, true)));
		}
		qb.add("COMMIT TRANSACTION");
		
		try {
			executeSql(qb.m_query);
			printTotalChanges();
		}
		catch (std::exception& ex) {
			printf("Failed to add book: %s\n\nSQL command was:\n\n%s\n\n", ex.what(), S(qb.m_query));
			executeSql("ROLLBACK TRANSACTION");
			if (confirm("Retry")) {
				goto enterBook;
			}
		}
	}

	bool getStoryId(IdValue& storyId, std::string const & story)
	{
		if (hasRowValue(fmt("SELECT 1 FROM Stories WHERE Story=%s", ESC_S(story)))) {
			auto idValid = [&]() { return storyId == EmptyId || hasRowValue(fmt(
				"SELECT 1 FROM Stories WHERE StoryID=%llu AND Story=%s", storyId, ESC_S(story)));
			};
			for (;;) {
				printf("\nStory name already exists, select a storyID to use from the ones listed or leave empty to add new.\n\n");
				resetListingData("st.eq." + story);
				runListData("stid.st.stra.nn.bt", "stid.st", Table::stories);
				printf("\n");
				if (!idValid()) { storyId = EmptyId; }
				input(storyId, "StoryID", nullptr, nullptr, optional);
				if (idValid()) { return storyId != EmptyId; }
			}
		}
		return false;
	}

	void addStory()
	{
		explainNotSupported();

		if (auto bid = bidargi(0)) {
			auto aid = idargi(1, "AuthorID", cf(&Litt::selAuthor), getListAuthor());
			auto story = argi(2, "Story");
			IdValue storyId = EmptyId;
			std::string rating;
			GenreIds genreIds;
			if (!getStoryId(storyId, story)) {
				rating = argi(3, "Rating", RatingRegEx);
				inputGenres(genreIds, "GenreID");
			}
			if (confirmf("Add story '%s' [%llu] with rating='%s' to '%s [%llu]' for author %s [%llu]",
				S(story), storyId, S(rating), S(selTitle(bid)), bid, S(selAuthor(aid)), aid)) {
				QueryBuilder qb("BEGIN TRANSACTION;");
				qb.adf("INSERT OR IGNORE INTO AuthorBooks (BookID,AuthorID) VALUES(%llu,%llu);", bid, aid);
				if (!rating.empty()) {
					storyId = getNextIdValue("StoryID", "Stories");
					qb.adf("INSERT INTO Stories (StoryID,Story,Rating) VALUES(%llu,%s,%s);", storyId, ESC_S(story), S(rating));
					for (auto gi : genreIds) {
						qb.adf("INSERT OR IGNORE INTO StoryGenres (StoryID,GenreID) VALUES(%llu,%llu);", storyId, gi);
						qb.adf("INSERT OR IGNORE INTO BookGenres (BookID,GenreID) VALUES(%llu,%llu);", bid, gi);
					}
				}
				qb.adf("INSERT INTO BookStories (BookID,AuthorID,StoryID) VALUES(%llu,%llu,%llu);", bid, aid, storyId);
				qb.add("COMMIT TRANSACTION");

				try {
					executeSql(qb.m_query);
					printTotalChanges();
				}
				catch (std::exception& ex) {
					printf("Failed to add story: %s\n\nSQL command was:\n\n%s\n\n", ex.what(), S(qb.m_query));
					executeSql("ROLLBACK TRANSACTION");
				}
			}
		}
	}

	void setBookSeries()
	{
		if (auto bookId = bidargi(0)) {
			auto seriesId = idargi(1, "SeriesID", cf(&Litt::selSeries), getListSeries());
			auto part = argi(2, "Part or 'delete' to remove");
			if (part != "delete") {
				if (confirmf("Add '%s [%llu]' to '%s [%llu]' as part %s",
					S(selTitle(bookId)), bookId, S(selSeries(seriesId)), seriesId, S(part))) {
					executeWriteSqlf("INSERT OR REPLACE INTO BookSeries (BookID,SeriesID,\"Part in Series\") VALUES(%llu,%llu,%s)",
						bookId, seriesId, S(escSqlVal(part, true)));
				}
			}
			else {
				if (confirmf("Remove '%s [%llu]' from '%s [%llu]'", S(selTitle(bookId)), bookId, S(selSeries(seriesId)), seriesId)) {
					executeWriteSqlf("DELETE FROM BookSeries WHERE BookID=%llu AND SeriesID=%llu", bookId, seriesId);
				}
			}
		}
	}

	void addBookGenre()
	{
		if (auto bookId = bidargi(0)) {
			auto genreId = gidargi(1);
			if (confirmf("Add '%s' => '%s'", S(selGenre(genreId)), S(selTitle(bookId)))) {
				executeWriteSqlf("INSERT OR IGNORE INTO BookGenres (BookID,GenreID) VALUES (%llu, %llu)", bookId, genreId);
			}
		}
	}

	void addStoryGenre()
	{
		if (auto storyId = stidargi(0)) {
			auto genreId = gidargi(1);
			if (confirmf("Add '%s' => '%s'", S(selGenre(genreId)), S(selStory(storyId)))) {
				executeWriteSqlf("INSERT OR IGNORE INTO StoryGenres (StoryID,GenreID) VALUES (%llu, %llu)", storyId, genreId);
			}
		}
	}

	void setBookGenre()
	{
		if (auto bookId = bidargi(0)) {
			auto genreId = gidargi(1);
			auto checkExists = selectSingleValue(fmt("SELECT GenreID FROM BookGenres WHERE BookID=%llu AND GenreID=%llu", bookId, genreId),
				fmt("GenreID %llu for BookID %llu", genreId, bookId).c_str());
			if (auto newGI = gidargi(2, "New GenreID", optional)) {
				if (confirmf("Change '%s' => '%s' for '%s'", S(selGenre(genreId)), S(selGenre(newGI)), S(selTitle(bookId)))) {
					executeWriteSqlf("UPDATE BookGenres SET GenreID=%llu WHERE BookID=%llu AND GenreID=%llu", newGI, bookId, genreId);
				}
			}
			else {
				if (confirmf("Remove '%s' from '%s'", S(selGenre(genreId)), S(selTitle(bookId)))) {
					executeWriteSqlf("DELETE FROM BookGenres WHERE BookID=%llu AND GenreID=%llu", bookId, genreId);
				}
			}
		}
	}

	void setStoryGenre()
	{
		if (auto storyId = stidargi(0)) {
			auto genreId = gidargi(1);
			auto checkExists = selectSingleValue(fmt("SELECT GenreID FROM StoryGenres WHERE StoryID=%llu AND GenreID=%llu", storyId, genreId),
				fmt("GenreID %llu for StoryID %llu", genreId, storyId).c_str());
			if (auto newGI = gidargi(2, "New GenreID", optional)) {
				if (confirmf("Change '%s' => '%s' for '%s'", S(selGenre(genreId)), S(selGenre(newGI)), S(selStory(storyId)))) {
					executeWriteSqlf("UPDATE StoryGenres SET GenreID=%llu WHERE StoryID=%llu AND GenreID=%llu", newGI, storyId, genreId);
				}
			}
			else {
				if (confirmf("Remove '%s' from '%s'", S(selGenre(genreId)), S(selStory(storyId)))) {
					executeWriteSqlf("DELETE FROM StoryGenres WHERE StoryID=%llu AND GenreID=%llu", storyId, genreId);
				}
			}
		}
	}

	void addDateRead()
	{
		if (auto bi = bidargi(0)) {
			auto dr = argi(1, "Date read", DateReadRegEx);
			auto sid = idargi(2, "SourceID", cf(&Litt::selSource), getListSource());
			if (confirmf("Add date read '%s' with source '%s' to '%s'", S(dr), S(selSource(sid)), S(selTitle(bi)))) {
				executeWriteSqlf("INSERT INTO DatesRead (BookID,\"Date Read\",SourceID) VALUES(%llu,%s,%llu)", bi, ESC_S(dr), sid);
			}
		}
	}

	std::string getDrArg(IdValue bookId, int argIndex)
	{
		auto const drRows = selectRowValues(fmt("SELECT \"Date Read\" FROM DatesRead WHERE BookID=%llu ORDER BY \"Date Read\"", bookId));
		if (drRows.empty())
			throw std::runtime_error(fmt("No date read:s for book %llu", bookId));

		if (!hasArg(argIndex) && drRows.size() == 1) {
			return drRows[0].at(0);
		}
		else {
			auto const drOrIndex = argi(argIndex, "Current date read value or index");
			if (int index; toInt(drOrIndex, index)) {
				if (0 <= index && index < (int)drRows.size()) {
					return drRows[index].at(0);
				}
				else {
					throw std::runtime_error(fmt("No date read found at index %i for book %llu", index, bookId));
				}
			}
			return drOrIndex;
		}
	}

	void setBookDateRead()
	{
		auto const deleteOrDrRegEx = fmt("(delete)|(%s)", DateReadRegEx);

		if (auto const bookId = bidargi(0)) {
			auto const newDr = argi(1, "New date read or 'delete' to remove", deleteOrDrRegEx.c_str());
			auto const dr = getDrArg(bookId, 2);
			if (newDr != "delete") {
				if (confirmf("Change date read '%s' => '%s' for '%s'", S(dr), S(newDr), S(selTitle(bookId)))) {
					executeWriteSqlf("UPDATE DatesRead SET \"Date Read\"=%s WHERE BookID=%llu AND \"Date Read\"=%s",
						ESC_S(newDr), bookId, ESC_S(dr));
				}
			}
			else {
				if (confirmf("Remove date read '%s' from '%s'", S(dr), S(selTitle(bookId)))) {
					executeWriteSqlf("DELETE FROM DatesRead WHERE BookID=%llu AND \"Date Read\"=%s", bookId, ESC_S(dr));
				}
			}
		}
	}

	void setBookSource()
	{
		if (auto const bookId = bidargi(0)) {
			auto const sourceId = soidargi(1);
			auto const dr = getDrArg(bookId, 2);
			if (confirmf("Set source to '%s' for %s of '%s'", S(selSource(sourceId)), S(dr), S(selTitle(bookId)))) {
				executeWriteSqlf("UPDATE DatesRead SET SourceID=%llu WHERE BookID=%llu AND \"Date Read\"=%s", sourceId, bookId, ESC_S(dr));
			}
		}
	}

	void setOriginalTitle()
	{
		if (auto bookId = bidargi(0)) {
			auto originalTitle = argi(1, "Original title or 'delete' to remove");
			if (originalTitle != "delete") {
				auto langId = lidargi(2);
				if (confirmf("Set original title of '%s' => '%s'", S(selTitle(bookId)), S(originalTitle))) {
					executeWriteSqlf(
						"INSERT INTO OriginalTitles (BookID, \"Original Title\", LangID) VALUES (%llu, %s, %llu)"
						" ON CONFLICT(BookID) DO UPDATE SET \"Original Title\"=excluded.\"Original Title\", LangID=excluded.LangID", 
						bookId, ESC_S(originalTitle), langId);
				}
			}
			else {
				if (confirmf("Remove original title of '%s'", S(selTitle(bookId)))) {
					executeWriteSqlf("DELETE FROM OriginalTitles WHERE BookID=%llu", bookId);
				}
			}
		}
	}

	void setRating()
	{
		if (auto bookId = bidargi(0)) {
			auto rating = argi(1, "Rating", RatingRegEx);
			if (confirmf("Set rating of '%s' => %s", S(selTitle(bookId)), S(rating))) {
				executeWriteSqlf("UPDATE Books SET Rating = %s WHERE BookID=%llu", S(rating), bookId);
			}
		}
	}

	void setStoryRating()
	{
		if (auto storyId = stidargi(0)) {
			auto rating = argi(1, "Rating", RatingRegEx);
			if (confirmf("Set rating of '%s' => %s", S(selStory(storyId)), S(rating))) {
				executeWriteSqlf("UPDATE Stories SET Rating = %s WHERE StoryID=%llu", S(rating), storyId);
			}
		}
	}

	void setBookPubDate()
	{
		if (auto bookId = bidargi(0)) {
			auto pubdate = argi(1, "Publication date", PubDateRegEx);
			if (confirmf("Set Date of '%s' => %s", S(selTitle(bookId)), S(pubdate))) {
				executeWriteSqlf("UPDATE Books SET Date = %s WHERE BookID=%llu", ESC_S(pubdate), bookId);
			}
		}
	}

	void setBookOriginalTitlePubDate()
	{
		if (auto bookId = bidargi(0)) {
			auto pubdate = argi(1, "Publication date", PubDateRegEx);
			if (confirmf("Set otDate of '%s' => %s", S(selTitle(bookId)), S(pubdate))) {
				executeWriteSqlf("UPDATE OriginalTitles SET otDate = %s WHERE BookID=%llu", ESC_S(pubdate), bookId);
			}
		}
	}

	void setBookOwned()
	{
		if (auto bookId = bidargi(0)) {
			auto owned = intargi(1, "Owned");
			if (confirmf("Set owned of '%s' => %i", S(selTitle(bookId)), owned)) {
				executeWriteSqlf("UPDATE Books SET Owned = %i WHERE BookID=%llu", owned, bookId);
			}
		}
	}
	
	void executeSimpleAddAction(const char* name, void (Litt::*addMethod)(std::string const&), unsigned argIndex = 0)
	{
		if (auto arg = argi(argIndex, name, optional); !arg.empty()) {
			if (confirmf("Add %s '%s'", name, S(arg))) {
				(this->*addMethod)(arg);
			}
		}
	}

	void executeUserSql()
	{
		if (auto sql = argi(0, "sql", optional); !sql.empty()) {
			if (confirm("Execute SQL")) {
				OutputQuery q(*this); // Note: May not be a pure query, could also be DELETE etc.
				// Set first few columns to width 30, better than letting label and/or first value determine it.
				// (Don't include too many columns, then fitWidth will shrink them!)
				q.columnWidths.assign(5, 30);
				q.initColumnWidths();
				q.init();
				q.a(sql);
				runOutputQuery(q); 
				int changes = sqlite3_changes(m_conn.get());
				if (changes != 0) {
					if (m_rowCount > 0) { printf("\n"); }
					printf("Modified %i rows\n", changes);
				}
			}
		}
	}

	IdValue bidargi(int index, const char* name = "BookID", InputOptions iopt = optional)
	{
		return idargi(index, name, cf(&Litt::selTitle), getListBook(), iopt);
	}

	IdValue stidargi(int index, const char* name = "StoryID", InputOptions iopt = optional)
	{
		return idargi(index, name, cf(&Litt::selStory), getListStory(), iopt);
	}

	IdValue gidargi(int index, const char* name = "GenreID", InputOptions iopt = required)
	{
		return idargi(index, name, cf(&Litt::selGenre), getListGenre(), iopt);
	}

	IdValue lidargi(int index, const char* name = "LangID", InputOptions iopt = required)
	{
		return idargi(index, name, cf(&Litt::selLanguage), getListLanguage(), iopt);
	}

	IdValue soidargi(int index, const char* name = "SourceID", InputOptions iopt = required)
	{
		return idargi(index, name, cf(&Litt::selSource), getListSource(), iopt);
	}

	// Compute the action hash for use in action switch.
	constexpr static unsigned short actionHash(const char* action)
	{
		/* // Commented out, as VS2017 litt.exe is 632 bytes smaller without it, no diff on VS2019.
		// So that <c> and <cc> returns consecutive values to help optimize the jump table.
		// Similar for h and h<1..2>
		if (action[1] == 0)             return 10 * action[0];
		if (action[2] == 0) {
			if (action[0] == action[1]) return 10 * action[0] + 1;
			if (action[0] == 'h')       return 10 * action[0] + (action[1] - ('0' - 1));
		}
		else {
			// Consecutivize <c>bc and <>bcy
			if (action[1] == 'b' && action[2] == 'c') {
				const unsigned short offset = (action[3] == 0) ? 0 : 6; // Assume 'y' if not 0.
				switch (action[0]) {
				case 'a': return 1 + offset;
				case 'g': return 2 + offset;
				case 's': return 3 + offset;
				case 'c': return 4 + offset;
				case 'l': return 5 + offset;
				case 'o': return 6 + offset;
				}
			}
		}*/
		// General case, enough for the above actions as well if we comment that code out.
		unsigned short hash = 0;
		while (*action) {
			hash = 23 * (hash ^ (unsigned short)(*action++));
		}
		return hash;
	}
	
	void executeAction() 
	{
		constexpr auto a = actionHash;
		switch (auto const& action = m_action; a(action.c_str())) {
		case a("h"): case a("h0"): case a("h1"): case a("h2"): showHelp((action.length()!=2) ? 2 : (action[1]-'0')); break;
		case a("a"): case a("aa"):  listAuthors(action, arg(0), arg(1)); break;
		case a("ps"):               listPseudonyms(arg(0), arg(1)); break;
		case a("b"): case a("bb"):  listBooks(action, arg(0)); break;
		case a("s"): case a("ss"):  listSeries(action, arg(0)); break;
		case a("g"): case a("gg"):  listGenres(action, arg(0)); break;
		case a("ot"):               listOriginalTitles(); break;
		case a("st"):case a("stt"): listStories(action, arg(0)); break;
		case a("so"):case a("soo"): listSources(action, arg(0)); break;
		case a("c"): case a("cc"):  listBookCategories(action, arg(0)); break;
		case a("l"): case a("ll"):  listBookLanguages(action, arg(0)); break;
		case a("rereads"):    listRereads(); break;
		case a("reot"):       listReot(); break;
		case a("sametitle"):  listSametitle(); break;
		case a("sameisbn"):   listSameISBN(); break;
		case a("titlestory"): listTitlestory(); break;
		case a("samestory"):  listSamestory(); break;
		case a("abc"): listBookCounts(arg(0), arg(1) == "1", "nn.35", "ai"); break;
		case a("gbc"): listBookCounts(arg(0), arg(1) == "1", "ge.35", "gi"); break;
		case a("sbc"): listBookCounts(arg(0), true, "so.35", "soid"); break; // DR always included when SO is.
		case a("cbc"): listBookCounts(arg(0), arg(1) == "1", "cat", "catid"); break;
		case a("lbc"): listBookCounts(arg(0), arg(1) == "1", "la", "laid"); break;
		case a("obc"): listBookCounts(arg(0), arg(1) == "1", "otla", "otli", Table::originalTitles); break;
		case a("abcy"): case a("gbcy"): case a("sbcy"): case a("cbcy"): case a("lbcy"): case a("obcy"): {
			auto count = intarg(0, "count", 10);
			auto firstYear = intarg(1, "firstYear", getLocalTime().wYear - 4);
			auto lastYear = intarg(2, "lastYear", firstYear + 4);
			auto snSel = "nn"; auto snGby = "ai"; // assume 'a' by default.
			auto startTable = Table::books;
			switch (action[0]) {
				case 'g': snSel = "ge"; snGby = "gi"; break;
				case 's': snSel = "so"; snGby = "soid"; break;
				case 'c': snSel = "cat"; snGby = "catid"; break;
				case 'l': snSel = "la"; snGby = "laid"; break;
				case 'o': snSel = "otla"; snGby = "otli"; startTable = Table::originalTitles; break;
			}
			listYearlyBooksCounts(count, firstYear, lastYear, snSel, snGby, startTable);
			break;
		}
		case a("brd"):  listBooksReadPerDate(arg(0)); break;
		case a("brwd"): listBooksReadPerPeriod("%w", "Weekday", arg(0, WcS), getPeriodColumns(1)); break;
		case a("brm"):  listBooksReadPerPeriod("%Y-%m", "Year-Month", arg(0, WcS), getPeriodColumns(1)); break;
		case a("bry"):  listBooksReadPerPeriod("%Y", "Year", arg(0, WcS), getPeriodColumns(1)); break;
		case a("brp"):  listBooksReadPerPeriod(argm(0,"periodDef"), argm(1,"periodName"), arg(2, WcS), getPeriodColumns(3)); break;
		case a("brym"): {
			const char* months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
			std::vector<PeriodColumn> cols;
			for (int m = 1; m <= 12; ++m) cols.emplace_back(fmt("dr.____-%02d-*", m), months[m - 1]);
			listBooksReadPerPeriod("%Y", "Year", arg(0, WcS), std::move(cols));
			break;
		}
		case a("brmy"): {
			auto st = getLocalTime();
			auto first = intarg(0, "firstYear", st.wYear);
			auto last = intarg(1, "lastYear", st.wYear);
			std::vector<PeriodColumn> cols;
			for (int y = first; y <= last; ++y) cols.emplace_back(fmt("dr.%04d-*", y), std::to_string(y));
			appendToWhereCondition(LogOp_AND, getWhereCondition(fmt("dr.range.%i-01-01.%i-12-31", first, last)));
			listBooksReadPerPeriod("%m", "Month", WcS, std::move(cols));
			break;
		}
		case a("add-a"):   addAuthor(); break;
		case a("add-g"):   executeSimpleAddAction("genre", &Litt::addGenre); break;
		case a("add-s"):   executeSimpleAddAction("series", &Litt::addSeries); break;
		case a("add-so"):  executeSimpleAddAction("book source", &Litt::addSource); break;
		case a("add-c"):   executeSimpleAddAction("book category", &Litt::addBookCategory); break;
		case a("add-b"):   addBook(); break;
		case a("add-st"):  addStory(); break;
		case a("set-s"):   setBookSeries(); break;
		case a("add-bg"):  addBookGenre(); break;
		case a("add-stg"): addStoryGenre(); break;
		case a("set-g"):   setBookGenre(); break;
		case a("set-stg"): setStoryGenre(); break;
		case a("add-dr"):  addDateRead(); break;
		case a("set-dr"):  setBookDateRead(); break;
		case a("set-so"):  setBookSource(); break;
		case a("set-ot"):  setOriginalTitle(); break;
		case a("set-r"):   setRating(); break;
		case a("set-str"): setStoryRating(); break;
		case a("set-bd"):  setBookPubDate(); break;
		case a("set-otd"): setBookOriginalTitlePubDate(); break;
		case a("set-own"): setBookOwned(); break;
		case a("execute"): executeUserSql(); break;
		default:
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
