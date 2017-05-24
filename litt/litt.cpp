// litt.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

const char OptDelim = '.';
const char* dgSQL = "(SELECT BookID, group_concat(\"Date read\",', ') AS 'Date(s)' FROM Books INNER JOIN DatesRead USING(BookID) GROUP BY BookID)";
const char* ngSQL = "(SELECT BookID, ltrim(group_concat(\"First Name\"||' '||\"Last Name\",', ')) AS 'Author(s)' FROM Books INNER JOIN AuthorBooks USING(BookID) INNER JOIN Authors USING(AuthorID) GROUP BY BookID)";

enum class DumpMode {
	column,
	csv,
	list,
};

enum class ColumnType {
	text = 0,
	numeric = 1,
};

struct ColumnInfo {
	char const* name;
	int         defWidth;
	ColumnType  type;
	char const* label; // optional
	bool        isGroupAggregate;
	int         width;
};

struct LittState {
	int const  consoleCodePageIn  = GetConsoleCP();
	int const  consoleCodePageOut = GetConsoleOutputCP();
	bool const stdOutIsConsole    = GetFileType(GetStdHandle(STD_OUTPUT_HANDLE)) == FILE_TYPE_CHAR;

	// Maps short name to column info.
	std::map<std::string, ColumnInfo> columnInfos;

	bool     headerOn = true;
	bool     selectDistinct = false;
	bool     showQuery = false;
	bool     explainQuery = false;
	bool     showNumberOfRows = false;
	DumpMode mode     = DumpMode::column;

	std::string dbPath; // Path to LITT db file

	std::vector<ColumnInfo*> orderBy; // Overrides the default action order.
	std::vector<ColumnInfo*> selectedColumns; // Overrides the default action columns.
	std::vector<ColumnInfo*> additionalColumns; // Added to the action or overridden columns.

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

			//
			// Some special-purpose virtual columns, these are not generally usable:
			//	

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
			fprintf(stderr, "Invalid short column name '%s'", sn.c_str());
			return nullptr;
		}
	}

	std::vector<ColumnInfo*> getColumns(std::string const & sns)
	{
		std::vector<ColumnInfo*> res;
		std::stringstream ss(sns);
		std::string sn;
		while (std::getline(ss, sn, OptDelim)) {
			auto column = getColumn(sn); 
			if (column == nullptr) { 
				return std::vector<ColumnInfo*>();
			}
			res.push_back(column);
		}
		return res;
	}
};

struct QueryBuilder {
	QueryBuilder(LittState const & ls) {

	}

};

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

bool parseCommandLine(int argc, char **argv, LittState& ls)
{
	for (int i = 1; i < argc; ++i) {
		if (argv[i][0] == '-' && argv[i][1] != '\0') { // NOTE: We will allow a single '-' to be used as an action argument.
			char const        opt    = argv[i][1];
			std::string const optVal = argv[i][2] != '\0' ? &argv[i][2] : "";
			switch (opt) {
			case 'd':
				if (optVal == "col" || optVal == "column") ls.mode = DumpMode::column;
				else if (optVal == "csv") ls.mode = DumpMode::csv;
				else if (optVal == "list") ls.mode = DumpMode::list;
				else {
					fprintf(stderr, "Invalid dump mode option value '%s'\n", optVal.c_str());
					return false;
				}
				break;
			case 'h':
				if (optVal == "on") ls.headerOn = true;
				else if (optVal == "off") ls.headerOn = false;
				else {
					fprintf(stderr, "Invalid header option value '%s'\n", optVal.c_str());
					return false;
				}
				break;
			case 'o': 
				ls.orderBy = ls.getColumns(optVal);
				if (ls.orderBy.empty()) { return false; }
				break;
			case 'c': 
				ls.selectedColumns = ls.getColumns(optVal);
				if (ls.selectedColumns.empty()) { return false; }
				break;
			case 'a': {
				auto add = ls.getColumns(optVal);
				if (add.empty()) { return false; }
				ls.additionalColumns.insert(ls.additionalColumns.end(), add.begin(), add.end());
				}
				break;
			case 'w': 
				// add to where condition
				break;
			case 's': {
				std::stringstream ss(optVal);
				std::string sn;
				while (std::getline(ss, sn, OptDelim)) {
					auto column = ls.getColumn(sn); 
					if (column == nullptr) { return false; }
					int width = 0; std::string w;
					if (std::getline(ss, w, OptDelim) && toInt(w, width) && width > 0) {
						column->width = width;
					}
					else {
						fprintf(stderr, "Invalid column width '%s' for '%s'", w.c_str(), sn.c_str());
						return false;
					}
				}
				}
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
			default:
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
		const char* defDbName = "litt.sqlite";
		char mydocs[MAX_PATH];
		if (GetEnvironmentVariableA("MYDOCS", mydocs, MAX_PATH)) {
			ls.dbPath = std::string(mydocs) + "\\litt\\" + defDbName;
		}
		else {
			ls.dbPath = defDbName;
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
    -d[dumpMode]    (Determines how the results are displayed)
    -h[on|off]      (Determines if a header row is shown or not)
    -c[selColumns]  (Determines the included colums, overrides default columns for the action)
    -a[addColumns]  (Include these columns in addition to the default ones for the action)
    -o[colOrder]    (Determines sort order for results)
    -w[whereCond]   (Adds a WHERE condition - will be AND:ed with the one specified by the action and arguments.
                     If several -w options are included their values will be OR:ed.)
    -s[colSizes]    (Override the default column sizes)
    -q              (Debug - dumps the SQLITE commands instead of producing results.)
    -u              (Makes sure the results only contain UNIQUE/DISTICT values)
    
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

Dumpmode values:
    csv      Comma-separated values
    column   Left-aligned columns. (specified (or default) widths are used)
    html     HTML <table> code
    line     One value per line
    insert   SQL insert statements for TABLE
    list     Values delimited by .separator string
    tabs     Tab-separated values
    tcl      TCL list elements

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

void configureOutputSettingsIfConsole(LittState const & ls)
{
	if (ls.stdOutIsConsole) {
		SetConsoleOutputCP(CP_UTF8); // Note: will break if the bytes of the character are not all written at once, this may happen if split over output buffers.
		static char buf[1000*10]; // OBS! Setting to too large or too small values will break CP_UTF8 above it seems! The WriteConsoleW limit?
		setvbuf(stdout, buf, _IOFBF, sizeof buf); // line buffer not supported on win32
	}
}

void restoreOutputSettingsIfConsole(LittState const & ls)
{
	if (ls.stdOutIsConsole) {
		SetConsoleOutputCP(ls.consoleCodePageOut);
	}
}

int runSelectQuery(LittState const & ls, std::string const & sql)
{
	if (ls.showQuery) {
		// !!! also print relevant options?
		printf("%s", sql.c_str());
		return 0;
	}

	auto callback = [](void *pArg, int argc, char **argv, char **azColName) {
		auto ls = const_cast<LittState const*>(static_cast<LittState*>(pArg));
		if (ls->rowCount++ == 0) {
			for (int i = 0; i < argc; i++) {
				printf("%s", azColName[i]);
				if (i + 1 != argc) putchar('|');
			}
			printf("\n");
		}
		for (int i = 0; i < argc; i++) {
			printf("%s", argv[i] ? argv[i] : "");
			if (i + 1 != argc) putchar('|');
		}
		printf("\n");
		return 0;
	};

	sqlite3 *db = nullptr;
	int res = sqlite3_open(ls.dbPath.c_str(), &db);
	if (res != SQLITE_OK) {
		fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
		goto out;
	}

	configureOutputSettingsIfConsole(ls);

	// !!! convert sql to utf-8 if not already

	ls.rowCount = 0;
	char *zErrMsg = nullptr;
	res = sqlite3_exec(db, sql.c_str(), callback, const_cast<LittState*>(&ls), &zErrMsg);
	if (res != SQLITE_OK) {
		fprintf(stderr, "SQL error: %s\n", zErrMsg);
		sqlite3_free(zErrMsg);
	}

	if (ls.showNumberOfRows) {
		printf("# = %i\n", ls.rowCount);
	}

	restoreOutputSettingsIfConsole(ls); 
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
	// addMainWhereCond %ln% ln & addMainWhereCond %fn% fn
	if (ls.action == "a") {
		return runSingleTableOutputCmd(ls, "ai.ln.fn", "Authors", "ai");
	}
	else {
		return runListData(ls, "bi.ln.fn.bt.dr.so.ge", "ln.fn.dr.bi");
	}
	return 1;
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
