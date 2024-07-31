##################################
### Data importation functions ###
##################################

#' Check the validity of importation function inputs
#' @param df data frame representing a base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#' @param database an open SQLite database connection
#' @param table_name name of the base or data table represented by df
#' @param check_UIDs Does the table-specific UID column exist in df? (Default=FALSE)
#' @returns
#'  - Ok:
#'      NULL
#'  - Err: dbError
#' @examples
#' list_fnames_tables = fn_simulate_tables(
#'      n_entries=50,
#'      n_dates=3,
#'      n_sites=3,
#'      n_treatments=3,
#'      n_loci=10e3,
#'      save_data_tables=TRUE)$list_fnames_tables
#' df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
#' database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
#' null_error = fn_check_import_inputs(df=df, database=database, table_name="phenotypes", 
#'  check_UIDs=FALSE)
#' non_null_error = fn_check_import_inputs(df=df[, -1:-5], database=database, 
#'  table_name="phenotypes", check_UIDs=FALSE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_check_import_inputs = function(df, database, table_name, check_UIDs=FALSE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(save_data_tables=TRUE)$list_fnames_tables
    # df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "phenotypes"
    # check_UIDs = FALSE
    ################################################################
    ### Are the inputs error types?
    error = NULL
    for (input_name in c("df", "database", "table_name")) {
        input = eval(parse(text=input_name))
        if (methods::is(input, "gpError")) {
            if (is.null(error)) {
                error = input
            } else {
                error = chain(error, input)
            }
        }
    }
    ### Is the database connection valid?
    if (!methods::is(database, "SQLiteConnection")) {
        error_new = methods::new("dbError",
            code=000,
            message=paste0("Error in FUNCTION_NAME(...): broken SQLite connection. Please reconnect via DBI::dbConnect(...)."))
        if (!is.null(error)) {
            error = chain(error, error_new)
        } else {
            error = error_new
        }
    }
    ### Is the input data table empty?
    if (!methods::is(df, "gpError") && ((nrow(df) < 1) || (ncol(df) < 1))) {
        error_new = methods::new("dbError",
            code=000,
            message=paste0("Error in FUNCTION_NAME(...): empty data frame, df."))
        if (!is.null(error)) {
            error = chain(error, error_new)
        } else {
            error = error_new
        }
    }
    ### Is the input data table not one of the three expected base and data tables?
    if (!methods::is(table_name, "gpError") && (sum(GLOBAL_df_valid_tables()$NAME %in% table_name) == 0)) {
        error_new = methods::new("dbError",
            code=000,
            message=paste0("Error in FUNCTION_NAME(...): unrecognised '", 
                table_name, "' table name. Please select from: '", 
                paste(GLOBAL_df_valid_tables()$NAME, collapse="', '"), "'."))
        if (!is.null(error)) {
            error = chain(error, error_new)
        } else {
            error = error_new
        }
    }
    ### Is the input phenotypes or environments table missing at least one required column?
    ### Furthermore, is the phenotypes table missing at least one ENTRY name?
    if (!methods::is(df, "gpError") && ((table_name == "phenotypes") | (table_name == "environments"))) {
        vec_requested_column_names = eval(parse(text=paste0("GLOBAL_list_required_colnames_per_table()$", table_name)))
        vec_idx_missing_required_columns = which(!(vec_requested_column_names %in% toupper(colnames(df))))
        if (length(vec_idx_missing_required_columns) > 0) {
            error_new = methods::new("dbError",
                code=000,
                message=paste0("Error in FUNCTION_NAME(...): the input '", 
                    table_name, "' table is missing the '", 
                    paste(vec_requested_column_names[vec_idx_missing_required_columns], collapse="', '"), 
                    "' column/s."))
            if (!is.null(error)) {
                error = chain(error, error_new)
            } else {
                error = error_new
            }
        } else if (table_name == "phenotypes") {
            idx_col_ENTRY = which(grepl("^ENTRY$", toupper(colnames(df))))
            vec_idx_missing_ENTRY_names = which(is.na(df[, idx_col_ENTRY]))
            if (length(vec_idx_missing_ENTRY_names) > 0) {
                error_new = methods::new("dbError",
                    code=000,
                    message=paste0("Error in FUNCTION_NAME(...): the input '", 
                        table_name, "' table has at least 1 row without an entry name, i.e. at the following rows: ", 
                        paste(vec_idx_missing_ENTRY_names, collapse=", ")))
                if (!is.null(error)) {
                    error = chain(error, error_new)
                } else {
                    error = error_new
                }
            }
        }
    }
    ### Check for UIDs
    if (!methods::is(df, "gpError") && (check_UIDs)) {
        if (sum(grepl("_UID$", colnames(df))) == 0) {
            error_new = methods::new("dbError",
                code=000,
                message=paste0("Error in FUNCTION_NAME(...): the input '", 
                    table_name, "' table is missing the 'UID' column."))
            if (!is.null(error)) {
                error = chain(error, error_new)
            } else {
                error = error_new
            }
        }
    }
    ### Output
    return(error)
}

#' Remove quotes and newline characters ("\\n" in Linux and "\\r\\n" in Windows)
#' @param df data frame representing a base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok:
#'      df: data frame
#'  - Err: dbError
#' @examples
#' list_fnames_tables = fn_simulate_tables(
#'      n_entries=50,
#'      n_dates=3,
#'      n_sites=3,
#'      n_treatments=3,
#'      n_loci=10e3,
#'      save_data_tables=TRUE)$list_fnames_tables
#' df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
#' df[1,1] = paste0("'", df[1,1], "'")
#' df[2,2] = paste0('"', df[2,2], '"')
#' df[3,3] = paste0(df[3,3], "\n")
#' df[4,4] = paste0(df[4,4], "\r\n")
#' df = fn_remove_quotes_and_newline_characters_in_data(df=df, verbose=TRUE)
#' @export
fn_remove_quotes_and_newline_characters_in_data = function(df, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(save_data_tables=TRUE)$list_fnames_tables
    # df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    # verbose = FALSE
    ################################################################
    if ((nrow(df)==0) || (ncol(df)==0)) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_remove_quotes_and_newline_characters_in_data(...): ",
            "The input data frame sis empty."))
        return(error)
    }
    if (verbose) {
        print("Removing single and double quotes from the table:")
        pb = utils::txtProgressBar(min=0, max=ncol(df), style=3)
    }
    if (verbose) {
        vec_n_quotes_per_column = c()
        vec_n_newlines_per_column = c()
    }
    for (j in 1:ncol(df)) {
        # j = 1
        x = df[, j]
        if (!is.numeric(x)) {
            df[, j] = gsub('"', "", gsub("'", "", gsub("\\n", "", gsub("\\r", "", x))))
            if (verbose) {
                vec_n_quotes_per_column = c(vec_n_quotes_per_column, sum(grepl('"', x) | grepl("'", x)))
                vec_n_newlines_per_column = c(vec_n_quotes_per_column, sum(grepl('\n', x) | grepl('\r', x)))
            }
        } else if (verbose) {
            vec_n_quotes_per_column = c(vec_n_quotes_per_column, 0)
        }
        if (verbose) {utils::setTxtProgressBar(pb, j)}
    }
    if (verbose) {
        close(pb)
        print("Distribution of single and double quotes across columns:")
        txtplot::txtdensity(vec_n_quotes_per_column)
    } 
    return(df)
}

#' Add POSIX time column into a data table
#' @param df data frame representing a base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#' @param database an open SQLite database connection
#' @param table_name name of the base or data table represented by df
#' @param bool_add_FVI_year_season Add the corresponding forage value index (FVI) seasons and year? 
#'  (Default=TRUE)
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok:
#'      df: data frame
#'  - Err: dbError
#' @examples
#' list_fnames_tables = fn_simulate_tables(
#'      n_entries=50,
#'      n_dates=3,
#'      n_sites=3,
#'      n_treatments=3,
#'      n_loci=10e3,
#'      save_data_tables=TRUE)$list_fnames_tables
#' df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
#' database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
#' df = fn_add_POSIX_time(df=df, database=database, table_name="phenotypes", 
#'  bool_add_FVI_year_season=TRUE, verbose=TRUE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_add_POSIX_time = function(df, database, table_name, bool_add_FVI_year_season=TRUE, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(save_data_tables=TRUE)$list_fnames_tables
    # df = utils::read.delim(list_fnames_tables$fname_environments, header=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "environments"
    # bool_add_FVI_year_season = TRUE
    # verbose = TRUE
    # # vec_hash_incoming = unlist(apply(df, MARGIN=1, FUN=rlang::hash))
    # # df_test = data.frame(ENVIRONMENT_HASH=vec_hash_incoming[1:10], ENVIRONMENT_UID=1:10, df[1:10, ])
    # # DBI::dbWriteTable(conn=database, name=table_name, value=df_test, overwrite=TRUE)
    ################################################################
    ### Check inputs
    error = fn_check_import_inputs(df=df, database=database, table_name=table_name)
    if (!is.null(error)) {
        error@message = gsub("FUNCTION_NAME", "fn_define_POSIX_time", error@message)
        return(error)
    }
    ### Does POSIX time column already exist in the incoming table?
    if (sum(toupper(colnames(df)) == "POSIX_DATE_TIME") == 1) {
        if (verbose) {
            print(paste0("POSIX_DATE_TIME column already exists in the incoming '", table_name, "' table."))
        }
        return(df)
    }
    ### The year, month, and day columns were already checked in fn_check_import_inputs(...) above, 
    ### now we determine if hour, minutes, and seconds column are also available for defining the POSIX time
    vec_idx_year_month_day_hour = c()
    for (datetime_name in c("YEAR", "MONTH", "DAY", "HOUR")) {
        # datetime_name = c("YEAR", "MONTH", "DAY")[1]
        if (is.null(eval(parse(text=paste0("df$`", toupper(datetime_name), "`"))))) {next}
        eval(parse(text=paste0("df$`", toupper(datetime_name), "` = as.numeric(df$`", toupper(datetime_name), "`)")))
        vec_idx_year_month_day_hour = c(vec_idx_year_month_day_hour, which(toupper(colnames(df)) == datetime_name))
    }
    if (length(vec_idx_year_month_day_hour) == 4) {
        str_hour = as.character(df[1, vec_idx_year_month_day_hour[4]])
        vec_colon_count = sum(grepl(":", unlist(strsplit(str_hour, split=""))))
        if (vec_colon_count < 2) {
            str_hour_suffix = ":00:00"
        } else if (vec_colon_count == 2) {
            str_hour_suffix = ":00"
        } else {
            str_hour_suffix = ""
        }
        df$POSIX_DATE_TIME = unlist(apply(df, MARGIN=1, FUN=function(x){as.numeric(as.POSIXlt(paste0(paste(as.character(x[vec_idx_year_month_day_hour[1:3]]), collapse="-"), " ", as.character(x[vec_idx_year_month_day_hour[4]]), str_hour_suffix)))}))
    } else {
        df$POSIX_DATE_TIME = unlist(apply(df, MARGIN=1, FUN=function(x){as.numeric(as.POSIXlt(paste(as.character(x[vec_idx_year_month_day_hour]), collapse="-")))}))
    }
    vec_POSIX_DATE_TIME = as.POSIXlt(df$POSIX_DATE_TIME)
    if (
        sum(as.numeric(format(vec_POSIX_DATE_TIME, format="%Y")) == df[, vec_idx_year_month_day_hour[1]]) != nrow(df) |
        sum(as.numeric(format(vec_POSIX_DATE_TIME, format="%m")) == df[, vec_idx_year_month_day_hour[2]]) != nrow(df) |
        sum(as.numeric(format(vec_POSIX_DATE_TIME, format="%d")) == df[, vec_idx_year_month_day_hour[3]]) != nrow(df) |
        (length(vec_idx_year_month_day_hour) == 4) && sum(as.numeric(format(vec_POSIX_DATE_TIME, format="%H")) == df[, vec_idx_year_month_day_hour[4]]) != nrow(df)
    ) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_add_POSIX_time(...): ",
                "Error in parsing the date and time. Please check the format of the YEAR, MONTH, DAY and HOUR columns."))
        return(error)
    }
    if (bool_add_FVI_year_season) {
        if (verbose) {
            print("Adding FVI year-season column.")
        }
        df$FVI_YEAR_SEASON = ""
        vec_idx_summer_starting_last_year = which(df$MONTH <= 2)
        vec_idx_autumn = which((df$MONTH >= 3) & (df$MONTH <= 5))
        vec_idx_winter = which((df$MONTH >= 6) & (df$MONTH <= 7))
        vec_idx_early_spring = which((df$MONTH >= 8) & (df$MONTH <= 9))
        vec_idx_late_spring = which((df$MONTH >= 10) & (df$MONTH <= 11))
        vec_idx_summer_ending_this_year = which(df$MONTH == 12)
        df$YEAR[vec_idx_summer_starting_last_year] = df$YEAR[vec_idx_summer_starting_last_year] - 1 ### Adjust the year of the summer which started last to the year before for ease of understanding
        df$FVI_YEAR_SEASON[vec_idx_summer_starting_last_year] = paste0(df$YEAR[vec_idx_summer_starting_last_year], "-SUMMER")
        df$FVI_YEAR_SEASON[vec_idx_autumn] = paste0(df$YEAR[vec_idx_autumn], "-AUTUMN")
        df$FVI_YEAR_SEASON[vec_idx_winter] = paste0(df$YEAR[vec_idx_winter], "-WINTER")
        df$FVI_YEAR_SEASON[vec_idx_early_spring] = paste0(df$YEAR[vec_idx_early_spring], "-EARLY_SPRING")
        df$FVI_YEAR_SEASON[vec_idx_late_spring] = paste0(df$YEAR[vec_idx_late_spring], "-LATE_SPRING")
        df$FVI_YEAR_SEASON[vec_idx_summer_ending_this_year] = paste0(df$YEAR[vec_idx_summer_ending_this_year], "-SUMMER")
    }
    ### Output
    return(df)
}

#' Set all column names into uppercase and remove duplicate column names
#' @param df data frame representing a base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#' @param database an open SQLite database connection
#' @param table_name name of the base or data table represented by df
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok:
#'      df: data frame without quotes and newline characters
#'  - Err: dbError
#' @examples
#' list_fnames_tables = fn_simulate_tables(
#'      n_entries=50,
#'      n_dates=3,
#'      n_sites=3,
#'      n_treatments=3,
#'      n_loci=10e3,
#'      save_data_tables=TRUE)$list_fnames_tables
#' df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
#' database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
#' df = fn_rename_columns_and_remove_duplicate_columns(df=df, database=database, 
#'  table_name="phenotypes", verbose=TRUE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_rename_columns_and_remove_duplicate_columns = function(df, database, table_name, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(save_data_tables=TRUE)$list_fnames_tables
    # df = utils::read.delim(list_fnames_tables$fname_environments, header=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "environments"
    # verbose = TRUE
    ################################################################
    ### Check inputs
    error = fn_check_import_inputs(df=df, database=database, table_name=table_name)
    if (!is.null(error)) {
        error@message = gsub("FUNCTION_NAME", "fn_rename_columns_and_remove_duplicate_columns", error@message)
        return(error)
    }
    ### List incoming column names
    vec_incoming_colnames = colnames(df)
    ### Remove leading and trailing whitespaces
    vec_incoming_colnames = gsub("^ ", "", vec_incoming_colnames)
    vec_incoming_colnames = gsub(" $", "", vec_incoming_colnames)
    ### Convert the symbols below into underscores
    vec_symbols_into_underscores = c(" ", "-", ",", ";", ":", "<", ">", "'", "`", "~", "!", "@", "#", "\"", "\\.", "\\?", "\\$", "\\^", "\\(", ")", "\\[", "]", "\\{", "}")
    for (symbol in vec_symbols_into_underscores) {
        # symbol = vec_symbols_into_underscores[1]
        vec_incoming_colnames = gsub(symbol, "_", vec_incoming_colnames)
    }
    ### Remove duplicate underscores
    for (i in 1:10) {
        vec_incoming_colnames = gsub("__", "_", vec_incoming_colnames)
    }
    ### Remove leading and trailing underscores
    vec_incoming_colnames = gsub("^_", "", vec_incoming_colnames)
    vec_incoming_colnames = gsub("_$", "", vec_incoming_colnames)
    ### Replace plus symbol into 'PLUS'
    vec_incoming_colnames = gsub("\\+", "PLUS", vec_incoming_colnames)
    ### Replace asterisk into 'STAR'
    vec_incoming_colnames = gsub("\\*", "STAR", vec_incoming_colnames)
    ### Replace percent symbol into 'PERCENT'
    vec_incoming_colnames = gsub("%", "PERCENT", vec_incoming_colnames)
    ### Replace ampersand into 'AND'
    vec_incoming_colnames = gsub("&", "AND", vec_incoming_colnames)
    ### Replace forward slashes into 'PER'
    vec_incoming_colnames = gsub("/", "PER", vec_incoming_colnames)
    ### Replace pipes into 'GIVEN'
    vec_incoming_colnames = gsub("\\|", "GIVEN", vec_incoming_colnames)
    ### Set all column names to uppercase as SQLite is case-insensitive
    vec_incoming_colnames = toupper(vec_incoming_colnames)
    ### Rename columns
    colnames(df) = vec_incoming_colnames
    ### Remove duplicated columns, keeping only the first instance
    vec_idx_col_duplicates = duplicated(colnames(df))
    if (verbose) {
        if (sum(vec_idx_col_duplicates) > 0) {
            print("Duplicated columns removed:")
            print(paste0("- ", paste(colnames(df)[vec_idx_col_duplicates], collapse="\n - ")))
            df = df[, !vec_idx_col_duplicates, drop=FALSE]
        } else {
            print("No duplicate columns")
        }
    }
    ### Identify intersection between incoming and existing column names
    vec_incoming_colnames = colnames(df)
    vec_existing_colnames = DBI::dbGetQuery(conn=database, name=table_name, statement=paste0("PRAGMA TABLE_INFO(", table_name, ")"))$name
    vec_common_colnames = vec_incoming_colnames[which(vec_incoming_colnames %in% vec_existing_colnames)]
    if (verbose) {
        if (length(vec_common_colnames) > 0) {
            print(paste0("The following column names are already present in the existing ", table_name, " table:"))
            cat("\t-", paste(vec_common_colnames, collapse="\n\t- "), "\n")
        } else {
            db_name = DBI::dbGetQuery(conn=database, statement="PRAGMA DATABASE_LIST")$file
            if (length(vec_existing_colnames)==0) {
                print(paste0("The ", table_name, " table does not exist yet in the database (", db_name, ")."))
            } else {
                print(paste0("All columns in the incoming ", table_name, " table is already present in the existing table in the database (", db_name, ")."))
            }
        }
    }
    ### Output
    return(df)
}

#' Define a consistent prefix to hash and UID column names for each table type
#' @param table_name name of the base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#' @returns
#'  - Ok:
#'      prefix_of_HASH_and_UID_columns: character string
#'  - Err: dbError
#' @examples
#' prefix_of_HASH_and_UID_columns = fn_define_hash_and_UID_prefix(table_name="phenotypes")
#' @export
fn_define_hash_and_UID_prefix = function(table_name) {
    ################################################################
    ### TEST
    # table_name = "environments"
    ################################################################
    if (sum(grepl(table_name, GLOBAL_df_valid_tables()$NAME)) == 0) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_define_hash_and_UID_prefix(...): ",
                "The table named: ", table_name, " is not a valid base or data table."))
        return(error)
    }
    if (table_name == "loci") {
        prefix_of_HASH_and_UID_columns = "LOCUS"
    } else {
        prefix_of_HASH_and_UID_columns = toupper(gsub("ies$", "y", table_name, ignore.case=TRUE))
        prefix_of_HASH_and_UID_columns = toupper(gsub("s$", "", prefix_of_HASH_and_UID_columns, ignore.case=TRUE))
    }
    return(prefix_of_HASH_and_UID_columns)
}

#' Add hash and UID columns, then remove duplicate rows as defined by the required columns for each base and data table.
#' @param df data frame representing a base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#' @param database an open SQLite database connection
#' @param table_name name of the base or data table represented by df
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok:
#'      df: data frame without quotes and newline characters
#'  - Err: dbError
#' @examples
#' list_fnames_tables = fn_simulate_tables(
#'      n_entries=50,
#'      n_dates=3,
#'      n_sites=3,
#'      n_treatments=3,
#'      n_loci=10e3,
#'      save_data_tables=TRUE)$list_fnames_tables
#' df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
#' database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
#' df = fn_add_hash_UID_and_remove_duplicate_rows(df=df, database=database, 
#'  table_name="phenotypes", verbose=TRUE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_add_hash_UID_and_remove_duplicate_rows = function(df, database, table_name, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(save_data_tables=TRUE)$list_fnames_tables
    # df = utils::read.delim(list_fnames_tables$fname_environments, header=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "environments"
    # verbose = TRUE
    # # vec_hash_incoming = unlist(apply(df, MARGIN=1, FUN=rlang::hash))
    # # df_test = data.frame(ENVIRONMENT_HASH=vec_hash_incoming[1:10], ENVIRONMENT_UID=1:10, df[1:10, ])
    # # DBI::dbWriteTable(conn=database, name=table_name, value=df_test, overwrite=TRUE)
    ################################################################
    ### Check inputs
    error = fn_check_import_inputs(df=df, database=database, table_name=table_name)
    if (!is.null(error)) {
        error@message = gsub("FUNCTION_NAME", "fn_add_hash_UID_and_remove_duplicate_rows", error@message)
        return(error)
    }
    ### Do not add hashes and UIDs on the genotypes table because hashing each row is inefficient.
    ### Instead, we assume that UIDs from the entries and loci tables were used as the ENTRY_UID and column names, respectively.
    if (table_name == "genotypes") {
        return(df)
    }
    ### Check input data frame where we rename and remove duplicate columns and/or remove duplicate columns if needed
    if (sum(colnames(df) == toupper(colnames(df))) != ncol(df)) {
        df = fn_rename_columns_and_remove_duplicate_columns(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(df, "dbError")) {
            error = chain(df, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_add_hash_UID_and_remove_duplicate_rows(...): error renaming and removing duplication columns for incoming '", 
                table_name, "' table.")))
            return(error)
        }
    }
    ### Define the has and UID prefix
    prefix_of_HASH_and_UID_columns = fn_define_hash_and_UID_prefix(table_name=table_name)
    ### Check if we have non-empty hash column
    vec_hash = eval(parse(text=paste0("df$", prefix_of_HASH_and_UID_columns, "_HASH")))
    if (is.null(vec_hash) || (sum(is.na(vec_hash)) > 0)) {
        ### Hash each row of the incoming table using unique identifying columns (excluding the DESCRIPTION column in sites, treatments, traits, and abiotics base tables)
        vec_identifying_columns = eval(parse(text=paste0("GLOBAL_list_required_colnames_per_table()$", table_name)))
        vec_identifying_columns = vec_identifying_columns[!grepl("DESCRIPTION", vec_identifying_columns)]
        if ((table_name == "phenotypes") | (table_name == "environments")) {
            ### Include the additional identifying columns (i.e. date-time-related) to the phenotypes and environments data tables
            vec_identifying_columns = c(vec_identifying_columns, GLOBAL_list_required_colnames_per_table()$additional_IDs)
        }
        vec_idx_identifying_columns = which(colnames(df) %in% vec_identifying_columns)
        vec_hash_incoming = unlist(apply(df[, vec_idx_identifying_columns, drop=FALSE], MARGIN=1, FUN=rlang::hash))
    } else {
        vec_hash_incoming = vec_hash
    }
    ### Remove duplicate rows
    vec_bool_incoming_rows_duplicates = duplicated(vec_hash_incoming)
    if (sum(vec_bool_incoming_rows_duplicates) > 0) {
        if (verbose) {
            print("Duplicate rows in the incoming data.")
            print(paste("Removing duplicate rows at indexes: ", paste(which(vec_bool_incoming_rows_duplicates), collapse=", ")))
            print(paste0("Retaining ", sum(!vec_bool_incoming_rows_duplicates), " rows (original rows = ", nrow(df), ")"))
        }
        df = df[!vec_bool_incoming_rows_duplicates, , drop=FALSE]
        vec_hash_incoming = vec_hash_incoming[!vec_bool_incoming_rows_duplicates]
    }
    ### Compare incoming and existing hashes and remove duplicate rows
    vec_existing_tables = DBI::dbGetQuery(conn=database, statement=paste0("PRAGMA TABLE_LIST"))$name
    if (sum(vec_existing_tables %in% table_name) == 1) {
        vec_hash_existing = DBI::dbGetQuery(conn=database, name=table_name, statement=paste0("SELECT ", prefix_of_HASH_and_UID_columns, "_HASH FROM ", table_name))[, 1]
        vec_bool_incoming_rows_duplicates = vec_hash_incoming %in% vec_hash_existing
        if (table_name == "entries") {
            vec_entry_names_existing = DBI::dbGetQuery(conn=database, name=table_name, statement=paste0("SELECT ENTRY FROM ", table_name))[, 1]
            vec_bool_compare_entry_names = rowSums((df == "UKN") | is.na(df)) == 3
            vec_bool_incoming_rows_duplicates_using_entry_names = df$ENTRY %in% vec_entry_names_existing
            vec_bool_incoming_rows_duplicates[vec_bool_compare_entry_names] = vec_bool_incoming_rows_duplicates_using_entry_names[vec_bool_compare_entry_names]
        }
        if (sum(vec_bool_incoming_rows_duplicates) > 0) {
            if (verbose) {
                print("Some rows in the incoming table are already present in the existing database table.")
                print(paste0("Removing row/s in the incoming table after removing duplicates at indexes: ", paste(which(vec_bool_incoming_rows_duplicates), collapse=", ")))
                print(paste0("Retaining ", sum(!vec_bool_incoming_rows_duplicates), " rows (original rows = ", nrow(df), ")"))
            }
            df = df[!vec_bool_incoming_rows_duplicates, , drop=FALSE]
            vec_hash_incoming = vec_hash_incoming[!vec_bool_incoming_rows_duplicates]
            if ((nrow(df)==0) & (length(vec_hash_incoming)==0)) {
                if (verbose) {
                    print(paste0("No new data in incoming '", table_name, "' table."))
                    print("Returning an empty data frame.")
                }
                return(df)
            }
        }
    }
    ### Add the HASH column as well as the UID column in the incoming table continuing the count from the last UID in the existing table
    if (sum(vec_existing_tables %in% table_name) == 1) {
        UID_last = max(DBI::dbGetQuery(conn=database, name=table_name, statement=paste0("SELECT ", prefix_of_HASH_and_UID_columns, "_UID FROM ", table_name))[,1])
    } else {
        UID_last = 0
    }
    vec_UID = c(1:nrow(df)) + UID_last
    if ((sum(colnames(df) == paste0(prefix_of_HASH_and_UID_columns, "_HASH")) == 0) && (sum(colnames(df) == paste0(prefix_of_HASH_and_UID_columns, "_UID")) == 0)) {
        eval(parse(text=paste0("df = data.frame(", prefix_of_HASH_and_UID_columns, "_HASH = vec_hash_incoming, ", prefix_of_HASH_and_UID_columns, "_UID = vec_UID, df)")))
    }
    if (sum(colnames(df) == paste0(prefix_of_HASH_and_UID_columns, "_HASH")) == 0) {
        paste0("df$", prefix_of_HASH_and_UID_columns, "_HASH = vec_hash_incoming")
    }
    if (sum(colnames(df) == paste0(prefix_of_HASH_and_UID_columns, "_UID")) == 0) {
        paste0("df$", prefix_of_HASH_and_UID_columns, "_UID = vec_UID")
    }
    ### Output
    return(df)
}

#' Extract entries, loci, and genotypes tables from an allele frequency table
#' @param df allele frequency table in data frame format:
#'  read from a tab-delimited file with a header line and the first 3 columns refer to the
#'  chromosome (chr), position (pos), and allele (allele),
#'  with subsequent columns referring to the allele frequencies of a sample, entry or pool.
#'  Names of the samples, entries, or pools in the header line can be any unique string of characters.
#' @param database an open SQLite database connection
#' @param table_name name of the data table, i.e. the "genotypes" table (Default="genotypes")
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok:
#'      $df_genotypes: 2-column genotypes table or data frame. The first column is the ENTRY_UID 
#'          checked for redundancy if an existing "entries" table exist in the database. The second
#'          column is the serialised allele frequencies per entry, i.e. a vector of raw bytes is 
#'          stored per row of the data frame.
#'      $df_loci: "loci" base table or data frame
#'      $df_entries: "entries" base table or data frame
#'  - Err: dbError
#' @examples
#' list_fnames_tables = fn_simulate_tables(
#'      n_entries=50,
#'      n_dates=3,
#'      n_sites=3,
#'      n_treatments=3,
#'      n_loci=10e3,
#'      save_data_tables=TRUE)$list_fnames_tables
#' df = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE)
#' database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
#' list_df_genotypes_df_loci_df_entries = fn_convert_allele_frequency_table_into_blobs_and_dfs(df=df, 
#'  database=database, table_name="genotypes", verbose=TRUE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_convert_allele_frequency_table_into_blobs_and_dfs = function(df, database, table_name="genotypes", verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # df = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "genotypes"
    # verbose = TRUE
    ################################################################
    ### Check inputs
    if (!grepl("chr", colnames(df)[1], ignore.case=TRUE) |
        !grepl("pos", colnames(df)[2], ignore.case=TRUE) |
        !grepl("allele", colnames(df)[3], ignore.case=TRUE)) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_convert_allele_frequency_table_into_blobs_and_dfs(...): ",
            "the input data frame is not in allele frequency table format. Please refer to: ", 
            "https://github.com/jeffersonfparil/imputef?tab=readme-ov-file#allele-frequency-table-tsv."))
        return(error)
    }
    ### Extract loci table
    df_loci = fn_add_hash_UID_and_remove_duplicate_rows(df=data.frame(CHROMOSOME=df[,1], POSITION_PER_CHROMOSOME=df[,2], ALLELE=df[,3]), database=database, table_name="loci", verbose=verbose)
    if (methods::is(df_loci, "dbError")) {
        error = chain(df_loci, methods::new("dbError",
            code=000,
            message=paste0("Error in fn_add_hash_UID_and_remove_duplicate_rows(...): error renaming and removing duplication columns for incoming base '", 
            "loci' table from '", table_name, "' data table.")))
        return(error)
    }
    ### Extract the UID and hashes of the entries if they exist in the database using the entry names alone
    df_entries_tmp = data.frame(ENTRY=colnames(df)[-1:-3], TYPE="UKN", POPULATION="UKN", CULTIVAR="UKN")
    df_entries = fn_add_hash_UID_and_remove_duplicate_rows(df=df_entries_tmp, database=database, table_name="entries", verbose=verbose)
    if (methods::is(df_entries, "dbError")) {
        error = chain(df_entries, methods::new("dbError",
            code=000,
            message=paste0("Error in fn_add_hash_UID_and_remove_duplicate_rows(...): error renaming and removing duplication columns for incoming base '", 
            "entries' table from '", table_name, "' data table.")))
        return(error)
    }
    if (nrow(df_entries) == 0) {
        ### If all entries are already imported into the database previously then we set the output base entries table to NULL
        df_entries = NULL
    }
    ### If the entries table have been initialised then extract those information for the intersecting entries and update the UIDs of the new incoming entries
    vec_existing_tables = DBI::dbGetQuery(conn=database, statement=paste0("PRAGMA TABLE_LIST"))$name
    if (sum(vec_existing_tables == "entries") == 1) {
        df_entries_existing = DBI::dbGetQuery(conn=database, name="entries", statement="SELECT * FROM entries")
        vec_idx_overlap_existing = which(df_entries_existing$ENTRY %in% df_entries_tmp$ENTRY)
        if (is.null(df_entries)) {
            df_entries = df_entries_existing[vec_idx_overlap_existing, , drop=FALSE]
        } else {
            max_UID = max(df_entries_existing$ENTRY_UID)
            df_entries$ENTRY_UID = df_entries$ENTRY_UID + max_UID
            df_entries = rbind(df_entries, df_entries_existing[vec_idx_overlap_existing, , drop=FALSE])
        }
    }
    ### Extract the genotypes table
    df_genotypes = data.frame(ENTRY_UID=df_entries$ENTRY_UID, I(lapply(df[, -1:-3], FUN=function(x){serialize(object=x, connection=NULL)})))
    colnames(df_genotypes) = c("ENTRY_UID", "BLOB")
    if (verbose) {
        print(paste0("Extracted ", ncol(df)-3, " entries x ", nrow(df)-1, " loci genotypes table."))
    }
    ### Output
    return(list(
        df_genotypes=df_genotypes,
        df_loci=df_loci,
        df_entries=df_entries
    ))
}

#' Prepare a data table and extract based tables from it
#' @param df data frame representing a data table, e.g. phenotypes, environments or genotypes table
#' @param database an open SQLite database connection
#' @param table_name name of the base or data table represented by df
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok:
#'      $df_possibly_modified: data table with column names converted to uppercase, hash and UID 
#'          columns added, and duplicate rows and column ommitted
#'      $df_entries: "entries" base table or data frame
#'      $df_dates: "dates" base table or data frame
#'      $df_sites: "sites" base table or data frame
#'      $df_treatments: "treatments" base table or data frame
#'      $df_traits: "traits" base table or data frame
#'      $df_abiotics: "abiotics" base table or data frame
#'      $df_loci: "loci" base table or data frame
#'  - Err: dbError
#' @examples
#' list_fnames_tables = fn_simulate_tables(
#'      n_entries=50,
#'      n_dates=3,
#'      n_sites=3,
#'      n_treatments=3,
#'      n_loci=10e3,
#'      save_data_tables=TRUE)$list_fnames_tables
#' df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
#' database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
#' list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df, 
#'  database=database, table_name="phenotypes", verbose=TRUE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_prepare_data_table_and_extract_base_tables = function(df, database, table_name, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "phenotypes"
    # verbose = TRUE
    ################################################################
    ### Convert allele frequency table into the expected genotypes table
    if (table_name == "genotypes") {
        list_df_genotypes_df_loci_df_entries = fn_convert_allele_frequency_table_into_blobs_and_dfs(df=df, database=database, verbose=verbose)
        df = list_df_genotypes_df_loci_df_entries$df_genotypes
    }
    ### Check inputs
    error = fn_check_import_inputs(df=df, database=database, table_name=table_name)
    if (!is.null(error)) {
        error@message = gsub("FUNCTION_NAME", "fn_prepare_data_table_and_extract_base_tables", error@message)
        return(error)
    }
    ### Filter and add ID columns
    if (table_name != "genotypes") {
        df = fn_rename_columns_and_remove_duplicate_columns(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(df, "dbError")) {
            error = chain(df, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_prepare_data_table_and_extract_base_tables(...): error renaming and removing duplicate columns for incoming '",
                table_name, "' table.")))
            return(error)
        }
        df = fn_add_POSIX_time(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(df, "dbError")) {
            error = chain(df, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_prepare_data_table_and_extract_base_tables(...): error adding POSIX time for incoming '",
                table_name, "' table.")))
            return(error)
        }
        df = fn_add_hash_UID_and_remove_duplicate_rows(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(df, "dbError")) {
            error = chain(df, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_prepare_data_table_and_extract_base_tables(...): error adding UIDs and removing duplicate rows for incoming'",
                table_name, "' table.")))
            return(error)
        }
    }
    ### If we have no new data, then we return an empty data frame and null base tables
    if (nrow(df) == 0) {
        return(NULL)
    }
    ### Instantiate the base tables
    df_entries = NULL
    df_dates = NULL
    df_sites = NULL
    df_treatments = NULL
    df_traits = NULL
    df_abiotics = NULL
    df_loci = NULL
    ### Extract the base tables
    for (i in 1:nrow(GLOBAL_df_valid_tables())) {
        # i = 3
        ### Skip if not a base table
        if (GLOBAL_df_valid_tables()$CLASS[i] != "base") {next}
        ### Skip if the base table does not extract its contents from the current data table
        vec_data_sources = unlist(strsplit(GLOBAL_df_valid_tables()$DATA_SOURCE[i], ","))
        if (sum(vec_data_sources %in% table_name) == 0) {next}
        ### Extract base table information from the data table
        base_table_name = GLOBAL_df_valid_tables()$NAME[i]
        if (GLOBAL_df_valid_tables()$EXTRACT_COLUMN_NAMES_FROM_DATA_TABLE[i]) {
            ### For base tables, the contents of which will be extracted from the column names of the input data table
            if ((base_table_name == "loci") & (table_name == "genotypes")) {
                df_base_table = list_df_genotypes_df_loci_df_entries$df_loci
            } else {
                ### For "traits", and "abiotics" tables
                vec_idx_column_names = which(!(
                    colnames(df) %in% c(
                        GLOBAL_list_required_colnames_per_table()[[table_name]], 
                        GLOBAL_list_required_colnames_per_table()[["dates"]],
                        GLOBAL_list_required_colnames_per_table()[["additional_IDs"]]) |
                    grepl("_HASH$", colnames(df), ignore.case=TRUE) | 
                    grepl("_UID$", colnames(df), ignore.case=TRUE) 
                ))
                vec_column_names = colnames(df)[vec_idx_column_names]
                df_base_table = data.frame(vec_column_names, "")
                colnames(df_base_table) = c(fn_define_hash_and_UID_prefix(base_table_name), "DESCRIPTION")
                df_base_table = fn_rename_columns_and_remove_duplicate_columns(df=df_base_table, database=database, table_name=base_table_name, verbose=verbose)
                if (methods::is(df_base_table, "dbError")) {
                    error = chain(df_base_table, methods::new("dbError",
                        code=000,
                        message=paste0("Error in fn_prepare_data_table_and_extract_base_tables(...): error renaming and removing duplicate columns for incoming base '",
                        base_table_name, "' table from '", table_name, "' data table.")))
                    return(error)
                }
                df_base_table = fn_add_hash_UID_and_remove_duplicate_rows(df=df_base_table, database=database, table_name=base_table_name, verbose=verbose)
                if (methods::is(df_base_table, "dbError")) {
                    error = chain(df_base_table, methods::new("dbError",
                        code=000,
                        message=paste0("Error in fn_prepare_data_table_and_extract_base_tables(...): error adding UIDs and removing duplicate rows for incoming base '",
                        base_table_name, "' table from '", table_name, "' data table.")))
                    return(error)
                }
            }
        } else {
            ### For base tables, the contents of which will be extracted from the data points of the input data table
            if ((base_table_name == "entries") & (table_name == "genotypes")) {
                df_base_table = list_df_genotypes_df_loci_df_entries$df_entries
            } else if ((base_table_name == "loci") & (table_name == "genotypes")) {
                df_base_table = list_df_genotypes_df_loci_df_entries$df_loci
            } else {
                vec_required_colnames = colnames(df)[colnames(df) %in% GLOBAL_list_required_colnames_per_table()[[base_table_name]]]
                if (length(vec_required_colnames) == 0) {next} ### Skip for data tables extracting an inappropriate base tables, e.g df_phenotypes --> df_abiotics
                vec_IDs = eval(parse(text=paste0("unique(paste0(", paste(paste0("df$", vec_required_colnames), collapse=", '\t', "), "))")))
                df_base_table = as.data.frame(matrix(unlist(strsplit(vec_IDs, "\t")), byrow=TRUE, ncol=length(vec_required_colnames)))
                if (GLOBAL_df_valid_tables()$NEEDS_DESCRIPTION[i]) {
                    df_base_table$DESCRIPTION = ""
                    colnames(df_base_table)[1:(ncol(df_base_table)-1)] = vec_required_colnames
                } else {
                    colnames(df_base_table) = vec_required_colnames
                }
                df_base_table = fn_add_hash_UID_and_remove_duplicate_rows(df=df_base_table, database=database, table_name=base_table_name, verbose=verbose)
                if (methods::is(df_base_table, "dbError")) {
                    error = chain(df_base_table, methods::new("dbError",
                        code=000,
                        message=paste0("Error in fn_prepare_data_table_and_extract_base_tables(...): error adding UIDs and removing duplicate rows for incoming base '",
                        base_table_name, "' table from '", table_name, "' data table.")))
                    return(error)
                }
                ### Add base table UIDs into the source data table
                base_table_prefix_of_HASH_and_UID_columns = fn_define_hash_and_UID_prefix(table_name=base_table_name)
                # vec_base_UIDs = df_base_table$`base_table_prefix_of_HASH_and_UID_columns_UID`
                eval(parse(text=paste0("df$`", base_table_prefix_of_HASH_and_UID_columns, "_UID` = NA")))
                vec_idx_data_column_selected = colnames(df) %in% vec_required_colnames
                vec_idx_data_column_selected_order = order(colnames(df)[vec_idx_data_column_selected])
                vec_data_string_IDs = unlist(apply(df, MARGIN=1, FUN=function(x){paste(gsub(" ", "", x[vec_idx_data_column_selected][vec_idx_data_column_selected_order]), collapse="\t")}))
                if (sum(DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST")$name %in% df_base_table) == 0) {
                    df_base_table_all = df_base_table
                } else {
                    df_base_table_all = rbind((DBI::dbGetQuery(conn=database, statement=paste("SELECT * FROM ", base_table_name))), df_base_table)
                }
                for (i in 1:nrow(df_base_table_all)) {
                    # i = 1
                    base_UID = eval(parse(text=paste0("df_base_table_all$`", base_table_prefix_of_HASH_and_UID_columns, "_UID`[i]")))
                    vec_idx_base_column_selected = colnames(df_base_table_all) %in% vec_required_colnames
                    vec_idx_base_column_selected_order = order(colnames(df_base_table_all)[vec_idx_base_column_selected])
                    vec_idx_add_base_UID = which(vec_data_string_IDs %in% paste(gsub(" ", "", df_base_table_all[i, vec_idx_base_column_selected][vec_idx_base_column_selected_order]), collapse="\t"))
                    eval(parse(text=paste0("df$`", base_table_prefix_of_HASH_and_UID_columns, "_UID`[vec_idx_add_base_UID] = base_UID")))
                }
            }
        }
        if (nrow(df_base_table) > 0) {
            assign(paste0("df_", base_table_name), df_base_table)
        }
    }
    ### Output the base tables as well as the input data table which may have been modified/appended 
    ### to extract the base tables and renders it prepared for importation into the database
    return(list(
        df_possibly_modified=df,
        df_entries=df_entries,
        df_dates=df_dates,
        df_sites=df_sites,
        df_treatments=df_treatments,
        df_traits=df_traits,
        df_abiotics=df_abiotics,
        df_loci=df_loci
    ))
}

fn_set_classification_of_rows = function(df, database, table_name, verbose=verbose) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # unlink("test.sqlite")
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "phenotypes"
    # verbose = TRUE
    # df = fn_add_hash_UID_and_remove_duplicate_rows(df=utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE), database=database, table_name=table_name, verbose=verbose)
    # ### Add a subset of the table into the database
    # df_test = fn_prepare_data_table_and_extract_base_tables(df=df[1:floor(nrow(df)/2), , drop=FALSE], database=database, table_name=table_name, verbose=verbose)$df_possibly_modified
    # DBI::dbWriteTable(conn=database, name=table_name, value=df_test, overwrite=TRUE)
    ################################################################
    ### Check that the table exists in the database
    if (sum(DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST")$name %in% table_name) == 0) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_set_classification_of_rows(...): ",
                "the '", table_name, "' table does not exist in the database. ",
                "Please initialise the database first."))
        return(error)
    }
    ### Classify rows by unique IDs
    if (table_name == "entries") {
        ### For entries table we use the entry names to identify unique entries if the type, population and cultivar information are all unknown.
        ### This occurs when the entries table was extracted from a genotypes table.
        vec_idx_rows_of_entries_tables_from_genotypes_table = (rowSums(df[, (colnames(df) %in% GLOBAL_list_required_colnames_per_table()$entries[-1])] == "UKN") == 3)
        ### Identify common entry names between incoming and existing entries tables
        df_existing = DBI::dbGetQuery(conn=database, statement=paste0("SELECT * FROM ", table_name))
        vec_existing_entries = df_existing$ENTRY
        vec_incoming_entries = df$ENTRY
        vec_idx_incoming_info_to_be_replaced_with_existing_info = which(vec_incoming_entries %in% vec_existing_entries)
        ### Replace the contents of the incoming entries table with that of the existing one
        for (i in vec_idx_incoming_info_to_be_replaced_with_existing_info) {
            idx_common_existing = which(vec_existing_entries == vec_incoming_entries[i])
            df[i, ] = df_existing[idx_common_existing, ]
        }
    }
    prefix_of_HASH_and_UID_columns = fn_define_hash_and_UID_prefix(table_name=table_name)
    if (table_name == "genotypes") {
        vec_existing_UIDs = DBI::dbGetQuery(conn=database, statement=paste0("SELECT ENTRY_UID FROM ", table_name))[,1]
        vec_incoming_UIDs = df[[paste0("ENTRY_UID")]]
    } else {
        vec_existing_UIDs = DBI::dbGetQuery(conn=database, statement=paste0("SELECT ", prefix_of_HASH_and_UID_columns, "_UID FROM ", table_name))[,1]
        vec_incoming_UIDs = df[[paste0(prefix_of_HASH_and_UID_columns, "_UID")]]
    }
    vec_bool_rows_in_existing_table_present_in_incoming_table = vec_existing_UIDs %in% vec_incoming_UIDs
    vec_bool_rows_in_incoming_table_present_in_existing_table = vec_incoming_UIDs %in% vec_existing_UIDs
    vec_bool_rows_exclusive_to_existing_table = !vec_bool_rows_in_existing_table_present_in_incoming_table
    vec_bool_rows_exclusive_to_incoming_table = !vec_bool_rows_in_incoming_table_present_in_existing_table
    list_set_classification_of_rows = list(
        n_existing_rows=length(vec_existing_UIDs),
        n_incoming_rows=length(vec_incoming_UIDs),
        n_intersecting_rows=sum(vec_bool_rows_in_existing_table_present_in_incoming_table),
        n_rows_exclusive_to_existing_table=sum(vec_bool_rows_exclusive_to_existing_table),
        n_rows_exclusive_to_incoming_table=sum(vec_bool_rows_exclusive_to_incoming_table),
        vec_bool_rows_exclusive_to_existing_table=vec_bool_rows_exclusive_to_existing_table,
        vec_bool_rows_exclusive_to_incoming_table=vec_bool_rows_exclusive_to_incoming_table
    )
    if (verbose) {
        print(paste0("Total number of rows in the existing '", table_name, "' table is ", list_set_classification_of_rows$n_existing_rows, "."))
        print(paste0("Total number of rows in the incoming '", table_name, "' table is ", list_set_classification_of_rows$n_incoming_rows, "."))
        print(paste0("Number of intersecting rows in the existing and incoming '", table_name, "' table is ", list_set_classification_of_rows$n_intersecting_rows, "."))
        print(paste0("Number of rows exclusive to the existing '", table_name, "' table is ", list_set_classification_of_rows$n_rows_exclusive_to_existing_table, "."))
        print(paste0("Number of rows exclusive to the incoming '", table_name, "' table is ", list_set_classification_of_rows$n_rows_exclusive_to_incoming_table, "."))
    }
    return(list_set_classification_of_rows)
}

fn_set_classification_of_columns = function(df, database, table_name, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # unlink("test.sqlite")
    # df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "phenotypes"
    # verbose = TRUE
    # ### Add a subset of the table into the database
    # df_test = fn_prepare_data_table_and_extract_base_tables(df=df[, c(1:20, seq(21, ncol(df), by=2))], database=database, table_name=table_name, verbose=verbose)$df_possibly_modified
    # DBI::dbWriteTable(conn=database, name=table_name, value=df_test, overwrite=TRUE)
    ################################################################
    ### Check that the table exists in the database
    if (sum(DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST")$name %in% table_name) == 0) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_set_classification_of_columns(...): ",
                "the '", table_name, "' table does not exist in the database. ",
                "Please initialise the database first."))
        return(error)
    }
    ### Classify columns
    vec_existing_colnames = DBI::dbGetQuery(conn=database, statement=sprintf("PRAGMA TABLE_INFO('%s')", table_name))$name
	vec_incoming_colnames = colnames(df)
    vec_bool_columns_in_existing_table_present_in_incoming_table = vec_existing_colnames %in% vec_incoming_colnames
    vec_bool_columns_in_incoming_table_present_in_existing_table = vec_incoming_colnames %in% vec_existing_colnames
    vec_bool_columns_exclusive_to_existing_table = !vec_bool_columns_in_existing_table_present_in_incoming_table
    vec_bool_columns_exclusive_to_incoming_table = !vec_bool_columns_in_incoming_table_present_in_existing_table
    list_set_classification_of_columns = list(
        n_existing_columns=length(vec_existing_colnames),
        n_incoming_columns=length(vec_incoming_colnames),
        n_intersecting_columns=sum(vec_bool_columns_in_existing_table_present_in_incoming_table),
        n_columns_exclusive_to_existing_table=sum(vec_bool_columns_exclusive_to_existing_table),
        n_columns_exclusive_to_incoming_table=sum(vec_bool_columns_exclusive_to_incoming_table),
        vec_bool_columns_exclusive_to_existing_table=vec_bool_columns_exclusive_to_existing_table,
        vec_bool_columns_exclusive_to_incoming_table=vec_bool_columns_exclusive_to_incoming_table
    )
    if (verbose) {
        print(paste0("Total number of columns in the existing '", table_name, "' table is ", list_set_classification_of_columns$n_existing_columns, "."))
        print(paste0("Total number of columns in the incoming '", table_name, "' table is ", list_set_classification_of_columns$n_incoming_columns, "."))
        print(paste0("Number of intersecting columns in the existing and incoming '", table_name, "' table is ", list_set_classification_of_columns$n_intersecting_columns, "."))
        print(paste0("Number of columns exclusive to the existing '", table_name, "' table is ", list_set_classification_of_columns$n_columns_exclusive_to_existing_table, "."))
        print(paste0("Number of columns exclusive to the incoming '", table_name, "' table is ", list_set_classification_of_columns$n_columns_exclusive_to_incoming_table, "."))
    }
    return(list_set_classification_of_columns)
}

fn_add_new_columns = function(df, database, table_name, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # unlink("test.sqlite")
    # df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "phenotypes"
    # verbose = TRUE
    # ### Add a subset of the table into the database
    # df_test = fn_prepare_data_table_and_extract_base_tables(df=df[, c(1:20, seq(21, ncol(df), by=2))], database=database, table_name=table_name, verbose=verbose)$df_possibly_modified
    # DBI::dbWriteTable(conn=database, name=table_name, value=df_test, overwrite=TRUE)
    ################################################################
	### Determine which are the new columns to add
    list_set_classification_of_columns = fn_set_classification_of_columns(df=df, database=database, table_name=table_name, verbose=verbose)
    if (methods::is(list_set_classification_of_columns, "dbError")) {
        error = chain(list_set_classification_of_columns, methods::new("dbError",
            code=000,
            message=paste0("Error in fn_add_new_columns(...): error classifying columns of incoming '", table_name, "' table.")))
        return(error)
    }
	vec_idx_incoming_columns_to_add = which(list_set_classification_of_columns$vec_bool_columns_exclusive_to_incoming_table)
	vec_incoming_colnames = colnames(df)
    if (length(vec_idx_incoming_columns_to_add) == 0) {
        if (verbose) {print("No new columns to add.")}
    } else {
        for (j in vec_idx_incoming_columns_to_add) {
            # j = vec_idx_incoming_columns_to_add[1]
            new_column = vec_incoming_colnames[j]
            if (methods::is(df[,j], "character")) {
                column_type = "TEXT"
            } else {
                column_type = "REAL"
            }
            query = sprintf("ALTER TABLE '%s' ADD '%s' %s",
                table_name,
                new_column,
                column_type
            )
            DBI::dbExecute(conn=database, statement=query)
            # head(DBI::dbGetQuery(conn=database, name=table_name, statement=paste0("SELECT * FROM ", table_name)))
        }
    }
    return(database)
}

### Note that it is easier to:
###     (1) replace the entire table, i.e. when the source table is a complete superset of the destination table, and
###     (2) add new rows
fn_append = function(df, database, table_name, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # unlink("test.sqlite")
    # df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "phenotypes"
    # verbose = TRUE
    # df = fn_prepare_data_table_and_extract_base_tables(df=df, database=database, table_name=table_name, verbose=verbose)$df_possibly_modified
    # ### Add a subset of the table into the database
    # vec_idx_rows = 1:20
    # vec_idx_columns = c(1:20, seq(21, ncol(df), by=2))
    # df_test = fn_prepare_data_table_and_extract_base_tables(df=df[vec_idx_rows, vec_idx_columns], database=database, table_name=table_name, verbose=verbose)$df_possibly_modified
    # DBI::dbWriteTable(conn=database, name=table_name, value=df_test, overwrite=TRUE)
    ################################################################
    ### Check inputs
    error = fn_check_import_inputs(df=df, database=database, table_name=table_name, check_UIDs=TRUE)
    if (!is.null(error)) {
        error@message = gsub("FUNCTION_NAME", "fn_append", error@message)
        return(error)
    }
    ### Find the set classifications among row and columns between existing and incoming tables
    list_set_classification_of_rows = fn_set_classification_of_rows(df=df, database=database, table_name=table_name, verbose=verbose)
    if (methods::is(list_set_classification_of_rows, "dbError")) {
        error = chain(list_set_classification_of_rows, methods::new("dbError",
            code=000,
            message=paste0("Error in fn_append(...): error classifying rows of incoming '", table_name, "' table.")))
        return(error)
    }
    list_set_classification_of_columns = fn_set_classification_of_columns(df=df, database=database, table_name=table_name, verbose=verbose)
    if (methods::is(list_set_classification_of_columns, "dbError")) {
        error = chain(list_set_classification_of_columns, methods::new("dbError",
            code=000,
            message=paste0("Error in fn_append(...): error classifying columns of incoming '", table_name, "' table.")))
        return(error)
    }
    ### If the existing table is the same as the incoming table, then there is nothing for us to do
    if (
        (list_set_classification_of_rows$n_intersecting_rows == list_set_classification_of_rows$n_existing_rows) &&
        (list_set_classification_of_rows$n_intersecting_rows == list_set_classification_of_rows$n_incoming_rows) &&
        (list_set_classification_of_rows$n_rows_exclusive_to_existing_table == 0) &&
        (list_set_classification_of_columns$n_columns_exclusive_to_existing_table == 0)
    ) {
        if (verbose) {print(paste0("The existing and incoming '", table_name, "' tables are identical. No data was uploaded."))}
        return(database)
    }
    ### If the existing table is a proper subset of the incoming table, then we simply overwrite the destination table
    if (
        (list_set_classification_of_rows$n_rows_exclusive_to_existing_table == 0) &&
        (list_set_classification_of_columns$n_columns_exclusive_to_existing_table == 0) &&
        (list_set_classification_of_rows$n_rows_exclusive_to_incoming_table > 0) &&
        (list_set_classification_of_columns$n_columns_exclusive_to_incoming_table > 0)
    ) {
        if (verbose) {
            print(paste0("The existing '", table_name, "' table is a proper subset of the incoming table."))
            print(paste0("Overwiting the existing '", table_name, "' table."))
        }
        DBI::dbWriteTable(conn=database, name=table_name, value=df, overwrite=TRUE)
        return(database)
    }
    ### Add new columns (Note that we need to do this before adding new rows)
    if (list_set_classification_of_rows$n_rows_exclusive_to_incoming_table > 0) {
        database = fn_add_new_columns(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(database, "dbError")) {
            error = chain(database, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_append(...): error adding new columns into incoming '", table_name, "' table.")))
            return(error)
        }
    }
    ### If there are no intersecting rows, then we simply append the incoming table
    if (list_set_classification_of_rows$n_intersecting_rows == 0) {
        if (verbose) {
            print(paste0("There are no row intersections between the existing and incoming '", table_name, "' tables."))
            print(paste0("Appending the incoming '", table_name, "' table."))
        }
        DBI::dbAppendTable(conn=database, name=table_name, value=df[list_set_classification_of_rows$vec_bool_rows_exclusive_to_incoming_table, , drop=FALSE])
        return(database)
    }
    ### If there are intersecting rows and new incoming columns, then we insert the incoming data from the new columns as well as overwrite the data in the intersecting columns
    vec_idx_incoming_intersecting_rows = which(!list_set_classification_of_rows$vec_bool_rows_exclusive_to_incoming_table)
    ### Define the UID prefix
    prefix_of_HASH_and_UID_columns = fn_define_hash_and_UID_prefix(table_name=table_name)
    ### Identify the numeric (REAL) and string (TEXT) columns to insert/overwrite data into
    vec_idx_numerics = which(unlist(lapply(df[1, ], FUN=function(x){is.numeric(x)})))
    vec_idx_strings = which(unlist(lapply(df[1, ], FUN=function(x){!is.numeric(x)})))
    if (verbose) {
        print(paste0("There are row intersections between the existing and incoming '", table_name, "' tables."))
        print(paste0("Replacing the contents of the existing table at the intersecting rows and columns using the data from the incoming '", table_name, "' table."))
    }
    pb = utils::txtProgressBar(min=0, max=length(vec_idx_incoming_intersecting_rows), style=3)
    for (i in vec_idx_incoming_intersecting_rows) {
        # i = 1
        query = paste0(
            "UPDATE ",
            table_name,
            " SET ",
            paste0(paste0(colnames(df)[vec_idx_numerics], "=", df[i, vec_idx_numerics], collapse=", "), ","),
            paste0(paste0(colnames(df)[vec_idx_strings], "='", df[i, vec_idx_strings], collapse="', "), "'"),
            " WHERE ", prefix_of_HASH_and_UID_columns, "_UID=",
            eval(parse(text=paste0("df$", prefix_of_HASH_and_UID_columns, "_UID[i]")))
        )
        DBI::dbExecute(conn=database, statement=query)
        utils::setTxtProgressBar(pb, i)
    }
    close(pb)
    ### Append the rows exclusive to the incoming table
    DBI::dbAppendTable(conn=database, name=table_name, value=df[list_set_classification_of_rows$vec_bool_rows_exclusive_to_incoming_table, , drop=FALSE])
    ### Output the database connection
    return(database)
}

fn_initialise_db = function(fname_db, list_df_data_tables, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # fname_db = "test.sqlite"
    # list_df_data_tables = list(
    #     df_phenotypes = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE),
    #     df_environments = utils::read.delim(list_fnames_tables$fname_environments, header=TRUE),
    #     df_genotypes = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE))
    # verbose = TRUE
    ################################################################
    if (file.exists(fname_db)) {
        unlink(fname_db)
    }
    ### Create and open the connection to the database
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    ### Prepare the data tables and extract the base tables from the data tables
    error = NULL
    for (table_name in GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"]) {
        # table_name = GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"][2]
        df = list_df_data_tables[[paste0("df_", table_name)]]
        ### Remove quotes which will interfere with SQL queries
        df = fn_remove_quotes_and_newline_characters_in_data(df, verbose=FALSE)
        if (!is.null(df)) {
            list_tables = fn_prepare_data_table_and_extract_base_tables(df=df, database=database, table_name=table_name, verbose=verbose)
            if (methods::is(list_tables, "dbError")) {
                if (is.null(error)) {
                    error = chain(list_tables, methods::new("dbError",
                        code=000,
                        message=paste0("Error in fn_initialise_db(...): error preparing '", table_name, "' table.")))
                } else {
                    error = chain(error, 
                        chain(list_tables, methods::new("dbError",
                            code=000,
                            message=paste0("Error in fn_initialise_db(...): error preparing '", table_name, "' table."))))
                }
            } else {
                ### Skip data tables which are entirely redundant as determined by fn_prepare_data_table_and_extract_base_tables(...) above
                if (is.null(list_tables)) {next}
                ### Initialise the data table
                DBI::dbWriteTable(conn=database, name=table_name, value=list_tables$df_possibly_modified, overwrite=TRUE)
                ### Initialise or append the base tables
                vec_df_base_names = names(list_tables)[-1]
                vec_df_base_names = vec_df_base_names[unlist(lapply(vec_df_base_names, FUN=function(x){!is.null((list_tables[[x]]))}))]
                for (df_base_name in vec_df_base_names) {
                    # df_base_name = vec_df_base_names[1]
                    table_name = gsub("df_", "", df_base_name)
                    if (verbose) {
                        print("--------------------------------------------------")
                        print(paste0("Initialise/append '", table_name, "' table"))
                        print("--------------------------------------------------")
                    }
                    df_base = list_tables[[df_base_name]]
                    if (sum(DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST")$name %in% table_name) == 0) {
                        ### Write the table if it has not been initialised yet
                        DBI::dbWriteTable(conn=database, name=table_name, value=df_base)
                    } else {
                        ### Append the table into the existing table in the database
                        database = fn_append(df=df_base, database=database, table_name=table_name, verbose=verbose)
                        if (methods::is(database, "dbError")) {
                            error = chain(database, methods::new("dbError",
                                code=000,
                                message=paste0("Error in fn_initialise_db(...): error appending '", table_name, "' table.")))
                        }
                    }
                }
                if (verbose) {
                    print("Tables list:")
                    print(DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST"))
                }
            }
        }
    }
    if (!is.null(error)) {
        return(error)
    }
    if (verbose) {
        print(DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST"))
    }
    DBI::dbDisconnect(conn=database)
    return(0)
}

fn_update_database = function(fname_db, df, table_name, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # fname_db = "test.sqlite"
    # ### Load data tables for subsampling
    # df_phenotypes = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    # df_environments = utils::read.delim(list_fnames_tables$fname_environments, header=TRUE)
    # ### Note for df_genotypes: make sure to check.names=FALSE to ensure the entry names are consistent across data tables, i.e. entry names here are the column names
    # df_genotypes = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE)
    # verbose = TRUE
    # ### Extract base tables for subsampling
    # unlink(fname_db)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    # list_tables_from_phenotypes = fn_prepare_data_table_and_extract_base_tables(df=df_phenotypes, database=database, table_name="phenotypes", verbose=verbose)
    # list_tables_from_environments = fn_prepare_data_table_and_extract_base_tables(df=df_environments, database=database, table_name="environments", verbose=verbose)
    # list_tables_from_genotypes = fn_prepare_data_table_and_extract_base_tables(df=df_genotypes, database=database, table_name="genotypes", verbose=verbose)
    # DBI::dbDisconnect(conn=database)
    # ### Subsample data tables by random sampling and initialise the database
    # vec_idx_phenotypes = sample(c(1:nrow(df_phenotypes)), size=floor(0.25*nrow(df_phenotypes)))
    # vec_idx_environments = sample(c(1:nrow(df_environments)), size=floor(0.25*nrow(df_environments)))
    # vec_idx_genotypes = sample(c(1:nrow(df_genotypes)), size=floor(0.25*nrow(df_genotypes)))
    # fn_initialise_db(
    #     fname_db="test.sqlite",
    #     df_phenotypes=df_phenotypes[vec_idx_phenotypes, , drop=FALSE],
    #     df_environments=df_environments[vec_idx_environments, , drop=FALSE],
    #     df_genotypes=df_genotypes[vec_idx_genotypes, , drop=FALSE],
    #     verbose=TRUE)
    # ### Test inputs
    # fname_db = "test.sqlite"
    # verbose=TRUE
    # df = df_phenotypes; table_name = "phenotypes"
    # df = df_environments; table_name = "environments"
    # df = df_genotypes; table_name = "genotypes"
    # df = list_tables_from_phenotypes$df_treatments; table_name = "treatments"
    # df = list_tables_from_environments$df_abiotics; table_name = "abiotics"
    # df = list_tables_from_genotypes$df_entries; table_name = "entries"
    ################################################################
    ### Remove quotes which will interfere with SQL queries
    df = fn_remove_quotes_and_newline_characters_in_data(df, verbose=FALSE)
    ### Connect to the database
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    if (GLOBAL_df_valid_tables()$CLASS[GLOBAL_df_valid_tables()$NAME == table_name] == "data") {
        list_tables = fn_prepare_data_table_and_extract_base_tables(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(list_tables, "dbError")) {
            error = chain(list_tables, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_update_database(...): error preparing '", table_name, "' table.")))
            return(error)
        }
        df = list_tables$df_possibly_modified
    } else {
        df = fn_add_hash_UID_and_remove_duplicate_rows(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(df, "dbError")) {
            error = chain(df, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_update_database(...): error adding UIDs and removing duplicate rows in '", table_name, "' table.")))
            return(error)
        }
    }
    if (nrow(df) > 0) {
        database = fn_append(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(database, "dbError")) {
            error = chain(database, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_update_database(...): error appending '", table_name, "' table.")))
            return(error)
        }
        if (GLOBAL_df_valid_tables()$CLASS[GLOBAL_df_valid_tables()$NAME == table_name] == "data") {
            for (i in 1:nrow(GLOBAL_df_valid_tables())) {
                # i = 2
                if (GLOBAL_df_valid_tables()$CLASS[i] != "base") {next}
                base_table_name = GLOBAL_df_valid_tables()$NAME[i]
                df_base_table  = list_tables[[paste0("df_", base_table_name)]]
                if (is.null(df_base_table)) {next}
                if (nrow(df_base_table) > 0) {
                    database = fn_append(df=df_base_table, database=database, table_name=base_table_name, verbose=verbose)
                    if (methods::is(database, "dbError")) {
                        error = chain(database, methods::new("dbError",
                            code=000,
                            message=paste0("Error in fn_update_database(...): error appending '", 
                            base_table_name, "' base table from '", table_name, "' data table.")))
                        return(error)
                    }
                }
            }
        }
    }
    DBI::dbDisconnect(conn=database)
    return(0)
}
