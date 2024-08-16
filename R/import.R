##################################
### Data importation functions ###
##################################

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

#' Check the validity of importation function inputs
#' @param df data frame representing a base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#' @param database an open SQLite database connection
#' @param table_name name of the base or data table represented by df
#' @param check_UIDs Does the table-specific UID column exist in df? (Default=FALSE)
#' @param check_if_table_exists Does the table exist in the database? (Default=FALSE)
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
#'      table_name="phenotypes", check_UIDs=FALSE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_check_import_inputs = function(df, database, table_name, check_UIDs=FALSE, check_if_table_exists=FALSE) {
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
    for (input_name in c("df", "database")) {
        input = eval(parse(text=input_name))
        if (methods::is(input, "dbError")) {
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
    if (!methods::is(df, "dbError") && ((nrow(df) < 1) || (ncol(df) < 1))) {
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
    if (!methods::is(table_name, "dbError") && (sum(GLOBAL_df_valid_tables()$NAME %in% table_name) == 0)) {
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
    if (!methods::is(df, "dbError") && ((table_name == "phenotypes") | (table_name == "environments"))) {
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
    ### Does the UID column exist in the incoming table?
    if (!methods::is(df, "dbError") && (check_UIDs)) {
        prefix_of_HASH_and_UID_columns = fn_define_hash_and_UID_prefix(table_name=table_name)
        if (table_name == "genotypes") {
            prefix_of_HASH_and_UID_columns = "ENTRY"
        }
        if (sum(grepl(paste0(prefix_of_HASH_and_UID_columns, "_UID$"), colnames(df))) == 0) {
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
    ### Does the table exist in the database?
    if (!methods::is(df, "dbError") && (check_if_table_exists)) {
        if (sum(DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST")$name %in% table_name) == 0) {
            error_new = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_set_classification_of_rows(...): ",
                    "the '", table_name, "' table does not exist in the database. ",
                    "Please initialise the table first."))
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
        ### Rename column names
        colnames(df)[j] = gsub('"', "", gsub("'", "", gsub("\\n", "", gsub("\\r", "", colnames(df)[j]))))
        ### Rename non-numeric/string columns
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
#'      bool_add_FVI_year_season=TRUE, verbose=TRUE)
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
    } else {
        if (verbose) {
            print(paste0("Adding POSIX_DATE_TIME column in the incoming '", table_name, "' table."))
        }
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
        vec_YEAR = df$YEAR
        vec_YEAR[vec_idx_summer_starting_last_year] = vec_YEAR[vec_idx_summer_starting_last_year] - 1 ### Adjust the year of the summer which started last to the year before for ease of understanding
        df$FVI_YEAR_SEASON[vec_idx_summer_starting_last_year] = paste0(vec_YEAR[vec_idx_summer_starting_last_year], "-SUMMER")
        df$FVI_YEAR_SEASON[vec_idx_autumn] = paste0(vec_YEAR[vec_idx_autumn], "-AUTUMN")
        df$FVI_YEAR_SEASON[vec_idx_winter] = paste0(vec_YEAR[vec_idx_winter], "-WINTER")
        df$FVI_YEAR_SEASON[vec_idx_early_spring] = paste0(vec_YEAR[vec_idx_early_spring], "-EARLY_SPRING")
        df$FVI_YEAR_SEASON[vec_idx_late_spring] = paste0(vec_YEAR[vec_idx_late_spring], "-LATE_SPRING")
        df$FVI_YEAR_SEASON[vec_idx_summer_ending_this_year] = paste0(vec_YEAR[vec_idx_summer_ending_this_year], "-SUMMER")
    }
    ### Output
    return(df)
}

#' Set all column names into uppercase, remove duplicate column names, and 
#' check for mismatches in column types between existing and incoming tables
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
#' df = fn_rename_columns_remove_duplicate_columns_and_check_column_type_mismatch(df=df, 
#'      database=database, table_name="phenotypes", verbose=TRUE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_rename_columns_remove_duplicate_columns_and_check_column_type_mismatch = function(
    df, database, table_name, verbose=TRUE
) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # df = utils::read.delim(list_fnames_tables$fname_environments, header=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "environments"
    # verbose = TRUE
    # # ### Test column type mismatch
    # # list_fnames_tables = fn_simulate_tables(
    # #     n_entries=50,
    # #     n_dates=3,
    # #     n_sites=3,
    # #     n_treatments=3,
    # #     n_loci=10e3,
    # #     save_data_tables=TRUE)$list_fnames_tables
    # # df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    # # ### Remove quotes and newline characters in the data frame
    # # df = fn_remove_quotes_and_newline_characters_in_data(df=df)
    # # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # # ### Prepare the tables
    # # list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    # # df = list_df_data_and_base_tables$df_possibly_modified
    # # n = nrow(df); p = ncol(df)
    # # df_q1 = droplevels(df[1:floor(n/2),     1:floor(p/2)])
    # # df_q4 = droplevels(df[(floor(n/2)+1):n, 1:floor(p/2)])
    # # ### Initialise with df_q1
    # # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # # DBI::dbWriteTable(conn=database, name="phenotypes", value=df_q1)
    # # ### In df_q4, convert a value in a numeric column into a string to simulate error in the input table
    # # vec_idx = c()
    # # for (j in 1:ncol(df_q4)) {
    # #     if (is.numeric(df_q4[, j])) {
    # #         vec_idx = c(vec_idx, j)
    # #     }
    # # }
    # # df_q4[1, vec_idx[1]] = "Oooppsiieee!"
    # # #### Input
    # # df = df_q4
    # # database = database
    # # table_name = "phenotypes"
    # # verbose = TRUE
    ################################################################
    ### Check inputs
    error = fn_check_import_inputs(df=df, database=database, table_name=table_name)
    if (!is.null(error)) {
        error@message = gsub("FUNCTION_NAME", "fn_rename_columns_remove_duplicate_columns_and_check_column_type_mismatch", error@message)
        return(error)
    }
    if (verbose) {
        print(paste0("Renaming columns and removing duplicate columns in the incoming '", table_name, "' table."))
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
    ### Identify intersection between existing and incoming column names
    vec_incoming_column_names = colnames(df)
    df_existing_table_info = DBI::dbGetQuery(conn=database, statement=paste0("PRAGMA TABLE_INFO(", table_name, ")"))
    vec_existing_column_names = df_existing_table_info$name
    vec_common_column_names = vec_incoming_column_names[which(vec_incoming_column_names %in% vec_existing_column_names)]
    if (verbose) {
        if (length(vec_common_column_names) > 0) {
            print(paste0("The following column names are already present in the existing ", table_name, " table:"))
            cat("\t-", paste(vec_common_column_names, collapse="\n\t- "), "\n")
        } else {
            db_name = DBI::dbGetQuery(conn=database, statement="PRAGMA DATABASE_LIST")$file[1]
            if (length(vec_existing_column_names)==0) {
                print(paste0("The ", table_name, " table does not exist yet in the database (", db_name, ")."))
            } else {
                print(paste0("All columns in the incoming ", table_name, " table is already present in the existing table in the database (", db_name, ")."))
            }
        }
    }
    ### Check for column type mismatch between existing and incoming tables
    error = NULL
    for (column_name in vec_common_column_names) {
        # column_name = vec_common_column_names[2]
        existing_type = df_existing_table_info$type[df_existing_table_info$name == column_name]
        incoming_type = class(eval(parse(text=paste0("df$`", column_name, "`"))))
        if (sum(grepl(existing_type, c("REAL", "INTEGER", "NUMERIC"))) > 0) {
            if (sum(grepl(incoming_type, c("numeric", "integer", "complex", "logical"))) == 0) {
                error_new = methods::new("dbError",
                    code=000,
                    message=paste0("Error in fn_rename_columns_remove_duplicate_columns_and_check_column_type_mismatch(...): ", 
                    "the column ", column_name, " is expected to be numeric. ", 
                    "Please check the incoming ", table_name, " table."))
                if (is.null(error)) {error = error_new} else {error = chain(error, error_new)}
            }
        } else {
            if (sum(grepl(incoming_type, c("character"))) == 0) {
                error_new = methods::new("dbError",
                    code=000,
                    message=paste0("Error in fn_rename_columns_remove_duplicate_columns_and_check_column_type_mismatch(...): ", 
                    "the column ", column_name, " is expected to be non-numeric (string/text). ", 
                    "Please check the incoming ", table_name, " table."))
                if (is.null(error)) {error = error_new} else {error = chain(error, error_new)}
            }
        }
    }
    if (!is.null(error)) {
        return(error)
    }
    ### Output
    return(df)
}

#' Add hash and UID columns, then remove duplicate rows as defined by the required columns for each base and data table.
#' @param df data frame representing a base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#' @param database an open SQLite database connection
#' @param table_name name of the base or data table represented by df
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok:
#'      $df: data frame without quotes and newline characters
#'      $df_duplicated_in_database: data frame without quotes and newline characters with UID duplicates in the database
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
#'      table_name="phenotypes", verbose=TRUE)$df
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_add_hash_UID_and_remove_duplicate_rows = function(df, database, table_name, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(save_data_tables=TRUE)$list_fnames_tables
    # df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "phenotypes"
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
    if (verbose) {
        print(paste0("Adding hash and UID columns and removing duplicate rows in the incoming '", table_name, "' table."))
    }
    ### Do not add hashes and UIDs on the genotypes table because hashing each row is inefficient.
    ### Instead, we assume that UIDs from the entries and loci tables were used as the ENTRY_UID and column names, respectively.
    if (table_name == "genotypes") {
        if (verbose) {
            print("The incoming table is a 'genotypes' data table which does not need hashing and UIDs because row names are already UIDs.")
        }
        return(list(
            df=df,
            df_duplicated_in_database=df[rep(FALSE, times=nrow(df)), , drop=FALSE]
        ))
    }
    ### Check input data frame where we rename and remove duplicate columns and/or remove duplicate columns if needed
    if (sum(colnames(df) == toupper(colnames(df))) != ncol(df)) {
        df = fn_rename_columns_remove_duplicate_columns_and_check_column_type_mismatch(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(df, "dbError")) {
            error = chain(df, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_add_hash_UID_and_remove_duplicate_rows(...): error renaming and removing duplication columns for incoming '", 
                table_name, "' table.")))
            return(error)
        }
    }
    ### Define the hash and UID prefix
    prefix_of_HASH_and_UID_columns = fn_define_hash_and_UID_prefix(table_name=table_name)
    ### Hash each row of the incoming table using unique identifying columns (excluding the DESCRIPTION column in sites, treatments, traits, and abiotics base tables)
    vec_identifying_columns = eval(parse(text=paste0("GLOBAL_list_required_colnames_per_table()$", table_name)))
    vec_identifying_columns = vec_identifying_columns[!grepl("DESCRIPTION", vec_identifying_columns)]
    if ((table_name == "phenotypes") | (table_name == "environments")) {
        ### Include the additional identifying columns (i.e. date-time-related) to the phenotypes and environments data tables
        vec_identifying_columns = c(vec_identifying_columns, GLOBAL_list_required_colnames_per_table()$additional_IDs)
    }
    vec_idx_identifying_columns = which(colnames(df) %in% vec_identifying_columns)
    vec_hash_incoming = unlist(apply(df[, vec_idx_identifying_columns, drop=FALSE], MARGIN=1, FUN=rlang::hash))
    ### Define the HASH column
    eval(parse(text=paste0("df$", prefix_of_HASH_and_UID_columns, "_HASH = vec_hash_incoming")))
    ### Remove duplicate rows from the incoming table
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
    ### Define the UID column temporarily after removing duplicates in the incoming table.
    ### This will be modified if there are duplicates in the existing database table.
    eval(parse(text=paste0("df$", prefix_of_HASH_and_UID_columns, "_UID = 1:nrow(df)")))
    ### Identify rows in the incoming table which have duplicates in the existing database table
    vec_existing_tables = DBI::dbGetQuery(conn=database, statement=paste0("PRAGMA TABLE_LIST"))$name
    if (sum(vec_existing_tables %in% table_name) == 1) {
        ### Check if hash column exists
        vec_existing_column_names = DBI::dbGetQuery(conn=database, statement=paste0("PRAGMA TABLE_INFO(", table_name, ")"))$name
        if (sum(vec_existing_column_names %in% paste0(prefix_of_HASH_and_UID_columns, "_HASH")) == 0) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_add_hash_UID_and_remove_duplicate_rows(...): ", 
                "Missing ", prefix_of_HASH_and_UID_columns, "_HASH column in the existing database. ",
                "Please reset the ", table_name, " table, e.g. remove and reload where hash and UID columns ", 
                "were added prior to writing tables into the database."))
            return(error)
        }
        ### Extract the existing has and UIDs
        vec_hash_existing = DBI::dbGetQuery(conn=database, statement=paste0("SELECT ", prefix_of_HASH_and_UID_columns, "_HASH FROM ", table_name))[, 1]
        vec_UID_existing = DBI::dbGetQuery(conn=database, statement=paste0("SELECT ", prefix_of_HASH_and_UID_columns, "_UID FROM ", table_name))[, 1]
        ### Identify the duplicate rows by hashes
        vec_bool_incoming_rows_duplicates = vec_hash_incoming %in% vec_hash_existing
        vec_bool_existing_rows_duplicates = vec_hash_existing %in% vec_hash_incoming
    } else {
        ### No duplicates if the table does not exist in the database yet
        vec_UID_existing = 0
        vec_bool_incoming_rows_duplicates = rep(FALSE, times=nrow(df))
        vec_bool_existing_rows_duplicates = NULL
    }
    ### Divide the table into rows without duplicates in the existing database table and those which do
    df_duplicated_in_database = df[vec_bool_incoming_rows_duplicates, , drop=FALSE]
    df = df[!vec_bool_incoming_rows_duplicates, , drop=FALSE]
    ### Update the UID column of the duplicate rows, only if there are duplicate rows
    if (nrow(df_duplicated_in_database) > 0) {
        for (hash in eval(parse(text=paste0("df_duplicated_in_database$", prefix_of_HASH_and_UID_columns, "_HASH")))) {
            idx_incoming = which(eval(parse(text=paste0("df_duplicated_in_database$", prefix_of_HASH_and_UID_columns, "_HASH"))) == hash)
            idx_existing = which(vec_hash_existing == hash)
            eval(parse(text=paste0("df_duplicated_in_database$", prefix_of_HASH_and_UID_columns, "_UID[idx_incoming] = vec_UID_existing[idx_existing]")))
        }
    }
    ### Continue the UID from the highest UID in the exiting database table
    if (nrow(df) > 0) {
        UID_max = max(vec_UID_existing)
        eval(parse(text=paste0("df$", prefix_of_HASH_and_UID_columns, "_UID = (UID_max+1):(UID_max+nrow(df))")))
    }
    ### Output
    return(list(
        df=df,
        df_duplicated_in_database=df_duplicated_in_database
    ))
}

#' Extract entries, loci, and genotypes tables from an allele frequency table
#' @param df_allele_frequency_table allele frequency table in data frame format:
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
#' df_allele_frequency_table = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE)
#' database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
#' list_df_genotypes_df_loci_df_entries = fn_convert_allele_frequency_table_into_blobs_and_dfs(
#'      df_allele_frequency_table=df_allele_frequency_table, 
#'      database=database, table_name="genotypes", verbose=TRUE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_convert_allele_frequency_table_into_blobs_and_dfs = function(
    df_allele_frequency_table, database, table_name="genotypes", verbose=TRUE
) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # df_allele_frequency_table = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # table_name = "genotypes"
    # verbose = TRUE
    ################################################################
    ### Check inputs
    if (!grepl("chr", colnames(df_allele_frequency_table)[1], ignore.case=TRUE) |
        !grepl("pos", colnames(df_allele_frequency_table)[2], ignore.case=TRUE) |
        !grepl("allele", colnames(df_allele_frequency_table)[3], ignore.case=TRUE)) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_convert_allele_frequency_table_into_blobs_and_dfs(...): ",
            "the input data frame is not in allele frequency table format. Please refer to: ", 
            "https://github.com/jeffersonfparil/imputef?tab=readme-ov-file#allele-frequency-table-tsv."))
        return(error)
    }
    if (verbose) {
        print(paste0("Converting an allele frequency table (tsv format) into a proper '", table_name, "' table with 2 column. ",
        "The first column being entry UIDs, and the second being a BLOBs consisting of serialised (raw bytes) allele frequencies."))
    }
    ### Extract loci table
    list_df_loci = fn_add_hash_UID_and_remove_duplicate_rows(
        df=data.frame(CHROMOSOME=df_allele_frequency_table[,1], POSITION_PER_CHROMOSOME=df_allele_frequency_table[,2], ALLELE=df_allele_frequency_table[,3]), 
        database=database, table_name="loci", verbose=verbose)
    if (methods::is(list_df_loci, "dbError")) {
        error = chain(list_df_loci, methods::new("dbError",
            code=000,
            message=paste0("Error in fn_convert_allele_frequency_table_into_blobs_and_dfs(...): error renaming and removing duplication columns for incoming base '", 
            "loci' table from '", table_name, "' data table.")))
        return(error)
    }
    ### Return the complete loci table if the loci table has not been initialised
    ### or an empty loci table if the loci has already been initialised.
    ### This is because we do not expect new loci to be added from new genotype data, i.e.
    ### we expect the same set of loci to be appended into the genotypes table.
    if (nrow(list_df_loci$df) * nrow(list_df_loci$df_duplicated_in_database) > 0) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_convert_allele_frequency_table_into_blobs_and_dfs(...): ",
            "Mismatch in the incoming loci and existing loci in the genotypes data. Please check the genotype data."))
        return(error)
    } else {
        ### Return all the loci information with hash and UID columns
        df_loci = rbind(list_df_loci$df, list_df_loci$df_duplicated_in_database)
    }
    ### Using the incomplete information from the genotypes table we define hashes and UIDs of the entries table
    df_entries = data.frame(ENTRY=colnames(df_allele_frequency_table)[-1:-3], TYPE="UKN", POPULATION="UKN", CULTIVAR="UKN")
    list_df_entries = fn_add_hash_UID_and_remove_duplicate_rows(df=df_entries, database=database, table_name="entries", verbose=verbose)
    if (methods::is(list_df_entries, "dbError")) {
        error = chain(list_df_entries, methods::new("dbError",
            code=000,
            message=paste0("Error in fn_convert_allele_frequency_table_into_blobs_and_dfs(...): error renaming and removing duplication columns for incoming base '", 
            "entries' table from '", table_name, "' data table.")))
        return(error)
    }
    ### We include both non-overlapping and overlapping rows to find the actual duplicates in the existing database table using the entry names alone,
    ### while keeping the same order as the genotype data
    df_entries$ENTRY_HASH = NA
    df_entries$ENTRY_UID = NA
    for (i in 1:nrow(df_entries)) {
        entry_name = df_entries$ENTRY[i]
        idx = which(list_df_entries$df_duplicated_in_database$ENTRY == entry_name)
        if (length(idx) < 1) {next}
        if (length(idx) < 1) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_convert_allele_frequency_table_into_blobs_and_dfs(...): ",
                "Duplicate entry names is not allowed in the genotype data. Please rectify names."))
            return(error)
        }
        df_entries$ENTRY_HASH[i] = list_df_entries$df_duplicated_in_database$ENTRY_HASH[idx]
        df_entries$ENTRY_UID[i] = list_df_entries$df_duplicated_in_database$ENTRY_UID[idx]
    }
    vec_existing_tables = DBI::dbGetQuery(conn=database, statement=paste0("PRAGMA TABLE_LIST"))$name
    if (sum(vec_existing_tables == "entries") == 1) {
        ### Keep only the unique entries in the incoming entries table
        df_entries_existing = DBI::dbGetQuery(conn=database, name="entries", statement="SELECT * FROM entries")
        vec_common_entry_names = df_entries$ENTRY[df_entries$ENTRY %in% df_entries_existing$ENTRY]
        for (entry_name in vec_common_entry_names) {
            df_entries$ENTRY_HASH[df_entries$ENTRY==entry_name] = df_entries_existing$ENTRY_HASH[df_entries_existing$ENTRY==entry_name]
            df_entries$ENTRY_UID[df_entries$ENTRY==entry_name] = df_entries_existing$ENTRY_UID[df_entries_existing$ENTRY==entry_name]
        }
    }
    ### Extract the genotypes table
    df_genotypes = data.frame(ENTRY_UID=df_entries$ENTRY_UID, I(lapply(df_allele_frequency_table[, -1:-3], FUN=function(x){serialize(object=x, connection=NULL)})))
    colnames(df_genotypes) = c("ENTRY_UID", "BLOB")
    if (verbose) {
        print(paste0("Extracted ", ncol(df_allele_frequency_table)-3, " entries x ", nrow(df_allele_frequency_table), " loci genotypes table."))
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
#' @param table_name name of the data table represented by df
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok list:
#'      $df_possibly_modified: data table with column names converted to uppercase, hash and UID 
#'          columns added, and duplicate rows and column ommitted
#'      $df_entries: "entries" base table or data frame
#'      $df_dates: "dates" base table or data frame
#'      $df_sites: "sites" base table or data frame
#'      $df_treatments: "treatments" base table or data frame
#'      $df_traits: "traits" base table or data frame
#'      $df_abiotics: "abiotics" base table or data frame
#'      $df_loci: "loci" base table or data frame
#'  - Ok NULL: the data table and its resulting base tables are entirely redundant
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
#'      database=database, table_name="phenotypes", verbose=TRUE)
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
    ### Check inputs
    error = fn_check_import_inputs(df=df, database=database, table_name=table_name)
    if (!is.null(error)) {
        error@message = gsub("FUNCTION_NAME", "fn_prepare_data_table_and_extract_base_tables", error@message)
        return(error)
    }
    if (sum(GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="base"] %in% table_name) > 0) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_prepare_data_table_and_extract_base_tables(...): the input '",
                table_name, "' table is not a data table."))
        return(error)
    }
    if (verbose) {
        print(paste0("Prepare the data table and extract base tables from the incoming '", table_name, "' data table."))
    }
    ### Convert allele frequency table into the expected genotypes table
    if (table_name == "genotypes") {
        list_df_genotypes_df_loci_df_entries = fn_convert_allele_frequency_table_into_blobs_and_dfs(df_allele_frequency_table=df, database=database, verbose=verbose)
        df = list_df_genotypes_df_loci_df_entries$df_genotypes
    }
    ### Filter and add ID columns
    if (table_name != "genotypes") {
        df = fn_rename_columns_remove_duplicate_columns_and_check_column_type_mismatch(df=df, database=database, table_name=table_name, verbose=verbose)
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
        list_df = fn_add_hash_UID_and_remove_duplicate_rows(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(list_df, "dbError")) {
            error = chain(list_df, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_prepare_data_table_and_extract_base_tables(...): error adding UIDs and removing duplicate rows for incoming'",
                table_name, "' table.")))
            return(error)
        }
        ### Extract the entire data table including the rows duplicated in the existing table in the database
        df = rbind(list_df$df, list_df$df_duplicated_in_database)
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
        # i = 1
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
                df_base_table = fn_rename_columns_remove_duplicate_columns_and_check_column_type_mismatch(df=df_base_table, database=database, table_name=base_table_name, verbose=verbose)
                if (methods::is(df_base_table, "dbError")) {
                    error = chain(df_base_table, methods::new("dbError",
                        code=000,
                        message=paste0("Error in fn_prepare_data_table_and_extract_base_tables(...): error renaming and removing duplicate columns for incoming base '",
                        base_table_name, "' table from '", table_name, "' data table.")))
                    return(error)
                }
                df_base_table = fn_add_hash_UID_and_remove_duplicate_rows(df=df_base_table, database=database, table_name=base_table_name, verbose=verbose)$df
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
                vec_idx_required_columns = which(colnames(df) %in% GLOBAL_list_required_colnames_per_table()[[base_table_name]])
                vec_required_column_names = colnames(df)[vec_idx_required_columns]
                if (length(vec_required_column_names) == 0) {next} ### Skip for data tables extracting an inappropriate base tables, e.g df_phenotypes --> df_abiotics
                vec_idx_numeric_required_column = c()
                for (j in vec_idx_required_columns) {
                    vec_idx_numeric_required_column = c(vec_idx_numeric_required_column, is.numeric(df[, j]))
                }
                vec_idx_numeric_required_column = which(vec_idx_numeric_required_column)
                vec_IDs = eval(parse(text=paste0("unique(paste0(", paste(paste0("df$", vec_required_column_names), collapse=", '\t', "), "))")))
                df_base_table = as.data.frame(matrix(unlist(strsplit(vec_IDs, "\t")), byrow=TRUE, ncol=length(vec_required_column_names)))
                for (j in vec_idx_numeric_required_column) {
                    df_base_table[, j] = as.numeric(df_base_table[, j])
                }
                if (GLOBAL_df_valid_tables()$NEEDS_DESCRIPTION[i]) {
                    df_base_table$DESCRIPTION = ""
                    colnames(df_base_table)[1:(ncol(df_base_table)-1)] = vec_required_column_names
                } else {
                    colnames(df_base_table) = vec_required_column_names
                }
                df_base_table = fn_add_hash_UID_and_remove_duplicate_rows(df=df_base_table, database=database, table_name=base_table_name, verbose=verbose)$df
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
                vec_idx_data_column_selected = colnames(df) %in% vec_required_column_names
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
                    vec_idx_base_column_selected = colnames(df_base_table_all) %in% vec_required_column_names
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

#' Classify the rows of the incoming base or data table to check for intersection/s with existing 
#'  data and base data tables in the database
#' @param df data frame representing a base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#' @param database an open SQLite database connection
#' @param table_name name of the base or data table represented by df
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok:
#'      $n_existing_rows: total number of rows in the existing table
#'      $n_incoming_rows: total number of rows in the incoming table
#'      $n_intersecting_rows: number of intersecting rows in the existing and incoming table
#'      $n_rows_exclusive_to_existing_table: number of rows exclusive to the existing table
#'      $n_rows_exclusive_to_incoming_table: number of rows exclusive to the incoming table
#'      $vec_bool_rows_exclusive_to_existing_table: vector of booleans referring to rows exclusive
#'          to the existing table
#'      $vec_bool_rows_exclusive_to_incoming_table: vector of booleans referring to rows exclusive
#'          to the incoming table
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
#' unlink("test.sqlite")
#' database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
#' list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df,
#'      database=database, table_name="phenotypes", verbose=TRUE)
#' df = list_df_data_and_base_tables$df_possibly_modified
#' df_first_half = droplevels(df[1:floor(nrow(df)/2), ])
#' ### Import the first half of the data table into the database
#' DBI::dbWriteTable(conn=database, name="phenotypes", value=df_first_half)
#' ### Detect row intersections
#' list_set_classification_of_rows = fn_set_classification_of_rows(df=df,
#'      database=database, table_name="phenotypes", verbose=TRUE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
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
    ### Check inputs including if hash and UID columns exist and is the table currently exist in the database
    error = fn_check_import_inputs(df=df, database=database, table_name=table_name, check_UIDs=TRUE, check_if_table_exists=TRUE)
    if (!is.null(error)) {
        error@message = gsub("FUNCTION_NAME", "fn_set_classification_of_rows", error@message)
        return(error)
    }
    if (verbose) {
        print(paste0("Classifying rows in the incoming '", table_name, "' table to check for intersections with the data in the existing database."))
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

#' Classify the columns of the incoming base or data table to check for intersection/s with existing 
#'  data and base data tables in the database
#' @param df data frame representing a base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#' @param database an open SQLite database connection
#' @param table_name name of the base or data table represented by df
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok:
#'      $n_existing_columns: total number of columns in the existing table
#'      $n_incoming_columns: total number of columns in the incoming table
#'      $n_intersecting_columns: number of intersecting columns in the existing and incoming table
#'      $n_columns_exclusive_to_existing_table: number of columns exclusive to the existing table
#'      $n_columns_exclusive_to_incoming_table: number of columns exclusive to the incoming table
#'      $vec_bool_columns_exclusive_to_existing_table: vector of booleans referring to columns
#'          exclusive to the existing table
#'      $vec_bool_columns_exclusive_to_incoming_table: vector of booleans referring to columns
#'          exclusive to the incoming table
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
#' ### Prepare the tables
#' list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df, 
#'      database=database, table_name="phenotypes", verbose=TRUE)
#' df = list_df_data_and_base_tables$df_possibly_modified
#' df_first_half = droplevels(df[, 1:floor(ncol(df)/2)])
#' ### Import the first half of the data table into the database
#' DBI::dbWriteTable(conn=database, name="phenotypes", value=df_first_half)
#' ### Detect column intersections
#' list_set_classification_of_columns = fn_set_classification_of_columns(df=df, database=database, 
#'      table_name="phenotypes", verbose=TRUE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
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
    ### Check inputs including if hash and UID columns exist and is the table currently exist in the database
    error = fn_check_import_inputs(df=df, database=database, table_name=table_name, check_UIDs=TRUE, check_if_table_exists=TRUE)
    if (!is.null(error)) {
        error@message = gsub("FUNCTION_NAME", "fn_set_classification_of_columns", error@message)
        return(error)
    }
    if (verbose) {
        print(paste0("Classifying columns in the incoming '", table_name, "' table to check for intersections with the data in the existing database."))
    }
    ### Remove quotes which will interfere with SQL queries
    df = fn_remove_quotes_and_newline_characters_in_data(df, verbose=FALSE)
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

#' Add new columns into an existing data table in the database
#' @param df data frame representing a data table, e.g. phenotypes, environments or genotypes table
#' @param database an open SQLite database connection
#' @param table_name name of the data table represented by df
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok: database - the same open SQLite database connection from the input
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
#' ### Prepare the tables
#' list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df, 
#'      database=database, table_name="phenotypes", verbose=TRUE)
#' df = list_df_data_and_base_tables$df_possibly_modified
#' df_first_half = droplevels(df[, 1:floor(ncol(df)/2)])
#' ### Import the first half of the data table into the database
#' DBI::dbWriteTable(conn=database, name="phenotypes", value=df_first_half)
#' ### Add new columns
#' database = fn_add_new_columns(df=df, database=database, table_name="phenotypes", verbose=TRUE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
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
    if (verbose) {
        print(paste0("Adding new columns to an existing '", table_name, "' table in the database."))
    }
    ### Remove quotes which will interfere with SQL queries
    df = fn_remove_quotes_and_newline_characters_in_data(df, verbose=FALSE)
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
            # head(DBI::dbGetQuery(conn=database, statement=paste0("SELECT * FROM ", table_name)))
        }
        if (verbose) {
            print(
                paste0("The following new columns were appended into the '", table_name, "' table: ", 
                paste(vec_incoming_colnames[vec_idx_incoming_columns_to_add], collapse=", "))
            )
        }
    }
    return(database)
}

#' Append data into existing data table in the database
#' @param df data frame representing a base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#' @param database an open SQLite database connection
#' @param table_name name of the base or data table represented by df
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok: database - the same open SQLite database connection from the input
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
#' ### Prepare the tables
#' list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df, 
#'      database=database, table_name="phenotypes", verbose=TRUE)
#' df = list_df_data_and_base_tables$df_possibly_modified
#' n = nrow(df); p = ncol(df)
#' vec_idx_columns_HASH_UID_and_required = 
#'     unique(c(1:17, grep("_UID$", colnames(df)), grep("_HASH$", colnames(df))))
#' df_q1 = droplevels(df[1:floor(n/2),     
#'     unique(c(vec_idx_columns_HASH_UID_and_required, 1:floor(p/2)))])
#' df_q2 = droplevels(df[1:floor(n/2),     
#'     unique(c(vec_idx_columns_HASH_UID_and_required, (floor(p/2)+1):p))])
#' df_q3 = droplevels(df[(floor(n/2)+1):n, 
#'     unique(c(vec_idx_columns_HASH_UID_and_required, (floor(p/2)+1):p))])
#' df_q4 = droplevels(df[(floor(n/2)+1):n, 
#'     unique(c(vec_idx_columns_HASH_UID_and_required, 1:floor(p/2)))])
#' ### Import the df_q1 into the database
#' DBI::dbWriteTable(conn=database, name="phenotypes", value=df_q1)
#' ### Add new rows and columns, i.e. df_q3
#' database = fn_append(df=df_q3, database=database, table_name="phenotypes", verbose=TRUE)
#' ### Add df_q2
#' database = fn_append(df=df_q2, database=database, table_name="phenotypes", verbose=TRUE)
#' ### Add df_q4
#' database = fn_append(df=df_q4, database=database, table_name="phenotypes", verbose=TRUE)
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @details
#' Note that it is easier to:
#'     (1) replace the entire table, i.e. when the source table is a complete superset of the destination table, and
#'     (2) add new rows
#' @export
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
    error = fn_check_import_inputs(df=df, database=database, table_name=table_name, check_UIDs=TRUE, check_if_table_exists=TRUE)
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
    if (verbose) {
        print(paste0("Appending data into the existing '", table_name, "' table in the database."))
    }
    ### Remove quotes which will interfere with SQL queries
    df = fn_remove_quotes_and_newline_characters_in_data(df, verbose=FALSE)
    ### If the existing table is the same as the incoming table, then there is nothing for us to do
    if (
        (list_set_classification_of_rows$n_rows_exclusive_to_existing_table == 0) &&
        (list_set_classification_of_columns$n_columns_exclusive_to_existing_table == 0) &&
        (list_set_classification_of_rows$n_rows_exclusive_to_incoming_table == 0) &&
        (list_set_classification_of_columns$n_columns_exclusive_to_incoming_table == 0)
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
            print(paste0("Overwriting the existing '", table_name, "' table."))
        }
        DBI::dbWriteTable(conn=database, name=table_name, value=df, overwrite=TRUE)
        return(database)
    }
    ### Add new columns (Note that we need to do this before adding new rows)
    if (list_set_classification_of_columns$n_columns_exclusive_to_incoming_table > 0) {
        database = fn_add_new_columns(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(database, "dbError")) {
            error = chain(database, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_append(...): error adding new columns from incoming '", table_name, "' table.")))
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
    if (verbose) {pb = utils::txtProgressBar(min=0, max=length(vec_idx_incoming_intersecting_rows), style=3)}
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
        if (verbose) {utils::setTxtProgressBar(pb, i)}
    }
    if (verbose) {close(pb)}
    ### Append the rows exclusive to the incoming table
    DBI::dbAppendTable(conn=database, name=table_name, value=df[list_set_classification_of_rows$vec_bool_rows_exclusive_to_incoming_table, , drop=FALSE])
    ### Output the database connection
    return(database)
}

#' Initialise the database
#' @param fname_db name of the SQLite database file
#' @param list_df_data_tables list containing the 3 data tables as data frames, 
#'  i.e. 'df_phenotypes', 'df_environments' and 'df_genotypes' data tables.
#'  Note that 'df_genotypes' is a data frame corresponding to an allele frequency table, where
#'          the first 3 columns refer to the CHROMOSOME, POSITION_PER_CHROMOSOME, and ALLELE,
#'          followed by the allele frequencies per sample, entry or pool with their corresponding
#'          entry names as the column names.
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok: 0
#'  - Err: dbError
#' @examples
#' fname_data_tables = fn_simulate_tables(
#'      n_entries=50,
#'      n_dates=3,
#'      n_sites=3,
#'      n_treatments=3,
#'      n_loci=10e3,
#'      save_data_tables=TRUE)$list_fnames_tables$fname_data_tables
#' list_df_data_tables = list()
#' for (table_name in c("phenotypes", "environments", "genotypes")) {
#'     eval(parse(text=paste0("list_df_data_tables$df_", table_name, 
#'          " = as.data.frame(readxl::read_excel(path=fname_data_tables, sheet=table_name))")))
#' }
#' fn_initialise_db(fname_db="test.sqlite", list_df_data_tables=list_df_data_tables)
#' unlink("test.sqlite")
#' @export
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
    if (verbose) {
        vec_data_table_names = c()
        vec_data_table_nrows = c()
        vec_data_table_ncols = c()
        for (table_name in GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"]) {
            df = list_df_data_tables[[paste0("df_", table_name)]]
            if (is.null(df)) {
                df = list_df_data_tables[[table_name]]
            }
            vec_data_table_names = c(vec_data_table_names, table_name)
            if (!is.null(df)) {
                vec_data_table_nrows = c(vec_data_table_nrows, nrow(df))
                vec_data_table_ncols = c(vec_data_table_ncols, ncol(df))
            } else {
                vec_data_table_nrows = c(vec_data_table_nrows, 0)
                vec_data_table_ncols = c(vec_data_table_ncols, 0)
            }
        }
        print(paste0("Initialising the database: '", fname_db, "' with the following data tables: ", 
            paste0("     - ", vec_data_table_names, ": nrows=", vec_data_table_nrows, ", ncols=", vec_data_table_ncols)
        ))
    }
    ### Create and open the connection to the database
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    ### Prepare the data tables and extract the base tables from the data tables
    for (table_name in GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"]) {
        # table_name = GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"][3]
        df = list_df_data_tables[[paste0("df_", table_name)]]
        if (is.null(df)) {
            df = list_df_data_tables[[table_name]]
        }
        if (is.null(df)) {
            ### Skip if we have no data table
            next
        }
        if (is.null(df)) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_initialise_db(...): '", table_name, "' table does not exist in the input list_df_data_tables."))
            return(error)
        }
        ### Remove quotes which will interfere with SQL queries
        df = fn_remove_quotes_and_newline_characters_in_data(df, verbose=FALSE)
        if (methods::is(df, "dbError")) {
            error = chain(df, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_initialise_db(...): error preparing '", table_name, "' table.")))
            return(error)
        }
        list_tables = fn_prepare_data_table_and_extract_base_tables(df=df, database=database, table_name=table_name, verbose=verbose)
        if (methods::is(list_tables, "dbError")) {
            error = chain(list_tables, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_initialise_db(...): error preparing '", table_name, "' table.")))
            return(error)
        }
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
                    return(error)
                }
            }
        }
        if (verbose) {
            print("Tables list:")
            print(DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST"))
        }
    }
    ### Index the tables for faster queries
    for (table_name in GLOBAL_df_valid_tables()$NAME) {
        # table_name = GLOBAL_df_valid_tables()$NAME[1]
        table_name_prefix = fn_define_hash_and_UID_prefix(table_name=table_name)
        ### Skip if the table has already been indexed
        if (nrow(DBI::dbGetQuery(conn=database, statement=paste0("PRAGMA INDEX_LIST(", table_name, ")"))) > 0) {next}
        if (verbose) {
            print(paste0("Creating unique indexes for the ", table_name_prefix, "_UID column in the ", table_name, " table."))
        }
        ### Check if the table has already been initialised
        vec_table_names = DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST")$name
        if (sum(vec_table_names %in% table_name) > 0) {
            if (table_name != "genotypes") {
                DBI::dbExecute(conn=database, statement=paste0("CREATE UNIQUE INDEX IDX_", table_name_prefix, " ON ", table_name, "(", table_name_prefix, "_UID)"))
            } else {
                DBI::dbExecute(conn=database, statement=paste0("CREATE UNIQUE INDEX IDX_", table_name_prefix, " ON ", table_name, "(ENTRY_UID)"))
            }
        }
    }
    if (verbose) {
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
        print("Initialised database contents:")
        print(DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST"))
        print("Entries table:")
        print(DBI::dbGetQuery(conn=database, statement="SELECT * FROM entries"))
        print("Sites table:")
        print(DBI::dbGetQuery(conn=database, statement="SELECT * FROM sites"))
        print("Dates table:")
        print(DBI::dbGetQuery(conn=database, statement="SELECT * FROM dates"))
        print("Traits table:")
        print(DBI::dbGetQuery(conn=database, statement="SELECT * FROM traits"))
        print("Abiotics table:")
        print(DBI::dbGetQuery(conn=database, statement="SELECT * FROM abiotics"))
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    }
    DBI::dbDisconnect(conn=database)
    return(0)
}

#' Update the database
#' @param fname_db name of the SQLite database file
#' @param df data frame representing a base (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table
#'  Note that if the table is a genotypes tables then we expect `df` to be a data frame corresponding 
#'          to an allele frequency table, where the first 3 columns refer to the CHROMOSOME, 
#'          POSITION_PER_CHROMOSOME, and ALLELE, followed by the allele frequencies per sample, 
#'          entry or pool with their corresponding entry names as the column names.
#' @param table_name name of the base or data table represented by df
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok: 0
#'  - Err: dbError
#' @examples
#' ### Initialise the database
#' fname_db = "test.sqlite"
#' fname_data_tables = fn_simulate_tables(
#'      n_entries=50,
#'      n_dates=3,
#'      n_sites=3,
#'      n_treatments=3,
#'      n_loci=10e3,
#'      save_data_tables=TRUE)$list_fnames_tables$fname_data_tables
#' list_df_data_tables = list()
#' for (table_name in c("phenotypes", "environments", "genotypes")) {
#'     eval(parse(text=paste0("list_df_data_tables$df_", table_name, 
#'     " = as.data.frame(readxl::read_excel(path=fname_data_tables, sheet=table_name))")))
#' }
#' fn_initialise_db(fname_db=fname_db, list_df_data_tables=list_df_data_tables)
#' ### Update the database with new data
#' list_sim = fn_simulate_tables(
#'      n_entries=50,
#'      n_dates=3,
#'      n_sites=3,
#'      n_treatments=3,
#'      n_loci=10e3,
#'      save_data_tables=TRUE)
#' fn_update_database(fname_db=fname_db, df=list_sim$df_phenotypes, 
#'      table_name="phenotypes", verbose=TRUE)
#' unlink("test.sqlite")
#' @export
fn_update_database = function(fname_db, df, table_name, verbose=TRUE) {
    ################################################################
    # ### TEST
    # fname_db = "test.sqlite"
    # list_fnames_tables = fn_simulate_tables(
    #         n_entries=50,
    #         n_dates=3,
    #         n_sites=3,
    #         n_treatments=3,
    #         n_loci=10e3,
    #         save_data_tables=TRUE)$list_fnames_tables
    # ### Partition the phenotypes data table
    # df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    # # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    # # list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df, 
    # #         database=database, table_name="phenotypes", verbose=TRUE)
    # # df = list_df_data_and_base_tables$df_possibly_modified
    # n = nrow(df); p = ncol(df)
    # # vec_idx_columns_HASH_UID_and_required = 1:17
    # vec_idx_columns_HASH_UID_and_required = 1:15
    # df_q1 = droplevels(df[1:floor(n/2),     1:floor(p/2)])
    # df_q2 = droplevels(df[1:floor(n/2),     c(vec_idx_columns_HASH_UID_and_required, (floor(p/2)+1):p)])
    # df_q3 = droplevels(df[(floor(n/2)+1):n, c(vec_idx_columns_HASH_UID_and_required, (floor(p/2)+1):p)])
    # df_q4 = droplevels(df[(floor(n/2)+1):n, 1:floor(p/2)])
    # df_centre = droplevels(df[floor(n*(1/3)):floor(n*(2/3)), c(vec_idx_columns_HASH_UID_and_required, floor(p*(1/3)):floor(p*(2/3)))])
    # ### Populate the data tables list to and initialise the database
    # list_df_data_tables = list()
    # for (table_name in c("phenotypes", "environments", "genotypes")) {
    #     if (table_name == "phenotypes") {
    #         list_df_data_tables$df_phenotypes = df_q1
    #     } else {
    #         eval(parse(text=paste0("list_df_data_tables$df_", table_name, 
    #             " = as.data.frame(readxl::read_excel(path=list_fnames_tables$fname_data_tables, sheet=table_name))")))
    #     }
    # }
    # fn_initialise_db(fname_db=fname_db, list_df_data_tables=list_df_data_tables)
    # ### Update the database using a phenotypes data table with additional traits (df_q2)
    # df = df_q2
    # table_name = "phenotypes"
    # verbose=TRUE
    ################################################################
    ### Connect to the database
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    ### Check inputs
    error = fn_check_import_inputs(df=df, database=database, table_name=table_name)
    if (!is.null(error)) {
        error@message = gsub("FUNCTION_NAME", "fn_update_database", error@message)
        return(error)
    }
    ### Remove quotes which will interfere with SQL queries
    df = fn_remove_quotes_and_newline_characters_in_data(df, verbose=FALSE)
    ### Add hash and UID columns, remove duplicates and separate the rows which are duplicated in the existing database as determined by hashes
    list_df = fn_add_hash_UID_and_remove_duplicate_rows(df=df, database=database, table_name=table_name, verbose=verbose)
    if (methods::is(list_df, "dbError")) {
        error = chain(list_df, methods::new("dbError",
            code=000,
            message=paste0("Error in fn_update_database(...): error appending '", table_name, "' table.")))
        return(error)
    }
    if (nrow(list_df$df_duplicated_in_database) > 0) {
       df = rbind(list_df$df, list_df$df_duplicated_in_database)
    } else {
        df = list_df$df
    }
    ### Append the incoming table into the existing table in the database
    if (nrow(df) > 0) {
        if (verbose) {
            print(paste0("Appending the incoming '", table_name, "' table into the database."))
        }
        ### Convert the allele frequency table into 2-column genotypes table
        if (table_name == "genotypes") {
            list_tables_genotypes = fn_prepare_data_table_and_extract_base_tables(df=df, database=database, table_name=table_name, verbose=verbose)
            df = list_tables_genotypes$df_possibly_modified
        }
        database = fn_append(df=df, database=database, table_name=table_name, verbose=verbose)
        ### Initialise the data table if it has not been initialised yet
        if (methods::is(database, "dbError") && grepl("Please initialise the table first", database@message)) {
            database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
            DBI::dbWriteTable(conn=database, name=table_name, value=df)
        }
        if (methods::is(database, "dbError")) {
            error = chain(database, methods::new("dbError",
                code=000,
                message=paste0("Error in fn_update_database(...): error appending '", table_name, "' table.")))
            return(error)
        }
        ### If the incoming table is a data table then we also need to append its associated base tables
        if (GLOBAL_df_valid_tables()$CLASS[GLOBAL_df_valid_tables()$NAME == table_name] == "data") {
            if (verbose) {
                print(paste0("Appending the base tables extracted from the incoming '", table_name, "' table into the database."))
            }
            ### Use the pre-processed `list_tables_genotypes` for the genotypes table above
            if (table_name == "genotypes") {
                list_tables = list_tables_genotypes
            } else {
                list_tables = fn_prepare_data_table_and_extract_base_tables(df=df, database=database, table_name=table_name, verbose=verbose)
            }
            ### If all the rows are already present in the existing data table in the database, then we skip appending the associated base tables
            if (!is.null(list_tables)) {
                for (i in 1:nrow(GLOBAL_df_valid_tables())) {
                    # i = 2
                    if (GLOBAL_df_valid_tables()$CLASS[i] != "base") {next}
                    base_table_name = GLOBAL_df_valid_tables()$NAME[i]
                    df_base_table  = list_tables[[paste0("df_", base_table_name)]]
                    if (is.null(df_base_table)) {next}
                    if (nrow(df_base_table) > 0) {
                        if ((base_table_name == "entries") & (table_name == "genotypes")) {
                            ### Skip appending entries base table if the data table is genotypes as the genotypes table only has the ENTRY_UID info, and
                            ### the CULTIVAR, POPULATION, and TYPE info are all unknown (UNK).
                            next
                        }
                        database = fn_append(df=df_base_table, database=database, table_name=base_table_name, verbose=verbose)
                        ### Initialise the base table if it has not been initialised yet
                        if (methods::is(database, "dbError") && grepl("Please initialise the table first", database@message)) {
                            database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
                            DBI::dbWriteTable(conn=database, name=base_table_name, value=df_base_table)
                        }
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
    }
    if (verbose) {
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
        print("Updated database contents:")
        print(DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST"))
        print("Entries table:")
        print(DBI::dbGetQuery(conn=database, statement="SELECT * FROM entries"))
        print("Sites table:")
        print(DBI::dbGetQuery(conn=database, statement="SELECT * FROM sites"))
        print("Dates table:")
        print(DBI::dbGetQuery(conn=database, statement="SELECT * FROM dates"))
        print("Traits table:")
        print(DBI::dbGetQuery(conn=database, statement="SELECT * FROM traits"))
        print("Abiotics table:")
        print(DBI::dbGetQuery(conn=database, statement="SELECT * FROM abiotics"))
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    }
    DBI::dbDisconnect(conn=database)
    return(0)
}
