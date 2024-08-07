##################################
### Data exportation functions ###
##################################

#' Check the validity of exportation function inputs
#' @param database an open SQLite database connection
#' @param table_name name of the (entries, dates, sites, treatments, traits, abiotics, 
#'  and loci) or data (phenotypes, environments and genotypes) table represented by df
#' @param list_filters list of named vectors where each vector refers to a valid 
#'  column name in the specified table with the values to be selected.
#'  A numeric column is always specified as a range of values, i.e. minimum and maximum values,
#'  but these can be the same value, and the same numeric column can be included multiple times with 
#'  various ranges which is equivalent to using a vector of values or ranges of values to filter.
#'  A text column is always specified by a vector of strings. (Default=NULL)
#' @param vec_columns_to_show vector of column names to include, where "*" means all 
#'  columns in the table. (Default="*")
#' @param unique_column_name column name which we are targetting to be unique after filtering. 
#'  Note that we do not explicitly test if the values in this column are actually unique 
#'  just that this column exists. (Default=NULL)
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
#' list_df_data_tables = list(
#'     df_phenotypes=utils::read.delim(list_fnames_tables$fname_phenotypes, 
#'          header=TRUE),
#'     df_environments=utils::read.delim(list_fnames_tables$fname_environments, 
#'          header=TRUE),
#'     df_genotypes=utils::read.delim(list_fnames_tables$fname_genotypes, 
#'          header=TRUE, check.names=FALSE))
#' fn_initialise_db(fname_db="test.sqlite", 
#'          list_df_data_tables=list_df_data_tables, verbose=TRUE)
#' database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
#' list_filters=list(
#'     REPLICATION="rep_1",
#'     TREATMENT=sample(unique(list_df_data_tables$df_phenotypes$TREATMENT), 
#'         size=min(c(2, length(unique(list_df_data_tables$df_phenotypes$TREATMENT)))))
#' )
#' null_error = fn_check_export_inputs(database=database, table_name="phenotypes",
#'      list_filters=list_filters, vec_columns_to_show="*", unique_column_name="ENTRY")
#' non_null_error_1 = fn_check_export_inputs(database=database, 
#'      table_name="non_existent_table", list_filters=list_filters)
#' non_null_error_2 = fn_check_export_inputs(database=database, table_name="entries",
#'      vec_columns_to_show="non_existent_column")
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_check_export_inputs = function(database, table_name, list_filters=NULL, vec_columns_to_show="*", unique_column_name=NULL) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # fname_db = "test.sqlite"
    # list_df_data_tables = list(
    #     df_phenotypes=utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE),
    #     df_environments=utils::read.delim(list_fnames_tables$fname_environments, header=TRUE),
    #     df_genotypes=utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE))
    # fn_initialise_db(fname_db=fname_db, list_df_data_tables=list_df_data_tables, verbose=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    # table_name = "phenotypes"
    # list_filters=list(
    #             REPLICATION="rep_1",
    #             TREATMENT=sample(unique(list_df_data_tables$df_phenotypes$TREATMENT), size=min(c(2, length(unique(list_df_data_tables$df_phenotypes$TREATMENT))))))
    # vec_columns_to_show = "*"
    # unique_column_name = NULL
    ################################################################
    error = NULL
    ### Is the database connection valid?
    if (!methods::is(database, "SQLiteConnection")) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in FUNCTION_NAME(...): broken SQLite connection. Please reconnect via DBI::dbConnect(...)."))
        return(error)
    }
    ### Is the database not empty?
    bool_at_least_one_expected_table_present = FALSE
    vec_existing_table_names = DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST")$name
    for (tname in vec_existing_table_names) {
        if (sum(GLOBAL_df_valid_tables()$NAME == tname) > 0) {
            bool_at_least_one_expected_table_present = TRUE
            break
        }
    }
    if (!bool_at_least_one_expected_table_present) {
        error_new = methods::new("dbError",
            code=000,
            message=paste0("Error in FUNCTION_NAME(...): database is empty or does not contain the expected base and data tables: '",
                paste(GLOBAL_df_valid_tables()$NAME, collapse="', '"), "'."))
        if (!is.null(error)) {
            error = chain(error, error_new)
        } else {
            error = error_new
        }
    }
    ### Is the table name among the valid base and data tables?
    if (sum(GLOBAL_df_valid_tables()$NAME == table_name) == 0) {
        error_new = methods::new("dbError",
            code=000,
            message=paste0("Error in FUNCTION_NAME(...): invalid table name. We expected the following base and data tables: '",
                paste(GLOBAL_df_valid_tables()$NAME, collapse="', '"), "'."))
        if (!is.null(error)) {
            error = chain(error, error_new)
        } else {
            error = error_new
        }
    }
    ### Does the database have the requested table name?
    if (
        methods::is(database, "SQLiteConnection") &&
        bool_at_least_one_expected_table_present &&
        (sum(GLOBAL_df_valid_tables()$NAME == table_name) == 1) &&
        (sum(vec_existing_table_names == table_name) == 0)
    ) {
        error_new = methods::new("dbError",
            code=000,
            message=paste0("Error in FUNCTION_NAME(...): requested '", table_name, "' table does not exist in the database yet."))
        if (!is.null(error)) {
            error = chain(error, error_new)
        } else {
            error = error_new
        }
    }
    ### Doest the list of filters exist as column in the table?
    if (
        methods::is(database, "SQLiteConnection") &&
        bool_at_least_one_expected_table_present &&
        (sum(GLOBAL_df_valid_tables()$NAME == table_name) == 1) &&
        (sum(vec_existing_table_names == table_name) == 1)
    ) {
        if (!is.null(list_filters)) {
            vec_requested_column_names = names(list_filters)
        } else {
            vec_requested_column_names = c()
        }
        if ((length(vec_columns_to_show) == 1) && (vec_columns_to_show != "*")) {
            vec_requested_column_names = c(vec_requested_column_names, vec_columns_to_show)
        }
        if (!is.null(unique_column_name)) {
            vec_requested_column_names = c(vec_requested_column_names, unique_column_name)
        }
        vec_existing_column_names = DBI::dbGetQuery(conn=database, statement=paste0("PRAGMA TABLE_INFO(", table_name, ")"))$name
        vec_missing_column_names = c()
        for (column_name in vec_requested_column_names) {
            # column_name = vec_requested_column_names[1]
            if (sum(vec_existing_column_names == column_name) == 0) {
                vec_missing_column_names = c(vec_missing_column_names, column_name)
            }
        }
        if (length(vec_missing_column_names) > 0) {
            error_new = methods::new("dbError",
                code=000,
                message=paste0("Error in FUNCTION_NAME(...): requested '", table_name, "' table does not have the following columns for querying: '",
                    paste(vec_missing_column_names, collapse="', '"), "'."))
            if (!is.null(error)) {
                error = chain(error, error_new)
            } else {
                error = error_new
            }
        }
    }
    return(error)
}

#' Query and join tables along common column/s
#' @param database an open SQLite database connection
#' @param list_tables_and_filters a list of lists, where each list corresponds to,
#'   and named after a table in the database, they contain:
#'      - key_names: a vector of column names corresponding to the identifying columns 
#'          where at least one column is common across all the listed tables
#'      - column_names: a vector of column names to be included in the output table, 
#'          where "*" means all columns in the table
#'      - list_filters: a list of named vectors where each vector refers to a valid 
#'          column name in the specified table with the values to be selected.
#'          A numeric column is always specified as a range of values, i.e. minimum and 
#'          maximum values, but these can be the same value, and the same numeric column 
#'          can be included multiple times with various ranges which is equivalent to 
#'          using a vector of values or ranges of values to filter. 
#'          A text column is always specified by a vector of strings.
#' @param unique_column_name a common column name across all included tables in 
#'  `list_tables_and_filters` which we are requiring to be unique after filtering. 
#'  (Default=NULL)
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok:
#'      merged data frame including all the columns requested per table, and
#'          all the rows passing the filtering requirements listed in
#'          `list_tables_and_filters`.
#'  - Err: dbError
#' @examples
#' list_fnames_tables = fn_simulate_tables(
#'      n_entries=50,
#'      n_dates=3,
#'      n_sites=3,
#'      n_treatments=3,
#'      n_loci=10e3,
#'      save_data_tables=TRUE)$list_fnames_tables
#' list_df_data_tables = list(
#'     df_phenotypes=utils::read.delim(list_fnames_tables$fname_phenotypes, 
#'          header=TRUE),
#'     df_environments=utils::read.delim(list_fnames_tables$fname_environments, 
#'          header=TRUE),
#'     df_genotypes=utils::read.delim(list_fnames_tables$fname_genotypes, 
#'          header=TRUE, check.names=FALSE))
#' fn_initialise_db(fname_db="test.sqlite", 
#'          list_df_data_tables=list_df_data_tables, verbose=TRUE)
#' database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
#' list_tables_and_filters = list(
#'     entries=list(
#'         key_names=c("ENTRY_UID"), 
#'         column_names=c("*"),
#'         list_filters=list(
#'             ENTRY=sample(unique(list_df_data_tables$df_phenotypes$ENTRY), 
#'                  size=min(c(10, 
#'                  length(unique(list_df_data_tables$df_phenotypes$ENTRY))))),
#'             POPULATION=list_df_data_tables$df_phenotypes$POPULATION[1])
#'     ),
#'     phenotypes=list(
#'         key_names=c("ENTRY_UID", "REPLICATION", "TREATMENT", "SITE", "FVI_YEAR_SEASON"),
#'         column_names=c("*"),
#'         list_filters=list(
#'             REPLICATION="rep_1",
#'             SITE=list_df_data_tables$df_phenotypes$SITE[1],
#'             POSIX_DATE_TIME=list_df_data_tables$df_phenotypes$POSIX_DATE_TIME[1],
#'             TREATMENT=sample(unique(list_df_data_tables$df_phenotypes$TREATMENT), 
#'                  size=min(c(1, 
#'                  length(unique(list_df_data_tables$df_phenotypes$TREATMENT))))))
#'     ),
#'      genotypes=list(
#'         key_names=c("ENTRY_UID"),
#'         column_names=c("*"),
#'         list_filters=NULL
#'     )
#' )
#' df_query = fn_query_and_left_join_tables(database=database, 
#'      list_tables_and_filters=list_tables_and_filters, unique_column_name="ENTRY_UID")
#' DBI::dbDisconnect(database)
#' unlink("test.sqlite")
#' @export
fn_query_and_left_join_tables = function(database, list_tables_and_filters, unique_column_name=NULL, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # fname_db = "test.sqlite"
    # list_df_data_tables = list(
    #     df_phenotypes=utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE),
    #     df_environments=utils::read.delim(list_fnames_tables$fname_environments, header=TRUE),
    #     df_genotypes=utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE))
    # fn_initialise_db(fname_db=fname_db, list_df_data_tables=list_df_data_tables, verbose=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    # list_tables_and_filters = list(
    #     entries=list(
    #         key_names=c("ENTRY_UID"), 
    #         column_names=c("*"),
    #         list_filters=list(
    #             ENTRY=sample(unique(list_df_data_tables$df_phenotypes$ENTRY), size=min(c(2, length(unique(list_df_data_tables$df_phenotypes$ENTRY))))))
    #     ),
    #     phenotypes=list(
    #         key_names=c("ENTRY_UID", "REPLICATION", "TREATMENT", "SITE", "FVI_YEAR_SEASON"),
    #         column_names=c("*"),
    #         list_filters=list(
    #             REPLICATION="rep_1",
    #             TREATMENT=sample(unique(list_df_data_tables$df_phenotypes$TREATMENT), size=min(c(2, length(unique(list_df_data_tables$df_phenotypes$TREATMENT))))))
    #     ),
    #      genotypes=list(
    #         key_names=c("ENTRY_UID"),
    #         column_names=c("*"),
    #         list_filters=NULL
    #     )
    # )
    # unique_column_name=NULL
    # verbose = TRUE
    ################################################################
    vec_table_names = names(list_tables_and_filters)
    if (length(vec_table_names) > 1) {
        vec_range_i = c(1:(length(vec_table_names)-1))
    } else {
        vec_range_i = c(0)
    }
    vec_common_key_names = c()
    for (i in vec_range_i) {
        # i = 1
        vec_common_key_names = c(vec_common_key_names,
            list_tables_and_filters[[1]]$key_names[list_tables_and_filters[[1]]$key_names %in% list_tables_and_filters[[i+1]]$key_names])
    }
    vec_common_key_names = unique(vec_common_key_names)
    if (length(vec_common_key_names) == 0) {
        error = methods::new("dbError",
            code=000,
            message=paste0(
                "Error in fn_query_and_left_join_tables(...): no common column across all the tables listed in `list_tables_and_filters. `",
                "Please make sure the `key_names` in each table have at least one column name in common."
            ))
            return(error)
    }
    vec_select = c()
    vec_on = c()
    vec_where = c()
    error = fn_check_export_inputs(database=database, table_name=vec_table_names[1], list_filters=list_tables_and_filters[[1]]$list_filters, vec_columns_to_show=list_tables_and_filters[[1]]$column_names, unique_column_name=unique_column_name)
    if (methods::is(error, "dbError")) {
        error@message = gsub("FUNCTION_NAME", "fn_query_and_left_join_tables", error@message)
        return(error)
    }
    for (i in vec_range_i) {
        # i = 1
        error = fn_check_export_inputs(database=database, table_name=vec_table_names[i+1], list_filters=list_tables_and_filters[[i+1]]$list_filters, vec_columns_to_show=list_tables_and_filters[[i+1]]$column_names, unique_column_name=unique_column_name)
        if (methods::is(error, "dbError")) {
            error@message = gsub("FUNCTION_NAME", "fn_query_and_left_join_tables", error@message)
            return(error)
        }
        table_name_to_the_left = vec_table_names[1] ### Anchored to the first table
        table_name_to_the_right = vec_table_names[i+1]
        ### Define filtering parameters
        list_filters_1 = list_tables_and_filters[[table_name_to_the_left]]$list_filters
        if (table_name_to_the_left != table_name_to_the_right) {
            list_filters_2 = list_tables_and_filters[[table_name_to_the_right]]$list_filters
        } else {
            list_filters_2 = NULL
        }
        if ((!is.null(list_filters_1) & (i==1)) | ((vec_range_i[1]==0) & !is.null(list_filters_1) & (i==0))) {
            vec_column_filters_1 = names(list_filters_1)
            vec_string_filters_1 = c()
            for (column in vec_column_filters_1) {
                # column = vec_column_filters_1[1]
                vec_y = list_filters_1[[column]]
                if ((length(vec_y)==1) && (vec_y=="*")) {next} ### Skip if we want to include all, i.e. *
                ### If we have 2 elements then we assume a range if the elements are numeric
                if ((length(vec_y) == 2) && is.numeric(vec_y)) {
                    vec_string_filters_1 = c(vec_string_filters_1, paste(paste0(table_name_to_the_left, ".", column), "BETWEEN", vec_y[1], "AND", vec_y[2]))
                } else {
                    if (is.numeric(vec_y)) {
                        ### For numeric elements but not a range, i.e. there were 1 or more than 2 elements
                        vec_string_filters_1 = c(vec_string_filters_1, paste(paste0(table_name_to_the_left, ".", column), "IN", paste0("(", paste(unique(vec_y), collapse=", "), ")")))
                    } else {
                        ### For strings
                        vec_string_filters_1 = c(vec_string_filters_1, paste(paste0(table_name_to_the_left, ".", column), "IN", paste0("('", paste(unique(vec_y), collapse="', '"), "')")))
                    }
                }
            }
        } else {
            vec_string_filters_1 = c()
        }
        if (!is.null(list_filters_2)) {
            vec_column_filters_2 = names(list_filters_2)
            vec_string_filters_2 = c()
            for (column in vec_column_filters_2) {
                # column = vec_column_filters_2[1]
                vec_y = list_filters_2[[column]]
                ### If we have 2 elements then we assume a range if the elements are numeric
                if ((length(vec_y) == 2) && is.numeric(vec_y)) {
                    vec_string_filters_2 = c(vec_string_filters_2, paste(paste0(table_name_to_the_right, ".", column), "BETWEEN", vec_y[1], "AND", vec_y[2]))
                } else {
                    if (is.numeric(vec_y)) {
                        ### For numeric elements but not a range, i.e. there were 1 or more than 2 elements
                        vec_string_filters_2 = c(vec_string_filters_2, paste(paste0(table_name_to_the_right, ".", column), "IN", paste0("(", paste(unique(vec_y), collapse=", "), ")")))
                    } else {
                        ### For strings
                        vec_string_filters_2 = c(vec_string_filters_2, paste(paste0(table_name_to_the_right, ".", column), "IN", paste0("('", paste(unique(vec_y), collapse="', '"), "')")))
                    }
                }
            }
        } else {
            vec_string_filters_2 = c()
        }
        vec_select = c(vec_select, 
            paste0(table_name_to_the_left, ".", list_tables_and_filters[[table_name_to_the_left]]$column_names), 
            paste0(table_name_to_the_right, ".", list_tables_and_filters[[table_name_to_the_right]]$column_names))
        vec_on = c(vec_on, paste(paste(
            paste0(table_name_to_the_left, ".", vec_common_key_names),
            paste0(table_name_to_the_right, ".", vec_common_key_names), sep="="),
            collapse=", "))
        vec_where = c(vec_where, paste(c(
            vec_string_filters_1, 
            vec_string_filters_2),
            collapse="\nAND "))
    }
    ### Put the individual query filters together
    vec_select = unique(vec_select)
    vec_where = unique(vec_where)
    vec_where = vec_where[which(vec_where != "")]
    vec_query = c(
        "SELECT", 
        paste(vec_select, collapse=", "), 
        paste0("FROM ", vec_table_names[1]))
    if (length(vec_range_i) > 0) {
        for (i in vec_range_i) {
            # i = 1
            vec_query = c(vec_query, 
                paste0("LEFT JOIN ", vec_table_names[i+1]),
                "ON",
                vec_on[i])
        }
    }
    vec_query = c(vec_query, "WHERE", paste(vec_where, collapse="\nAND"))
    query = paste(vec_query, collapse="\n")
    ### Query
    df_query = DBI::dbGetQuery(conn=database, statement=query)
    ### Log messages
    if (verbose) {
        cat(query, "\n")
        print(paste0("Query output: ", nrow(df_query), " rows and ", ncol(df_query), " columns"))
    }
    ### If we want to set a unique column, e.g. if we want to extract genotype data for a specific harvest we want to make sure there are not duplicates which may be due to including data from other sites
    if ((nrow(df_query) > 0) && !is.null(unique_column_name)) {
        if (verbose) {
            print(paste0("Expecting to get unique elements on the column: '", unique_column_name, "'."))
        }
        if (length(unique(eval(parse(text=paste0("df_query$", unique_column_name))))) != nrow(df_query)) {
            print(paste0("The unique column: ", unique_column_name, " is not unique under the current filters."))
            print("Consider the adding the columns listed below to the filter:")
            vec_column_filters = unique(unlist(lapply(list_tables_and_filters, FUN=function(x){names(x$list_filters)})))
            for (j in which(!(colnames(df_query) %in% vec_column_filters))) {
                if ((class(df_query[, j])[1] != "blob") && (length(table(df_query[, j])) > 1)) {
                    print(paste0("   - ", colnames(df_query)[j]))
                }
            }
        }
    }
    ### Output
    return(df_query)
}

# Assess the number of unique elements in each column of a table after an optional filtering
fn_assess_df_subsets = function(database, table_name, list_filters=NULL, vec_columns_levels_to_count=NULL, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # fname_db = "test.sqlite"
    # list_df_data_tables = list(
    #     df_phenotypes=utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE),
    #     df_environments=utils::read.delim(list_fnames_tables$fname_environments, header=TRUE),
    #     df_genotypes=utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE))
    # fn_initialise_db(fname_db=fname_db, list_df_data_tables=list_df_data_tables, verbose=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    # table_name = "phenotypes"
    # list_filters=list(
    #             REPLICATION="*",
    #             MONTH=c(1, 12),
    #             TREATMENT=sample(unique(list_df_data_tables$df_phenotypes$TREATMENT), size=min(c(2, length(unique(list_df_data_tables$df_phenotypes$TREATMENT)))))
    #             )
    # vec_columns_levels_to_count = NULL
    # verbose = TRUE
    ################################################################
    ### Check for input error errors
    vec_columns_to_show = "*"
    unique_column_name = NULL
    error = fn_check_export_inputs(database=database, table_name=table_name, list_filters=list_filters, vec_columns_to_show=vec_columns_to_show, unique_column_name=unique_column_name)
    if (methods::is(error, "dbError")) {
        error@message = gsub("FUNCTION_NAME", "fn_assess_df_subsets", error@message)
        return(error)
    }
    ### Extract the data frame we're interested in from a single table, i.e. no left joins
    list_tables_and_filters = list()
    eval(parse(text=paste0("list_tables_and_filters$`", table_name, "`=list(key_names=c('*'), column_names=vec_columns_to_show, list_filters=list_filters)")))
    df = fn_query_and_left_join_tables(
        database=database, 
        list_tables_and_filters=list_tables_and_filters,
        unique_column_name=unique_column_name,
        verbose=verbose)
    if (verbose) {
        print(utils::str(df))
    }
    ### Define the levels per field used for filtering
    list_column_levels_or_values = list()
    for (column_name in names(list_filters)) {
        # column_name = names(list_filters)[1]
        vec_levels_or_values = list_filters[[column_name]]
        if ((length(vec_levels_or_values) == 1) && (vec_levels_or_values == "*")) {
            vec_levels_or_values = eval(parse(text=paste0("unique(df$`", column_name, "`)")))
        }
        eval(parse(text=paste0("list_column_levels_or_values$`", column_name, "` = vec_levels_or_values")))
    }
    ### Define all possible combinations of the field levels
    ### Note that all numeric columns are assumed to be ranges and not UID.
    vec_string_for_expand_grid = c()
    for (i in 1:length(list_column_levels_or_values)) {
        # i = 1
        if (is.numeric(list_column_levels_or_values[[i]]) & !grepl("_UID$", names(list_column_levels_or_values)[i])) {
            string_vec_contents = paste0("'RANGE:", list_column_levels_or_values[[i]][1], ":", list_column_levels_or_values[[i]][2], "'")
        } else {
            string_vec_contents = paste0("'", paste(list_column_levels_or_values[[i]], collapse="', '"), "'")
        }
        vec_string_for_expand_grid = c(vec_string_for_expand_grid, paste0(names(list_column_levels_or_values)[i], "=c(", string_vec_contents, ")"))
    }
    df_all_possible_combinations = eval(parse(text=paste0("expand.grid(", paste(vec_string_for_expand_grid, collapse=", "), ")")))
    ### Add the fields where we will put the number of levels or unique items for each required column
    if (is.null(vec_columns_levels_to_count)) {
        vec_column_names_to_count = unique(unlist(GLOBAL_list_required_colnames_per_table()))
    }
    for (req_col_name in vec_column_names_to_count) {
        eval(parse(text=paste0("df_all_possible_combinations$`N_UNIQ_", req_col_name, "` = NA")))
    }
    ### View subsets where each subset refer to a combination among all possible field level combinations
    ### Then count the number of unique elements of levels across the columns we wish to count defined by `vec_column_names_to_count`.
    ### Note that all numeric columns are assumed to be ranges and not UID.
    list_out = list()
    for (i in 1:nrow(df_all_possible_combinations)) {
        # i = 1
        vec_string_idx = c()
        for (column_name in names(list_filters)) {
            # column_name = names(list_filters)[2]
            x = eval(parse(text=paste0("df_all_possible_combinations$`", column_name, "`[i]")))
            if (grepl("^RANGE", x)) {
                vec_x = as.numeric(unlist(strsplit(as.character(x), ":"))[-1])
                vec_string_idx = c(vec_string_idx, paste0("(df$", column_name, ">=", vec_x[1], ")"))
                vec_string_idx = c(vec_string_idx, paste0("(df$", column_name, "<=", vec_x[2], ")"))
            } else {
                vec_string_idx = c(vec_string_idx, paste0("(df$", column_name, "=='", x, "')"))
            }
        }
        vec_idx = eval(parse(text=paste0("which(", paste(vec_string_idx, collapse=" & "), ")")))
        df_sub = droplevels(df[vec_idx, , drop=FALSE])
        ### Count the number of unique elements or levels
        for (req_col_name in vec_column_names_to_count) {
            # req_col_name = vec_column_names_to_count[1]
            y = eval(parse(text=paste0("table(df_sub$`", req_col_name, "`)")))
            eval(parse(text=paste0("df_all_possible_combinations$`N_UNIQ_", req_col_name, "`[i] = length(y)")))
            eval(parse(text=paste0("list_out$`", paste0(paste(vec_string_idx, collapse=" & "), ":", req_col_name), "` = y")))
        }
    }
    if (verbose) {
        print(df_all_possible_combinations)
    }
    ### Output
    list_out$df_all_possible_combinations = df_all_possible_combinations
    return(list_out)
}

fn_deserialise_genotype_data = function(database, df_genotypes, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # fname_db = "test.sqlite"
    # list_df_data_tables = list(
    #     df_phenotypes=utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE),
    #     df_environments=utils::read.delim(list_fnames_tables$fname_environments, header=TRUE),
    #     df_genotypes=utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE, check.names=FALSE))
    # fn_initialise_db(fname_db=fname_db, list_df_data_tables=list_df_data_tables, verbose=TRUE)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    # df_genotypes = DBI::dbGetQuery(conn=database, statement="SELECT * FROM genotypes")
    # verbose = TRUE
    ################################################################
    error = NULL
    for (table_name in c("entries", "loci")) {
        error_tmp = fn_check_export_inputs(database, table_name="entries", list_filters=NULL, vec_columns_to_show="*", unique_column_name=NULL)
        if (!is.null(error_tmp)) {
            error_tmp@message = gsub("FUNCTION_NAME", "fn_deserialise_genotype_data", error_tmp@message)
        }
        if (!is.null(error) & !is.null(error_tmp)) {
            error = chain(error, error_tmp)
        }
    }
    if (!is.null(error)) {
        return(error)
    }
    df_entries = DBI::dbGetQuery(conn=database, statement="SELECT * FROM entries")
    df_loci = DBI::dbGetQuery(conn=database, statement="SELECT * FROM loci")
    vec_q = unserialize(as.raw(unlist(df_genotypes$BLOB[1])))
    n = nrow(df_genotypes)
    p = length(vec_q)
    G = matrix(NA, nrow=n, ncol=p)
    rownames(G) = df_entries$ENTRY[df_entries$ENTRY_UID %in% df_genotypes$ENTRY_UID]
    colnames(G) = unlist(apply(df_loci, MARGIN=1, FUN=function(x){paste(gsub(" ", "", x[-1:-2]), collapse="\t")}))
    if (verbose) {pb = utils::txtProgressBar(min=0, max=n, style=3)}
    for (i in 1:n) {
        G[i, ] = unserialize(as.raw(unlist(df_genotypes$BLOB[i])))
        if (verbose) {utils::setTxtProgressBar(pb, i)}
    }
    if (verbose) {close(pb)}
    return(G)
}
