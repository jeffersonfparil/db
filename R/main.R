#' Create a single database from an MS Excel file
#' @param fname_db name of the SQLite database file. If NULL, then this will be saved in 
#'  the same directory as `fname_xlsx` or `fname_phenotypes_tsv` with the same base name
#'  but with `.sqlite` extension instead of `.xlsx` or `.tsv`. (Default=NULL)
#' @param fname_xlsx name of the MS Excel file with 3 tabs where each tab represent one of 
#'  the three data tables including the required columns for each. These tabs should be
#'  named "phenotypes", "environments", and "genotypes". A tab can be empty, 
#'  e.g. "genotypes" tab can be left empty if the genotype file is massive, in which case
#'  the tab-delimited allele frequency table file may be specified. 
#'  See gp::GLOBAL_list_required_colnames_per_table() for the required columns per table.
#'  (Default=NULL)
#' @param fname_phenotypes_tsv name of the phenotypes table file. This file is tab-delimited.
#'  See gp::GLOBAL_list_required_colnames_per_table()$phenotypes for the required columns.
#'  (Default=NULL)
#' @param fname_environments_tsv name of the environments table file. This file is tab-delimited.
#'  See gp::GLOBAL_list_required_colnames_per_table()$environments for the required columns.
#'  (Default=NULL)
#' @param fname_genotypes_tsv name of the allele frequency table file. This is a tab-delimited
#'  file with a header line and the first 3 columns refer to the chromosome (chr), position (pos),
#'  and allele (allele), with subsequent columns referring to the allele frequencies of a sample.
#'  Names of the samples in the header line can be any unique string of characters. (Default=NULL)
#' @param overwrite Overwrite the database file, if it exists? (Default=FALSE)
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok: 0
#'  - Err: dbError
#' @examples
#' fname_xlsx = fn_simulate_tables(
#'         n_entries=50,
#'         n_dates=3,
#'         n_sites=3,
#'         n_treatments=3,
#'         n_loci=10e3,
#'         save_data_tables=TRUE)$list_fnames_tables$fname_data_tables
#' fn_create_database_from_xlsx_or_tsv(fname_xlsx=fname_xlsx)
#' @export
fn_create_database_from_xlsx_or_tsv = function(
    fname_db=NULL,
    fname_xlsx=NULL,
    fname_phenotypes_tsv=NULL,
    fname_environments_tsv=NULL,
    fname_genotypes_tsv=NULL,
    overwrite=FALSE,
    verbose=TRUE) 
{
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # fname_xlsx = list_fnames_tables$fname_data_tables
    # fname_db = NULL
    # fname_genotypes_tsv = NULL
    # overwrite = TRUE
    # verbose = TRUE
    ################################################################
    ### Check the input file/s
    if (!is.null(fname_xlsx)) {
        if (!file.exists(fname_xlsx)) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_create_database_from_xlsx_or_tsv(...): ",
                "The '", fname_xlsx,"' MS Excel file does not exist."))
            return(error)
        }
    }
    if (!is.null(fname_phenotypes_tsv)) {
        if (!file.exists(fname_phenotypes_tsv)) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_create_database_from_xlsx_or_tsv(...): ",
                "The '", fname_phenotypes_tsv,"' tab-delimited phenotypes file does not exist."))
            return(error)
        }
    }
    if (!is.null(fname_environments_tsv)) {
        if (!file.exists(fname_environments_tsv)) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_create_database_from_xlsx_or_tsv(...): ",
                "The '", fname_environments_tsv,"' tab-delimited environments file does not exist."))
            return(error)
        }
    }
    if (!is.null(fname_genotypes_tsv)) {
        if (!file.exists(fname_genotypes_tsv)) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_create_database_from_xlsx_or_tsv(...): ",
                "The '", fname_genotypes_tsv,"' tab-delimited genotypes file does not exist."))
            return(error)
        }
    }
    if (is.null(fname_xlsx) & is.null(fname_phenotypes_tsv) & is.null(fname_environments_tsv) & is.null(fname_genotypes_tsv)) {
       error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_create_database_from_xlsx_or_tsv(...): ",
            "No input files supplied."))
        return(error)
    }
    ### Define the name of database file
    if (is.null(fname_db)) {
        if (!is.null(fname_xlsx)) {
            fname_db = file.path(dirname(fname_xlsx), gsub(".xlsx$", ".sqlite", basename(fname_xlsx)))
        } else if (!is.null(fname_phenotypes_tsv)) {
            fname_db = file.path(dirname(fname_phenotypes_tsv), gsub(".tsv$", ".sqlite", basename(fname_phenotypes_tsv)))
        } else if (!is.null(fname_environments_tsv)) {
            fname_db = file.path(dirname(fname_environments_tsv), gsub(".tsv$", ".sqlite", basename(fname_environments_tsv)))
        } else if (!is.null(fname_genotypes_tsv)) {
            fname_db = file.path(dirname(fname_genotypes_tsv), gsub(".tsv$", ".sqlite", basename(fname_genotypes_tsv)))
        }
    }
    if (!dir.exists(dirname(fname_db))) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_create_database_from_xlsx_or_tsv(...): ",
            "The '", dirname(fname_db), "' directory which will hold the '", basename(fname_db),"' database file does not exist."))
        return(error)
    }
    if (!overwrite & file.exists(fname_db)) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_create_database_from_xlsx_or_tsv(...): ",
            "The '", fname_db, "' database file exists and the option to overwrite is FALSE."))
        return(error)
    }
    ### Extract the data tables
    list_df_data_tables = list(df_phenotypes=NULL, df_environments=NULL, df_genotypes=NULL)
    for (table_name in GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"]) {
        # table_name = GLOBAL_df_valid_tables()$NAME[10]
        if ((table_name == "genotypes") & !is.null(fname_genotypes_tsv)) {
            df = utils::read.delim(fname_genotypes_tsv, check.names=FALSE)
        } else {
            ### The tables in the MS Excel file takes priority
            if (!is.null(fname_xlsx)) {
                ### Use a large guess_max value to avoid mis classifying numeric columns as binary when there are many missing initial values.
                ### Also define a bunch of missing values which can be invisible in MS Excel, e.g. \r\n.
                ### Additionally, try converting each column into numerics but only if they were actually numerics, i.e. no warnings.
                df = as.data.frame(readxl::read_excel(path=fname_xlsx, sheet=table_name, na=c("", " ", "\r", "\n", "\r\n", "NA", "na", "N/A", "NaN"), guess_max=round(0.01*.Machine$integer.max)), check.names=FALSE)
                for (j in 1:ncol(df)) {
                    # j = 1
                    vec_y = tryCatch(as.numeric(df[, j]), warning=function(x){"SKIP"})
                    if (!is.na(vec_y[1]) && (vec_y[1] == "SKIP")) {
                        next
                    } else {
                        df[, j] = vec_y
                    }
                }
            }
            ### If the tab in the MS Excel file is empty then we use the tab-delimited file
            if (is.null(fname_xlsx) || (nrow(df) == 0)) {
                if ((table_name == "phenotypes") & !is.null(fname_phenotypes_tsv)) {
                    df = utils::read.delim(fname_phenotypes_tsv, header=TRUE, sep="\t", check.names=FALSE)
                } else if ((table_name == "environments") & !is.null(fname_environments_tsv)) {
                    df = utils::read.delim(fname_environments_tsv, header=TRUE, sep="\t", check.names=FALSE)
                } else if ((table_name == "genotypes") & !is.null(fname_genotypes_tsv)) {
                    df = utils::read.delim(fname_genotypes_tsv, header=TRUE, sep="\t", check.names=FALSE)
                } else {
                    df = data.frame()
                }
            }
        }
        if (nrow(df) > 0) {
            list_df_data_tables[[paste0("df_", table_name)]] = df
        }
    }
    out = fn_initialise_db(fname_db=fname_db, list_df_data_tables=list_df_data_tables, verbose=TRUE)
    if (methods::is(out, "dbError")) {
        return(out)
    }
    if (verbose) {
        print(paste0("Please find the database file at: '", fname_db, "'"))
        print("Information of each table:")
        database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
        for (table_name in GLOBAL_df_valid_tables()$NAME) {
            # table_name = GLOBAL_df_valid_tables()$NAME[1]
            df_table_info_summary = DBI::dbGetQuery(conn=database, statement=paste0("PRAGMA TABLE_INFO(", table_name, ")"))[, 2:3]
            print("#########################")
            print(table_name)
            print("#########################")
            print(df_table_info_summary)
        }
        DBI::dbDisconnect(conn=database)
    }
    return(0)
}

#' Update database using an MS Excel file
#' @param fname_db name of the SQLite database file.
#' @param fname_xlsx name of the MS Excel file with 3 tabs where each tab represent one of 
#'  the three data tables including the required columns for each. These tabs should be
#'  named "phenotypes", "environments", and "genotypes". A tab can be empty, 
#'  e.g. "genotypes" tab can be left empty if the genotype file is massive, in which case
#'  the tab-delimited allele frequency table file may be specified.
#'  See gp::GLOBAL_list_required_colnames_per_table() for the required columns per table.
#'  (Default=NULL)
#' @param fname_phenotypes_tsv name of the phenotypes table file. This file is tab-delimited.
#'  See gp::GLOBAL_list_required_colnames_per_table()$phenotypes for the required columns.
#'  (Default=NULL)
#' @param fname_environments_tsv name of the environments table file. This file is tab-delimited.
#'  See gp::GLOBAL_list_required_colnames_per_table()$environments for the required columns.
#'  (Default=NULL)
#' @param fname_genotypes_tsv name of the allele frequency table file. This is a tab-delimited
#'  file with a header line and the first 3 columns refer to the chromosome (chr), position (pos),
#'  and allele (allele), with subsequent columns referring to the allele frequencies of a sample.
#'  Names of the samples in the header line can be any unique string of characters. (Default=NULL)
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok: 0
#'  - Err: dbError
#' @examples
#' list_fnames_tables = fn_simulate_tables(
#'         n_entries=50,
#'         n_dates=3,
#'         n_sites=3,
#'         n_treatments=3,
#'         n_loci=10e3,
#'         save_data_tables=TRUE)$list_fnames_tables
#' list_df_initial = list()
#' list_df_update = list()
#' for (table_name in GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"]) {
#'     df = as.data.frame(readxl::read_excel(
#'         path=list_fnames_tables$fname_data_tables, 
#'         sheet=table_name, 
#'         guess_max=round(0.01*.Machine$integer.max)), 
#'         check.names=FALSE)
#'     n = nrow(df)
#'     eval(parse(text=paste0("list_df_initial$", table_name, " = df[1:ceiling(n/2), ]")))
#'     eval(parse(text=paste0("list_df_update$", table_name, " = df[(ceiling(n/2)+1):n, ]")))
#' }
#' fname_xlsx_initial = "test_initial.xlsx"
#' fname_xlsx_update = "test_update.xlsx"
#' writexl::write_xlsx(x=list_df_initial, path=fname_xlsx_initial)
#' writexl::write_xlsx(x=list_df_update, path=fname_xlsx_update)
#' ### Initialise the database
#' fn_create_database_from_xlsx_or_tsv(fname_xlsx=fname_xlsx_initial)
#' ### Update
#' fname_xlsx = fname_xlsx_update
#' fname_db = gsub(".xlsx$", ".sqlite", fname_xlsx_initial)
#' fn_update_database_from_xlsx_or_tsv(fname_xlsx=fname_xlsx, fname_db=fname_db)
#' unlink(fname_xlsx_initial)
#' unlink(fname_xlsx_update)
#' unlink(fname_db)
#' @export
fn_update_database_from_xlsx_or_tsv = function(
    fname_db,
    fname_xlsx=NULL,
    fname_phenotypes_tsv=NULL,
    fname_environments_tsv=NULL,
    fname_genotypes_tsv=NULL,
    verbose=TRUE)
{
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(
    #         n_entries=50,
    #         n_dates=3,
    #         n_sites=3,
    #         n_treatments=3,
    #         n_loci=10e3,
    #         save_data_tables=TRUE)$list_fnames_tables
    # list_df_initial = list()
    # list_df_update = list()
    # for (table_name in GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"]) {
    #     df = as.data.frame(readxl::read_excel(
    #         path=list_fnames_tables$fname_data_tables, 
    #         sheet=table_name, 
    #         guess_max=round(0.01*.Machine$integer.max)), 
    #         check.names=FALSE)
    #     n = nrow(df)
    #     eval(parse(text=paste0("list_df_initial$", table_name, " = df[1:ceiling(n/2), ]")))
    #     eval(parse(text=paste0("list_df_update$", table_name, " = df[(ceiling(n/2)+1):n, ]")))
    # }
    # fname_xlsx_initial = "test_initial.xlsx"
    # fname_xlsx_update = "test_update.xlsx"
    # writexl::write_xlsx(x=list_df_initial, path=fname_xlsx_initial)
    # writexl::write_xlsx(x=list_df_update, path=fname_xlsx_update)
    # ### Initialise the database
    # fn_create_database_from_xlsx_or_tsv(fname_xlsx=fname_xlsx_initial)
    # fname_xlsx = fname_xlsx_update
    # fname_db = gsub(".xlsx$", ".sqlite", fname_xlsx_update)
    # fname_genotypes_tsv = NULL
    # verbose = TRUE
    ################################################################
    ### Check the input file/s
    if (!is.null(fname_xlsx)) {
        if (!file.exists(fname_xlsx)) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_update_database_from_xlsx_or_tsv(...): ",
                "The '", fname_xlsx,"' MS Excel file does not exist."))
            return(error)
        }
    }
    if (!is.null(fname_phenotypes_tsv)) {
        if (!file.exists(fname_phenotypes_tsv)) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_update_database_from_xlsx_or_tsv(...): ",
                "The '", fname_phenotypes_tsv,"' tab-delimited phenotypes file does not exist."))
            return(error)
        }
    }
    if (!is.null(fname_environments_tsv)) {
        if (!file.exists(fname_environments_tsv)) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_update_database_from_xlsx_or_tsv(...): ",
                "The '", fname_environments_tsv,"' tab-delimited environments file does not exist."))
            return(error)
        }
    }
    if (!is.null(fname_genotypes_tsv)) {
        if (!file.exists(fname_genotypes_tsv)) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_update_database_from_xlsx_or_tsv(...): ",
                "The '", fname_genotypes_tsv,"' tab-delimited genotypes file does not exist."))
            return(error)
        }
    }
    if (is.null(fname_xlsx) & is.null(fname_phenotypes_tsv) & is.null(fname_environments_tsv) & is.null(fname_genotypes_tsv)) {
       error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_update_database_from_xlsx_or_tsv(...): ",
            "No input files supplied."))
        return(error)
    }
    ### Check the database file
    if (!file.exists(fname_db)) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error in fn_update_database_from_xlsx_or_tsv(...): ",
            "The '", fname_db, "' database file does not exist."))
        return(error)
    }
    ### Extract the data tables
    list_df_data_tables = list(df_phenotypes=NULL, df_environments=NULL, df_genotypes=NULL)
    error = NULL
    for (table_name in GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"]) {
        # table_name = GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"][3]
        if ((table_name == "genotypes") && !is.null(fname_genotypes_tsv)) {
            df = utils::read.delim(fname_genotypes_tsv, check.names=FALSE)
        } else {
            ### The tables in the MS Excel file takes priority
            if (!is.null(fname_xlsx)) {
                ### Use a large guess_max value to avoid mis classifying numeric columns as binary when there are many missing initial values.
                ### Also define a bunch of missing values which can be invisible in MS Excel, e.g. \r\n.
                ### Additionally, try converting each column into numerics but only if they were actually numerics, i.e. no warnings.
                df = as.data.frame(readxl::read_excel(path=fname_xlsx, sheet=table_name, na=c("", " ", "\r", "\n", "\r\n", "NA", "na", "N/A", "NaN"), guess_max=round(0.01*.Machine$integer.max)), check.names=FALSE)
                for (j in 1:ncol(df)) {
                    # j = 1
                    vec_y = tryCatch(as.numeric(df[, j]), warning=function(x){"SKIP"})
                    if (!is.na(vec_y[1]) && (vec_y[1] == "SKIP")) {
                        next
                    } else {
                        df[, j] = vec_y
                    }
                }
            }
            ### If MS Excel file is NULL or a tab is empty then we use the tab-delimited file
            if (is.null(fname_xlsx) || (nrow(df) == 0)) {
                if ((table_name == "phenotypes") & !is.null(fname_phenotypes_tsv)) {
                    df = utils::read.delim(fname_phenotypes_tsv, header=TRUE, sep="\t", check.names=FALSE)
                } else if ((table_name == "environments") & !is.null(fname_environments_tsv)) {
                    df = utils::read.delim(fname_environments_tsv, header=TRUE, sep="\t", check.names=FALSE)
                } else if ((table_name == "genotypes") & !is.null(fname_genotypes_tsv)) {
                    df = utils::read.delim(fname_genotypes_tsv, header=TRUE, sep="\t", check.names=FALSE)
                } else {
                    df = data.frame()
                }
            }
        }
        if (nrow(df) > 0) {
            list_df_data_tables[[paste0("df_", table_name)]] = df
        } else {
            next
        }
        out = fn_update_database(fname_db=fname_db, df=df, table_name=table_name, verbose=verbose)
        if (methods::is(out, "dbError")) {
            if (is.null(error)) {
                error = out
            } else {
                error = chain(error, out)
            }
        }
    }
    if (!is.null(error)) {
        return(error)
    }
    if (verbose) {
        print(paste0("Please find the updated database file at: '", fname_db, "'"))
        print("Information of each table:")
        database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
        for (table_name in GLOBAL_df_valid_tables()$NAME) {
            # table_name = GLOBAL_df_valid_tables()$NAME[1]
            df_table_info_summary = DBI::dbGetQuery(conn=database, statement=paste0("PRAGMA TABLE_INFO(", table_name, ")"))[, 2:3]
            print("#########################")
            print(table_name)
            print("#########################")
            print(df_table_info_summary)
        }
        DBI::dbDisconnect(conn=database)
    }
    return(0)
}
