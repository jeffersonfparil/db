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
    # fname_phenotypes_tsv = NULL
    # fname_environments_tsv = NULL
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
        # table_name = GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"][1]
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

#' Export phenotype and genotype data from the database using vectors of 
#' @param fname_db name of the SQLite database file.
#' @param vec_ENTRY vector of entry names to extract. (Default=c("*"))
#' @param vec_REPLICATION vector of replications to extract. (Default=c("*"))
#' @param vec_TREATMENT vector of treatments to extract. (Default=c("*"))
#' @param vec_SITE vector of sites to extract. (Default=c("*"))
#' @param vec_YEAR vector of years to extract. (Default=c("*"))
#' @param vec_MONTH vector of months to extract. (Default=c("*"))
#' @param vec_DAY vector of days to extract. (Default=c("*"))
#' @param vec_trait_names vector of trait names to include in the phenotypes table. (Default=c("*"))
#' @param fname_basename_out base name of the output/queried phenotype and genotype files. (Default=NULL)
#' @param overwrite Overwrite the queried/output phenotypes and genotypes files? (Default=FALSE)
#' @param verbose Show messages? (Default=TRUE)
#' @returns
#'  - Ok: 
#'      $fname_phenotypes_tsv: name of the output/queried phenotypes table file. 
#'          This file is tab-delimited.
#'      $fname_genotypes_tsv: name of the output/queried allele frequency table file. 
#'          This is a tab-delimited file with a header line and the first 3 columns refer to the 
#'          chromosome (chr), position (pos), and allele (allele), with subsequent columns referring 
#'          to the allele frequencies of a sample.
#'  - Err: dbError
#' @examples
#' fname_xlsx = fn_simulate_tables(
#'     n_entries=50,
#'     n_dates=3,
#'     n_sites=3,
#'     n_treatments=3,
#'     n_loci=10e3,
#'     save_data_tables=TRUE)$list_fnames_tables$fname_data_tables
#' fname_db = "test.sqlite"
#' fn_create_database_from_xlsx_or_tsv(fname_db=fname_db, fname_xlsx=fname_xlsx, overwrite=TRUE)
#' database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
#' vec_entry_names = sample(DBI::dbGetQuery(conn=database, 
#'      statement="SELECT ENTRY FROM entries")[, 1], size=3)
#' DBI::dbDisconnect(conn=database)
#' list_fnames_out = fn_export_phenotypes_and_genotypes_data_from_database(
#'      fname_db=fname_db, 
#'      vec_ENTRY=vec_entry_names)
#' unlink(list_fnames_out$fname_phenotypes)
#' unlink(list_fnames_out$fname_genotypes)
#' @export
fn_export_phenotypes_and_genotypes_data_from_database = function(
    fname_db, 
    vec_ENTRY=c("*"), 
    vec_REPLICATION=c("*"),
    vec_TREATMENT=c("*"),
    vec_SITE=c("*"),
    vec_YEAR=c("*"),
    vec_MONTH=c("*"),
    vec_DAY=c("*"),
    vec_trait_names=c("*"),
    fname_basename_out=NULL,
    overwrite=FALSE,
    verbose=TRUE
) {
    ################################################################
    ### TEST
    # set.seed(42069)
    # fname_xlsx = fn_simulate_tables(
    #     n_entries=50,
    #     n_dates=3,
    #     n_sites=3,
    #     n_treatments=3,
    #     n_loci=10e3,
    #     save_data_tables=TRUE)$list_fnames_tables$fname_data_tables
    # fname_db = "test.sqlite"
    # fn_create_database_from_xlsx_or_tsv(fname_db=fname_db, fname_xlsx=fname_xlsx, overwrite=TRUE)
    # vec_ENTRY = c("*")
    # vec_REPLICATION = c("*")
    # vec_TREATMENT = c("*")
    # vec_SITE = c("*")
    # vec_YEAR = c("*")
    # vec_MONTH = c("*")
    # vec_DAY = c("*")
    # vec_trait_names = c("*")
    # fname_basename_out = NULL
    # overwrite = TRUE
    # verbose = TRUE
    ################################################################
    ### Check database file
    if (!file.exists(fname_db)) {
        error = methods::new("dbError",
            code=000,
            message=paste0("The database: ", fname_db, " does not exist."))
        return(error)
    }
    ### Open the database connection
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    ### Define the query vectors if defaults were used
    if (vec_ENTRY[1]=="*") {
        vec_ENTRY = DBI::dbGetQuery(conn=database, statement="SELECT ENTRY FROM entries")[,1]
    }
    if (vec_REPLICATION[1]=="*") {
        vec_REPLICATION = unique(DBI::dbGetQuery(conn=database, statement="SELECT REPLICATION FROM phenotypes")[,1])
    }
    if (vec_TREATMENT[1]=="*") {
        vec_TREATMENT = unique(DBI::dbGetQuery(conn=database, statement="SELECT TREATMENT FROM phenotypes")[,1])
    }
    if (vec_SITE[1]=="*") {
        vec_SITE = unique(DBI::dbGetQuery(conn=database, statement="SELECT SITE FROM phenotypes")[,1])
    }
    if (vec_YEAR[1]=="*") {
        vec_YEAR = unique(DBI::dbGetQuery(conn=database, statement="SELECT YEAR FROM phenotypes")[,1])
    }
    if (vec_MONTH[1]=="*") {
        vec_MONTH = unique(DBI::dbGetQuery(conn=database, statement="SELECT MONTH FROM phenotypes")[,1])
    }
    if (vec_DAY[1]=="*") {
        vec_DAY = unique(DBI::dbGetQuery(conn=database, statement="SELECT DAY FROM phenotypes")[,1])
    }
    if (vec_trait_names[1]=="*") {
        vec_trait_names = unique(DBI::dbGetQuery(conn=database, statement="SELECT TRAIT FROM traits")[,1])
    }
    ### Extract the corresponding entry UIDs of the entry names
    df_entries = DBI::dbGetQuery(conn=database, statement="SELECT * FROM entries")
    vec_ENTRY_UID = df_entries$ENTRY_UID[df_entries$ENTRY %in% vec_ENTRY]
    if (length(vec_ENTRY_UID) == 0) {
        if (verbose) {
            print(paste0("Error: the requested entries do not exist in the database: ", paste(vec_ENTRY, collapse=", ")))
            print("Error: the requested entries do not exist in the database.")
        }
        error = methods::new("dbError",
            code=000,
            message=paste0("Error: the requested entries do not exist in the database"))
        return(error)
    }
    ### Define the list of table filters for querying
    list_tables_and_filters = list(
        entries=list(
            key_names=c("ENTRY_UID"), 
            column_names=c("*"),
            list_filters=list(ENTRY_UID=vec_ENTRY_UID)
        ),
        phenotypes=list(
            key_names=c("ENTRY_UID"),
            column_names=unique(c("REPLICATION", "TREATMENT", "SITE", "YEAR", "MONTH", "DAY", vec_trait_names)),
            list_filters=list(
                REPLICATION=vec_REPLICATION,
                TREATMENT=vec_TREATMENT,
                SITE=vec_SITE,
                YEAR=vec_YEAR,
                MONTH=vec_MONTH,
                DAY=vec_DAY
            )
        ),
        genotypes=list(
            key_names=c("ENTRY_UID"),
            column_names=c("*"),
            list_filters=NULL
        )
    )
    ### Check output files
    if (is.null(fname_basename_out)) {
        query_hash = rlang::hash(list_tables_and_filters)
        fname_phenotypes_tsv = gsub(".sqlite$", paste0("-exported_phenotypes-hash_", query_hash, ".tsv"), fname_db)
        fname_genotypes_tsv = gsub(".sqlite$", paste0("-exported_genotypes-hash_", query_hash, ".tsv"), fname_db)
    }
    if (!overwrite & file.exists(fname_phenotypes_tsv)) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error: the queried phenotypes file: ", fname_phenotypes_tsv, " exists. Use a different output filename or set overwrite=TRUE."))
        return(error)
    }
    if (!overwrite & file.exists(fname_genotypes_tsv)) {
        error = methods::new("dbError",
            code=000,
            message=paste0("Error: the queried genotypes file: ", fname_genotypes_tsv, " exists. Use a different output filename or set overwrite=TRUE."))
        return(error)
    }
    if (verbose) {print("Extracting and merging data from phenotypes and genotypes data from the database...")}
    df_query = db::fn_query_and_left_join_tables(database=database, list_tables_and_filters=list_tables_and_filters, unique_column_name=NULL, verbose=verbose)
    if (methods::is(df_query, "dbError")) {
        return(df_query)
    }
    if (nrow(df_query) == 0) {
        if (verbose) {
            print("Error: no data in the database matching the following requirements: ")
            print(list_tables_and_filters)
        }
        error = methods::new("dbError",
            code=000,
            message=paste0("Error: no data in the database matching the following requirements: ", list_tables_and_filters))
        return(error)
    }
    if (verbose) {print("Extracting the phenotypes table...")}
    df_phenotypes = df_query[, 1:(ncol(df_query)-2), drop=FALSE]
    if (verbose) {
        print("Deserialising the genotype data into an allele frequency table...")
        print("Removing entries without genotype data...")
    }
    vec_idx_retain = which(!is.na(df_query[, ncol(df_query)-1]))
    df_genotypes = df_query[vec_idx_retain, (ncol(df_query)-1):ncol(df_query), drop=FALSE]
    df_allele_frequencies_table = db::fn_deserialise_genotype_data(database=database, df_genotypes=df_genotypes, verbose=verbose)
    DBI::dbDisconnect(conn=database)
    if (verbose) {print("Save queried phenotypes and allele frequency table...")}
    utils::write.table(df_phenotypes, file=fname_phenotypes_tsv, sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE)
    utils::write.table(df_allele_frequencies_table, file=fname_genotypes_tsv, sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE)
    if (verbose) {
        print(paste0("Queried phenotypes: ", fname_phenotypes_tsv))
        print(paste0("Queried genotypes: ", fname_genotypes_tsv))
    }
    return(list(
        fname_phenotypes_tsv=fname_phenotypes_tsv,
        fname_genotypes_tsv=fname_genotypes_tsv
    ))
}

fn_export_phenotypes_environments_and_genotypes_data_from_database = function() {
    print("TODO: export all 3 data tables from the database.")
}