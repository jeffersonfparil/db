### Create a single database from a single excel file with 3 tabs, where each tab represent one of the three data tables including the required columns for each.
### A tab can be empty, e.g. "genotypes" tab can be left empty if the genotype file is massive in which case you should load the allele frequency table (tsv) file into R and use fn_update_database(...) to import the genotype data.
### Note to remove leading ', i.e. single quotes in front of numbers to avoid parsing numeric fields as strings.
fn_create_database_from_xlsx = function(fname_xlsx, fname_genotype_tsv=NULL, verbose=TRUE) {
    ################################################################
    ### TEST
    # list_fnames_tables = fn_simulate_tables(n_dates=3, n_sites=3, n_treatments=3, save_data_tables=TRUE)$list_fnames_tables
    # fname_xlsx = list_fnames_tables$fname_data_tables
    # fname_genotype_tsv = NULL
    # verbose = TRUE
    #
    # fname_xlsx = "/group/pasture/Jeff/RDatabase/res/STR_NUE_WUE_Hamilton_2023_2024.xlsx"
    # fname_genotype_tsv = NULL
    # # fname_genotype_tsv = "/group/pasture/Jeff/RDatabase/res/old/geno.tsv"
    # verbose = TRUE
    # fname_xlsx = "/group/pasture/Jeff/RDatabase/res/LUCERNE_CTL_Hamilton_2023_2024.xlsx" ### df_genotypes located elsewhere because of size, i.e. saved as a separated allele frequency table (tsv) file
    # fname_genotype_tsv = "/group/pasture/Jeff/lucerne/workdir/SKIM_HQ-1720884419.7498076.558359205-IMPUTED.tsv"
    # verbose = TRUE
    # fn_create_database_from_xlsx(fname_xlsx=fname_xlsx, fname_genotype_tsv=fname_genotype_tsv, verbose=verbose)
    ################################################################
    list_df_data_tables = list(df_phenotypes=NULL, df_environments=NULL, df_genotypes=NULL)
    for (table_name in GLOBAL_df_valid_tables()$NAME[GLOBAL_df_valid_tables()$CLASS=="data"]) {
        # table_name = GLOBAL_df_valid_tables()$NAME[8]
        if ((table_name == "genotypes") && !is.null(fname_genotype_tsv)) {
            df = utils::read.delim(fname_genotype_tsv, check.names=FALSE)
        } else {
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
        if (nrow(df) > 0) {
            list_df_data_tables[[paste0("df_", table_name)]] = df
        }
    }
    fname_db = file.path(dirname(fname_xlsx), gsub(".xlsx$", ".sqlite", basename(fname_xlsx)))
    out = fn_initialise_db(fname_db=fname_db, list_df_data_tables=list_df_data_tables, verbose=TRUE)
    if (verbose) {
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
    return(out)
}

# database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="res/STR_NUE_WUE_Hamilton_2023_2024.sqlite")
# database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="res/LUCERNE_CTL_Hamilton_2023_2024.sqlite")
# DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_LIST")
# DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_INFO(phenotypes)")
# DBI::dbGetQuery(conn=database, statement="SELECT * FROM entries LIMIT 10")
# DBI::dbGetQuery(conn=database, statement="SELECT * FROM entries WHERE POPULATION IN ('DB-MS-31-22-007') LIMIT 10")
# DBI::dbGetQuery(conn=database, statement="SELECT * FROM genotypes WHERE ENTRY_UID IN (1615) LIMIT 10")
# DBI::dbGetQuery(conn=database, statement="SELECT * FROM genotypes LIMIT 10")
# DBI::dbDisconnect(conn=database)

fn_update_database_from_xlsx = function() {
    NULL
}

fn_create_database_from_tsv = function() {
    NULL
}

fn_update_database_from_tsv = function() {
    NULL
}