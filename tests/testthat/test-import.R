# library(testthat)
# source("R/import.R")

test_that("fn_define_hash_and_UID_prefix", {
    prefix_1 = fn_define_hash_and_UID_prefix(table_name="phenotypes")
    prefix_2 = fn_define_hash_and_UID_prefix(table_name="entries")
    error = fn_define_hash_and_UID_prefix(table_name="NOT_A_BASE_OR_DATA_TABLE_NAME")
    expect_equal(prefix_1, "PHENOTYPE")
    expect_equal(prefix_2, "ENTRY")
    expect_equal(class(error)[1], "dbError")
})

test_that("fn_check_import_inputs", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    err = methods::new("dbError", code=000, message="!!!Error type!!!!")
    df_empty = df[df$ENTRY_UID==0, , drop=FALSE]
    null_error = fn_check_import_inputs(df=df, database=database, table_name="phenotypes", check_UIDs=FALSE, check_if_table_exists=FALSE)
    error_df_1 = fn_check_import_inputs(df=err, database=database, table_name="phenotypes", check_UIDs=FALSE, check_if_table_exists=FALSE)
    error_database = fn_check_import_inputs(df=df, database=err, table_name="phenotypes", check_UIDs=FALSE, check_if_table_exists=FALSE)
    error_df_2 = fn_check_import_inputs(df=df_empty, database=database, table_name="phenotypes", check_UIDs=FALSE, check_if_table_exists=FALSE)
    error_table_name = fn_check_import_inputs(df=df, database=database, table_name="unexpected_table_name", check_UIDs=FALSE, check_if_table_exists=FALSE)
    error_missing_req_columns = fn_check_import_inputs(df=df[, -1:-5], database=database, table_name="phenotypes", check_UIDs=FALSE, check_if_table_exists=FALSE)
    error_missing_UID = fn_check_import_inputs(df=df, database=database, table_name="phenotypes", check_UIDs=TRUE, check_if_table_exists=FALSE)
    error_uninitialised_table = fn_check_import_inputs(df=df, database=database, table_name="phenotypes", check_UIDs=FALSE, check_if_table_exists=TRUE)
    expect_equal(is.null(null_error), TRUE)
    expect_equal(class(error_df_1)[1], "dbError")
    expect_equal(class(error_database)[1], "dbError")
    expect_equal(class(error_df_2)[1], "dbError")
    expect_equal(class(error_table_name)[1], "dbError")
    expect_equal(class(error_missing_req_columns)[1], "dbError")
    expect_equal(class(error_missing_UID)[1], "dbError")
    expect_equal(class(error_uninitialised_table)[1], "dbError")
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_remove_quotes_and_newline_characters_in_data", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    df[1,1] = paste0("'", df[1,1], "'")
    df[2,2] = paste0('"', df[2,2], '"')
    df[3,3] = paste0(df[3,3], "\n")
    df[4,4] = paste0(df[4,4], "\r\n")
    df = fn_remove_quotes_and_newline_characters_in_data(df=df, verbose=TRUE)
    expect_equal(grepl("'", df[1,1]), FALSE)
    expect_equal(grepl('"', df[2,2]), FALSE)
    expect_equal(grepl("\n", df[3,3]), FALSE)
    expect_equal(grepl("\r\n", df[4,4]), FALSE)
})

test_that("fn_add_POSIX_time", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    vec_POSIX_DATE_TIME = df$POSIX_DATE_TIME
    df = df[, !(colnames(df) %in% "POSIX_DATE_TIME")]
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    df = fn_add_POSIX_time(df=df, database=database, table_name="phenotypes", bool_add_FVI_year_season=TRUE, verbose=TRUE)
    expect_equal(sum(df$POSIX_DATE_TIME == vec_POSIX_DATE_TIME), nrow(df))
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_rename_columns_and_remove_duplicate_columns", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    colnames(df) = tolower(colnames(df))
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    df = fn_rename_columns_and_remove_duplicate_columns(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    expect_equal(sum(colnames(df) == toupper(colnames(df))), ncol(df))
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_add_hash_UID_and_remove_duplicate_rows", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    df = rbind(df[1, ], df)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    df_with_hash_UID = fn_add_hash_UID_and_remove_duplicate_rows(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    expect_equal(ncol(df_with_hash_UID), ncol(df)+2)
    expect_equal(nrow(df_with_hash_UID), nrow(df)-1)
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_convert_allele_frequency_table_into_blobs_and_dfs", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    list_df_genotypes_df_loci_df_entries = fn_convert_allele_frequency_table_into_blobs_and_dfs(df=df, database=database, table_name="genotypes", verbose=TRUE)
    expect_equal(length(list_df_genotypes_df_loci_df_entries), 3)
    for (i in sample(4:ncol(df), size=10)) {
        expect_equal(length(list_df_genotypes_df_loci_df_entries$df_genotypes$BLOB[[i-3]]), length(serialize(object=df[, i], connection=NULL)))
    }
    expect_equal(nrow(list_df_genotypes_df_loci_df_entries$df_loci), nrow(df))
    expect_equal(nrow(list_df_genotypes_df_loci_df_entries$df_entries), ncol(df)-3)
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_prepare_data_table_and_extract_base_tables", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df_phenotypes = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df_phenotypes, database=database, table_name="phenotypes", verbose=TRUE)
    expect_equal(names(list_df_data_and_base_tables), c("df_possibly_modified", "df_entries", "df_dates", "df_sites", "df_treatments", "df_traits", "df_abiotics", "df_loci"))
    expect_equal(list_df_data_and_base_tables$df_possibly_modified[, 3:(ncol(df_phenotypes)+2)], df_phenotypes)
    expect_equal(nrow(list_df_data_and_base_tables$df_entries), 50)
    expect_equal(nrow(list_df_data_and_base_tables$df_dates), 3)
    expect_equal(nrow(list_df_data_and_base_tables$df_sites), 3)
    expect_equal(nrow(list_df_data_and_base_tables$df_treatments), 3)
    expect_equal(nrow(list_df_data_and_base_tables$df_traits), 50)
    expect_equal(is.null(list_df_data_and_base_tables$df_abiotics), TRUE)
    expect_equal(is.null(list_df_data_and_base_tables$df_loci), TRUE)
    df_genotypes = utils::read.delim(list_fnames_tables$fname_genotypes, header=TRUE)
    list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df_genotypes, database=database, table_name="genotypes", verbose=TRUE)
    expect_equal(names(list_df_data_and_base_tables), c("df_possibly_modified", "df_entries", "df_dates", "df_sites", "df_treatments", "df_traits", "df_abiotics", "df_loci"))
    expect_equal(dim(list_df_data_and_base_tables$df_possibly_modified), c(ncol(df_genotypes)-3, 2))
    expect_equal(nrow(list_df_data_and_base_tables$df_entries), 50)
    expect_equal(is.null(list_df_data_and_base_tables$df_dates), TRUE)
    expect_equal(is.null(list_df_data_and_base_tables$df_sites), TRUE)
    expect_equal(is.null(list_df_data_and_base_tables$df_treatments), TRUE)
    expect_equal(is.null(list_df_data_and_base_tables$df_traits), TRUE)
    expect_equal(is.null(list_df_data_and_base_tables$df_abiotics), TRUE)
    expect_equal(nrow(list_df_data_and_base_tables$df_loci), 10e3)
    ### If a data table is improperly imported into the database, i.e. without adding hash and UID columns:
    DBI::dbWriteTable(conn=database, name="phenotypes", value=df_phenotypes)
    list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df_phenotypes, database=database, table_name="phenotypes", verbose=TRUE)
    expect_equal(class(list_df_data_and_base_tables)[1], "dbError")
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("list_set_classification_of_rows", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    ### If the table does not exist in the database yet
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    list_set_classification_of_rows = fn_set_classification_of_rows(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    expect_equal(class(list_set_classification_of_rows)[1], "dbError")
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
    ### Prepare the tables
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    df = list_df_data_and_base_tables$df_possibly_modified
    df_first_half = droplevels(df[1:floor(nrow(df)/2), ])
    ### Import the first half of the data table into the database
    DBI::dbWriteTable(conn=database, name="phenotypes", value=df_first_half)
    ### Detect row intersections
    list_set_classification_of_rows = fn_set_classification_of_rows(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    expect_equal(list_set_classification_of_rows$n_existing_rows, nrow(df_first_half))
    expect_equal(list_set_classification_of_rows$n_incoming_rows, nrow(df))
    expect_equal(list_set_classification_of_rows$n_intersecting_rows, floor(nrow(df)/2))
    expect_equal(list_set_classification_of_rows$n_rows_exclusive_to_existing_table, 0)
    expect_equal(list_set_classification_of_rows$n_rows_exclusive_to_incoming_table, nrow(df) - nrow(df_first_half))
    expect_equal(list_set_classification_of_rows$vec_bool_rows_exclusive_to_existing_table, rep(FALSE, times=nrow(df_first_half)))
    expect_equal(list_set_classification_of_rows$vec_bool_rows_exclusive_to_incoming_table, c(rep(FALSE, times=nrow(df_first_half)), rep(TRUE, times=(nrow(df)-nrow(df_first_half)))))
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("list_set_classification_of_columns", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    ### if the table does not exist in the database yet
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    list_set_classification_of_columns = fn_set_classification_of_columns(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    expect_equal(class(list_set_classification_of_columns)[1], "dbError")
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
    ### Prepare the tables
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    df = list_df_data_and_base_tables$df_possibly_modified
    df_first_half = droplevels(df[, 1:floor(ncol(df)/2)])
    ### Import the first half of the data table into the database
    DBI::dbWriteTable(conn=database, name="phenotypes", value=df_first_half)
    ### Detect column intersections
    list_set_classification_of_columns = fn_set_classification_of_columns(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    expect_equal(list_set_classification_of_columns$n_existing_columns, ncol(df_first_half))
    expect_equal(list_set_classification_of_columns$n_incoming_columns, ncol(df))
    expect_equal(list_set_classification_of_columns$n_intersecting_columns, floor(ncol(df)/2))
    expect_equal(list_set_classification_of_columns$n_columns_exclusive_to_existing_table, 0)
    expect_equal(list_set_classification_of_columns$n_columns_exclusive_to_incoming_table, ncol(df) - ncol(df_first_half))
    expect_equal(list_set_classification_of_columns$vec_bool_columns_exclusive_to_existing_table, rep(FALSE, times=ncol(df_first_half)))
    expect_equal(list_set_classification_of_columns$vec_bool_columns_exclusive_to_incoming_table, c(rep(FALSE, times=ncol(df_first_half)), rep(TRUE, times=(ncol(df)-ncol(df_first_half)))))
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_add_new_columns", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    ### Prepare the tables
    list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df, 
         database=database, table_name="phenotypes", verbose=TRUE)
    df = list_df_data_and_base_tables$df_possibly_modified
    df_first_half = droplevels(df[, 1:floor(ncol(df)/2)])
    ### Import the first half of the data table into the database
    DBI::dbWriteTable(conn=database, name="phenotypes", value=df_first_half)
    expect_equal(colnames(df_first_half), DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_INFO(phenotypes)")$name)
    ### Add new columns
    database = fn_add_new_columns(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    expect_equal(colnames(df), DBI::dbGetQuery(conn=database, statement="PRAGMA TABLE_INFO(phenotypes)")$name)
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("fn_append", {
    set.seed(123)
    list_fnames_tables = fn_simulate_tables(
         n_entries=50,
         n_dates=3,
         n_sites=3,
         n_treatments=3,
         n_loci=10e3,
         save_data_tables=TRUE)$list_fnames_tables
    df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    ### Prepare the tables
    list_df_data_and_base_tables = fn_prepare_data_table_and_extract_base_tables(df=df, 
         database=database, table_name="phenotypes", verbose=TRUE)
    df = list_df_data_and_base_tables$df_possibly_modified
    n = nrow(df); p = ncol(df)
    # vec_idx_columns_HASH_UID_and_required = which(colnames(df) %in% c("PHENOTYPE_HASH", "PHENOTYPE_UID", GLOBAL_list_required_colnames_per_table()$phenotypes))
    vec_idx_columns_HASH_UID_and_required = 1:17
    df_q1 = droplevels(df[1:floor(n/2),     1:floor(p/2)])
    df_q2 = droplevels(df[1:floor(n/2),     c(vec_idx_columns_HASH_UID_and_required, (floor(p/2)+1):p)])
    df_q3 = droplevels(df[(floor(n/2)+1):n, c(vec_idx_columns_HASH_UID_and_required, (floor(p/2)+1):p)])
    df_q4 = droplevels(df[(floor(n/2)+1):n, 1:floor(p/2)])
    df_centre = droplevels(df[floor(n*(1/3)):floor(n*(2/3)), c(vec_idx_columns_HASH_UID_and_required, floor(p*(1/3)):floor(p*(2/3)))])
    DBI::dbDisconnect(database)
    ### Add new columns
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    DBI::dbWriteTable(conn=database, name="phenotypes", value=df_q1)
    database = fn_append(df=df_q2, database=database, table_name="phenotypes", verbose=TRUE)
    df_q1_U_q2 = cbind(df_q1, df_q2[, (length(vec_idx_columns_HASH_UID_and_required)+1):ncol(df_q2)])
    df_from_db = DBI::dbGetQuery(conn=database, statement="SELECT * FROM phenotypes")
    expect_equal(df_q1_U_q2, df_from_db)
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
    ### Add new rows
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    DBI::dbWriteTable(conn=database, name="phenotypes", value=df_q1)
    database = fn_append(df=df_q4, database=database, table_name="phenotypes", verbose=TRUE)
    df_q1_U_q4 = rbind(df_q1, df_q4)
    df_from_db = DBI::dbGetQuery(conn=database, statement="SELECT * FROM phenotypes")
    expect_equal(df_q1_U_q4, df_from_db)
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
    ### Add new columns and new rows
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    DBI::dbWriteTable(conn=database, name="phenotypes", value=df_q1)
    database = fn_append(df=df_q3, database=database, table_name="phenotypes", verbose=TRUE)
    df_NA_q2 = data.frame(matrix(NA, nrow=nrow(df_q1), ncol=ncol(df_q3[, (length(vec_idx_columns_HASH_UID_and_required)+1):ncol(df_q3)])))
    colnames(df_NA_q2) = colnames(df_q3[, (length(vec_idx_columns_HASH_UID_and_required)+1):ncol(df_q3)])
    df_NA_q4 = data.frame(matrix(NA, nrow=nrow(df_q3), ncol=ncol(df_q1)))
    colnames(df_NA_q4) = colnames(df_q4)
    df_NA_q4[, 1:length(vec_idx_columns_HASH_UID_and_required)] = df_q4[, 1:length(vec_idx_columns_HASH_UID_and_required)]
    df_q1_U_q3 = rbind(
        cbind(df_q1,    df_NA_q2),
        cbind(df_NA_q4, df_q3[, (length(vec_idx_columns_HASH_UID_and_required)+1):ncol(df_q3)])
    )
    df_from_db = DBI::dbGetQuery(conn=database, statement="SELECT * FROM phenotypes")
    expect_equal(df_q1_U_q3, df_from_db)
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
    ### Initialise with centre and add the entire table
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
    DBI::dbWriteTable(conn=database, name="phenotypes", value=df_centre)
    database = fn_append(df=df, database=database, table_name="phenotypes", verbose=TRUE)
    df_from_db = DBI::dbGetQuery(conn=database, statement="SELECT * FROM phenotypes")
    expect_equal(df, df_from_db)
    DBI::dbDisconnect(database)
    unlink("test.sqlite")
})

test_that("", {
    set.seed(123)

})

test_that("", {
    set.seed(123)

})
