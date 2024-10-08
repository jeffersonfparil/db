% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{fn_add_hash_UID_and_remove_duplicate_rows}
\alias{fn_add_hash_UID_and_remove_duplicate_rows}
\title{Add hash and UID columns, then remove duplicate rows as defined by the required columns for each base and data table.}
\usage{
fn_add_hash_UID_and_remove_duplicate_rows(
  df,
  database,
  table_name,
  verbose = TRUE
)
}
\arguments{
\item{df}{data frame representing a base (entries, dates, sites, treatments, traits, abiotics,
and loci) or data (phenotypes, environments and genotypes) table}

\item{database}{an open SQLite database connection}

\item{table_name}{name of the base or data table represented by df}

\item{verbose}{Show messages? (Default=TRUE)}
}
\value{
\itemize{
\item Ok:
$df: data frame without quotes and newline characters
$df_duplicated_in_database: data frame without quotes and newline characters with UID duplicates in the database
\item Err: dbError
}
}
\description{
Add hash and UID columns, then remove duplicate rows as defined by the required columns for each base and data table.
}
\examples{
list_fnames_tables = fn_simulate_tables(
     n_entries=50,
     n_dates=3,
     n_sites=3,
     n_treatments=3,
     n_loci=10e3,
     save_data_tables=TRUE)$list_fnames_tables
df = utils::read.delim(list_fnames_tables$fname_phenotypes, header=TRUE)
database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname="test.sqlite")
df = fn_add_hash_UID_and_remove_duplicate_rows(df=df, database=database, 
     table_name="phenotypes", verbose=TRUE)$df
DBI::dbDisconnect(database)
unlink("test.sqlite")
}
