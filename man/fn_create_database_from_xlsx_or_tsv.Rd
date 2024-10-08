% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{fn_create_database_from_xlsx_or_tsv}
\alias{fn_create_database_from_xlsx_or_tsv}
\title{Create a single database from an MS Excel file or tab-delimited files}
\usage{
fn_create_database_from_xlsx_or_tsv(
  fname_db = NULL,
  fname_xlsx = NULL,
  fname_phenotypes_tsv = NULL,
  fname_environments_tsv = NULL,
  fname_genotypes_tsv = NULL,
  overwrite = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{fname_db}{name of the SQLite database file. If NULL, then this will be saved in
the same directory as \code{fname_xlsx} or \code{fname_phenotypes_tsv} with the same base name
but with \code{.sqlite} extension instead of \code{.xlsx} or \code{.tsv}. (Default=NULL)}

\item{fname_xlsx}{name of the MS Excel file with 3 tabs where each tab represent one of
the three data tables including the required columns for each. These tabs should be
named "phenotypes", "environments", and "genotypes". A tab can be empty,
e.g. "genotypes" tab can be left empty if the genotype file is massive, in which case
the tab-delimited allele frequency table file may be specified.
See gp::GLOBAL_list_required_colnames_per_table() for the required columns per table.
(Default=NULL)}

\item{fname_phenotypes_tsv}{name of the phenotypes table file. This file is tab-delimited.
See gp::GLOBAL_list_required_colnames_per_table()$phenotypes for the required columns.
(Default=NULL)}

\item{fname_environments_tsv}{name of the environments table file. This file is tab-delimited.
See gp::GLOBAL_list_required_colnames_per_table()$environments for the required columns.
(Default=NULL)}

\item{fname_genotypes_tsv}{name of the allele frequency table file. This is a tab-delimited
file with a header line and the first 3 columns refer to the chromosome (chr), position (pos),
and allele (allele), with subsequent columns referring to the allele frequencies of a sample.
Names of the samples in the header line can be any unique string of characters. (Default=NULL)}

\item{overwrite}{Overwrite the database file, if it exists? (Default=FALSE)}

\item{verbose}{Show messages? (Default=TRUE)}
}
\value{
\itemize{
\item Ok: fname_db: filename of the SQLite database file
\item Err: dbError
}
}
\description{
Create a single database from an MS Excel file or tab-delimited files
}
\examples{
fname_xlsx = fn_simulate_tables(
        n_entries=50,
        n_dates=3,
        n_sites=3,
        n_treatments=3,
        n_loci=10e3,
        save_data_tables=TRUE)$list_fnames_tables$fname_data_tables
fn_create_database_from_xlsx_or_tsv(fname_xlsx=fname_xlsx)
}
