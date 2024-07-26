#' Global variable encapsulating the valid table names, their classifications, and source table for the base tables
#' @details
#' Note that the genotypes table usually comes from an allele frequency table file (for format details, see: https://github.com/jeffersonfparil/imputef?tab=readme-ov-file#allele-frequency-table-tsv)
#' @export
GLOBAL_df_valid_tables = function() {
    return(data.frame(
        CLASS=c(rep("base", times=7), rep("data", times=3)),
        NAME=c("entries", "dates", "sites", "treatments", "traits", "abiotics", "loci", "phenotypes", "environments", "genotypes"),
        EXTRACT_COLUMN_NAMES_FROM_DATA_TABLE=c(rep(FALSE, times=4), rep(TRUE, times=2), rep(FALSE, times=4)),
        NEEDS_DESCRIPTION=c(rep(FALSE, times=2), rep(TRUE, times=4), rep(FALSE, times=4)),
        DATA_SOURCE=c("phenotypes,environments,genotypes", rep("phenotypes,environments", times=3), "phenotypes", "environments", "genotypes", rep(NA, times=3))
    ))
}

#' Global variable listing the required column names for each base and data tables
#' @details 
#' Note that we do not include the genotypes table here because the columns required is dynamic, i.e. depend on the genome coverage.
#' @export
GLOBAL_list_required_colnames_per_table = function() {
    return(list(
    entries=c("ENTRY", "TYPE", "POPULATION", "CULTIVAR"),
    dates=c("POSIX_DATE_TIME", "YEAR", "MONTH", "DAY", "HOUR"),
    sites=c("SITE", "DESCRIPTION"),
    treatments=c("TREATMENT", "DESCRIPTION"),
    traits=c("TRAIT", "DESCRIPTION"),
    abiotics=c("ABIOTIC", "DESCRIPTION"),
    loci=c("CHROMOSOME", "POSITION_PER_CHROMOSOME", "ALLELE"),
    phenotypes=c("ENTRY", "TYPE", "POPULATION", "CULTIVAR", "REPLICATION", "TREATMENT", "SITE", "YEAR", "MONTH", "DAY"),
    environments=c("TREATMENT", "REPLICATION", "SENSOR", "SITE", "YEAR", "MONTH", "DAY"),
    additional_IDs=c("PLOT", "ROW_OR_BLOCK", "COLUMN", "HOUR")
    ))
}
