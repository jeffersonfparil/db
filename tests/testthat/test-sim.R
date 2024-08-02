# library(testthat)
# source("R/sim.R")

test_that("fn_simulate_tables", {
    set.seed(123)
    n_entries=10
    n_dates=10
    n_sites=10
    n_treatments=10
    n_traits=20
    n_replications=3
    n_sensors=3
    n_abiotics=10
    n_loci=100e3
    n_chromosomes=7
    save_data_tables=TRUE
    verbose=TRUE
    list_sim = fn_simulate_tables(
        n_entries=n_entries,
        n_dates=n_dates,
        n_sites=n_sites,
        n_treatments=n_treatments,
        n_traits=n_traits,
        n_replications=n_replications,
        n_sensors=n_sensors,
        n_abiotics=n_abiotics,
        n_loci=n_loci,
        n_chromosomes=n_chromosomes,
        save_data_tables=save_data_tables,
        verbose=verbose
    )
    expect_equal(nrow(list_sim$df_phenotypes), n_entries*n_dates*n_sites*n_treatments*n_replications)
    expect_equal(ncol(list_sim$df_phenotypes), n_traits+15)
    expect_equal(nrow(list_sim$df_environments), n_dates*n_sites*n_treatments*n_replications*n_sensors)
    expect_equal(ncol(list_sim$df_environments), n_abiotics+9)
    expect_equal(nrow(list_sim$df_genotypes), n_loci)
    expect_equal(ncol(list_sim$df_genotypes), n_entries+3)
    ### Test saved phenotypes table
    df_phenotypes = utils::read.delim(list_sim$list_fnames_tables$fname_phenotypes)
    dim(df_phenotypes)
    dim(list_sim$df_phenotypes)
    vec_idx_column_reloaded = c(ncol(df_phenotypes)-n_traits):ncol(df_phenotypes)
    vec_idx_numeric_reloaded = which(unlist(lapply(df_phenotypes[, vec_idx_column_reloaded], FUN=is.numeric)))
    vec_idx_string_reloaded = which(unlist(lapply(df_phenotypes[, vec_idx_column_reloaded], FUN=function(x){is.character(x)})))
    vec_idx_column_perse = c(ncol(list_sim$df_phenotypes)-n_traits):ncol(list_sim$df_phenotypes)
    vec_idx_numeric_perse = which(unlist(lapply(list_sim$df_phenotypes[, vec_idx_column_perse], FUN=function(x){is.numeric(x)})))
    vec_idx_string_perse = which(unlist(lapply(list_sim$df_phenotypes[, vec_idx_column_perse], FUN=function(x){!is.numeric(x)})))
    expect_equal(sum(df_phenotypes[, vec_idx_column_reloaded[vec_idx_numeric_reloaded]] - list_sim$df_phenotypes[, vec_idx_column_perse[vec_idx_numeric_perse]]), 0)
    expect_equal(sum(df_phenotypes[, vec_idx_column_reloaded[vec_idx_string_reloaded]] != list_sim$df_phenotypes[, vec_idx_column_perse[vec_idx_string_perse]]), 0)
    ### Test saved environments table
    df_environments = utils::read.delim(list_sim$list_fnames_tables$fname_environments)
    dim(df_environments)
    dim(list_sim$df_environments)
    vec_idx_column_reloaded = c(ncol(df_environments)-n_abiotics):ncol(df_environments)
    vec_idx_numeric_reloaded = which(unlist(lapply(df_environments[, vec_idx_column_reloaded], FUN=is.numeric)))
    vec_idx_string_reloaded = which(unlist(lapply(df_environments[, vec_idx_column_reloaded], FUN=function(x){is.character(x)})))
    vec_idx_column_perse = c(ncol(list_sim$df_environments)-n_abiotics):ncol(list_sim$df_environments)
    vec_idx_numeric_perse = which(unlist(lapply(list_sim$df_environments[, vec_idx_column_perse], FUN=function(x){is.numeric(x)})))
    vec_idx_string_perse = which(unlist(lapply(list_sim$df_environments[, vec_idx_column_perse], FUN=function(x){!is.numeric(x)})))
    expect_equal(sum(df_environments[, vec_idx_column_reloaded[vec_idx_numeric_reloaded]] - list_sim$df_environments[, vec_idx_column_perse[vec_idx_numeric_perse]]), 0)
    expect_equal(sum(df_environments[, vec_idx_column_reloaded[vec_idx_string_reloaded]] != list_sim$df_environments[, vec_idx_column_perse[vec_idx_string_perse]]), 0)
    ### Test saved genotypes table
    df_genotypes = utils::read.delim(list_sim$list_fnames_tables$fname_genotypes)
    dim(df_genotypes)
    dim(list_sim$df_genotypes)
    expect_equal(nrow(df_genotypes), nrow(list_sim$df_genotypes))
    expect_equal(ncol(df_genotypes), ncol(list_sim$df_genotypes))
})
