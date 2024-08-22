### Data visualisations

### 1. Aggregated data: overall means including spread and grouped means and spreads

### 2. Time-series data

fn_boxplot = function() {
    ### Simulate a database and extract a subset of phenotypes, genotypes, and environments data tables
    fname_xlsx = fn_simulate_tables(
        n_entries=50,
        n_dates=3,
        n_sites=3,
        n_treatments=3,
        n_loci=10e3,
        save_data_tables=TRUE)$list_fnames_tables$fname_data_tables
    fname_db = fn_create_database_from_xlsx_or_tsv(fname_xlsx=fname_xlsx)
    database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    vec_entry_names = sample(DBI::dbGetQuery(conn=database, 
        statement="SELECT ENTRY FROM entries")[, 1], size=3)
    DBI::dbDisconnect(conn=database)
    list_fnames_out = fn_export_phenotypes_environments_and_genotypes_data_from_database(
        fname_db=fname_db, 
        vec_ENTRY=vec_entry_names)
    fname_phenotypes_tsv = list_fnames_out$fname_phenotypes_tsv
    fname_genotypes_tsv = list_fnames_out$fname_genotypes_tsv
    fname_environments_tsv = list_fnames_out$fname_environments_tsv



    ### Plot phenotypes data table

    ### Load the data
    df_phenotypes = utils::read.delim(file=fname_phenotypes_tsv, sep="\t", header=TRUE, check.names=FALSE)
    ### Define the explanatory and response variables
    vec_explanatory_column_names = c(GLOBAL_list_required_colnames_per_table()$phenotypes, GLOBAL_list_required_colnames_per_table()$additional_IDs)
    vec_explanatory_column_names = vec_explanatory_column_names[vec_explanatory_column_names %in% colnames(df_phenotypes)]
    vec_numeric_column_names = c()
    for (j in 1:ncol(df_phenotypes)) {
        if (is.numeric(df_phenotypes[, j])) {
            vec_numeric_column_names = c(vec_numeric_column_names, colnames(df_phenotypes)[j])
        }
    }
    vec_response_column_names = sample(vec_numeric_column_names[!(vec_numeric_column_names %in% vec_explanatory_column_names)], size=3)
    ### Plot
    for (i in 1:length(vec_response_column_names)) {
        # i = 1
        response_variable_name = vec_response_column_names[i]
        for (j in 1:(length(vec_explanatory_column_names)+1)) {
            # j = 1
            explanatory_variable_name = vec_explanatory_column_names[j]
            if (j <= length(vec_explanatory_column_names)) {
                # eval(parse(text=paste0("boxplot(", response_variable_name, " ~ ", vec_explanatory_column_names[j], ", data=df_phenotypes)")))
                df_for_plotting = eval(parse(text=paste0("data.frame(", 
                    response_variable_name, "=df_phenotypes$", response_variable_name, ", ",
                    explanatory_variable_name, "=df_phenotypes$", explanatory_variable_name, 
                    ")")))
                string_explanatory = explanatory_variable_name
            } else {
                # eval(parse(text=paste0("boxplot(", response_variable_name, " ~ ", string_explanatory, ", data=df_phenotypes)")))
                df_for_plotting = eval(parse(text=paste0("data.frame(", 
                    response_variable_name, "=df_phenotypes$", response_variable_name, ", ",
                    paste(paste0(vec_explanatory_column_names, "=df_phenotypes$", vec_explanatory_column_names), collapse=", "),
                    ")")))
                string_explanatory = paste(vec_explanatory_column_names, collapse=" + ")
            }
            utils::str(df_for_plotting)
            # boxplot(y ~ ., data=df_for_plotting)
            p = eval(parse(text=paste0("ggplot2::ggplot(data=df_for_plotting, mapping=ggplot2::aes(x=", explanatory_variable_name, ", y=", response_variable_name, ", fill=", explanatory_variable_name, "))")))
            p = p + ggplot2::geom_boxplot()
            if (j > length(vec_explanatory_column_names)) {
                p = p + eval(parse(text=paste0("ggplot2::facet_wrap(~ ", string_explanatory, ")")))
            }
            p
            grDevices::dev.off()
        }
    }
    return(0)
}