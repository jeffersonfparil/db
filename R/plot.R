### Data visualisations

### 1. Aggregated data: overall means including spread and grouped means and spreads

### 2. Time-series data

fn_boxplots = function(df, vec_response_column_names, vec_explanatory_column_names, verbose=TRUE) {
    ################################################################
    ### TEST
    # fname_xlsx = fn_simulate_tables(
    #     n_entries=50,
    #     n_dates=3,
    #     n_sites=3,
    #     n_treatments=3,
    #     n_loci=10e3,
    #     save_data_tables=TRUE)$list_fnames_tables$fname_data_tables
    # fname_db = fn_create_database_from_xlsx_or_tsv(fname_xlsx=fname_xlsx)
    # database = DBI::dbConnect(drv=RSQLite::SQLite(), dbname=fname_db)
    # vec_entry_names = sample(DBI::dbGetQuery(conn=database, 
    #     statement="SELECT ENTRY FROM entries")[, 1], size=3)
    # DBI::dbDisconnect(conn=database)
    # list_fnames_out = fn_export_phenotypes_environments_and_genotypes_data_from_database(
    #     fname_db=fname_db, 
    #     vec_ENTRY=vec_entry_names)
    # fname_phenotypes_tsv = list_fnames_out$fname_phenotypes_tsv
    # fname_genotypes_tsv = list_fnames_out$fname_genotypes_tsv
    # fname_environments_tsv = list_fnames_out$fname_environments_tsv
    # ### Load the data
    # df = utils::read.delim(file=fname_phenotypes_tsv, sep="\t", header=TRUE, check.names=FALSE)
    # ### Define the explanatory and response variables
    # vec_explanatory_column_names = c(GLOBAL_list_required_colnames_per_table()$phenotypes, GLOBAL_list_required_colnames_per_table()$additional_IDs)
    # vec_explanatory_column_names = vec_explanatory_column_names[vec_explanatory_column_names %in% colnames(df)]
    # vec_numeric_column_names = c()
    # for (j in 1:ncol(df)) {
    #     if (is.numeric(df[, j])) {
    #         vec_numeric_column_names = c(vec_numeric_column_names, colnames(df)[j])
    #     }
    # }
    # vec_response_column_names = sample(vec_numeric_column_names[!(vec_numeric_column_names %in% vec_explanatory_column_names)], size=3)
    ################################################################
    list_plots = list()
    for (i in 1:length(vec_response_column_names)) {
        # i = 1
        ### Define the data frame for plotting
        response_variable_name = vec_response_column_names[i]
        for (j in 1:(length(vec_explanatory_column_names)+1)) {
            # j = 1
            if (j <= length(vec_explanatory_column_names)) {
                ### Use one explanatory variable at a time
                explanatory_variable_name = vec_explanatory_column_names[j]
                df_for_plotting = eval(parse(text=paste0("data.frame(", 
                    response_variable_name, "=df$", response_variable_name, ", ",
                    explanatory_variable_name, "=df$", explanatory_variable_name, 
                    ")")))
                string_explanatory = explanatory_variable_name
            } else {
                ### Use all the explanatory variables together
                if (length(vec_explanatory_column_names) == 1) {next} ### Skip if we only have a single explanatory variable
                explanatory_variable_name = vec_explanatory_column_names[1] ### Set the lowest level grouping as the first explanatory variable
                df_for_plotting = eval(parse(text=paste0("data.frame(", 
                    response_variable_name, "=df$", response_variable_name, ", ",
                    paste(paste0(vec_explanatory_column_names, "=df$", vec_explanatory_column_names), collapse=", "),
                    ")")))
                string_explanatory = paste(vec_explanatory_column_names, collapse=" + ")
            }
            ### Boxplot
            p = eval(parse(text=paste0("ggplot2::ggplot(data=df_for_plotting, mapping=ggplot2::aes(x=", explanatory_variable_name, ", y=", response_variable_name, ", fill=", explanatory_variable_name, "))")))
            p = p + ggplot2::geom_boxplot(outlier.shape=NA)
            p = p + eval(parse(text=paste0("ggplot2::geom_jitter(width=0.1, alpha=0.3, color='black', shape=21, mapping=ggplot2::aes(fill=", explanatory_variable_name, "))")))
            if (j > length(vec_explanatory_column_names)) {
                ### Add fcet wrapping if we have more than 1 explanatory variable
                p = p + eval(parse(text=paste0("ggplot2::facet_wrap(~ ", string_explanatory, ")")))
            }
            ### Store the plot into the output list
            name = paste0(response_variable_name, "~", string_explanatory)
            eval(parse(text=paste0("list_plots$`", name, "` = p")))
        }
    }
    if (verbose) {
        for (p in list_plots) {
            p
        }
    }
    return(list_plots)
}