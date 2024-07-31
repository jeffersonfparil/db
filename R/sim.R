#' Simulate base and data tables and save the data tables as tab-delimited and multi-sheet Excel files
#' @param n_entries number of entries or samples or genotypes (Default=100)
#' @param n_dates number of measurement dates (Default=10)
#' @param n_sites number of measurement sites (Default=10)
#' @param n_treatments number of treatments (Default=10)
#' @param n_traits number of traits (Default=50)
#' @param n_replications number of replications (Default=3)
#' @param n_sensors number of sensors for measuring the abiotic factors (Default=3)
#' @param n_abiotics number of abiotic factors (Default=10)
#' @param n_loci number of loci (Default=100e3)
#' @param n_chromosomes number of chromosomes (Default=7)
#' @param save_data_tables save the data tables as tab-delimited and Excel files? (Default=FALSE)
#' @param verbose show simulation messages? (Default=TRUE)
#' @returns
#'  - Ok:
#'      + $vec_ids: vector of sample/entry/pool names (same order as in G)
#'      + $vec_chr: vector of chromosome or scaffold names (same order as in G)
#'      + $vec_pos: vector of numeric positions per chromosome (same order as in G)
#'      + $vec_all: vector of allele names (same order as in G)

#'      + $df_entries: entries base table
#'      + $df_dates: dates base table
#'      + $df_sites: sites base table
#'      + $df_treatments: treatments base table
#'      + $df_traits: traits base table
#'      + $df_abiotics: abiotics base table
#'      + $df_loci: loci base table
#'      + $df_phenotypes: phenotypes data table
#'      + $df_environments: environments data table
#'      + $df_genotypes: genotypes data table
#'      + $list_fnames_tables
#'          + $fname_phenotypes: filename of tab-delimited phenotypes data table
#'          + $fname_environments: filename of tab-delimited environments data table
#'          + $fname_genotypes: filename of tab-delimited genotypes data table
#'          + $fname_data_tables: filename of Excel file consisting of one sheet each of the phenotypes, environments, and genotype data tables
#'  - Err: dbError
#' @examples
#' list_sim = fn_simulate_tables(save_data_tables=FALSE)
#' list_fnames_tables = fn_simulate_tables(save_data_tables=TRUE)$list_fnames_tables
#' @export
fn_simulate_tables = function(
    n_entries=100,
    n_dates=10,
    n_sites=10,
    n_treatments=10,
    n_traits=50,
    n_replications=3,
    n_sensors=3,
    n_abiotics=10,
    n_loci=100e3,
    n_chromosomes=7,
    save_data_tables=FALSE,
    verbose=TRUE) 
{
    ################################################################
    ### TEST
    # n_entries=100
    # n_dates=10
    # n_sites=10
    # n_treatments=10
    # n_traits=100
    # n_replications=3
    # n_sensors=3
    # n_abiotics=10
    # n_loci=100e3
    # n_chromosomes=7
    # save_data_tables=TRUE
    # verbose=TRUE
    ################################################################
    ### Check input variables
    for (input_name in c("n_entries", "n_dates", "n_sites", "n_treatments", "n_traits", "n_replications", "n_abiotics", "n_loci", "n_chromosomes")) {
        # input_name = "n_entries"
        x = eval(parse(text=input_name))
        if (!is.numeric(x)) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_simulate_tables(...): requires numeric input variable ", input_name))
            return(error)
        }
        if (length(x) != 1) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_simulate_tables(...): requires a single numeric input variable ", input_name))
            return(error)
        }
        if (x < 0) {
            error = methods::new("dbError",
                code=000,
                message=paste0("Error in fn_simulate_tables(...): we cannot have a value less than 1 for input variable ", input_name))
            return(error)
        }
    }
    ### We will sample all strings from the list of words in GNU/Linux saved as and *.rda in ./data/ which ships with this R library
    # vec_words = utils::read.table("/usr/share/dict/words")$V1
    env = new.env()
    name_data = utils::data(vec_words, envir=env)[1]
    vec_words = env[[name_data]]
    ### Define the base tables, i.e. entries, dates, sites/location, treatments, traits, abiotic/environmental variables, and loci identities and descriptions
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%% Individuals, pools, or any level of single to multiple plant representation %%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # entries {
    #     INTEGER ENTRY_UID
    #     TEXT ENTRY
    #     TEXT TYPE
    #     TEXT POPULATION
    #     TEXT CULTIVAR
    # }
    if (n_entries > length(vec_words)) {
        if (verbose) {
            print("Too many genotypes to simulate.")
            print(paste0("Reducing the number of genotypes from ", n_entries, " to ", length(vec_words), "."))
        }
        n_entries = length(vec_words)
    } else if (n_entries < 1) {
        n_entries = 1
    }
    n_populations = ceiling(n_entries/10)
    n_cultivars = ceiling(n_populations/5)
    df_entries = data.frame(
        ENTRY_UID=1:n_entries,
        ENTRY=sample(vec_words, size=n_entries, replace=FALSE),
        TYPE=sample(c("Individual", "Pool"), size=n_entries, replace=TRUE),
        POPULATION=sample(sample(vec_words, size=n_populations), size=n_entries, replace=TRUE),
        CULTIVAR=sample(sample(vec_words, size=n_cultivars), size=n_entries, replace=TRUE)
    )
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%% Dates, times, and FVI seasons of measurements of traits and abiotic variables %%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # dates {
    #     INTEGER DATE_UID
    #     INTEGER POSIX_DATE_TIME
    #     INTEGER YEAR
    #     INTEGER MONTH
    #     INTEGER DAY
    #     INTEGER HOUR
    #     TEXT FVI_YEAR_SEASON
    # }
    vec_dates = sample(seq(from=as.POSIXlt("2020-01-01 00:00:00 AEST"), to=as.POSIXlt("2030-12-31 23:59:59 AEST"), by="hour"), size=n_dates)    
    df_dates = data.frame(
        DATE_UID=1:n_dates,
        POSIX_DATE_TIME=as.numeric(vec_dates),
        YEAR=as.numeric(format(vec_dates, format="%Y")),
        MONTH=as.numeric(format(vec_dates, format="%m")),
        DAY=as.numeric(format(vec_dates, format="%d")),
        HOUR=as.numeric(format(vec_dates, format="%H")),
        FVI_YEAR_SEASON=""
    )
    vec_idx_summer_starting_last_year = which(df_dates$MONTH <= 2)
    vec_idx_autumn = which((df_dates$MONTH >= 3) & (df_dates$MONTH <= 5))
    vec_idx_winter = which((df_dates$MONTH >= 6) & (df_dates$MONTH <= 7))
    vec_idx_early_spring = which((df_dates$MONTH >= 8) & (df_dates$MONTH <= 9))
    vec_idx_late_spring = which((df_dates$MONTH >= 10) & (df_dates$MONTH <= 11))
    vec_idx_summer_ending_this_year = which(df_dates$MONTH == 12)
    df_dates$YEAR[vec_idx_summer_starting_last_year] = df_dates$YEAR[vec_idx_summer_starting_last_year] - 1 ### Adjust the year of the summer which started last to the year before for ease of understanding
    df_dates$FVI_YEAR_SEASON[vec_idx_summer_starting_last_year] = paste0(df_dates$YEAR[vec_idx_summer_starting_last_year], "-SUMMER")
    df_dates$FVI_YEAR_SEASON[vec_idx_autumn] = paste0(df_dates$YEAR[vec_idx_autumn], "-AUTUMN")
    df_dates$FVI_YEAR_SEASON[vec_idx_winter] = paste0(df_dates$YEAR[vec_idx_winter], "-WINTER")
    df_dates$FVI_YEAR_SEASON[vec_idx_early_spring] = paste0(df_dates$YEAR[vec_idx_early_spring], "-EARLY_SPRING")
    df_dates$FVI_YEAR_SEASON[vec_idx_late_spring] = paste0(df_dates$YEAR[vec_idx_late_spring], "-LATE_SPRING")
        df_dates$FVI_YEAR_SEASON[vec_idx_summer_ending_this_year] = paste0(df_dates$YEAR[vec_idx_summer_ending_this_year], "-SUMMER")
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%% Location of trials and measurements %%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # sites {
    #     INTEGER SITE_UID
    #     TEXT SITE
    #     TEXT DESCRIPTION
    # }
    if (n_sites > length(vec_words)) {
        if (verbose) {
            print("Too many sites to simulate.")
            print(paste0("Reducing the number of sites from ", n_sites, " to ", length(vec_words), "."))
        }
        n_sites = length(vec_words)
    } else if (n_sites < 1) {
        n_sites = 1
    }
    df_sites = data.frame(
        SITE_UID=1:n_sites,
        SITE=sub("\\b([a-z])", "\\U\\1", (sample(vec_words, size=n_sites, replace=FALSE)), perl=TRUE),
        DESCRIPTION=""
    )
    for (i in 1:nrow(df_sites)) {
        df_sites$DESCRIPTION[i] = sub("\\b([a-z])", "\\U\\1", paste(sample(vec_words, size=sample(5:10, size=1)), collapse=" "), perl=TRUE)
    }
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%% Treatment descriptions %%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # treatments {
    #     INTEGER TREATMENT_UID
    #     TEXT TREATMENT
    #     TEXT DESCRIPTION
    # }
    if (n_treatments > length(vec_words)) {
        if (verbose) {
            print("Too many treatments to simulate.")
            print(paste0("Reducing the number of treatments from ", n_treatments, " to ", length(vec_words), "."))
        }
        n_treatments = length(vec_words)
    } else if (n_treatments < 1) {
        n_treatments = 1
    }
    df_treatments = data.frame(
        TREATMENT_UID=1:n_treatments,
        TREATMENT=c("Control", sub("\\b([a-z])", "\\U\\1", (sample((vec_words), size=(n_treatments-1), replace=FALSE)), perl=TRUE)),
        DESCRIPTION="Control or base conditions"
    )
    for (i in 2:nrow(df_treatments)) {
        df_treatments$DESCRIPTION[i] = sub("\\b([a-z])", "\\U\\1", paste(sample(vec_words, size=sample(5:10, size=1)), collapse=" "), perl=TRUE)
    }
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%% Traits descriptions %%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%
    # traits {
    #     INTEGER TRAIT_UID
    #     TEXT TRAIT
    #     TEXT DESCRIPTION
    # }
    if (n_traits > length(vec_words)) {
        if (verbose) {
            print("Too many traits to simulate.")
            print(paste0("Reducing the number of traits from ", n_traits, " to ", length(vec_words), "."))
        }
        n_traits = length(vec_words)
    } else if (n_traits < 1) {
        n_traits = 1
    }
    df_traits = data.frame(
        TRAIT_UID=1:n_traits,
        TRAIT=gsub("-", "_", gsub(" ", "", toupper(sample(vec_words, size=n_traits, replace=FALSE)))),
        DESCRIPTION=""
    )
    for (i in 1:nrow(df_traits)) {
        df_traits$DESCRIPTION[i] = sub("\\b([a-z])", "\\U\\1", paste(sample(vec_words, size=sample(5:10, size=1)), collapse=" "), perl = TRUE)
    }
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%% Climatic and edaphic variables descriptions %%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # abiotics {
    #     INTEGER ABIOTIC_UID
    #     TEXT ABIOTIC
    #     TEXT DESCRIPTION
    # }
    if (n_abiotics > length(vec_words)) {
        if (verbose) {
            print("Too many abiotics to simulate.")
            print(paste0("Reducing the number of abiotics from ", n_abiotics, " to ", length(vec_words), "."))
        }
        n_abiotics = length(vec_words)
    } else if (n_abiotics < 1) {
        n_abiotics = 1
    }
    df_abiotics = data.frame(
        ABIOTIC_UID=1:n_abiotics,
        ABIOTIC=sub("\\b([a-z])", "\\U\\1", (sample((vec_words), size=n_abiotics, replace=FALSE)), perl=TRUE),
        DESCRIPTION=""
    )
    for (i in 1:nrow(df_abiotics)) {
        df_abiotics$DESCRIPTION[i] = sub("\\b([a-z])", "\\U\\1", paste(sample(vec_words, size=sample(5:10, size=1)), collapse=" "), perl=TRUE)
    }
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%% Loci coordinates and identities across the genome %%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # loci {
    #     INTEGER LOCUS_UID
    #     TEXT CHROMOSOME
    #     INTEGER POSITION_PER_CHROMOSOME
    #     TEXT ALLELE
    # }
    vec_n_loci_per_chromosome = sample(seq(from=0.1, to=1, length=10), size=n_chromosomes, replace=TRUE)
    vec_n_loci_per_chromosome = floor(n_loci * (vec_n_loci_per_chromosome/sum(vec_n_loci_per_chromosome)))
    if (sum(vec_n_loci_per_chromosome) < n_loci) {
        vec_n_loci_per_chromosome[1] = vec_n_loci_per_chromosome[1] + (n_loci-sum(vec_n_loci_per_chromosome))
    }
    df_loci = data.frame(
        LOCUS_UID=1:n_loci,
        CHROMOSOME=unlist(lapply(1:n_chromosomes, FUN=function(i){rep(paste0("chr_", i), times=vec_n_loci_per_chromosome[i])})),
        POSITION=0,
        ALLELE=sample(c("A", "a", "T", "t", "C", "c", "G", "g"), size=n_loci, replace=TRUE)
    )
    for (i in 1:n_chromosomes) {
        vec_idx = which(df_loci$CHROMOSOME == paste0("chr_", i))
        df_loci$POSITION[vec_idx] = sort(sample.int(n=n_loci, size=vec_n_loci_per_chromosome[i], replace=FALSE))
    }
    ### Define the data tables, i.e. phenotypes, environmental data, and genotype/allele frequency data
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%% Phenotype data per entry, replication, treatment, site, and date-time %%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # phenotypes {
    #     INTEGER PHENOTYPE_UID
    #     TEXT ENTRY
    #     TEXT TYPE
    #     TEXT POPULATION
    #     TEXT CULTIVAR
    #     TEXT TREATMENT
    #     TEXT REPLICATION
    #     TEXT PLOT
    #     TEXT ROW_OR_BLOCK
    #     TEXT COLUMN
    #     TEXT SITE
    #     INTEGER POSIX_DATE_TIME
    #     REAL _NUMERIC_AND_BINARY_TRAITS_
    #     TEXT _CATEGORICAL_TRAITS_
    # }
    df_grid_entries_replications_treatments_sites_dates = expand.grid(
        ENTRY_TYPE_POPULATION_CULTIVAR=paste0(df_entries$ENTRY, "\t", df_entries$TYPE, "\t", df_entries$POPULATION, "\t", df_entries$CULTIVAR),
        REPLICATION=paste0("rep_", 1:n_replications),
        TREATMENT=df_treatments$TREATMENT,
        SITE=df_sites$SITE,
        POSIX_DATE_TIME=df_dates$POSIX_DATE_TIME
    )
    ### Include type, population and cultivar information
    mat_ENTRY_TYPE_POPULATION_CULTIVAR = matrix(
        unlist(
            strsplit(
                as.character(df_grid_entries_replications_treatments_sites_dates$ENTRY_TYPE_POPULATION_CULTIVAR)
            , split="\t")), 
        byrow=TRUE, ncol=4)
    ### Build the phenotypes table
    df_phenotypes = data.frame(
        PHENOTYPE_UID=1:nrow(df_grid_entries_replications_treatments_sites_dates),
        ENTRY=mat_ENTRY_TYPE_POPULATION_CULTIVAR[,1], 
        TYPE=mat_ENTRY_TYPE_POPULATION_CULTIVAR[,2], 
        POPULATION=mat_ENTRY_TYPE_POPULATION_CULTIVAR[,3], 
        CULTIVAR=mat_ENTRY_TYPE_POPULATION_CULTIVAR[,4], 
        PLOT=sample(paste0("plot_", 1:(n_entries*n_replications*n_sites*n_treatments*n_dates)), size=nrow(df_grid_entries_replications_treatments_sites_dates), replace=TRUE),
        ROW_OR_BLOCK=sample(paste0("block_", 1:n_replications), size=nrow(df_grid_entries_replications_treatments_sites_dates), replace=TRUE),
        COLUMN=sample(paste0("column_", 1), size=nrow(df_grid_entries_replications_treatments_sites_dates), replace=TRUE),
        df_grid_entries_replications_treatments_sites_dates[, -1]
    )
    for (trait in df_traits$TRAIT) {
        bool_numeric = sample(c(TRUE, FALSE), size=1)
        if (bool_numeric) {
            bool_norm = sample(c(TRUE, FALSE), size=1)
            if (bool_norm) {
                vec_y = stats::rnorm(mean=stats::rnorm(n=1), sd=abs(stats::rnorm(n=1)), n=nrow(df_phenotypes))
            } else {
                vec_y = stats::rchisq(df=abs(stats::rnorm(n=1)), ncp=abs(stats::rnorm(n=1)), n=nrow(df_phenotypes))
            }
        } else {
            vec_y = sample(sample(vec_words, size=sample(5:20, size=1)), size=nrow(df_phenotypes), replace=TRUE)
        }
        eval(parse(text=paste0("df_phenotypes$`", trait, "` = vec_y")))
    }
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%% Environmental data, i.e. abiotic variables including climatic and edaphic data %%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # environments {
    #     INTEGER ENVIRONMENT_UID
    #     TEXT TREATMENT
    #     TEXT REPLICATION
    #     TEXT SENSOR
    #     TEXT SITE
    #     INTEGER POSIX_DATE_TIME
    #     REAL _NUMERIC_AND_BINARY_ABIOTICS_
    #     TEXT _CATEGORICAL_ABIOTICS_
    # }
    df_grid_treatments_replication_sensor_sites_dates = expand.grid(
        TREATMENT=df_treatments$TREATMENT,
        REPLICATION=paste0("rep_", 1:n_replications),
        SENSOR=paste0("sensor_", 1:n_sensors),
        SITE=df_sites$SITE,
        POSIX_DATE_TIME=df_dates$POSIX_DATE_TIME
    )
    df_environments = data.frame(
        ENVIRONMENT_UID=1:nrow(df_grid_treatments_replication_sensor_sites_dates),
        df_grid_treatments_replication_sensor_sites_dates
    )
    for (abiotic in df_abiotics$ABIOTIC) {
        bool_numeric = sample(c(TRUE, FALSE), size=1)
        if (bool_numeric) {
            bool_norm = sample(c(TRUE, FALSE), size=1)
            if (bool_norm) {
                vec_y = stats::rnorm(mean=stats::rnorm(n=1), sd=abs(stats::rnorm(n=1)), n=nrow(df_environments))
            } else {
                vec_y = stats::rchisq(df=abs(stats::rnorm(n=1)), ncp=abs(stats::rnorm(n=1)), n=nrow(df_environments))
            }
        } else {
            vec_y = sample(sample(vec_words, size=sample(5:20, size=1)), size=nrow(df_environments), replace=TRUE)
        }
        eval(parse(text=paste0("df_environments$`", toupper(abiotic), "` = vec_y")))
    }
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%% Genotype data per entry across loci %%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # genotypes {
    #     INTEGER ENTRY_UID
    #     REAL _LOCUS_ID_ALLELE_FREQ_
    # }
    G = matrix(stats::rbeta(n=(n_entries*n_loci), shape1=2, shape2=2), nrow=n_entries)
    df_genotypes = data.frame(
        ENTRY_UID=df_entries$ENTRY_UID,
        G
    )
    colnames(df_genotypes) = c("ENTRY_UID", df_loci$LOCUS_UID)
    ### Save data tables excluding the UIDs and saving the genotype data as an allele frequency table file (see https://github.com/jeffersonfparil/imputef?tab=readme-ov-file#allele-frequency-table-tsv)
    if (save_data_tables) {
        dir = tempdir()
        id_datetime_rand = paste0(round(as.numeric(Sys.time())), "-", sample.int(n=.Machine$integer, size=1))
        list_fnames_tables = list(
            fname_phenotypes=file.path(dir, paste0("phenotypes-", id_datetime_rand, ".tsv")),
            fname_environments=file.path(dir, paste0("environments-", id_datetime_rand, ".tsv")),
            fname_genotypes=file.path(dir, paste0("genotypes-", id_datetime_rand, ".tsv")),
            fname_data_tables=file.path(dir, paste0("data_tables-", id_datetime_rand, ".xlsx"))
        )
        ### Replace POSIX_DATE_TIME with YEAR, MONTH and DAY columns
        df_out_phenotypes = data.frame(
            df_phenotypes[, 2, drop=FALSE], 
            YEAR=as.numeric(format(as.POSIXlt(df_phenotypes$POSIX_DATE_TIME), format="%Y")),
            MONTH=as.numeric(format(as.POSIXlt(df_phenotypes$POSIX_DATE_TIME), format="%m")),
            DAY=as.numeric(format(as.POSIXlt(df_phenotypes$POSIX_DATE_TIME), format="%d")),
            HOUR=as.numeric(format(as.POSIXlt(df_phenotypes$POSIX_DATE_TIME), format="%H")),
            df_phenotypes[, -1:-2, drop=FALSE])
        df_out_environments = data.frame(
            df_environments[, 2, drop=FALSE], 
            YEAR=as.numeric(format(as.POSIXlt(df_environments$POSIX_DATE_TIME), format="%Y")),
            MONTH=as.numeric(format(as.POSIXlt(df_environments$POSIX_DATE_TIME), format="%m")),
            DAY=as.numeric(format(as.POSIXlt(df_environments$POSIX_DATE_TIME), format="%d")),
            HOUR=as.numeric(format(as.POSIXlt(df_environments$POSIX_DATE_TIME), format="%H")),
            df_environments[, -1:-2, drop=FALSE])
        ### Save phenotypes and environments tables as tab-delimited files
        utils::write.table(x=df_out_phenotypes, file=list_fnames_tables$fname_phenotypes, sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE)
        utils::write.table(x=df_out_environments, file=list_fnames_tables$fname_environments, sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE)
        ### Save the genotype data as an allele frequency table file
        df_allele_frequencies = data.frame(chr=df_loci$CHROMOSOME, pos=df_loci$POSITION, allele=df_loci$ALLELE, t(G))
        colnames(df_allele_frequencies) = c("chr", "pos", "allele", df_entries$ENTRY)
        utils::write.table(x=df_allele_frequencies, file=list_fnames_tables$fname_genotypes, sep="\t", row.names=FALSE, col.names=TRUE, quote=FALSE)
        ### Save MS Excel file including all three tables in separate sheets
        writexl::write_xlsx(
            x=list(
                phenotypes=df_out_phenotypes, 
                environments=df_out_environments, 
                genotypes=df_allele_frequencies),
            path=list_fnames_tables$fname_data_tables)
    } else {
        list_fnames_tables = NULL
    }
    ### Output tables
    return(
        list(
            df_entries=df_entries,
            df_dates=df_dates,
            df_sites=df_sites,
            df_treatments=df_treatments,
            df_traits=df_traits,
            df_abiotics=df_abiotics,
            df_loci=df_loci,
            df_phenotypes=df_phenotypes,
            df_environments=df_environments,
            df_genotypes=df_genotypes,
            list_fnames_tables=list_fnames_tables
        )
    )
}
