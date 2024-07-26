# db

A simple SQLite database in R for genomic prediction.

## Schema

Each database will correspond to a single species with the following schema:

```mermaid
erDiagram
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Individuals, pools, or any level of single to multiple plant representation %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    entries {
        TEXT ENTRY_HASH
        INTEGER ENTRY_UID
        TEXT ENTRY
        TEXT TYPE
        TEXT POPULATION
        TEXT CULTIVAR
    }
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Dates, times, and FVI seasons of measurements of traits and abiotic variables %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    dates {
        TEXT DATE_HASH
        INTEGER DATE_UID
        INTEGER POSIX_DATE_TIME
        INTEGER YEAR
        INTEGER MONTH
        INTEGER DAY
        INTEGER HOUR
        TEXT FVI_YEAR_SEASON
    }
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Location of trials and measurements %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    sites {
        TEXT SITE_HASH
        INTEGER SITE_UID
        TEXT SITE
        TEXT DESCRIPTION
    }
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Treatment descriptions %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    treatments {
        TEXT TREATMENT_HASH
        INTEGER TREATMENT_UID
        TEXT TREATMENT
        TEXT DESCRIPTION
    }
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Traits descriptions %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%
    traits {
        TEXT TRAIT_HASH
        INTEGER TRAIT_UID
        TEXT TRAIT
        TEXT DESCRIPTION
    }
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Climatic and edaphic variables descriptions %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    abiotics {
        TEXT ABIOTIC_HASH
        INTEGER ABIOTIC_UID
        TEXT ABIOTIC
        TEXT DESCRIPTION
    }
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Loci coordinates and identities across the genome %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    loci {
        TEXT LOCUS_HASH
        INTEGER LOCUS_UID
        TEXT CHROMOSOME
        INTEGER POSITION_PER_CHROMOSOME
        TEXT ALLELE
    }
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Phenotype data per entry, replication, treatment, site, and date-time %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    phenotypes {
        TEXT PHENOTYPE_HASH
        INTEGER PHENOTYPE_UID
        TEXT ENTRY
        TEXT TREATMENT
        TEXT REPLICATION
        TEXT PLOT
        TEXT ROW_OR_BLOCK
        TEXT COLUMN
        TEXT SITE
        INTEGER POSIX_DATE_TIME
        REAL _NUMERIC_AND_BINARY_TRAITS_
        TEXT _CATEGORICAL_TRAITS_
    }
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Environmental data, i.e. abiotic variables including climatic and edaphic data %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    environments {
        TEXT ENVIRONMENT_HASH
        INTEGER ENVIRONMENT_UID
        TEXT TREATMENT
        TEXT REPLICATION
        TEXT SENSOR
        TEXT SITE
        INTEGER POSIX_DATE_TIME
        REAL _NUMERIC_AND_BINARY_ABIOTICS_
        TEXT _CATEGORICAL_ABIOTICS_
    }
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Genotype data per entry across loci %%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Note that the ENTRY_HASH will be pulled from the entries table instead of calculated from each genotype matrix row for efficiency
    genotypes {
        TEXT ENTRY_HASH
        INTEGER ENTRY_UID
        REAL _LOCUS_ID_ALLELE_FREQ_
    }
    %%%%%%%%%%%%%%%%%%%%%
    %%% Relationships %%%
    %%%%%%%%%%%%%%%%%%%%%
    entries ||--|| phenotypes : ""
    entries ||--|| genotypes : ""
    dates ||--|| phenotypes : ""
    dates ||--|| environments : ""
    traits ||--o{ phenotypes : "defines the trait fields"
    sites ||--|| phenotypes : ""
    sites ||--|| environments : ""
    treatments ||--|| phenotypes : ""
    treatments ||--|| environments : ""
    abiotics ||--o{ environments : "defines the abiotic variables"
    loci ||--o{ genotypes : "defines the loci"
```

A brief apology to the schema above:

1. Genotypes refer to individuals, population, families or pools as long as they relate to a single line of allele frequencies or genotype values in some genotype matrix.
2. [POSIX time](https://en.wikipedia.org/wiki/Unix_time) is used to have a low memory-footprint date-time relator across tables with the dates table detailing the human-readable dates, times and FVI seasons.
3. Requirements for input data frames should allow the automatic updates of the base tables (i.e. entries, dates, traits, sites, treatments, abiotics, and/or loci) so that there is no need to manually upload theses information which should reduce error.
4. Duplicate entries should be avoided by table-specific UIDs (except in the genotype table which uses the entries: ENTRY_UID as well as the loci UID: LOCUS_UID for its rows and column names). These UIDs should relate to specific genotypes, dates, traits, sites, treatments, abiotic variables, loci, and the sensible combinations of them, e.g. genotype-date-site-treatment-replication combination in the phenotypes table should define a row-specific PHENOTYPE_UID.
5. There is one database for each species because rarely do we need to analyse multiple species at a single trial (maybe not in the future looking at mixed pastures). This limitation is caused by the simple encoding of the loci and genotypes table where each column in the former refers to a locus which should be present across all entries which may span millions of columns.
6. There are 3 input tables which are imported into the database from which the base tables will be extracted from. The data tables are the:
    - phenotypes (contains the phenotype data for multiple traits including the dates, sites replications associated with the trials/measurements), 
    - environments (contains the climatic and edpahic measurements from the trial sites including the date, time and location of the measurements) and 
    - genotypes (contain the allele frequencies per entry across genome-wide loci) tables.
7. Hashing is used to determine initial uniqueness of each row of the data tables (i.e. phenotypes, and environments table, excluding the genotypes tables - the uniqueness of which will be based upon the entry, type, population and cultivar identities in the the entries table for hashing efficiency) upon importation into the database so that we can efficiently compare existing and incoming data after which we assign integer UIDs for more efficient queries.

