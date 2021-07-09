An introduction to the TOPMed DCC’s harmonized variable documentation
================
Adrienne Stilp
09 July, 2021

This repository contains documentation for phenotypes harmonized by the
[Trans-Omics for Precision Medicine (TOPMed)
Program](https://www.nhlbiwgs.org/)’s Data Coordinating Center (DCC).

This document gives an introduction to the [JSON](https://www.json.org/)
documentation of harmonized variables provided in this repository.
Because the harmonization is done using [R](https://www.r-project.org/),
we’ll show how to work with the documentation using R. All study data
used for harmonization comes from
[dbGaP](https://www.ncbi.nlm.nih.gov/gap/). The TOPMed DCC’s
harmonization process will be described further in a paper. The
reference will be added upon publication.

Repository contents
-------------------

| file                                 | description                                                                                                                                                                                              |
|--------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `example/`                           | A directory containing reproducible examples using simulated data. See the [Examples section](#examples) for more information.                                                                           |
| `harmonized-variable-documentation/` | A directory containing JSON documentation file for variables harmonized by the TOPMed DCC. Documentation files for the harmonized variables are located in dataset subdirectories within this directory. |
| `schema.json`                        | Schema definition for the JSON documentation.                                                                                                                                                            |
| `phs-mapping.tsv`                    | A mapping between TOPMed study and dbGaP study accession. See the [Study mapping file](#study-mapping-file) section for more information.                                                                |
| `README.md`                          | This document.                                                                                                                                                                                           |
| `README.Rmd`                         | Source for this document.                                                                                                                                                                                |

Study mapping file
------------------

The phenotypes for each TOPMed study come from one or more dbGaP study
accession. The `phs-mapping.tsv` file provides a mapping between a dbGaP
study accession and the TOPMed study that its phenotypes correspond to.
It also gives an integer that can be used to generate a unique integer
identifier for subjects when reproducing the harmonized variables from
the documentation provided in this repository. Including these unique
identifiers in the harmonized subject identifiers is necessary because
the same submitted subject identifier can appear in two different
studies for different subjects. For a single study with phenotypes
located in two different study accessions on dbGaP, the unique id values
(and the TOPMed study abbreviation) in this file are the same.

Example harmonization
---------------------

This directory contains simulated study dbGaP data and JSON
documentation for three example harmonized variables created from the
simulated study data. There are also three example JSON documents
showing how to reproduce the harmonized variables. See
`example/README.md` for instructions on running the examples.

Harmonized variable documentation
---------------------------------

All information necessary to reproduce a harmonized variable is
available in a single JSON document file. The
`harmonized-variable-documentation` subdirectory contains these files in
dataset-specific directories.

To start, we’ll read a JSON document into R, using one of the
reproducible example files from the `examples` subdirectory.

    json <- jsonlite::fromJSON('example/example_height_1.json', simplifyDataFrame = FALSE)

### Metadata

The first keys provide metadata for the entire harmonized variable.

Here are keys describing standard metadata required for phenotype
variables on dbGaP.

    json[c('name', 'description', 'data_type', 'measurement_units')]

    ## $name
    ## [1] "example_height_1"
    ## 
    ## $description
    ## [1] "Example standing height of subject at the baseline visit."
    ## 
    ## $data_type
    ## [1] "decimal"
    ## 
    ## $measurement_units
    ## [1] "cm"

In some cases, we harmonize multiple variables representing the same
phenotype concept.

    json[c('phenotype_concept', 'concept_variant')]

    ## $phenotype_concept
    ## [1] "example_height"
    ## 
    ## $concept_variant
    ## [1] 1

Different variables with the same concept have different
`concept_variant` numbers. We construct the variable name (`name`) by
concatenating the `phenotype_concept` and the `concept_variant` values
with an underscore.

We also provide some internal identifiers that we can use to link the
documentation to the harmonized variable at the TOPMed DCC.

    json[c('dcc_harmonization_id', 'version', 'date_harmonized')]

    ## $dcc_harmonization_id
    ## [1] 1
    ## 
    ## $version
    ## [1] 1
    ## 
    ## $date_harmonized
    ## [1] "2020-03-25 11:00:00"

The DCC can update a harmonized variable either by adding new studies or
by changing how a given study’s variables are processed. When the
variable is updated, the `version` given in this documentation is
incremented to indicate that it has been updated, and
`dcc_harmonization_id` is updated to the internal identifier of the new
variable.

Last, we provide comments about harmonization in Markdown format.

    cat(json$dcc_harmonization_comments)

    ## This variable was harmonized by converting units to cm, when necessary.

These comments give a brief description of the harmonization algorithm.
They can also include notes about the presence of a large cluster of
outliers; potential differences between studies that were not important
enough for removal of that study from harmonization; or a list of assays
used for measuring a specific variable.

### Provenance

In addition to metadata, the JSON documentation contains the provenance
of the harmonized variable. When performing harmonization, we work in
“harmonization units”. A harmonization unit is a group of subjects from
a single study whose variables are processed together to produce
harmonized values. It often corresponds to a single study (e.g., the
Amish or a subcohort within a study).

The `harmonization_units` key contains an array of objects, one for each
harmonization unit. Here is the first harmonization unit for this
harmonized variable.

    json$harmonization_units[[1]]

    ## $name
    ## [1] "StudyA"
    ## 
    ## $component_study_variables
    ## [1] "phs001985.v1.pht009922.v1.phv00427205.v1"
    ## [2] "phs001985.v1.pht009922.v1.phv00427207.v1"
    ## 
    ## $component_harmonized_variables
    ## list()
    ## 
    ## $harmonization_function
    ## [1] "harmonize <- function(phen_list) {\n  library(dplyr)\n  source_data <- phen_list$source_data\n\n  dataset <- source_data[[\"pht009922\"]]\n\n  dataset <- dataset %>%\n    # Subset to non-missing values.\n    filter(!is.na(HGTV1)) %>%\n    # Rename variable.\n    rename(example_height = HGTV1, age = AGEV1)\n\n  # Convert to numeric.\n  dataset$example_height <- as.numeric(dataset$example_height)\n  dataset$age <- as.numeric(dataset$age)\n\n  # Return the final dataset.\n  dataset\n\n}\n"

Here are the elements that are present for each harmonization unit:

-   `name`: the name of the harmonization unit
-   `component_study_variables`: a list of dbGaP variable accession
    numbers that were used to produce the values for the subjects
    included in this unit. Most units contain multiple component study
    variables: at least one representing the phenotype itself, and at
    least one for the age at measurement.
-   `component_harmonized_variables`: a list of harmonized variables
    used to produce the values for the subjects included in this unit.
    In this example, no component harmonized variables were used.
-   `harmonization function`: the definition of the R function that
    converts the component variable values into harmonized variable
    values for the group of subjects included in this unit.

Typically, harmonization units have either component study variables or
component harmonized variables, but not both.

More details about how to use this information are available in the
three example documents in the `examples` directory. See the Example
Harmonization section above for more information.

dbGaP consent considerations
----------------------------

We do not provide participant consent values with the harmonized
phenotype documentation. Because the harmonized data are derived from
data on dbGaP, the consent type for a given participant’s harmonized
data is inherited from the dbGaP data used as components for the
harmonization. Users must therefore ensure that the harmonized data are
used in accordance with the data use limitations for each consent group
to which they have access.

Race and ancestry guidelines
----------------------------

This repository contains documentation for harmonization of race and
ethnicity variables in TOPMed. If you wish to use them in your analysis,
please refer to the [Guidelines on the use and reporting of race,
ethnicity, and ancestry in the NHLBI Trans-Omics for Precision Medicine
(TOPMed)
program](https://www.nhlbiwgs.org/guidelines-use-and-reporting-race-ethnicity-and-ancestry-topmed).
