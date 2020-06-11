Reproduce the `example_height_1` variable
================
Adrienne Stilp
11 June, 2020

This document explains how to use the information in the JSON documentation to reproduce a given harmonized variable from the phenotype files available on dbGaP. In this example, we'll focus on the `example_height_1` variable.

Setup
-----

First, we'll need to load some packages to help work with the files.

``` {.r}
library(readr)
library(dplyr)
library(stringr)
library(jsonlite)
library(xml2)
```

Next, install the `dbgaptools` R package, which will allow us to read the dbGaP files more easily.

``` {.r}
if (!require(dbgaptools)) {
  devtools::install_github("UW-GAC/dbgaptools")
}
library(dbgaptools)
```

We will define some constants that we'll use throughout this vignette.

``` {.r}
REGEX_PHS <- "phs(\\d{6}).v(\\d+?)"
REGEX_PHT <- "pht(\\d{6}).v(\\d+?)"
REGEX_PHV <- "phv(\\d{8}).v(\\d+?)"
REGEX_DBGAP <- sprintf("%s.%s.%s", REGEX_PHS, REGEX_PHT, REGEX_PHV)
```

We'll also define some helper functions to read the XML data dictionaries from dbGaP.

``` {.r}
# Return the variable name from an xml node.
get_node_varname <- function(node) {
  varname <- xml2::xml_find_all(node[[1]], ".//name") %>%
    xml2::xml_text()
  return(varname)
}

# Return the dbGaP variable accession from an xml node.
get_node_phv <- function(node) {
  id = unname(xml2::xml_attrs(node[[1]])["id"])
  as.integer(stringr::str_match(id, REGEX_PHV)[, 2])
}

# Return the version of a dbGaP variable from an xml node.
get_node_phv_version <- function(node) {
    id <- unname(xml2::xml_attrs(node[[1]])["id"])
  as.integer(stringr::str_match(id, REGEX_PHV)[, 3])
}

# Read the variable name, accession, and version from an xml data dictionary into a data frame.
read_dd_xml <- function(filename) {
  # Set parent_dd_file to the filename of the XML data dictionary on disk
  xml_dd <- xml2::read_xml(filename)

  # Select variable nodes
  variable_nodes <- xml2::xml_find_all(xml_dd, "/data_table/variable")

  # Create a one-line data frame for each variable node.
  n_vars <- length(variable_nodes)
  dd <- data.frame(
    name = rep(NA, n_vars),
    phv = rep(NA, n_vars),
    phv_version = rep(NA, n_vars),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(variable_nodes)) {
    node <- variable_nodes[i]
    dd$name[i] <- get_node_varname(node)
    dd$phv[i] <- get_node_phv(node)
    dd$phv_version[i] = get_node_phv_version(node)
  }
  return(dd)
}
```

Required directory structure for dbGaP files
--------------------------------------------

For this example, we assume that the dbGaP files for a single study version are stored in a single directory named by the study accession and version number, e.g., `phs001985.v1`.

dbGaP provides three types of files for each dataset:

1.  A data dictionary containing metadata about each phenotype variable in this dataset (`*.data_dict.xml`).
2.  Data files containing the phenotype data (`*.txt`). There is one data file per consent group.
3.  A variable report containing summary information about each phenotype variable in this dataset (`*.var_report.xml`).

Each accession-version directory contains these three file types for the Subject file and for each of the phenotype datasets available on dbGaP for that study.

``` {.r}
files <- list.files("example_data/dbgap/phs001985.v1")
files
```

    ##  [1] "phs001985.v1.pht009920.v1.p1.Test_Study_1_Subject.MULTI.txt"                
    ##  [2] "phs001985.v1.pht009920.v1.p1.Test_Study_1_Subject.var_report.xml"           
    ##  [3] "phs001985.v1.pht009920.v1.Test_Study_1_Subject.data_dict.xml"               
    ##  [4] "phs001985.v1.pht009921.v1.p1.Test_Study_1_Sample.MULTI.txt"                 
    ##  [5] "phs001985.v1.pht009921.v1.p1.Test_Study_1_Sample.var_report.xml"            
    ##  [6] "phs001985.v1.pht009921.v1.Test_Study_1_Sample.data_dict.xml"                
    ##  [7] "phs001985.v1.pht009922.v1.p1.c1.Test_Study_1_Subject_Phenotypes.GRU.txt"    
    ##  [8] "phs001985.v1.pht009922.v1.p1.c2.Test_Study_1_Subject_Phenotypes.HMB.txt"    
    ##  [9] "phs001985.v1.pht009922.v1.p1.Test_Study_1_Subject_Phenotypes.var_report.xml"
    ## [10] "phs001985.v1.pht009922.v1.Test_Study_1_Subject_Phenotypes.data_dict.xml"    
    ## [11] "phs001985.v1.pht009923.v1.p1.c1.Test_Study_1_Sample_Attributes.GRU.txt"     
    ## [12] "phs001985.v1.pht009923.v1.p1.c2.Test_Study_1_Sample_Attributes.HMB.txt"     
    ## [13] "phs001985.v1.pht009923.v1.p1.Test_Study_1_Sample_Attributes.var_report.xml" 
    ## [14] "phs001985.v1.pht009923.v1.Test_Study_1_Sample_Attributes.data_dict.xml"

Note that for the Subject file, only one data file is available across all consent groups:

``` {.r}
subj_file <- files[grepl("Subject\\.MULTI", files)]
print(subj_file)
```

    ## [1] "phs001985.v1.pht009920.v1.p1.Test_Study_1_Subject.MULTI.txt"

For each of the phenotype dataset data files, there is one data file for each consent group:

``` {.r}
phen_files_1 <- files[grepl("Test_Study_1_Subject_Phenotypes\\..+?\\.txt$", files)]
print(phen_files_1)
```

    ## [1] "phs001985.v1.pht009922.v1.p1.c1.Test_Study_1_Subject_Phenotypes.GRU.txt"
    ## [2] "phs001985.v1.pht009922.v1.p1.c2.Test_Study_1_Subject_Phenotypes.HMB.txt"

Reading in dbGaP data files
---------------------------

The data files provided by dbGaP are tab-separated text files, which generally contain columns for `dbGaP_Subject_ID`, study-submitted subject ID (often but not always called `SUBJECT_ID`), and various other columns that contain phenotype data. In real-world cases, we have encoutered occasional problems reading these files in with either `readr::read_tsv()` or the base R function `read.table()` due to small formatting differences between files, such as:

-   extra tab columns at the end of each row
-   missing columns at the end of each row
-   data values that contain the `#` character, which is often interpreted as a comment by R.

The `dbgaptools::read_ds_file()` function handles many of these cases, but is not guaranteed to work on every dbGaP file.

Reproducing the harmonized values
---------------------------------

To illustrate the steps for reproducing harmonized phenotype values, we'll start with a simple study with component variables from only one phenotype file. We'll then go through an example showing how to work with a study that has component variables from multiple phenotype files.

We'll also need to use the phs mapping file to create a unique subject identifier across TOPMed that we can use in analyses.

Start by reading in the JSON file. We set the `simplifyDataFrame` argument to FALSE to read the JSON file in as R list structure instead of as a data frame.

``` {.r}
json <- fromJSON('example_height_1.json', simplifyDataFrame = FALSE)
```

Please review the overall documentation of the format of these files in the main repository README.

### Reproduce the first unit

We'll start by storing the first harmonization unit in its own variable.

``` {.r}
unit <- json$harmonization_units[[1]]
unit
```

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

Next, we'll obtain the component variables for this unit.

``` {.r}
(component_variables <- unit$component_study_variables)
```

    ## [1] "phs001985.v1.pht009922.v1.phv00427205.v1"
    ## [2] "phs001985.v1.pht009922.v1.phv00427207.v1"

We have to parse the various dbGaP accession numbers from these strings. We'll use a regular expression to extract the study accession (phs), data table accession (pht), and variable accession (phv).

``` {.r}
matches <- stringr::str_match(component_variables, REGEX_DBGAP)
matches
```

    ##      [,1]                                       [,2]     [,3] [,4]    
    ## [1,] "phs001985.v1.pht009922.v1.phv00427205.v1" "001985" "1"  "009922"
    ## [2,] "phs001985.v1.pht009922.v1.phv00427207.v1" "001985" "1"  "009922"
    ##      [,5] [,6]       [,7]
    ## [1,] "1"  "00427205" "1" 
    ## [2,] "1"  "00427207" "1"

Next, we'll store these in a data frame for easier processing later, rename the columns, and convert strings to integers where appropriate.

``` {.r}
components <- as.data.frame(matches, stringsAsFactors = FALSE) %>%
  transmute(
    accession_string = V1,
    phs = as.integer(V2),
    phs_version = as.integer(V3),
    pht = as.integer(V4),
    pht_version = as.integer(V5),
    phv = as.integer(V6),
    phv_version = as.integer(V7)
  )
components
```

    ##                           accession_string  phs phs_version  pht
    ## 1 phs001985.v1.pht009922.v1.phv00427205.v1 1985           1 9922
    ## 2 phs001985.v1.pht009922.v1.phv00427207.v1 1985           1 9922
    ##   pht_version    phv phv_version
    ## 1           1 427205           1
    ## 2           1 427207           1

All component study variables within a harmonization unit come from the same study accession. We can identify the study assession number (phs) so we know which dbGaP accession contains the phenotype data.

``` {.r}
(phs <- unique(components$phs))
```

    ## [1] 1985

``` {.r}
(phs_version <- unique(components$phs_version))
```

    ## [1] 1

To exactly recreate the harmonized values for this unit, we'll need to use the version of the dbGaP accession that is given in the component variable accession strings. You may have access to a different version of the study version than is given here. It may be possible to run the harmonization on variables from a different version of the study, but changes in the study data could prevent the harmonization from working correctly.

The next step is to read in the dbGaP Subject file for this accession. First, locate the Subject file in the directory containing dbGaP files for this study.

``` {.r}
dbgap_dir <- file.path('example_data/dbgap', sprintf('phs%06d.v%d', phs, phs_version))
list.files(dbgap_dir)
```

    ##  [1] "phs001985.v1.pht009920.v1.p1.Test_Study_1_Subject.MULTI.txt"                
    ##  [2] "phs001985.v1.pht009920.v1.p1.Test_Study_1_Subject.var_report.xml"           
    ##  [3] "phs001985.v1.pht009920.v1.Test_Study_1_Subject.data_dict.xml"               
    ##  [4] "phs001985.v1.pht009921.v1.p1.Test_Study_1_Sample.MULTI.txt"                 
    ##  [5] "phs001985.v1.pht009921.v1.p1.Test_Study_1_Sample.var_report.xml"            
    ##  [6] "phs001985.v1.pht009921.v1.Test_Study_1_Sample.data_dict.xml"                
    ##  [7] "phs001985.v1.pht009922.v1.p1.c1.Test_Study_1_Subject_Phenotypes.GRU.txt"    
    ##  [8] "phs001985.v1.pht009922.v1.p1.c2.Test_Study_1_Subject_Phenotypes.HMB.txt"    
    ##  [9] "phs001985.v1.pht009922.v1.p1.Test_Study_1_Subject_Phenotypes.var_report.xml"
    ## [10] "phs001985.v1.pht009922.v1.Test_Study_1_Subject_Phenotypes.data_dict.xml"    
    ## [11] "phs001985.v1.pht009923.v1.p1.c1.Test_Study_1_Sample_Attributes.GRU.txt"     
    ## [12] "phs001985.v1.pht009923.v1.p1.c2.Test_Study_1_Sample_Attributes.HMB.txt"     
    ## [13] "phs001985.v1.pht009923.v1.p1.Test_Study_1_Sample_Attributes.var_report.xml" 
    ## [14] "phs001985.v1.pht009923.v1.Test_Study_1_Sample_Attributes.data_dict.xml"

``` {.r}
(subj_file <- list.files(dbgap_dir, pattern = 'Subject\\.MULTI\\.txt$',
                         full.names = TRUE))
```

    ## [1] "example_data/dbgap/phs001985.v1/phs001985.v1.pht009920.v1.p1.Test_Study_1_Subject.MULTI.txt"

Read the Subject file into R, making sure that only blank values are set to missing and that all columns are read in as character type. The `read_ds_file` function handles this for us.

``` {.r}
subj <- read_ds_file(subj_file, processed = TRUE) %>%
  select(dbGaP_Subject_ID, SUBJECT_ID)
head(subj)
```

    ##   dbGaP_Subject_ID SUBJECT_ID
    ## 1          3243267     SUBJ_O
    ## 2          3243269     SUBJ_S
    ## 3          3243265     SUBJ_N
    ## 4          3243258     SUBJ_C
    ## 5          3243263     SUBJ_J
    ## 6          3243268     SUBJ_R

Next, we'll create two new unique subject identifiers. Because study subject identifiers are unique within a dbGaP study accession but not across TOPMed, we'll use these identifiers when working with the harmonized data. For these identifiers, we are interested in uniquely identifying phenotype records for a subject in specific study, instead of identifying the same subject enrolled in two different studies. Note that if the same person enrolled in two different studies (e.g., in both StudyA and StudyB), they would be assigned different unique identifiers for the two different studies in this step.

The first identifier is a unique identifier consisting of the TOPMed abbreviation for this study and the study-submitted subject identifier. Looking up this phs in the `example-phs-mapping.tsv` file, we can see that its study abbreviation is `StudyA`. We won't use this identifier when running the harmonization for this study, but we'll need it when combining harmonization across studies.

``` {.r}
subj <- subj %>%
  mutate(unique_subject_key = sprintf("%s_%s", "StudyA", SUBJECT_ID))
head(subj)
```

    ##   dbGaP_Subject_ID SUBJECT_ID unique_subject_key
    ## 1          3243267     SUBJ_O      StudyA_SUBJ_O
    ## 2          3243269     SUBJ_S      StudyA_SUBJ_S
    ## 3          3243265     SUBJ_N      StudyA_SUBJ_N
    ## 4          3243258     SUBJ_C      StudyA_SUBJ_C
    ## 5          3243263     SUBJ_J      StudyA_SUBJ_J
    ## 6          3243268     SUBJ_R      StudyA_SUBJ_R

Second, we'll add an identifier called `topmed_subject_id`. This identifier is used when running the harmonization functions. We generate this column by creating a sequential integer for each subject in the subject file and then adding a large number (specific for this study) to those sequential integers. A given subject in a study should be assigned the same unique identifier across all harmonized variables. The following steps show you how you can create that integer.

The large number for this accession can also be found in `unique_id` column of the `phs_mapping.tsv` file we looked at earlier:

``` {.r}
phs_mapping <- read_tsv("example-phs-mapping.tsv") %>%
  filter(topmed_study == "StudyA")
phs_mapping
```

    ## # A tibble: 1 x 3
    ##     phs topmed_study unique_id
    ##   <dbl> <chr>            <dbl>
    ## 1  1985 StudyA        10000000

``` {.r}
(study_integer <- phs_mapping$unique_id)
```

    ## [1] 1e+07

For StudyA, it is `10000000`.

Here's how we'll create the `topmed_subject_id` for this study.

``` {.r}
subj <- subj %>%
  mutate(topmed_subject_id = 1:n() + study_integer)
head(subj)
```

    ##   dbGaP_Subject_ID SUBJECT_ID unique_subject_key topmed_subject_id
    ## 1          3243267     SUBJ_O      StudyA_SUBJ_O          10000001
    ## 2          3243269     SUBJ_S      StudyA_SUBJ_S          10000002
    ## 3          3243265     SUBJ_N      StudyA_SUBJ_N          10000003
    ## 4          3243258     SUBJ_C      StudyA_SUBJ_C          10000004
    ## 5          3243263     SUBJ_J      StudyA_SUBJ_J          10000005
    ## 6          3243268     SUBJ_R      StudyA_SUBJ_R          10000006

Now that we've created the required identifiers, we can read in the phenotype files that contain the component study variables, and put them in the data structure required by the harmonization function. We'll start by locating the phenotype files in the directory containing the dbGaP files for this study. We can use the accession numbers of the component variables to identify the files in which those variables are found.

``` {.r}
(pht <- unique(components$pht))
```

    ## [1] 9922

``` {.r}
(pht_version <- unique(components$pht_version))
```

    ## [1] 1

In this case, all component study variables are from a single dataset on dbGaP. We'll construct the filename prefix for this dataset using these accession numbers.

``` {.r}
file_regex <- sprintf('^phs%06d\\.v%d.pht%06d\\.v%d\\.', phs, phs_version, pht, pht_version)
file_regex
```

    ## [1] "^phs001985\\.v1.pht009922\\.v1\\."

Next, identify files matching this regular expression.

``` {.r}
(pht_files <- list.files(dbgap_dir, pattern = file_regex, full.names = TRUE))
```

    ## [1] "example_data/dbgap/phs001985.v1/phs001985.v1.pht009922.v1.p1.c1.Test_Study_1_Subject_Phenotypes.GRU.txt"    
    ## [2] "example_data/dbgap/phs001985.v1/phs001985.v1.pht009922.v1.p1.c2.Test_Study_1_Subject_Phenotypes.HMB.txt"    
    ## [3] "example_data/dbgap/phs001985.v1/phs001985.v1.pht009922.v1.p1.Test_Study_1_Subject_Phenotypes.var_report.xml"
    ## [4] "example_data/dbgap/phs001985.v1/phs001985.v1.pht009922.v1.Test_Study_1_Subject_Phenotypes.data_dict.xml"

We'll use the data files to obtain the component variable values and the data dictionary to obtain the names of the component variables in the data file.

Find the data dictionary and read it in using one of the helper functions we defined earlier.

``` {.r}
(dd_file <- pht_files[endsWith(pht_files, 'data_dict.xml')])
```

    ## [1] "example_data/dbgap/phs001985.v1/phs001985.v1.pht009922.v1.Test_Study_1_Subject_Phenotypes.data_dict.xml"

``` {.r}
dd <- read_dd_xml(dd_file) %>%
  filter(phv %in% components$phv)
dd
```

    ##    name    phv phv_version
    ## 1 HGTV1 427205           1
    ## 2 AGEV1 427207           1

This step maps the variable identifiers (phv) to the variable names in the data file.

Next, read in and concatenate the data files from the two different consent groups. As before, make sure that only blank values are set to missing and that all columns are read in as character type. In this step we'll also add the `topmed_subject_id` column we created in the Subject file by joining on the `dbGaP_Subject_ID`.

``` {.r}
(phen_files <- pht_files[endsWith(pht_files, '.txt')])
```

    ## [1] "example_data/dbgap/phs001985.v1/phs001985.v1.pht009922.v1.p1.c1.Test_Study_1_Subject_Phenotypes.GRU.txt"
    ## [2] "example_data/dbgap/phs001985.v1/phs001985.v1.pht009922.v1.p1.c2.Test_Study_1_Subject_Phenotypes.HMB.txt"

``` {.r}
tmp_list <- lapply(phen_files, read_ds_file, processed = TRUE)
phen <- bind_rows(tmp_list) %>%
  left_join(subj, by = 'dbGaP_Subject_ID')
```

We also need to subset the data file to only the columns required for the harmonization function: `topmed_subject_id` and the 2 component variables. Note that we do not select the `unique_subject_key` variable we just created, because it is not allowed to be in the data frame when running the harmonization function. We'll add it back later before combining the harmonized data across both studies.

``` {.r}
component_variable_names <- as.list(dd$name)
phen <- phen %>%
  select(topmed_subject_id, !!! component_variable_names)
head(phen)
```

    ##   topmed_subject_id HGTV1 AGEV1
    ## 1          10000003   143    32
    ## 2          10000005   167    58
    ## 3          10000007   181    46
    ## 4          10000010   177    41
    ## 5          10000011   176    59
    ## 6          10000012   176    54

Once we've read the phenotype data file in to R and selected the appropriate columns, we can create the data structure required to run the harmonization function. When working with study component data, this structure has one element named `source_data`. The `source_data` element is a list containing study data downloaded from dbGaP. It has one element for each different dataset in the component variables. The elements are named by the dbGaP accession for that dataset (e.g. `pht012345`), and each of those elements is a data frame whose columns are `topmed_subject_id` and the component variable names in dbGaP from that dataset. Here's what it looks like for this harmonization unit.

``` {.r}
source_data <- list()
source_data[[sprintf("pht%06d", pht)]] <- phen
phen_list <- list(source_data = source_data)
lapply(phen_list$source_data, head)
```

    ## $pht009922
    ##   topmed_subject_id HGTV1 AGEV1
    ## 1          10000003   143    32
    ## 2          10000005   167    58
    ## 3          10000007   181    46
    ## 4          10000010   177    41
    ## 5          10000011   176    59
    ## 6          10000012   176    54

Now that the component variables are in the expected format, we can obtain the harmonization function from the JSON documentation for this harmonization unit.

``` {.r}
cat(unit$harmonization_function)
```

    ## harmonize <- function(phen_list) {
    ##   library(dplyr)
    ##   source_data <- phen_list$source_data
    ## 
    ##   dataset <- source_data[["pht009922"]]
    ## 
    ##   dataset <- dataset %>%
    ##     # Subset to non-missing values.
    ##     filter(!is.na(HGTV1)) %>%
    ##     # Rename variable.
    ##     rename(example_height = HGTV1, age = AGEV1)
    ## 
    ##   # Convert to numeric.
    ##   dataset$example_height <- as.numeric(dataset$example_height)
    ##   dataset$age <- as.numeric(dataset$age)
    ## 
    ##   # Return the final dataset.
    ##   dataset
    ## 
    ## }

``` {.r}
harmonize <- eval(parse(text = unit$harmonization_function))
```

This function processes the input data and returns a data frame with three columns: \* `topmed_subject_id` \* `example_height`: the harmonized data value for the corresponding `topmed_subject_id`. Note that the `concept_variant` number is not part of this column name. \* `age`: the age at measurement of the target variable.

We'll run the harmonization function on the `phen_list` list that we just created.

``` {.r}
out <- harmonize(phen_list)
dim(out)
```

    ## [1] 20  3

All harmonization functions return a data frame with columns `topmed_subject_id`, the phenotype concept, and `age`:

``` {.r}
head(out)
```

    ##   topmed_subject_id example_height age
    ## 1          10000003            143  32
    ## 2          10000005            167  58
    ## 3          10000007            181  46
    ## 4          10000010            177  41
    ## 5          10000011            176  59
    ## 6          10000012            176  54

Some harmonized variables do not have an associated age measurement. These variables can be identified by the JSON key `has_age_variable = false`. For those variables, the data frame returned by the harmonization function only has columns `topmed_subject_id` and the phenotype concept.

We'll complete the harmonization for this unit in two steps: renaming variables to include the concept variant number, and adding the unique subject we created earlier to the data frame.

``` {.r}
harmonized_studya <- out %>%
  rename(
    example_height_1 = example_height,
    age_at_example_height_1 = age
  ) %>%
  left_join(subj, by = "topmed_subject_id") %>%
  select(
    SUBJECT_ID,
    unique_subject_key,
    topmed_subject_id,
    example_height_1,
    age_at_example_height_1
  )
head(harmonized_studya)
```

    ##   SUBJECT_ID unique_subject_key topmed_subject_id example_height_1
    ## 1     SUBJ_N      StudyA_SUBJ_N          10000003              143
    ## 2     SUBJ_J      StudyA_SUBJ_J          10000005              167
    ## 3     SUBJ_V      StudyA_SUBJ_V          10000007              181
    ## 4     SUBJ_T      StudyA_SUBJ_T          10000010              177
    ## 5    SUBJ_N1     StudyA_SUBJ_N1          10000011              176
    ## 6    SUBJ_V1     StudyA_SUBJ_V1          10000012              176
    ##   age_at_example_height_1
    ## 1                      32
    ## 2                      58
    ## 3                      46
    ## 4                      41
    ## 5                      59
    ## 6                      54

### Reproduce a harmonization unit with component variables from multiple phenotype files

Next, we'll reproduce harmonization for a unit that has component variables from multiple files. In this case, we'll use the harmonization unit for the set of subjects in the second example study. This section will contain abbreviated instructions, as most steps were described in the previous section.

As before, store the harmonization unit in its own variable.

``` {.r}
unit <- json$harmonization_units[[2]]
unit
```

    ## $name
    ## [1] "StudyB"
    ## 
    ## $component_study_variables
    ## [1] "phs001986.v1.pht009926.v1.phv00427217.v1"
    ## [2] "phs001986.v1.pht009955.v1.phv00427373.v1"
    ## 
    ## $component_harmonized_variables
    ## list()
    ## 
    ## $harmonization_function
    ## [1] "harmonize <- function(phen_list) {\n  library(dplyr)\n  source_data <- phen_list$source_data\n\n  age_dat <- source_data[[\"pht009955\"]]\n\n  dataset <- source_data[[\"pht009926\"]]\n\n  dataset <- dataset %>%\n    # Subset to non-missing values.\n    filter(!is.na(height)) %>%\n    # Merge with the age dataset.\n    left_join(age_dat, by = \"topmed_subject_id\") %>%\n    # Convert to numeric and rename.\n    mutate(\n      example_height = as.numeric(height),\n      age = as.numeric(age),\n    ) %>%\n    # Convert from inches to cm.\n    mutate(\n      example_height = example_height * 2.54\n    ) %>%\n    # Select appropriate columns.\n    select(topmed_subject_id, example_height, age)\n\n  # Return the final dataset.\n  dataset\n\n}\n"

Obtain the component variables for this unit and parse the dbGaP accession numbers from these strings.

``` {.r}
(component_variables <- unit$component_study_variables)
```

    ## [1] "phs001986.v1.pht009926.v1.phv00427217.v1"
    ## [2] "phs001986.v1.pht009955.v1.phv00427373.v1"

``` {.r}
components <- stringr::str_match(component_variables, REGEX_DBGAP) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  transmute(
    accession_string = V1,
    phs = as.integer(V2),
    phs_version = as.integer(V3),
    pht = as.integer(V4),
    pht_version = as.integer(V5),
    phv = as.integer(V6),
    phv_version = as.integer(V7)
  ) %>%
  mutate(
    file_prefix = sprintf("phs%06d.v%d.pht%06d.v%d", phs, phs_version, pht, pht_version)
  )
components
```

    ##                           accession_string  phs phs_version  pht
    ## 1 phs001986.v1.pht009926.v1.phv00427217.v1 1986           1 9926
    ## 2 phs001986.v1.pht009955.v1.phv00427373.v1 1986           1 9955
    ##   pht_version    phv phv_version               file_prefix
    ## 1           1 427217           1 phs001986.v1.pht009926.v1
    ## 2           1 427373           1 phs001986.v1.pht009955.v1

For this study, all the component variables come from the same study accession, but unlike the previous study, they are stored in two different files on dbGaP, as evidenced by the different `pht` numbers.

Identify the study accession and the corresponding subject file.

``` {.r}
phs <- unique(components$phs)
phs_version <- unique(components$phs_version)
dbgap_dir <- file.path('example_data/dbgap', sprintf('phs%06d.v%d', phs, phs_version))
(subj_file <- list.files(dbgap_dir, pattern = 'Subject\\.MULTI\\.txt$',
                         full.names = TRUE))
```

    ## [1] "example_data/dbgap/phs001986.v1/phs001986.v1.pht009924.v1.p1.Test_Study_2_Subject.MULTI.txt"

Obtain the unique large integer for Study B from the phs mapping file:

``` {.r}
phs_mapping <- read_tsv("example-phs-mapping.tsv") %>%
  filter(topmed_study == "StudyB")
phs_mapping
```

    ## # A tibble: 1 x 3
    ##     phs topmed_study unique_id
    ##   <dbl> <chr>            <dbl>
    ## 1  1986 StudyB        20000000

``` {.r}
(study_integer <- phs_mapping$unique_id)
```

    ## [1] 2e+07

Read in the subject file and add the `topmed_study`, `topmed_subject_id`, and `unique_subject_key` columns.

``` {.r}
subj <- read_ds_file(subj_file, processed = TRUE) %>%
  mutate(
    topmed_subject_id = 1:n() + study_integer,
    unique_subject_key = sprintf("StudyB_%s", SUBJECT_ID)
  ) %>%
  select(dbGaP_Subject_ID, SUBJECT_ID, topmed_subject_id, unique_subject_key)
```

Read in the phenotype files that contain the component study variables, and put them in the data structure required by the harmonization function. In this case, the `phen_list$source_data` list element will have two elements, one for each of the two datasets. We'll combine all the steps we ran above into a for loop over the two included datasets.

``` {.r}
phts <- unique(components$pht)
pht <- phts[1]

phen_list <- list(source_data = list())

for (pht in phts) {
  # Find the component variables that are contained in this pht.
  components_in_this_pht <- components %>%
    # The !! is required here because the variable (pht) has the same name as
    # the dataset column (pht) we're filtering on.
    filter(pht == !!pht)

  # Get the set of dbGaP files corresponding to this dataset.
  file_regex <- unique(components_in_this_pht$file_prefix)
  pht_files <- list.files(dbgap_dir, pattern = file_regex, full.names = TRUE)

  # Get the set of component variable names from the data dictionary.
  dd_file <- pht_files[endsWith(pht_files, 'data_dict.xml')]
  dd <- read_dd_xml(dd_file) %>%
    filter(phv %in% components_in_this_pht$phv)
  component_variable_names <- as.list(dd$name)

  # Read in and concatenate the files from both consent groups.
  phen_files <- pht_files[endsWith(pht_files, '.txt')]
  tmp <- lapply(phen_files, read_ds_file, processed = TRUE)
  phen <- do.call(bind_rows, tmp) %>%
    left_join(subj, by = 'dbGaP_Subject_ID') %>%
    select(topmed_subject_id, !!!component_variable_names)

  # Add the final data frame to the phen_list structure.
  phen_list$source_data[[sprintf("pht%06d", pht)]] <- phen
}

lapply(phen_list$source_data, head)
```

    ## $pht009926
    ##   topmed_subject_id height
    ## 1          20000002     63
    ## 2          20000003     60
    ## 3          20000004     65
    ## 4          20000005     70
    ## 5          20000008     61
    ## 6          20000010     63
    ## 
    ## $pht009955
    ##   topmed_subject_id age
    ## 1          20000002  31
    ## 2          20000003  27
    ## 3          20000004  47
    ## 4          20000005  39
    ## 5          20000008  34
    ## 6          20000010  34

Obtain the harmonization function from the JSON documentation for this harmonization unit.

``` {.r}
cat(unit$harmonization_function)
```

    ## harmonize <- function(phen_list) {
    ##   library(dplyr)
    ##   source_data <- phen_list$source_data
    ## 
    ##   age_dat <- source_data[["pht009955"]]
    ## 
    ##   dataset <- source_data[["pht009926"]]
    ## 
    ##   dataset <- dataset %>%
    ##     # Subset to non-missing values.
    ##     filter(!is.na(height)) %>%
    ##     # Merge with the age dataset.
    ##     left_join(age_dat, by = "topmed_subject_id") %>%
    ##     # Convert to numeric and rename.
    ##     mutate(
    ##       example_height = as.numeric(height),
    ##       age = as.numeric(age),
    ##     ) %>%
    ##     # Convert from inches to cm.
    ##     mutate(
    ##       example_height = example_height * 2.54
    ##     ) %>%
    ##     # Select appropriate columns.
    ##     select(topmed_subject_id, example_height, age)
    ## 
    ##   # Return the final dataset.
    ##   dataset
    ## 
    ## }

``` {.r}
harmonize <- eval(parse(text = unit$harmonization_function))
```

This function processes the input data and returns a data frame with three columns: \* `topmed_subject_id` \* `example_height`: the harmonized data value for the corresponding `topmed_subject_id`. Note that the `concept_variant` number is not part of this column name. \* `age`: the age at measurement of the target variable.

We'll run the harmonization function on the `phen_list` list that we just created.

``` {.r}
out <- harmonize(phen_list)
head(out)
```

    ##   topmed_subject_id example_height age
    ## 1          20000002         160.02  31
    ## 2          20000003         152.40  27
    ## 3          20000004         165.10  47
    ## 4          20000005         177.80  39
    ## 5          20000008         154.94  34
    ## 6          20000010         160.02  34

As before, complete the harmonization for the second unit by renaming variables to include the concept variant number, and adding the unique subject we created earlier to the data frame.

``` {.r}
harmonized_studyb <- out %>%
  rename(
    example_height_1 = example_height,
    age_at_example_height_1 = age
  ) %>%
  left_join(subj, by = "topmed_subject_id") %>%
  select(
    SUBJECT_ID,
    unique_subject_key,
    topmed_subject_id,
    example_height_1,
    age_at_example_height_1
  )
head(harmonized_studyb)
```

    ##   SUBJECT_ID unique_subject_key topmed_subject_id example_height_1
    ## 1      s0463       StudyB_s0463          20000002           160.02
    ## 2      s0179       StudyB_s0179          20000003           152.40
    ## 3      s0526       StudyB_s0526          20000004           165.10
    ## 4      s0195       StudyB_s0195          20000005           177.80
    ## 5      s0118       StudyB_s0118          20000008           154.94
    ## 6      s0229       StudyB_s0229          20000010           160.02
    ##   age_at_example_height_1
    ## 1                      31
    ## 2                      27
    ## 3                      47
    ## 4                      39
    ## 5                      34
    ## 6                      34

Combine data from both harmonization units
------------------------------------------

After completing harmonization of all desired harmonization units, we only need to concatenate the data frames for all units:

``` {.r}
harmonized <- bind_rows(harmonized_studya, harmonized_studyb)
head(harmonized)
```

    ##   SUBJECT_ID unique_subject_key topmed_subject_id example_height_1
    ## 1     SUBJ_N      StudyA_SUBJ_N          10000003              143
    ## 2     SUBJ_J      StudyA_SUBJ_J          10000005              167
    ## 3     SUBJ_V      StudyA_SUBJ_V          10000007              181
    ## 4     SUBJ_T      StudyA_SUBJ_T          10000010              177
    ## 5    SUBJ_N1     StudyA_SUBJ_N1          10000011              176
    ## 6    SUBJ_V1     StudyA_SUBJ_V1          10000012              176
    ##   age_at_example_height_1
    ## 1                      32
    ## 2                      58
    ## 3                      46
    ## 4                      41
    ## 5                      59
    ## 6                      54

``` {.r}
tail(harmonized)
```

    ##    SUBJECT_ID unique_subject_key topmed_subject_id example_height_1
    ## 35      s0818       StudyB_s0818          20000007           149.86
    ## 36      s0299       StudyB_s0299          20000009           172.72
    ## 37      s0244       StudyB_s0244          20000011           147.32
    ## 38      s0374       StudyB_s0374          20000013           162.56
    ## 39      s0665       StudyB_s0665          20000014           175.26
    ## 40      s0091       StudyB_s0091          20000019           167.64
    ##    age_at_example_height_1
    ## 35                      32
    ## 36                      58
    ## 37                      46
    ## 38                      44
    ## 39                      41
    ## 40                      47

Final checks
------------

We'll do a final check on the subject id mapping. We need to make sure that there is a one-to-one mapping between `topmed_subject_id` and `unique_subject_key` across the entire harmonized data frame.

``` {.r}
mapping <- harmonized %>%
  select(unique_subject_key, topmed_subject_id)

stopifnot(all(!duplicated(mapping$unique_subject_key)))
stopifnot(all(!duplicated(mapping$topmed_subject_id)))
```

If these checks fail, you will need to verify that you created the `topmed_subject_id` values with a different `unique_id` for each different study accession on dbGaP.

Save the harmonized data
------------------------

Finally, save the harmonized data, in case you want to use them to harmonize a new variable.

``` {.r}
readr::write_tsv(harmonized, "example_height_1.tsv")
```

To check if you have run the harmonization correctly, please compare this file against the harmonized data in the `example_data/harmonized/example_height_1.tsv` file.

``` {.r}
check <- readr::read_tsv("example_data/harmonized/example_height_1.tsv")
stopifnot(all_equal(check, harmonized))
```
