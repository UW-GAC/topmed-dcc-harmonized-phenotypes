Reproduce the `example_bmi_1` variable
================
Adrienne Stilp
11 June, 2020

This document explains how to use the information in the JSON documentation to reproduce a given harmonized variable using other harmonized variables. To reproduce these types of variables, you will need to reproduce each component variable separately and then work with those harmonized values to reproduce the final harmonized variable.

In this document, we'll work with the `example_bmi_1.json` documentation. As we'll see soon, this harmonzied variable is produced from two other harmonized variables: `example_height_1` and `example_weight_1`. Please see the associated files for instructions on how to reproduce those harmonized variables from the original study data:

-   harmonized-variable-reproduction-height.md
-   harmonized-variable-reproduction-weight.md

In those files, we saved the final, combined harmonized variable in tab-separated text files with the required identifiers. We'll use those saved files in this harmonization.

Setup
=====

First, we'll need to load some packages to help work with the files.

``` {.r}
library(readr)
library(dplyr)
library(stringr)
library(jsonlite)
library(xml2)
```

Next, we'll read in the JSON documentation for `example_bmi_1`.

``` {.r}
json <- fromJSON('example_bmi_1.json', simplifyDataFrame = FALSE)
```

Reproducing the harmonized values
=================================

In this case, there is only one harmonization unit that uses the combined harmonized data:

``` {.r}
unit <- json$harmonization_units[[1]]
unit
```

    ## $name
    ## [1] "DCC_harmonized"
    ## 
    ## $component_study_variables
    ## list()
    ## 
    ## $component_harmonized_variables
    ## $component_harmonized_variables[[1]]
    ## $component_harmonized_variables[[1]]$dcc_harmonization_id
    ## [1] 1
    ## 
    ## $component_harmonized_variables[[1]]$name
    ## [1] "example_height_1"
    ## 
    ## $component_harmonized_variables[[1]]$version
    ## [1] 1
    ## 
    ## 
    ## $component_harmonized_variables[[2]]
    ## $component_harmonized_variables[[2]]$dcc_harmonization_id
    ## [1] 2
    ## 
    ## $component_harmonized_variables[[2]]$name
    ## [1] "example_weight_1"
    ## 
    ## $component_harmonized_variables[[2]]$version
    ## [1] 1
    ## 
    ## 
    ## 
    ## $harmonization_function
    ## [1] "harmonize <- function(phen_list) {\n  library(dplyr)\n  harmonized_data <- phen_list$harmonized_data\n  height <- harmonized_data$example_height_1 %>%\n    select(topmed_subject_id, example_height_1, age_at_example_height_1)\n  weight <- harmonized_data$example_weight_1 %>%\n    select(topmed_subject_id, example_weight_1)\n\n  dataset <- height %>%\n    # Merge with the weight dataset.\n    left_join(weight, by = c(\"topmed_subject_id\")) %>%\n    # Calculate BMI.\n    mutate(\n      example_bmi = as.numeric(example_weight_1) ^ 2 / as.numeric(example_height_1)\n    ) %>%\n    # Rename age column.\n    rename(\n      age = age_at_example_height_1\n    ) %>%\n    # Convert age to numeric.\n    mutate(\n      age = as.numeric(age)\n    ) %>%\n    # Keep only required columns.\n    select(topmed_subject_id, example_bmi, age)\n\n  # Return the final dataset.\n  dataset\n\n}\n"

You can see that the two required variables are `example_height_1` and `example_weight_1`. We will read in the harmonized values for these variables that were created from the `example_height_1.json` and `example_weight_1.json` documents.

``` {.r}
height <- readr::read_tsv("example_height_1.tsv")
head(as.data.frame(height))
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
weight <- readr::read_tsv("example_weight_1.tsv")
head(as.data.frame(weight))
```

    ##   SUBJECT_ID unique_subject_key topmed_subject_id example_weight_1
    ## 1     SUBJ_N      StudyA_SUBJ_N          10000003            41.94
    ## 2     SUBJ_J      StudyA_SUBJ_J          10000005            64.52
    ## 3     SUBJ_V      StudyA_SUBJ_V          10000007            80.81
    ## 4     SUBJ_T      StudyA_SUBJ_T          10000010            77.79
    ## 5    SUBJ_N1     StudyA_SUBJ_N1          10000011            94.40
    ## 6    SUBJ_V1     StudyA_SUBJ_V1          10000012            74.64
    ##   age_at_example_weight_1
    ## 1                      32
    ## 2                      58
    ## 3                      46
    ## 4                      41
    ## 5                      59
    ## 6                      54

We can determine the mapping between study subject identifier and `topmed_subject_id` from these files. We'll concatenate the identifier columns and then select the unique sets of identifiers.

``` {.r}
subj_map <- height %>%
  bind_rows(weight) %>%
  select(SUBJECT_ID, unique_subject_key, topmed_subject_id) %>%
  # Select unique sets.
  distinct()
head(subj_map)
```

    ## # A tibble: 6 x 3
    ##   SUBJECT_ID unique_subject_key topmed_subject_id
    ##   <chr>      <chr>                          <dbl>
    ## 1 SUBJ_N     StudyA_SUBJ_N               10000003
    ## 2 SUBJ_J     StudyA_SUBJ_J               10000005
    ## 3 SUBJ_V     StudyA_SUBJ_V               10000007
    ## 4 SUBJ_T     StudyA_SUBJ_T               10000010
    ## 5 SUBJ_N1    StudyA_SUBJ_N1              10000011
    ## 6 SUBJ_V1    StudyA_SUBJ_V1              10000012

``` {.r}
tail(subj_map)
```

    ## # A tibble: 6 x 3
    ##   SUBJECT_ID unique_subject_key topmed_subject_id
    ##   <chr>      <chr>                          <dbl>
    ## 1 s0818      StudyB_s0818                20000007
    ## 2 s0299      StudyB_s0299                20000009
    ## 3 s0244      StudyB_s0244                20000011
    ## 4 s0374      StudyB_s0374                20000013
    ## 5 s0665      StudyB_s0665                20000014
    ## 6 s0091      StudyB_s0091                20000019

We should also verify that there is a one-to-one mapping between `unique_subject_key` and `topmed_subject_id`:

``` {.r}
stopifnot(all(!duplicated(subj_map$unique_subject_key)))
stopifnot(all(!duplicated(subj_map$topmed_subject_id)))
```

If these checks fail, you will need to verify that you used the same study mapping integers as provided in the `example-phs-mapping.tsv` file when harmonizing the height and weight component variables. You will also need to check that you created the topmed\_subject\_id with a different `unique_id` for each different study accession on dbGaP. This check is essential because the harmonization functions generally combine data values for the same subject from the different component traits using `topmed_subject_id`.

Next, create the `phen_list` data structure from these harmonized\_variables. Unlike the examples for `example_height_1` and `example_weight_1`, the `phen_list` structure will not have a `source_data` element. Instead, it will have a `harmonized_data` element. This element is al ist containing harmonized data, with one element for each component harmonized trait. The name of those elements should be the name of that component variable. The contents of the elements is a data frame with columns `topmed_subject_id`, the component harmonized variable name (e.g., `example_height_1`), and the corresponding age variable, if it exists (e.g., `age_at_example_height_1`).

``` {.r}
harmonized_data <- list()
harmonized_data[["example_height_1"]] <- height %>%
  select(topmed_subject_id, example_height_1, age_at_example_height_1)
harmonized_data[["example_weight_1"]] <- weight %>%
  select(topmed_subject_id, example_weight_1, age_at_example_weight_1)
phen_list <- list(harmonized_data = harmonized_data)
lapply(phen_list$harmonized_data, head)
```

    ## $example_height_1
    ## # A tibble: 6 x 3
    ##   topmed_subject_id example_height_1 age_at_example_height_1
    ##               <dbl>            <dbl>                   <dbl>
    ## 1          10000003              143                      32
    ## 2          10000005              167                      58
    ## 3          10000007              181                      46
    ## 4          10000010              177                      41
    ## 5          10000011              176                      59
    ## 6          10000012              176                      54
    ## 
    ## $example_weight_1
    ## # A tibble: 6 x 3
    ##   topmed_subject_id example_weight_1 age_at_example_weight_1
    ##               <dbl>            <dbl>                   <dbl>
    ## 1          10000003             41.9                      32
    ## 2          10000005             64.5                      58
    ## 3          10000007             80.8                      46
    ## 4          10000010             77.8                      41
    ## 5          10000011             94.4                      59
    ## 6          10000012             74.6                      54

Next, we can extract the harmonization function from the JSON documentation for this harmonization unit.

``` {.r}
cat(unit$harmonization_function)
```

    ## harmonize <- function(phen_list) {
    ##   library(dplyr)
    ##   harmonized_data <- phen_list$harmonized_data
    ##   height <- harmonized_data$example_height_1 %>%
    ##     select(topmed_subject_id, example_height_1, age_at_example_height_1)
    ##   weight <- harmonized_data$example_weight_1 %>%
    ##     select(topmed_subject_id, example_weight_1)
    ## 
    ##   dataset <- height %>%
    ##     # Merge with the weight dataset.
    ##     left_join(weight, by = c("topmed_subject_id")) %>%
    ##     # Calculate BMI.
    ##     mutate(
    ##       example_bmi = as.numeric(example_weight_1) ^ 2 / as.numeric(example_height_1)
    ##     ) %>%
    ##     # Rename age column.
    ##     rename(
    ##       age = age_at_example_height_1
    ##     ) %>%
    ##     # Convert age to numeric.
    ##     mutate(
    ##       age = as.numeric(age)
    ##     ) %>%
    ##     # Keep only required columns.
    ##     select(topmed_subject_id, example_bmi, age)
    ## 
    ##   # Return the final dataset.
    ##   dataset
    ## 
    ## }

``` {.r}
harmonize <- eval(parse(text = unit$harmonization_function))
```

Next, we can run the harmonization on the `phen_list` structure that we just created to produce the final harmonized variable.

``` {.r}
out <- harmonize(phen_list)
head(out)
```

    ## # A tibble: 6 x 3
    ##   topmed_subject_id example_bmi   age
    ##               <dbl>       <dbl> <dbl>
    ## 1          10000003        12.3    32
    ## 2          10000005        24.9    58
    ## 3          10000007        36.1    46
    ## 4          10000010        34.2    41
    ## 5          10000011        50.6    59
    ## 6          10000012        31.7    54

``` {.r}
tail(out)
```

    ## # A tibble: 6 x 3
    ##   topmed_subject_id example_bmi   age
    ##               <dbl>       <dbl> <dbl>
    ## 1          20000007        23.9    32
    ## 2          20000009        20.1    58
    ## 3          20000011        17.4    46
    ## 4          20000013        38.6    44
    ## 5          20000014        36.8    41
    ## 6          20000019        47.7    47

As before, this harmonization function returns a data frame with columns `topmed_subject_id`, the phenotype concept, and age.

Finally, we can add the subject identifiers back to this data frame, and rename variables to add the phenotype concept from the JSON documentation.

``` {.r}
harmonized <- out %>%
  left_join(subj_map, by = "topmed_subject_id") %>%
  select(SUBJECT_ID, unique_subject_key, topmed_subject_id, everything()) %>%
  rename(
    example_bmi_1 = example_bmi,
    age_at_example_bmi_1 = age
  )
head(as.data.frame(harmonized))
```

    ##   SUBJECT_ID unique_subject_key topmed_subject_id example_bmi_1
    ## 1     SUBJ_N      StudyA_SUBJ_N          10000003      12.30044
    ## 2     SUBJ_J      StudyA_SUBJ_J          10000005      24.92713
    ## 3     SUBJ_V      StudyA_SUBJ_V          10000007      36.07876
    ## 4     SUBJ_T      StudyA_SUBJ_T          10000010      34.18805
    ## 5    SUBJ_N1     StudyA_SUBJ_N1          10000011      50.63273
    ## 6    SUBJ_V1     StudyA_SUBJ_V1          10000012      31.65415
    ##   age_at_example_bmi_1
    ## 1                   32
    ## 2                   58
    ## 3                   46
    ## 4                   41
    ## 5                   59
    ## 6                   54

We'll save this harmonized variable in the example data directory.

``` {.r}
readr::write_tsv(harmonized, "example_bmi_1.tsv")
```

To check if you have run the harmonization correctly, please compare this file against the harmonized data in the `example_data/harmonized/example_bmi_1.tsv` file.

``` {.r}
check <- readr::read_tsv("example_data/harmonized/example_bmi_1.tsv")
stopifnot(all_equal(check, harmonized))
```
