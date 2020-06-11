#! /usr/bin/env Rscript

# Script to create example dbGaP data for use in the example Rmd files.
# Usage: Rscript make_example_study_data.R

library(tidyverse)
library(dbgaptools)

# Set a seed for reproducibility
set.seed(123)


# Setup
n_subj <- 20
consent_groups <- c(1, 2)
file_prefix <- "Test_Study_2"

out_dir <- file_prefix
unlink(file_prefix, recursive = TRUE)
dir.create(out_dir)

## Subject file

subj <- data.frame(
  SUBJECT_ID = sprintf("s%04d", sample(1:1000, n_subj, replace = TRUE)),
  CONSENT = sample(unname(consent_groups), n_subj, replace = TRUE),
  stringsAsFactors = FALSE
)

# Data file
subj_data_file <- file.path(out_dir, sprintf("%s_Subject.txt", file_prefix))
readr::write_tsv(subj, subj_data_file, na = "")

# Data dictionary
dd <- bind_rows(list(
  tibble(
    VARNAME = "SUBJECT_ID",
    VARDESC = "Subject ID",
    TYPE = "string"
  ),
  tibble(
    VARNAME = "CONSENT",
    VARDESC = "Consent group as determined by DAC",
    TYPE = "encoded value",
    TMP_VALUES = "1=General Research Use (GRU); 2=Health/Medical/Biomedical (HMB)"
  )
))
dd <- dd %>%
  separate(TMP_VALUES, into = c("VALUES", "X"), sep = "; ")
names(dd)[names(dd) == "X"] <- NA
subj_dd_file <- str_replace(subj_data_file, ".txt", "_DD.txt")
readr::write_tsv(dd, subj_dd_file, na = "")

# Check with dbgaptools
stopifnot(is.null(check_subj(subj_data_file, subj_dd_file)))


## Phenotype file 1
phen1 <- subj %>%
  select(-CONSENT) %>%
  mutate(
    height = round(rnorm(n_subj, mean = 65, sd = 5)),
    bmi = rnorm(n_subj, mean = 25, sd = 4),
    weight = round(bmi * height^2 / 703, digits = 2)
  ) %>%
  select(-bmi)

# Write the data files.
phen1_data_file <- file.path(out_dir, sprintf("%s_Phenotypes.txt", file_prefix))
readr::write_tsv(phen1, phen1_data_file, na = "")

dd <- bind_rows(list(
  tibble(
    VARNAME = "SUBJECT_ID",
    VARDESC = "Subject ID",
    TYPE = "string",
    UNITS = NA
  ),
  tibble(
    VARNAME = "height",
    VARDESC = "Standing height",
    TYPE = "decimal",
    UNITS = "inches"
  ),
  tibble(
    VARNAME = "weight",
    VARDESC = "Body weight",
    TYPE = "decimal",
    UNITS = "pounds"
  )
)) %>%
  mutate(VALUES = NA)

phen1_dd_file <- str_replace(phen1_data_file, ".txt", "_DD.txt")
readr::write_tsv(dd, phen1_dd_file, na = "")

# Check with dbgaptools
stopifnot(is.null(
  check_pheno(phen1_data_file, phen1_dd_file)
))


## Phenotype file 2
phen2 <- subj %>%
  select(-CONSENT) %>%
  mutate(
    age = sample(25:60, n_subj, replace = T)
  )

# Write the data files.
phen2_data_file <- file.path(out_dir, sprintf("%s_Ages.txt", file_prefix))
readr::write_tsv(phen2, phen2_data_file, na = "")

dd <- bind_rows(list(
  tibble(
    VARNAME = "SUBJECT_ID",
    VARDESC = "Subject ID",
    TYPE = "string",
    UNITS = NA
  ),
  tibble(
    VARNAME = "age",
    VARDESC = "Age of subject",
    TYPE = "decimal",
    UNITS = "years"
  )
)) %>%
  mutate(VALUES = NA)

phen2_dd_file <- str_replace(phen2_data_file, ".txt", "_DD.txt")
readr::write_tsv(dd, phen2_dd_file, na = "")

# Check with dbgaptools
stopifnot(is.null(
  check_pheno(phen2_data_file, phen2_dd_file)
))


## Sample Subject mapping
ssm <- subj %>%
  select(-CONSENT) %>%
  mutate(
    SAMPLE_ID = sprintf("g%03d", sample(1:100, n_subj))
  )
ssm

ssm_data_file <- file.path(out_dir, sprintf("%s_SampleSubjectMapping.txt", file_prefix))
readr::write_tsv(ssm, ssm_data_file, na = "")

dd <- dd <- bind_rows(list(
  tibble(
    VARNAME = "SUBJECT_ID",
    VARDESC = "Subject ID",
    TYPE = "string",
  ),
  tibble(
    VARNAME = "SAMPLE_ID",
    VARDESC = "Sample ID",
    TYPE = "string",
  )
)) %>%
  mutate(VALUES = NA)

ssm_dd_file <- str_replace(ssm_data_file, ".txt", "_DD.txt")
readr::write_tsv(dd, ssm_dd_file, na = "")

# Check with dbgaptools
stopifnot(is.null(
  check_ssm(ssm_data_file, ssm_dd_file)
))

## Sample attributes
sat <- ssm %>%
  select(-SUBJECT_ID) %>%
  mutate(
    BODY_SITE = 'Blood',
    ANALYTE_TYPE = 'DNA',
    IS_TUMOR = 'No'
  )
sat

sat_data_file <- file.path(out_dir, sprintf("%s_SampleAttributes.txt", file_prefix))
readr::write_tsv(sat, sat_data_file, na = "")

dd <- dd <- dd <- bind_rows(list(
  tibble(
    VARNAME = "SAMPLE_ID",
    VARDESC = "Sample ID",
    TYPE = "string",
  ),
  tibble(
    VARNAME = "BODY_SITE",
    VARDESC = "Body site where sample was collected",
    TYPE = "string",
  ),
  tibble(
    VARNAME = "ANALYTE_TYPE",
    VARDESC = "Analyte type",
    TYPE = "string",
  ),
  tibble(
    VARNAME = "IS_TUMOR",
    VARDESC = "Tumor status",
    TYPE = "string",
  )
)) %>%
  mutate(
    UNITS = NA,
    VALUES = NA
  )


sat_dd_file <- str_replace(sat_data_file, ".txt", "_DD.txt")
readr::write_tsv(dd, sat_dd_file, na = "")

# Check with dbgaptools
stopifnot(is.null(
  check_sattr(sat_data_file, sat_dd_file)
))


## Final checks
stopifnot(is.null(
  check_cross_file(subj_data_file, ssm_data_file, ssm$SAMPLE_ID,
                   sattr_file = sat_data_file, pheno_file = phen1_data_file)
))

stopifnot(is.null(
  check_cross_file(subj_data_file, ssm_data_file, ssm$SAMPLE_ID,
                   sattr_file = sat_data_file, pheno_file = phen2_data_file)
))
