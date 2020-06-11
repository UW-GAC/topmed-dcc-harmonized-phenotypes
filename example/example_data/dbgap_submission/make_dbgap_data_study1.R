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
file_prefix <- "Test_Study_1"

out_dir <- file_prefix
unlink(file_prefix, recursive = TRUE)
dir.create(out_dir)

## Subject file

subj <- data.frame(
  SUBJECT_ID = sprintf("SUBJ_%s", make.unique(sample(LETTERS, n_subj, replace = TRUE), sep = "")),
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


## Phenotype file
phen <- subj %>%
  select(-CONSENT) %>%
  mutate(
    HGTV1 = round(rnorm(n_subj, mean = 165, sd = 13)),
    bmi = rnorm(n_subj, mean = 25, sd = 4),
    WGTV1 = round(bmi * (HGTV1 / 100)^2, digits = 2),
    AGEV1 = sample(25:60, n_subj, replace = T)
  ) %>%
  select(-bmi)

# Write the data files.
phen_data_file <- file.path(out_dir, sprintf("%s_Phenotypes.txt", file_prefix))
readr::write_tsv(phen, phen_data_file, na = "")

dd <- bind_rows(list(
  tibble(
    VARNAME = "SUBJECT_ID",
    VARDESC = "Subject ID",
    TYPE = "string",
    UNITS = NA
  ),
  tibble(
    VARNAME = "HGTV1",
    VARDESC = "Height at visit 1",
    TYPE = "decimal",
    UNITS = "cm"
  ),
  tibble(
    VARNAME = "WGTV1",
    VARDESC = "Weight at visit 1",
    TYPE = "decimal",
    UNITS = "kg"
  ),
  tibble(
    VARNAME = "AGEV1",
    VARDESC = "Age at visit 1",
    TYPE = "decimal",
    UNITS = "years"
  )
)) %>%
  mutate(VALUES = NA)

phen_dd_file <- str_replace(phen_data_file, ".txt", "_DD.txt")
readr::write_tsv(dd, phen_dd_file, na = "")

# Check with dbgaptools
stopifnot(is.null(
  check_pheno(phen_data_file, phen_dd_file)
))


## Sample Subject mapping
ssm <- subj %>%
  select(-CONSENT) %>%
  mutate(
    SAMPLE_ID = sprintf("SAMP_%03d", sample(1:100, n_subj))
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
                   sattr_file = sat_data_file, pheno_file = phen_data_file)
))
