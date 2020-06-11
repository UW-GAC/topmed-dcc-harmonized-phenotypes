#! /usr/bin/env Rscript

# Script to create example dbGaP data for use in the example Rmd files.
# Usage: Rscript make_example_study_data.R

library(xml2)
library(dbgaptools)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

example_dbgap_dir <- "dbgap"
unlink(example_dbgap_dir, recursive = TRUE)
dir.create(example_dbgap_dir)

dbgap_date <- "Wed Nov 27 10:00:00 2019"

### FIRST STUDY

# Setup
n_subj <- 20
phs <- 1
version <- 1
pht <- 1
file_prefix <- "Example_StudyA"
consent_groups <- c("HMB-NPU" = 1, "GRU-NPU" = 2)

study_dir <- sprintf("phs%06d.v%d", phs, version)
out_dir <- file.path(example_dbgap_dir, study_dir)
dir.create(out_dir)

header <- "#\n# SIMULATED dbGaP DATA\n#"

## Subject file

subj <- data.frame(
  dbGaP_Subject_ID = sample(1:n_subj),
  SUBJECT_ID = sprintf("SUBJ_%s", make.unique(sample(LETTERS, n_subj, replace = TRUE), sep = "")),
  consent = sample(unname(consent_groups), n_subj, replace = TRUE),
  stringsAsFactors = FALSE
)

n_vars <- ncol(subj)

# Data file
data_file <- file.path(out_dir,
                       sprintf("phs%06d.v1.pht%06d.v1.p1.%s_Subject.MULTI.txt", phs, pht, file_prefix))
writeLines(header, con = data_file)
readr::write_tsv(subj, data_file, na = "", append = TRUE, col_names = TRUE)

# Data dictionary
xml <- xml2::xml_new_document() %>%
  xml_add_child("data_table",
                id = sprintf("pht%06d.v1", pht),
                study_id = sprintf("phs%06d.v1", phs),
                participant_set = 1,
                date_created = dbgap_date) %>%
  xml_add_child("description", "The subject consent data table contains subject IDs and consent information.") %>%
  # Add the subject node
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 1)) %>%
  xml_add_child("name", "SUBJECT_ID") %>%
  xml_add_sibling("description", "Subject ID") %>%
  xml_add_sibling("type", "string") %>%
  xml_parent() %>%
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 2)) %>%
  xml_add_child("name", "consent") %>%
  xml_add_sibling("description", "Consent group as determined by DAC") %>%
  xml_add_sibling("type", "encoded value") %>%
  xml_add_sibling("value", "Consent group 1 DULs", code = "1") %>%
  xml_add_sibling("value", "Consent group 2 DULs", code = "2") %>%
  xml_root()

xml_file <- file.path(out_dir,
                      sprintf("phs%06d.v1.pht%06d.v1.%s_Subject.data_dict.xml", phs, pht, file_prefix))
xml2::write_xml(xml, xml_file)

## Phenotype file
pht <- pht + 1

phen <- subj %>%
  select(-consent) %>%
  mutate(
    HGTV1 = round(rnorm(n_subj, mean = 165, sd = 13)),
    bmi = rnorm(n_subj, mean = 25, sd = 4),
    WGTV1 = round(bmi * (HGTV1 / 100)^2, digits = 2),
    AGEV1 = sample(25:60, n_subj, replace = T)
  ) %>%
  select(-bmi)

# Put different consent groups in different files.
phen_1 <- phen %>%
  filter(dbGaP_Subject_ID %in% subj$dbGaP_Subject_ID[subj$consent == 1])
phen_2 <- phen %>%
  filter(dbGaP_Subject_ID %in% subj$dbGaP_Subject_ID[subj$consent == 2])

n_vars <- ncol(phen) - 1

# Write the data files.
data_file_1 <- file.path(
  out_dir,
  sprintf("phs%06d.v1.pht%06d.v1.p1.c1.%s_Phenotypes.%s.txt", phs, pht, file_prefix, names(consent_groups)[1])
)
writeLines(header, con = data_file_1)
readr::write_tsv(phen_1, data_file_1, na = "", append = TRUE, col_names = TRUE)
data_file_2 <- file.path(
  out_dir,
  sprintf("phs%06d.v1.pht%06d.v1.p1.c2.%s_Phenotypes.%s.txt", phs, pht, file_prefix, names(consent_groups)[2])
)
writeLines(header, con = data_file_2)
readr::write_tsv(phen_2, data_file_2, na = "", append = TRUE, col_names = TRUE)

# Create an XML file.
xml <- xml2::xml_new_document() %>%
  xml_add_child("data_table",
                id = sprintf("pht%06d.v1", pht),
                study_id = sprintf("phs%06d.v1", phs),
                participant_set = 1,
                date_created = dbgap_date) %>%
  xml_add_child("description", "Measured subject phenotypes") %>%
  # Add the subject node
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 3)) %>%
  xml_add_child("name", "SUBJECT_ID") %>%
  xml_add_sibling("description", "Subject ID") %>%
  xml_add_sibling("type", "string") %>%
  xml_parent() %>%
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 4)) %>%
  xml_add_child("name", "HGTV1") %>%
  xml_add_sibling("description", "Height at visit 1") %>%
  xml_add_sibling("type", "decimal") %>%
  xml_add_sibling("units", "cm") %>%
  xml_parent() %>%
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 5)) %>%
  xml_add_child("name", "WGTV1") %>%
  xml_add_sibling("description", "Weight at visit 1") %>%
  xml_add_sibling("type", "decimal") %>%
  xml_add_sibling("units", "kg") %>%
  xml_parent() %>%
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 6)) %>%
  xml_add_child("name", "AGEV1") %>%
  xml_add_sibling("description", "Age at visit 1") %>%
  xml_add_sibling("type", "decimal") %>%
  xml_add_sibling("units", "years") %>%
  xml_root()

# Write the XML data dictionary.
xml_file <- file.path(out_dir,
                      sprintf("phs%06d.v1.pht%06d.v1.%s_Phenotypes.data_dict.xml", phs, pht, file_prefix))
xml2::write_xml(xml, xml_file)


### SECOND STUDY

# Setup
n_subj_a <- n_subj
n_subj <- 10
phs <- 2
version <- 1
pht <- pht + 1
file_prefix <- "Example_StudyB"
consent_groups <- c("DS-IRB" = 1, "HMB-NPU" = 2)


study_dir <- sprintf("phs%06d.v%d", phs, version)
out_dir <- file.path(example_dbgap_dir, study_dir)
dir.create(out_dir)

header <- "#\n# SIMULATED dbGaP DATA\n#"

## Subject file

subj <- data.frame(
  dbGaP_Subject_ID = sample(1:n_subj) + n_subj_a,
  SUBJECT_ID = sprintf("s%04d", sample(1:1000, n_subj)),
  consent = sample(unname(consent_groups), n_subj, replace = TRUE),
  stringsAsFactors = FALSE
)

n_vars <- ncol(subj)

# Data file
data_file <- file.path(out_dir,
                       sprintf("phs%06d.v1.pht%06d.v1.p1.%s_Subject.MULTI.txt", phs, pht, file_prefix))
writeLines(header, con = data_file)
readr::write_tsv(subj, data_file, na = "", append = TRUE, col_names = TRUE)

# Data dictionary
xml <- xml2::xml_new_document() %>%
  xml_add_child("data_table",
                id = sprintf("pht%06d.v1", pht),
                study_id = sprintf("phs%06d.v1", phs),
                participant_set = 1,
                date_created = dbgap_date) %>%
  xml_add_child("description", "The subject consent data table contains subject IDs and consent information.") %>%
  # Add the subject node
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 7)) %>%
  xml_add_child("name", "SUBJECT_ID") %>%
  xml_add_sibling("description", "Subject ID") %>%
  xml_add_sibling("type", "string") %>%
  xml_parent() %>%
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 8)) %>%
  xml_add_child("name", "consent") %>%
  xml_add_sibling("description", "Consent group as determined by DAC") %>%
  xml_add_sibling("type", "encoded value") %>%
  xml_add_sibling("value", "Consent group 1 DULs", code = "1") %>%
  xml_add_sibling("value", "Consent group 2 DULs", code = "2") %>%
  xml_root()

xml_file <- file.path(out_dir,
                      sprintf("phs%06d.v1.pht%06d.v1.%s_Subject.data_dict.xml", phs, pht, file_prefix))
xml2::write_xml(xml, xml_file)

## Phenotype file
pht <- pht + 1

phen <- subj %>%
  select(-consent) %>%
  mutate(
    height = round(rnorm(n_subj, mean = 65, sd = 5)),
    bmi = rnorm(n_subj, mean = 25, sd = 4),
    weight = round(bmi * height^2 / 703, digits = 2)
  ) %>%
  select(-bmi)

# Put different consent groups in different files.
phen_1 <- phen %>%
  filter(dbGaP_Subject_ID %in% subj$dbGaP_Subject_ID[subj$consent == 1])
phen_2 <- phen %>%
  filter(dbGaP_Subject_ID %in% subj$dbGaP_Subject_ID[subj$consent == 2])

n_vars <- ncol(phen) - 1

# Write the data files.
data_file_1 <- file.path(
  out_dir,
  sprintf("phs%06d.v1.pht%06d.v1.p1.c1.%s_Visit1.%s.txt", phs, pht, file_prefix, names(consent_groups)[1])
)
writeLines(header, con = data_file_1)
readr::write_tsv(phen_1, data_file_1, na = "", append = TRUE, col_names = TRUE)
data_file_2 <- file.path(
  out_dir,
  sprintf("phs%06d.v1.pht%06d.v1.p1.c2.%s_Visit1.%s.txt", phs, pht, file_prefix, names(consent_groups)[2])
)
writeLines(header, con = data_file_2)
readr::write_tsv(phen_2, data_file_2, na = "", append = TRUE, col_names = TRUE)

# Create an XML file.
xml <- xml2::xml_new_document() %>%
  xml_add_child("data_table",
                id = sprintf("pht%06d.v1", pht),
                study_id = sprintf("phs%06d.v1", phs),
                participant_set = 1,
                date_created = dbgap_date) %>%
  xml_add_child("description", "Measured subject phenotypes") %>%
  # Add the subject node
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 9)) %>%
  xml_add_child("name", "SUBJECT_ID") %>%
  xml_add_sibling("description", "Subject ID") %>%
  xml_add_sibling("type", "string") %>%
  xml_parent() %>%
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 10)) %>%
  xml_add_child("name", "height") %>%
  xml_add_sibling("description", "Standing height") %>%
  xml_add_sibling("type", "decimal") %>%
  xml_add_sibling("units", "inches") %>%
  xml_parent() %>%
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 11)) %>%
  xml_add_child("name", "weight") %>%
  xml_add_sibling("description", "Body weight") %>%
  xml_add_sibling("type", "decimal") %>%
  xml_add_sibling("units", "pounds") %>%
  xml_root()

# Write the XML data dictionary.
xml_file <- file.path(out_dir,
                      sprintf("phs%06d.v1.pht%06d.v1.%s_Visit1.data_dict.xml", phs, pht, file_prefix))
xml2::write_xml(xml, xml_file)

## Second phenotype file with age
pht <- pht + 1

phen <- subj %>%
  select(-consent) %>%
  mutate(
    age = sample(25:60, n_subj, replace = T)
  )
# Put different consent groups in different files.
phen_1 <- phen %>%
  filter(dbGaP_Subject_ID %in% subj$dbGaP_Subject_ID[subj$consent == 1])
phen_2 <- phen %>%
  filter(dbGaP_Subject_ID %in% subj$dbGaP_Subject_ID[subj$consent == 2])

n_vars <- ncol(phen) - 1

# Write the data files.
data_file_1 <- file.path(
  out_dir,
  sprintf("phs%06d.v1.pht%06d.v1.p1.c1.%s_Ages.%s.txt", phs, pht, file_prefix, names(consent_groups)[1])
)
writeLines(header, con = data_file_1)
readr::write_tsv(phen_1, data_file_1, na = "", append = TRUE, col_names = TRUE)
data_file_2 <- file.path(
  out_dir,
  sprintf("phs%06d.v1.pht%06d.v1.p1.c2.%s_Ages.%s.txt", phs, pht, file_prefix, names(consent_groups)[2])
)
writeLines(header, con = data_file_2)
readr::write_tsv(phen_2, data_file_2, na = "", append = TRUE, col_names = TRUE)

# Create an XML file.
xml <- xml2::xml_new_document() %>%
  xml_add_child("data_table",
                id = sprintf("pht%06d.v1", pht),
                study_id = sprintf("phs%06d.v1", phs),
                participant_set = 1,
                date_created = dbgap_date) %>%
  xml_add_child("description", "Measured subject phenotypes") %>%
  # Add the subject node
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 12)) %>%
  xml_add_child("name", "SUBJECT_ID") %>%
  xml_add_sibling("description", "Subject ID") %>%
  xml_add_sibling("type", "string") %>%
  xml_parent() %>%
  xml_add_sibling("variable", id = sprintf("phv%08d.v1", 13)) %>%
  xml_add_child("name", "age") %>%
  xml_add_sibling("description", "Age of subject") %>%
  xml_add_sibling("type", "decimal") %>%
  xml_add_sibling("units", "years") %>%
  xml_root()

# Write the XML data dictionary.
xml_file <- file.path(out_dir,
                      sprintf("phs%06d.v1.pht%06d.v1.%s_Ages.data_dict.xml", phs, pht, file_prefix))
xml2::write_xml(xml, xml_file)
