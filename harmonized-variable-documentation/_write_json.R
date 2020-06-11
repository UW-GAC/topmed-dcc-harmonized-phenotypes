#! /usr/bin/env Rscript
#
# Internal script to produce JSON documentation files from the DCC's database.
#
# Usage Rscript _write_json.R <dataset_version_id>
#
library(dbTopmed)

args <- commandArgs(trailingOnly = TRUE)
dataset_version_id <- as.integer(args[1])
stopifnot(!is.na(dataset_version_id))

db <- getDb()


query <- paste(
  "SELECT * FROM view_harmonized_dataset_all",
  "WHERE dataset_version_id = ?"
)
dataset_info <- dbGetQuery(db, query, params = list(dataset_version_id))
stopifnot(nrow(dataset_info) == 1)

msg <- sprintf("Adding documentation for dataset version %d: %s v%d",
               dataset_version_id, dataset_info$dataset_name, dataset_info$dataset_version)
message(msg)

dataset_info$lower_name <- gsub(" ", "_", tolower(dataset_info$dataset_name))

outdir <- dataset_info$lower_name
if (file.exists(outdir)) {
  unlink(outdir, recursive = TRUE)
}
dir.create(outdir)

query <- paste(
  "SELECT * FROM view_harmonized_trait_dataset_all",
  "WHERE dataset_version_id = ?"
)
traits <- dbGetQuery(db, query, params = list(dataset_version_id))

for (i in seq_along(traits$harmonized_trait_id)) {
  trait <- traits[i, ]
  trait_name <- trait$trait_flavor_name
  set_id <- trait$harmonized_trait_set_version_id
  outfile <- file.path(outdir, sprintf("%s.json", trait_name))
  message(sprintf("- creating %s", trait_name))

  dbExportHarmonizedTraitSetVersionJson(db, set_id, outfile)
}

dbDisconnect(db)
