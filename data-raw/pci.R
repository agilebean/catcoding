## code to prepare `pci` dataset goes here

filepath <- system.file(
  "extdata",
  "pci.rds",
  package = "catcoding"
) %>% print

dataset <- readRDS(filepath) %>% as_tibble()

target.label <- "job_score"
features.labels <- dataset %>% select(-target.label) %>% names

pci <- dataset
usethis::use_data(pci, overwrite = TRUE)
