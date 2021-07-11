## code to prepare `designdim` dataset goes here

filepath <- system.file(
  "extdata",
  "designdim.items.rds",
  package = "catcoding"
) %>% print

dataset <- readRDS(filepath) %>% as_tibble()

# define target and features
target.label <- "NPS"
features.labels <- dataset %>% select(-target.label) %>% names

dataset %<>% 
  mutate(across(features.labels, as.factor)) %>% 
  # convert Likert to ordinal
  mutate(across(where(is.factor), as.ordered))

# create data structure
designdim <- list()
designdim$target.label <- target.label
designdim$features.labels <- features.labels
designdim$data <- dataset

usethis::use_data(designdim, overwrite = TRUE)
