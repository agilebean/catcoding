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

# convert Likert to factor
designdim <- dataset %>% 
  mutate(across(features.labels, as.factor)) %>% 
  # convert Likert to ordinal
  mutate(across(where(is.factor), as.ordered))

usethis::use_data(designdim, overwrite = TRUE)
