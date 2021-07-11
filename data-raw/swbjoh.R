## code to prepare `swbjoh` dataset goes here
filepath <- system.file(
  "extdata",
  "processed_data.rds",
  package = "catcoding"
) %>% print

dataset <- readRDS(filepath) %>% print

dataset %<>% 
  select(starts_with("big_five_"),
         "well_being_state"
         ) %>% 
  # tricky: replace "NA" by NA datatype
  mutate(across(everything(), ~na_if(.x, "NA"))) %>% 
  # remove rows with all NAs
  filter(if_all(everything(), ~!is.na(.)))
  
# define target and features
target.label <- "well_being_state"
features.labels <- dataset %>% select(-target.label) %>% names

# create data structure
swbjoh <- list()
swbjoh$target.label <- target.label
swbjoh$features.labels <- features.labels
swbjoh$data <- dataset
usethis::use_data(swbjoh, overwrite = TRUE)
