## code to prepare `swbsun` dataset goes here
filepath <- system.file(
  "extdata",
  "swbsun AspectsWB_2_Sample2.xlsx",
  package = "catcoding"
) %>% print

dataset <- readxl::read_excel(filepath)

dataset %<>% 
  rowwise() %>% 
  mutate(SWB = mean(c_across(starts_with("SWLS")))) %>% 
  select(starts_with("BFAS"), SWB) %>% 
  ungroup() %>% 
  mutate(across(-SWB, as_factor))

# define target and features
target.label <- "SWB"
features.labels <- dataset %>% select(-target.label) %>% names

# create data structure
swbsun <- list()
swbsun$target.label <- target.label
swbsun$features.labels <- features.labels
swbsun$data <- dataset

usethis::use_data(swbsun, overwrite = TRUE)

