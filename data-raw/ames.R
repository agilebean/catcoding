## code to prepare `ames` dataset goes here

dataset <- AmesHousing::make_ames()

# define target and features
target.label <- "Sale_Price"
features.labels <- dataset %>% select(-target.label) %>% names

# create data structure
ames <- list()
ames$target.label <- target.label
ames$features.labels <- features.labels
ames$data <- dataset

usethis::use_data(ames, overwrite = TRUE)
