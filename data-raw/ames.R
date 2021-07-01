## code to prepare `ames` dataset goes here

dataset <- AmesHousing::make_ames()

# define target and features
target.label <- "Sale_Price"
features.labels <- dataset %>% select(-target.label) %>% names

ames <- dataset

usethis::use_data(ames, overwrite = TRUE)
