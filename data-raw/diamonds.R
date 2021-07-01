## code to prepare `diamonds` dataset goes here

dataset <- ggplot2::diamonds

# define target and features
target.label <- "price"
features.labels <- dataset %>% select(-target.label) %>% names

# create data structure
diamonds <- list()
diamonds$target.label <- target.label
diamonds$features.labels <- features.labels
diamonds$data <- dataset

usethis::use_data(diamonds, overwrite = TRUE)
