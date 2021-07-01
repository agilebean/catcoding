## code to prepare `diamonds` dataset goes here

dataset <- ggplot2::diamonds

# define target and features
target.label <- "price"
features.labels <- dataset %>% select(-target.label) %>% names

usethis::use_data(dataset, overwrite = TRUE)
