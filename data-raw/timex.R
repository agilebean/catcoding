## code to prepare `timex` dataset goes here

filepath1 <- system.file(
  "extdata",
  "timex.descriptive.rds",
  package = "catcoding"
) %>% print

filepath2 <- system.file(
  "extdata",
  "timex.items.rds",
  package = "catcoding"
) %>% print

# no desc.stats exists. tricky: design.descriptives are design features!
data.descriptive <- readRDS(filepath1) 
data.items <- readRDS(filepath2)

dataset <- cbind(data.descriptive, data.items) %>% 
  select(referral:happy, -feedback, -email) %>% 
  mutate(
    HAPPINESS = rowMeans(
      select(., active:alert, starts_with("lifesatis"), happy)
    )
  ) %>% 
  na.omit() %>% # n=252, 82
  as_tibble()

# define target and features
target.label <- "HAPPINESS"
features.labels <- dataset %>% 
  select(-target.label) %>% 
  select(-c(active:ashamed)) %>% #DV: PANAS-PA/-NA 
  select(-starts_with("lifesatis"), -happy) %>% #DV: lifesatis+happiness-global
  names

# convert Likert to factor
dataset %<>% 
  mutate(across(features.labels, as.factor)) %>% 
  mutate(age = as.numeric(age)) %>% 
  # convert Likert to ordinal
  mutate(across(-c(referral:relationship, target.label),
                as.ordered))

# create data structure
timex <- list()
timex$target.label <- target.label
timex$features.labels <- features.labels
timex$data <- dataset

usethis::use_data(timex, overwrite = TRUE)


usethis::use_data(timex, overwrite = TRUE)
