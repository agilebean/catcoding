## code to prepare `smartflow` dataset goes here

filepath1 <- system.file(
  "extdata",
  "smartflow.descriptive.rds",
  package = "catcoding"
) %>% print

filepath2 <- system.file(
  "extdata",
  "smartflow.items.rds",
  package = "catcoding"
) %>% print

# no desc.stats exists. tricky: design.descriptives are design features!
data.descriptive <- readRDS(filepath1) 
data.items <- readRDS(filepath2)

dataset <- cbind(data.descriptive, data.items) %>% 
  select(-feedback, -email) %>% 
  mutate(., SMADDICTION = rowMeans(
    select(., starts_with("addicted")), na.rm = TRUE)) %>% 
  na.omit() %>% # n=307
  as_tibble()

# define target and features
target.label <- "SMADDICTION"
features.labels <- dataset %>% 
  select(-target.label, -starts_with("addicted")) %>% names

# convert Likert to factor
smartflow <- dataset %>% 
  mutate(across(features.labels, as.factor)) %>% 
  mutate(across(c(age, smartphonehours), as.numeric)) %>% 
  # convert Likert to ordinal
  mutate(across(-c(referral:relationship, target.label),
                as.ordered))

usethis::use_data(smartflow, overwrite = TRUE)
