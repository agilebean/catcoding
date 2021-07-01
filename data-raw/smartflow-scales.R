## code to prepare `smartflow.scales` dataset goes here

filepath1 <- system.file(
  "extdata",
  "smartflow.descriptive.rds",
  package = "catcoding"
) %>% print

filepath2 <- system.file(
  "extdata",
  "smartflow.scales.rds",
  package = "catcoding"
) %>% print

# no desc.stats exists. tricky: design.descriptives are design features!
data.descriptive <- readRDS(filepath1) 
data.scales <- readRDS(filepath2)

dataset <- cbind(data.descriptive, data.scales) %>% 
  select(-feedback, -email) %>% 
  na.omit() %>% # n=307
  as_tibble()

# define target and features
target.label <- "SmartphoneAddiction"
features.labels <- dataset %>% 
  select(-target.label, -SmartphoneHours) %>% names

# convert Likert to factor
smartflow.scales <- dataset %>% 
  mutate(across(referral:relationship, as.factor)) %>% 
  mutate(age = as.numeric(age))

usethis::use_data(smartflow.scales, overwrite = TRUE)
