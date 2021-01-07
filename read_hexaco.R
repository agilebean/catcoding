################################################################################
#
# Script:  read_hexaco.R
# Output:  training.set, testing.set - library: category_encoders
#
################################################################################
libraries <- c("dplyr", "magrittr", "tidyverse", "purrr"
               , "sjlabelled" # read SPSS
               , "caret", "doParallel"
               , "stargazer", "DataExplorer", "skimr"
               , "machinelearningtools"
               , "knitr", "pander"
)
sapply(libraries, require, character.only = TRUE)

# nominal <- FALSE # with ordinal as ORDERED factors
nominal <- TRUE # wit

# NEW <- TRUE
NEW <- FALSE

dataset.label <- "data/dataset hexaco.rds"

if (NEW) {
  
  filename <- "data/HEXACO Online data with sex age Years 1-4"
  filename.sav <- paste0(filename, ".sav")
  filename.rds <- paste0(filename, ".rds")
  
  system.time(
    file.raw <- sjlabelled::read_spss(
      filename.sav,
      atomic.to.fac = TRUE,
      verbose = FALSE)
  ) # 5.65s

  system.time(
    file.raw %>% saveRDS(filename.rds)  
  ) # 11.1s
  
  system.time(
    data.labels <- foreign::read.spss(filename.sav) %>%
      attributes %>%
      .$variable.labels %T>% print    
  ) # 1.2s
  
  file.raw %>% glimpse()
  file.raw %>% summary()
  file.raw$sexnumeric %>% table
  
  # missing values
  file.raw %>% map_df(~ is.na(.) %>% sum) %>% select(where(~ sum(.) != 0))
  
  file.raw %>% dim
  file.raw %>% na.omit %>% dim
  
  data.raw <- file.raw %>% 
    select(-id) %>% 
    na.omit %>% 
    as_tibble()
  
  system.time(
    data.raw %>% saveRDS(dataset.label)  
  ) # 10.4s
  
} else {
  # system.time(
  #   file.raw <- readRDS(filename.rds) 
  # ) # 0.8s
  
  system.time(
    data.raw <- readRDS(dataset.label)
  ) # 0.75s
}


data.raw %>% glimpse()
data.raw %>% summary()
# dataset %>% select(where(is.numeric)) %>% boxplot

data.play <- data.raw %>% head(100)

data.play %>% names()

system.time(
  dataset <- data.play %>% 
  # dataset <- data.raw %>% 
    rename(sex = sexnumeric) %>% 
    mutate(sex = fct_recode(
      sex,
      male = "1",
      female = "2")) %>% 
    mutate(
      across()
    ) %>% 
    select(-sex, -user_age, everything()) %>% 
    # remove the facet scores bec. they are derived as means of item scores
    select(-c(101:125))
)

reverse_code <- function(item_vector, min, max) {
  max + min - item_vector 
}

dataset %>% names
dataset$Oaesa1 + dataset$Oaesa1 %>% reverse_code(1, 5)

dataset
dataset %>% 
  mutate(
    across(
      c(1,2),
      ~reverse_code(.x, 1, 5)
    )
  )
  

