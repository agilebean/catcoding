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

filename <- "data/HEXACO Online data with sex age Years 1-4"
filename.sav <- paste0(filename, ".sav")
filename.rds <- paste0(filename, ".rds")

if (NEW) {
  
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
  
} else {
  system.time(
    file.raw <- readRDS(filename.rds) 
  ) # 0.8s
}

file.raw %>% glimpse()
file.raw %>% summary()
file.raw$sexnumeric %>% table

# missing values
file.raw %>% map_df(~ is.na(.x) %>% sum) %>% 
  # https://stackoverflow.com/a/65608760/7769076
  select(where(~ is.numeric(.x) && sum(.x) != 0))

# can be shorter bec factor column is converted into int:
file.raw %>% map_df(~ is.na(.) %>% sum) %>% select(where(~ sum(.) != 0))

# https://stackoverflow.com/a/63545281/7769076
file.raw %>% map_df(~ is.na(.x) %>% sum) %>% select(where(~ any(. != 0)))



