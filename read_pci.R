################################################################################
#
# Script:  read_pci.R
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

# NEW <- TRUE
NEW <- FALSE

source("plugins/labels.R")

if (NEW) {
  
  system.time(
    file.raw <- xlsx::read.xlsx(
      filename.xlsx, 
      sheetIndex = 1,
      startRow = 4,
      header = FALSE
      )
  ) # 81.0s
  
  system.time(
    file.raw %>% saveRDS(filename.rds)  
  ) # 0.14s
  
} else {
  system.time(
    file.raw <- readRDS(filename.rds) %>% as_tibble
  ) # 0.08s
}

file.raw %>% glimpse()
file.raw
play <- file.raw %>% 
  # remove 수험번호 (employee id)x2 + 참가자분류 (Participant classification)
  select(-c(1,2,5)) %>% print

play 

play %>% 
  set_names(1:length(play)) %>% 
  slice(1:2) %>% t

subscript1 <- rep(TRUE, 3) # 3
subscript2 <- rep(c(TRUE, FALSE), 21) # 45
subscript3 <- c(TRUE, FALSE, TRUE) # Division, Job score% 48
subscript4 <- rep(c(FALSE, TRUE, FALSE), 5) # 63
subscript5 <- FALSE

data.raw <- play[ , c(subscript1, subscript2, subscript3, 
                      subscript4, subscript5)]
data.raw %<>% set_names(1:length(data.raw))

data.raw %>% slice(1:2) %>% t

names.col1_3 <- c("company", "job_type", "passed")
names.col4_27 <- data.raw[1, c(4:27)] %>% as.character()
names.col4_27 %>% t
names.col28_30 <- c("agreement", "neutrality", "disagreement")
names.col28_30 %>% t
names.lastcol <- data.raw[1, 31]

data.raw %>% slice(1:2) %>% t

names(data.raw) <- c(
  names.col1_3, 
  names.col4_27,
  names.col28_30,
  names.lastcol
  )

data.raw
data.raw %<>% slice(-c(1:2)) 

names(data.raw) %<>% tolower() %>% 
  gsub(" ", "_", .) %>% 
  gsub("%", "", .)

data.raw$passed %>% table
factor.labels <- c("company", "job_type", "passed",
                   "division")

data.korean <- data.raw %>% 
  as_tibble() %>% 
  mutate(across(factor.labels, as_factor)) %>% 
  mutate(across(!any_of(factor.labels), as.numeric))
  
data.korean %>% summary()

# missing values
data.korean %>% map_df(~ is.na(.) %>% sum) %>% select(where(~ sum(.) != 0))

dataset <- data.korean %>% 
  mutate(
    company = fct_recode(
      company,
      company1 = "공영홈쇼핑", # Public Home Shopping
      company2 = "광명도시공사", # Gwangmyeong City Corporation|Construction
      company3 = "전력거래소", # Power exchange
      company4 = "한국서부발전㈜", # Korea Western Power Co., Ltd.
      company5 = "한국소방산업기술원", # Korea Fire Industry Technology Institute
      company6 = "한국해양진흥공사" # Korea Ocean Promotion Corporation
    )
  ) %>%
  mutate(
    job_type = fct_recode(
      job_type,
      executive = "관리직", 
      office = "사무직", 
      sales = "영업/판매직", 
      driver = "운전직", 
    )
  ) %>%  
  mutate(
    passed = fct_recode(
      passed,
      not_hired = "불합격", 
      hired = "합격"
    )
  ) %>% 
  mutate(
    division = fct_recode(
      division,
      managerial = "관리직", 
      support = "사무직", 
      sales = "영업/판매직", 
      driver = "운전직", 
    )
  ) %>% 
  select(-division) %>% 
  select(job_score, passed, everything())

dataset %>% summary

system.time(
  dataset %>% saveRDS(dataset.label)  
) # 0.05s

system.time(
  dataset <- readRDS(dataset.label)
) # 0.75s


dataset %>%
  group_by(company) %>%
  # group_by(passed) %>%
  tally() %>%
  mutate(perc = n/sum(n)*100)
