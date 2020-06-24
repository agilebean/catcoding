packs <- c(
  "tidyverse",
  "magrittr",
  "vtreat",
  "caret",
  "machinelearningtools",
  "AmesHousing"
)
sapply(packs, require, character.only = TRUE)

#######################################
# Step 1: Data Acquisition
#######################################
# diamonds
# diamonds %>% str
# library(help = "datasets")

# dataset.list <- list(diamonds, ames) %>% set_names(c("diamonds", "ames"))

if (DATASET.LABEL == "diamonds") {
  
  dataset <- diamonds
  target.label <- "price"
  features.labels <- dataset %>% select(-target.label) %>% names
  
  split.untreated <- 1.0
  # untreated: RMSE 761 clarity_lev_x_SI2    2.43515
  # CATS.ONLY untreated: RMSE 3825 92.5min
  test.ratio   <- 0.95
  # train.ratio <- 0.8 # RMSE 750 svm clarity_lev_x_SI2    2.82339
  train.ratio <- 0.5 # RMSE  757 svm clarity_lev_x_SI2    3.35615 or before x!
  # CATS.ONLY RMSE 642 3835 18.1min
  # train.ratio <- 0.4 # RMSE 773 svm 2.4
  # train.ratio <- 0.2 # RMSE 751 svm clarity_lev_x_SI2    3.28635

} else if (DATASET.LABEL == "ames") {
  
  dataset <- AmesHousing::make_ames()
  
  target.label <- "Sale_Price"
  features.labels <- dataset %>% select(-target.label) %>% names
  
  split.untreated <- 0.8
  # untreated ALL: RMSE 24247 gbm
  # CATS.ONLY: RMSE 31459 (2rep), 31810 (10rep) gbm <<<<<<<<<<<<<<

  # test.ratio   <- 0.9
  test.ratio   <- 1.0
  train.ratio <- 0.8  # RMSE gbm 24662, svm 24511
  # CATS.ONLY: RMSE 25252 (10rep)gbm <<<<<<<<<<<<<<
  # train.ratio <- 0.6 # RMSE gbm 26253
  # CATS.ONLY: 36717 svm
  # train.ratio <- 0.5 # RMSE gbm 26321
  # train.ratio <- 0.4 # RMSE gbm 25714
  # CATS.ONLY: 37484 gbm
  # train.ratio <- 0.2 # RMSE svm 29023
  # 
} else if (DATASET.LABEL == "designdim") {
  
  # no desc.stats exists. tricky: design.descriptives are design features!
  dataset <- readRDS("data/designdim.items.rds")
  
  target.label <- "NPS"
  features.labels <- dataset %>% select(-target.label) %>% names

} else if (DATASET.LABEL == "timex") {
  
  data.descriptive <- readRDS("data/timex.descriptive.rds") 
  data.items <- readRDS("data/timex.items.rds")
  
  dataset <- cbind(data.descriptive, data.items) %>% 
    select(referral:happy, -feedback, -email) %>% 
    mutate(
      HAPPINESS = rowMeans(
        select(., active:alert, starts_with("lifesatis"), happy)
      )
    ) %>% 
    na.omit() # n=252, 82
  
  target.label <- "HAPPINESS"
  features.labels <-
    dataset %>% 
    select(-target.label) %>% 
    select(-c(active:ashamed)) %>% #PANAS-PA/-NA
    select(-starts_with("lifesatis"), -happy) %>% 
    names
  
} else if (DATASET.LABEL == "smartflow") {
  
  data.descriptive <- readRDS("data/smartflow.descriptive.rds") 
  data.items <- readRDS("data/smartflow.items.rds")
  
  dataset <- cbind(data.descriptive, data.items) %>% 
    select(-feedback, -email) %>% 
    mutate(., SMADDICTION = rowMeans(
      select(., starts_with("addicted")), na.rm = TRUE)) %>% 
    na.omit() # n=307
  
  target.label <- "SMADDICTION" # TODO: smartphone addiction missing
  features.labels <- dataset %>% 
    select(-target.label, -starts_with("addicted")) %>% names
  
} else if (DATASET.LABEL == "smartflow.scales") {
  
  data.descriptives <- readRDS("data/smartflow.descriptive.rds") 
  data.scales <- readRDS("data/smartflow.scales.rds")
  
  dataset <- cbind(data.descriptives, data.scales) %>% 
    select(-feedback, -email) %>% 
    na.omit() # n=307
  
  target.label <- "SmartphoneAddiction" # TODO: smartphone addiction missing
  features.labels <- dataset %>% 
    select(-SmartphoneHours) %>% names

}  

if (is.null(TREATMENT)) {

  train.index <- createDataPartition(
    dataset[[target.label]], p = split.untreated, list = FALSE
  )
  training.set <- dataset[train.index, ] %T>% print
  testing.set <- dataset[-train.index, ] %T>% print
  
  
} else {
  
  not.test.index <- createDataPartition(
    dataset[[target.label]], p = test.ratio, list = FALSE
  )
  testing.set <- dataset[-not.test.index, ]
  if (nrow(testing.set) == 0) testing.set <- NULL
  
  # sample by train.ratio from data outside testing.set
  train.index <- not.test.index %>% as.vector %>% sample(length(.)* train.ratio) 
  training.set <- dataset[train.index, ] 
  
  config.index <- not.test.index[!not.test.index %in% train.index]
  config.set <- dataset[config.index, ] 

}

# names(config.set)[!names(config.set) %in% names(training.set)]

