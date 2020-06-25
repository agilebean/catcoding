# packs <- c(
#   "AmesHousing"
# )
# sapply(packs, require, character.only = TRUE)

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
