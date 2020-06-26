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
  dataset <- readRDS("data/designdim.items.rds") %>% as_tibble()
  
  target.label <- "NPS"
  features.labels <- dataset %>% select(-target.label) %>% names
  
  # convert Likert to factor
  dataset %<>% mutate(across(features.labels, as.factor))
  
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
    na.omit() %>% # n=252, 82
    as_tibble()
  
  target.label <- "HAPPINESS"
  features.labels <-
    dataset %>% 
    select(-target.label) %>% 
    select(-c(active:ashamed)) %>% #DV: PANAS-PA/-NA 
    select(-starts_with("lifesatis"), -happy) %>% #DV: lifesatis+happiness-global
    names
  
  # convert Likert to factor
  dataset %<>% 
    mutate(across(features.labels, as.factor)) %>% 
    mutate(age = as.numeric(age))
  
  
} else if (DATASET.LABEL == "smartflow") {
  
  data.descriptive <- readRDS("data/smartflow.descriptive.rds") 
  data.items <- readRDS("data/smartflow.items.rds")
  
  dataset <- cbind(data.descriptive, data.items) %>% 
    select(-feedback, -email) %>% 
    mutate(., SMADDICTION = rowMeans(
      select(., starts_with("addicted")), na.rm = TRUE)) %>% 
    na.omit() %>% # n=307
    as_tibble()
  
  target.label <- "SMADDICTION"
  features.labels <- dataset %>% 
    select(-target.label, -starts_with("addicted")) %>% names
  
  # convert Likert to factor
  dataset %<>% 
    mutate(across(features.labels, as.factor)) %>% 
    mutate(age = as.numeric(age))
  
  
} else if (DATASET.LABEL == "smartflow.scales") {
  
  data.descriptives <- readRDS("data/smartflow.descriptive.rds") 
  data.scales <- readRDS("data/smartflow.scales.rds")
  
  dataset <- cbind(data.descriptives, data.scales) %>% 
    select(-feedback, -email) %>% 
    na.omit() %>% # n=307
    as_tibble()
  
  target.label <- "SmartphoneAddiction"
  features.labels <- dataset %>% 
    select(-SmartphoneHours) %>% names
  
  # convert Likert to factor
  dataset %<>% 
    mutate(across(referral:relationship, as.factor)) %>% 
    mutate(age = as.numeric(age))
  
} 

# subset features & remove other DV items
dataset %<>% select(target.label, features.labels)

# put target as last column
dataset %<>% relocate(target.label, .after = last_col())

# create target
target <- dataset[[target.label]]

# get categorical features
no.cats <- dataset %>% select(where(is.factor)) %>% ncol

# get #features-original
no.features.original <- length(features.labels)

################################################################################
# default case: whole dataset
train.index <- createDataPartition(
  dataset[[target.label]], p = train.test.split, list = FALSE
)
training.set <- dataset[train.index, ]
if (train.test.split < 1.0) {
  
  testing.set <- dataset[-train.index, ]  
} else {
  
  print("no testing set")
  testing.set <- NULL
}

################################################################################
# special case: only cats without treatment
# cats must be converted in _getdata.R
if (CATS.ONLY) {
  
  print("feature selection: CATS ONLY")
  # training.set with target and only cats
  training.set %<>% 
    select_if(is.factor) %>% 
    # add target.label column bec all factors
    mutate(!!target.label := training.set[[!!target.label]])
  
  # testing.set with target and only cats
  if (!is.null(testing.set)) {
    
    testing.set %<>% 
      select_if(is.factor) %>% 
      mutate(!!target.label := testing.set[[!!target.label]]) 
    
  }
  
} else {
  
  print("feature selection: ALL")
}

# inform about dataset type, #features, target
if (is.numeric(target)) {
  
  print(
    paste(
      "This is a regression dataset with target *", target.label, "* and",
      no.cats, "categorical of", no.features.original, "features in total"
    )
  )
  
} else if (is.factor(target)) {
  
  print(
    paste(
      "This is a classification dataset with target:", target.label, "and",
      no.cats, "categorical of", no.features.original, "features in total"
    )
  )
  
}


