################################################################################
#
# Script:       catcoding.R
# Benchmarking: lm, svmRadial, gbm, rf
# Treatment:    None
#
################################################################################
packs <- c(
  "tidyverse",
  "magrittr",
  "vtreat",
  "caret",
  "machinelearningtools"
)
sapply(packs, require, character.only = TRUE)

# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")

# common variables
# DATASET.LABEL <- "diamonds"
# DATASET.LABEL <- "ames"
DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"

# TREATMENT <- "vtreat-design"
# TREATMENT <- NULL
TREATMENT <- "vtreat-cross"

# data splits
train.test.split   <- 1.0

# QUICK-TEST: use only cats to see whether it's worth catcoding 
# CATS.ONLY <- TRUE
CATS.ONLY <- FALSE

source("_getdata.R")
source("_strings.R")
source("_treatments.R")

dataset
target.label
features.labels
TREATMENT

################################################################################
if (is.null(TREATMENT)) {
  

  
  
} else {
  
  
  #### AMES results
  # untreated ALL: RMSE 24247 gbm
  # CATS.ONLY: RMSE 31459 (2rep), 31810 (10rep) gbm <<<<<<<<<<<<<<
  
 
  
  #### AMES results
  # train.ratio = 0.8 RMSE gbm 24662, svm 24511
  # CATS.ONLY: RMSE 25252 (10rep)gbm <<<<<<<<<<<<<<
  # train.ratio <- 0.6 # RMSE gbm 26253
  # CATS.ONLY: 36717 svm
  # train.ratio <- 0.5 # RMSE gbm 26321
  # train.ratio <- 0.4 # RMSE gbm 25714
  # CATS.ONLY: 37484 gbm
  # train.ratio <- 0.2 # RMSE svm 29023
  
}

# names(config.set)[!names(config.set) %in% names(training.set)]

training.set %>% names
testing.set %>% names

################################################################################

NEW <- TRUE
# NEW <- FALSE
### continue
CV.REPEATS <- 2
# CV.REPEATS <- 10
TRY.FIRST <- 500
# TRY.FIRST <- NULL

models_list_label() 
models_metrics_label()

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)

if (!is.null(TREATMENT)) {
  
  if (TREATMENT == "vtreat-design") {
    
    training.set <- training.set.scores
    testing.set <- testing.set.scores
    features.labels <- features.treated
    print("dataset treatment: vtreat::designTreatmentsN")
    
  } else if (TREATMENT == "vtreat-cross") {
    
    
    

    
    
    
    
  }

}

algorithm.list <- c(
  "lm"
  # , "svmRadial"
  , "gbm"
  , "rf"
)


# models.list <- readRDS(models_list_label())
if (NEW) {
  system.time(
    models.list <- benchmark_algorithms(
      target_label = target.label,
      features_labels = features.labels,
      training_set = training.set,
      testing_set = testing.set,
      training_configuration = training.configuration,
      algorithm_list = algorithm.list,
      cv_repeats = CV.REPEATS,
      try_first = TRY.FIRST,
      models_list_name = models_list_label()
    )
  )
} 

if (NEW) {
  models.metrics <- models.list %>% get_model_metrics() %T>% print
  models.metrics %>% saveRDS(models_metrics_label())
  
  # models.metrics <- models.list %>% 
  #   purrr::list_modify("lm" = NULL) %>% 
  #   get_model_metrics() 
  
} else {
  
  models.list <- readRDS(models_list_label())
  models.metrics <- readRDS(models_metrics_label())
  
}

models.metrics

library(gbm)
models.list$gbm %>% varImp()
models.list$svmRadial %>% varImp()

models_list_label() 
models_metrics_label()
