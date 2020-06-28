################################################################################
#
# Script:       benchamrk.R
# Benchmarking: lm, gbm, rf
#
################################################################################
packs <- c(
  "tidyverse",
  "magrittr",
  "vtreat",
  "caret",
  "machinelearningtools",
  "doParallel",
  "foreach"
)
sapply(packs, require, character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")
# NEW <- TRUE
NEW <- FALSE

source("plugins/strings.R")

####################################################
# dataset
# DATASET.LABEL <- "diamonds"
DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
# DATASET.LABEL <- "smartflow-scales"
# 
####################################################
ENCODER.LIST <- c(
  "no-encoding"
  , "vtreat-cross"
  , "vtreat-design"
  , "vtreat-dummy"
  , "scikit-target"
  , "scikit-ordinal"
  , "scikit-backward"
  , "scikit-helmert"
  , "scikit-james-stein"
  , "scikit-polynomial"
  , "scikit-binary"
  , "scikit-onehot"
)
CV.REPEATS <- 2
# CV.REPEATS <- 10
TRY.FIRST <- 1000

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)

models_list_label() 
models_metrics_label()
dataset_filename(DATASET.LABEL)
# prep_label()

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)

algorithm.list <- c(
  "lm"
  , "gbm"
  # , "rf"
)

####################################################
ENCODING <- "no-encoding"
# ENCODING <- "vtreat-cross"
# ENCODING <- "vtreat-design"
# ENCODING <- "vtreat-dummy"
# ENCODING <- "scikit-target"
# ENCODING <- "scikit-ordinal"
# ENCODING <- "scikit-helmert" # reached elapsed time limit
# ENCODING <- "scikit-backward" # reached elapsed time limit
# ENCODING <- "scikit-james-stein"
# ENCODING <- "scikit-polynomial"
# ENCODING <- "scikit-binary"
# ENCODING <- "scikit-onehot"
# ENCODING <- "scikit-woe" # target must be binary
####################################################

if (NEW) {
  
  if (startsWith(ENCODING, "embed")) { 
    
    # benchmark_algorithms with unprepped recipe and original training.set
    
  } else {
    
    
    data.list <- readRDS(dataset_filename(DATASET.LABEL))
    
    system.time(
      models.list <- benchmark_algorithms(
        target_label = data.list[[ENCODING]]$target.label,
        features_labels = data.list[[ENCODING]]$features.labels,
        training_set = data.list[[ENCODING]]$training.set,
        testing_set = data.list[[ENCODING]]$testing.set,
        training_configuration = training.configuration,
        algorithm_list = algorithm.list,
        cv_repeats = CV.REPEATS,
        try_first = TRY.FIRST,
        models_list_name = models_list_label(),
        push = FALSE,
        beep = TRUE
      )
    )
  }
  
} else {
  
  models.list <- readRDS(models_list_label())
}
models.metrics <- models.list %>% get_model_metrics() %T>% print

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
