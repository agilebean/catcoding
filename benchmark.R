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
  "machinelearningtools"
)
sapply(packs, require, character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")
# NEW <- TRUE
NEW <- FALSE

source("plugins/strings.R")

ENCODER.LIST <- c(
  "none",
  "vtreat-cross",
  "vtreat-dummy",
  "scikit-target",
  "scikit-ordinal",
  "scikit-backward",
  "scikit-helmert",
  "scikit-james-stein",
  "scikit-polynomial",
  "scikit-binary",
  "scikit-onehot"
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
dataset_label()
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
  , "rf"
)

if (NEW) {
  
  if (startsWith(ENCODING, "embed")) { 
    
    # benchmark_algorithms with unprepped recipe and original training.set
    
  } else {
    
    system.time(
      models.list <- benchmark_algorithms(
        target_label = target.label,
        features_labels = features.labels,
        training_set = training.set.encoded,
        testing_set = testing.set.encoded,
        training_configuration = training.configuration,
        algorithm_list = algorithm.list,
        cv_repeats = CV.REPEATS,
        try_first = TRY.FIRST,
        models_list_name = models_list_label()
      )
    )
  }
  
} else {
  
  models.list <- readRDS(models_list_label())
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
