################################################################################
#
# Script:       catcoding.R
# Benchmarking: lm, svmRadial, gbm, rf
# ENCODING:    None
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

####################################################
# common variables
# DATASET.LABEL <- "diamonds"
DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
# DATASET.LABEL <- "smartflow-scales"

####################################################
# ENCODING <- NULL
# ENCODING <- "vtreat-design"
# ENCODING <- "vtreat-cross"
ENCODING <- "vtreat-dummy"
# ENCODING <- "embed-bayes"

####################################################
# data splits
train.test.split <- 1.0
# config.ratio <- 0.2  # only for vtreat-design
# QUICK-TEST: use only cats to see whether it's worth catcoding 
# CATS.ONLY <- TRUE
CATS.ONLY <- FALSE

################################################################################
source("_getdata.R")
source("_strings.R")
################################################################################

if (is.null(ENCODING)) {

  source("_encoding none.R")
  
} else { # ENCODINGS
  
  if (ENCODING == "vtreat-cross") {
    
    source("_encoding vtreat-cross.R")
    
  } else if (ENCODING == "vtreat-design") {
    
    source("_encoding vtreat-design.R")
    
  } else if (ENCODING == "vtreat-dummy") { # SUMMY ENCODING
    
    source("_encoding vtreat-dummy.R")
    
  } else if (ENCODING == "embed-bayes") {
    
    source("_encoding embed-bayes.R")
  }
  
}

# inform about feature generation stats
print(paste(
  "From", no.cats, "categorical of", no.features.original,
  "original features in total, generated", length(features.labels), "features."
))

################################################################################

dataset
target.label
features.labels
ENCODING

training.set %>% glimpse
training.set %>% dim
testing.set %>% glimpse
testing.set %>% dim

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
dataset_label()


training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)

algorithm.list <- c(
  "lm"
  , "svmRadial"
  , "gbm"
  , "rf"
)

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
