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
DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
# DATASET.LABEL <- "smartflow-scales"


# TREATMENT <- NULL
# TREATMENT <- "vtreat-design"
# TREATMENT <- "vtreat-cross"
TREATMENT <- "vtreat-dummy"

# data splits
train.test.split <- 1.0
config.ratio <- 0.2
# QUICK-TEST: use only cats to see whether it's worth catcoding 
# CATS.ONLY <- TRUE
CATS.ONLY <- FALSE

source("_getdata.R")
source("_strings.R")

################################################################################
# default case: whole dataset
train.index <- createDataPartition(
  dataset[[target.label]], p = train.test.split, list = FALSE
)
training.set <- dataset[train.index, ] 
testing.set <- dataset[-train.index, ]
if (nrow(testing.set) == 0) testing.set <- NULL

################################################################################

if (is.null(TREATMENT)) {
  
  source("_encoding none.R")
  
} else { # TREATMENTS
  
  if (TREATMENT == "vtreat-cross") {
    
    source("_encoding vtreat-cross.R")
    
  } else if (TREATMENT == "vtreat-design") {
    
    source("_encoding vtreat-design.R")
    
  } else if (TREATMENT == "vtreat-dummy") { # SUMMY ENCODING
    
    source("_encoding vtreat-dummy.R")
    
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
TREATMENT

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
  # , "svmRadial"
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
