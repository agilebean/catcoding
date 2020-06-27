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
  "machinelearningtools",
  "reticulate"
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
# data splits
train.test.split <- 1.0
# QUICK-TEST: use only cats to see whether it's worth catcoding 
# CATS.ONLY <- TRUE
CATS.ONLY <- FALSE

################################################################################
# PREP <- TRUE
PREP <- FALSE

################################################################################
# apply encoding on dataset
apply_encoder <- function(encoding, training_set) {
  
  if (is.null(encoding)) {
    
    source("encoders/no-encoding.R")
    
  } else { # ENCODINGS
    
    if (encoding == "vtreat-cross") {
      
      source("encoders/vtreat-cross.R")
      
    } else if (encoding == "vtreat-design") {
      
      config.ratio <- 0.2  # only for vtreat-design
      source("encoders/vtreat-design.R")
      
    } else if (encoding == "vtreat-dummy") { # DUMMY ENCODING
      
      source("encoders/vtreat-dummy.R")
      
    } else if (startsWith(encoding, "embed")) {
      
      # PREP <- TRUE
      PREP <- FALSE
      source("encoders/embed-steps.R")
      
    } else if (startsWith(encoding, "scikit")) {
      
      source("encoders/scikit-encoders.R")
    }
  }
}
################################################################################
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
  "scikit-woe",
  "scikit-binary",
  "scikit-onehot"
)
####################################################
# ENCODING <- NULL
# ENCODING <- "vtreat-design"
# ENCODING <- "vtreat-cross"
# ENCODING <- "vtreat-dummy"
ENCODING <- "scikit-target"
# ENCODING <- "scikit-ordinal"
# ENCODING <- "scikit-helmert"
# ENCODING <- "scikit-backward-difference"
# ENCODING <- "scikit-james-stein"
# ENCODING <- "scikit-polynomial"
# ENCODING <- "scikit-woe"
# ENCODING <- "scikit-binary"
# ENCODING <- "scikit-onehot"
####################################################
# ENCODING <- "embed-bayes"
# ENCODING <- "embed-glm"
# ENCODING <- "embed-keras"
################################################################################
# get dataset
source("plugins/get_data.R")
source("plugins/strings.R")

# get original dataset
data.original.object <- get_dataset_original(DATASET.LABEL)

# split original dataset into training/testing.set
data.original <- split_dataset_original(
  data.original.object, ENCODING, train.test.split, CATS.ONLY)

training.original <- data.original$training.set %T>% glimpse
testing.original <- data.original$testing.set %T>% glimpse
features.original <- data.original$features.labels %T>% print

data.encoded <- apply_encoder(ENCODING)
training.set.encoded <- data.encoded$training.set %T>% print
testing.set.encoded <- data.encoded$testing.set %T>% print

# inform about feature generation stats
print(paste(
  "From", no.cats, "categorical of", no.features.original,
  "original features in total, generated", length(features.labels), "features."
))

CV.REPEATS <- 2
# CV.REPEATS <- 10
TRY.FIRST <- 1000

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)


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
# NEW <- TRUE
NEW <- FALSE
### continue
CV.REPEATS <- 2
# CV.REPEATS <- 10
TRY.FIRST <- 500
# TRY.FIRST <- NULL

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
