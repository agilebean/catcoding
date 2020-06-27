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
calibration.ratio <- 0.2  # only for vtreat-design
# QUICK-TEST: use only cats to see whether it's worth catcoding 
# CATS.ONLY <- TRUE
CATS.ONLY <- FALSE

################################################################################
# PREP <- TRUE
PREP <- FALSE

################################################################################
# apply encoding on dataset
apply_encoder <- function(
  encoding, training_original, testing_original, target_label) {
  
  if (is.null(encoding)) {
    
    source("encoders/no-encoding.R")
    
  } else { # ENCODINGS
    
    if (encoding == "vtreat-cross") {
      
      source("encoders/vtreat-cross.R")
      
    } else if (encoding == "vtreat-design") {
      
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
    
    # get categorical features
    no.cats <- training_original %>% select(where(is.factor)) %>% ncol
    
    # get #features-original
    no.features.original <- training.original %>% ncol -1
    # inform about feature generation stats
    print(paste(
      "From", no.cats, "categorical of", no.features.original,
      "original features in total, generated", length(features.labels), "features."
    ))
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
  "scikit-binary",
  "scikit-onehot"
)
####################################################
# ENCODING <- NULL
# ENCODING <- "vtreat-design"
# ENCODING <- "vtreat-cross"
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
apply_encoder(ENCODING, training.original, testing.original, target.label)
training.set %>% glimpse
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
original <- split_dataset_original(
  data.original.object, ENCODING, train.test.split, CATS.ONLY
)
target <- training.original[[target.label]]

apply_encoder(
  ENCODING, original$training.set, original$testing.set, original$target.label
)
training.set %>% glimpse
# training.set.encoded <- data.encoded$training.set %T>% print
# testing.set.encoded <- data.encoded$testing.set %T>% print


################################################################################
target.label
features.labels
ENCODING

training.set %>% glimpse
training.set %>% dim
# testing.set %>% glimpse
# testing.set %>% dim

################################################################################
