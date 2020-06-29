################################################################################
#
# Script:       _strings.R
# Output:       models_list_label, models_metrics_label, dataset_label
#
################################################################################
TRAIN.TEST.SPLIT <- 1.0
# QUICK-TEST: use only cats to see whether it's worth catcoding 
# CATS.ONLY <- TRUE
CATS.ONLY <- FALSE

DATASET.LABEL.LIST <- c(
  # "diamonds"
  "ames"
  , "designdim"
  , "timex"
  , "smartflow"
)

ENCODER.LIST <- c(
  "factor-encoding"
  , "vtreat-cross"
  , "vtreat-design"
  # , "vtreat-dummy"
  , "scikit-backward"
  , "scikit-baseN"
  , "scikit-binary"
  , "scikit-catboost"
  , "scikit-glmm"
  , "scikit-hashing"
  , "scikit-helmert"
  , "scikit-james-stein"
  , "scikit-loo"
  , "scikit-Mestimate"
  , "scikit-onehot"
  , "scikit-ordinal"
  , "scikit-polynomial"
  , "scikit-sum"
  , "scikit-target"
  , "scikit-woe"
)

VARTYPES.SELECT <- if (CATS.ONLY) {
  
  print("feature selection: CATS ONLY")
  c("lev") 
  
} else { 
  
  print("feature selection: ALL")
  # code "clean":  a numerical variable with no NAs or NaNs
  # code "lev": an indicator variable for a specific level of the original categorical variable.
  c("lev", "clean", "isBad")
}

models_list_label <- function(dataset_label, encoder) {
  output_filename(
    prefix = "models/models.list",
    dataset_label,
    TRAIN.TEST.SPLIT*100,
    encoder,
    paste0(CV.REPEATS, "repeats")
  )
}

models_metrics_label <- function() {
  output_filename(
    prefix = "models/models.metrics",
    DATASET.LABEL,
    TRAIN.TEST.SPLIT*100,
    ENCODING,
    paste0(CV.REPEATS, "repeats")
  )
}

dataset_filename <- function(dataset_label) {
  
  output_filename(
    prefix = "data/data",
    dataset_label,
    TRAIN.TEST.SPLIT*100
  )
}

prep_label <- function() {
  
  output_filename(
    prefix = "models/prep",
    DATASET.LABEL,
    ENCODING
  )
}

