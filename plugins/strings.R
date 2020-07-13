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

# PREPROCESS.OPTION <- "pca"
# PREPROCESS.OPTION <- "ica"
# PREPROCESS.OPTION <- "YeoJohnson"
# PREPROCESS.OPTION <- NULL
PREPROCESS.OPTION <- "none"

DATASET.LABEL.LIST <- c(
  # "diamonds"
  "ames"
  , "designdim"
  , "timex"
  , "smartflow"
)

ENCODER.LIST.study1 <- c(
  "factor-encoding"
  , "integer-encoding"
  , "embed-glm"
  , "embed-keras"
  , "vtreat-cross"
  , "vtreat-design"
  , "vtreat-dummy"
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
)

ENCODER.LIST.study2 <- c(
  "factor-encoding"
  , "integer-encoding"
  , "embed-glm"
  , "embed-keras"
  # , "vtreat-cross"
  # , "vtreat-design"
  # , "vtreat-dummy"
  # , "scikit-backward"
  # , "scikit-baseN"
  # , "scikit-binary"
  # , "scikit-catboost"
  # , "scikit-glmm"
  # , "scikit-hashing"
  , "scikit-helmert"
  , "scikit-james-stein"
  , "scikit-loo"
  , "scikit-Mestimate"
  , "scikit-onehot"
  , "scikit-ordinal"
  , "scikit-polynomial"
  # , "scikit-sum"
  # , "scikit-target"
)


ENCODER.LIST.study3 <- c(
  "scikit-loo",
  "scikit-Mestimate",
  "embed-glm",
  "embed-keras",
  "scikit-ordinal",
  "scikit-onehot"
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

models_list_label <- function(dataset_label, encoding) {
  output_filename(
    prefix = "models/models.list",
    dataset_label,
    TRAIN.TEST.SPLIT*100,
    encoding,
    PREPROCESS.OPTION,
    paste0("cv", CV.REPEATS)
  )
}

models_metrics_label <- function() {
  output_filename(
    prefix = "models/models.metrics",
    DATASET.LABEL,
    TRAIN.TEST.SPLIT*100,
    ENCODING,
    PREPROCESS.OPTION,
    paste0("cv", CV.REPEATS)
  )
}

dataset_filename <- function(dataset_label) {
  
  output_filename(
    prefix = "data/data",
    dataset_label,
    TRAIN.TEST.SPLIT*100
  )
}

benchmark_filename <- function() {
  output_filename(
    prefix = "output/benchmarks.all.datasets.all",
    paste0("cv", CV.REPEATS),
    PREPROCESS.OPTION
  )
  
}

prep_label <- function() {
  
  output_filename(
    prefix = "models/prep",
    DATASET.LABEL,
    ENCODING
  )
}

