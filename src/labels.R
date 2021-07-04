################################################################################
#
# Script:       labels.R
# Output:       models_list_label, models_metrics_label, dataset_label
#
################################################################################
TRAIN.SPLIT <- 1.0

# use only cats
CATS.ONLY <- TRUE
# CATS.ONLY <- FALSE

# TRANSFORM <- "pca"
# TRANSFORM <- "ica"
# TRANSFORM <- "YeoJohnson"
TRANSFORM <- NULL

preprocess_string <- c("center", "scale", "zv", TRANSFORM) %>% print

DATASET.LABEL.LIST <- c(
  "diamonds"
  , "ames"
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
  # , "scikit-glmm"
  # , "scikit-hashing"
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

output_dir <- function(...) {
  dots <- list(...)
  paste0(c(dots, recursive = TRUE), collapse = "/")
}

models_list_label <- function(
  dataset_label,
  encoder,
  prefix = "models.list",
  preprocess_option = TRANSFORM,
  cv_repeats = 2
  ) {
  
  output_dir(
    "models",
    paste0("cv", CV.REPEATS),
    # dataset_label,
    
    output_filename(
      prefix = prefix,
      dataset_label,
      TRAIN.SPLIT * 100,
      encoder,
      preprocess_option
    )
  )
}

models_metrics_label <- function(
  dataset_label,
  encoder,
  prefix = "models.metrics",
  preprocess_option = TRANSFORM,
  cv_repeats = paste0("cv", CV.REPEATS)
) {
  
  output_dir(
    "models",
    cv_repeats,
    # dataset_label,
    
    output_filename(
      prefix = prefix,
      dataset_label,
      TRAIN.SPLIT * 100,
      encoder,
      preprocess_option
    )
  )
}

dataset_filename <- function(dataset_label) {
  
  output_dir(
    "data",
    output_filename(
      dataset_label,
      "encoded",
      suffix = "rda"
    )
  )
}

benchmark_filename <- function(
  prefix = "output/benchmarks.datasets",
  cv_repeats = CV.REPEATS,
  transform = TRANSFORM
) {
  output_filename(
    prefix = prefix,
    paste0("cv", cv_repeats),
    transform
  )
}

prep_label <- function() {
  
  output_filename(
    prefix = "models/prep",
    DATASET.LABEL,
    ENCODING
  )
}

