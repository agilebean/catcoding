################################################################################
#
# Script:       labels.R
# Output:       models_list_label, models_metrics_label, dataset_label
#
################################################################################
# seed
SEED <- 171

# use only cats
CATS.ONLY <- TRUE
# CATS.ONLY <- FALSE

# train/test split
TRAIN.SPLIT <- 1.0

# STUDY <- "study1"
STUDY <- "study2"
# STUDY <- "study3"

CV.REPEATS <- 2
# CV.REPEATS <- 5
# CV.REPEATS <- 10
# CV.REPEATS <- 20

# CATS.ONLY <- FALSE
# TRANSFORM <- "pca"
# TRANSFORM <- "ica"
# TRANSFORM <- "YeoJohnson"
TRANSFORM <- NULL

models_list_label <- function(
  study, dataset_label, encoder, cv_repeats,
  prefix = "models.list", transform = TRANSFORM) {
  
  output_dir(
    study, "models", paste0("cv", cv_repeats),
    file = output_filename(
      prefix, dataset_label, TRAIN.SPLIT * 100, encoder, transform
    )
  )
}

models_explanation_label <- function(
  study, dataset_label, cv_repeats, encoder, 
  prefix = "models.explanation", transform = TRANSFORM) {
  
  output_dir(
    study, "xai", paste0("cv", cv_repeats),
    file = output_filename(
      prefix, dataset_label, TRAIN.SPLIT * 100, encoder, transform
    )
  )
}

dataset_filename <- function(dataset_label) {
  
  output_dir(
    "data", 
    file = output_filename(dataset_label, "encoded", suffix = "rds")
  )
}

benchmark_filename <- function(
  study,
  cv_repeats, 
  prefix = "benchmarks.top.encoders", 
  transform = TRANSFORM
) {
  output_dir(
    study, "benchmarks", paste0("cv", cv_repeats),
    file  = output_filename(
      prefix = prefix, transform)  
  )
}

benchmark_plot_label <- function(
  study, cv_repeats, 
  dataset_label,
  prefix = "benchmark.plot",
  transform = TRANSFORM
) {

  output_dir(
    study, "benchmarks", paste0("cv", cv_repeats),
    file  = output_filename(
      prefix = prefix, dataset_label, transform, suffix = "png")
  )
}

# for vtreat
# code "clean":  a numerical variable with no NAs or NaNs
# code "lev": an indicator variable for a specific level of the original categorical variable.
VARCODES.VTREAT <- if (CATS.ONLY) {
    # print("feature selection: CATS ONLY")
    c("lev") 
  } else { 
    # print("feature selection: ALL")
    c("lev", "clean", "isBad")
  } 

DATASET.LABEL.TEST <- c(
  "designdim"
)

DATASET.LABEL.study1 <- c(
  "diamonds"
  , "ames"
  , "designdim"
  , "timex"
  # , "smartflow"
  # , "smartflow.scales"
)

DATASET.LABEL.study2 <- c(
  "swbsun",
  "swbjoh"
)

# DATASET.LABEL.LIST <- DATASET.LABEL.study1
DATASET.LABEL.LIST <- DATASET.LABEL.study2

ENCODER.LIST.test <- c(
  "factor-encoding"
  # , "integer-encoding"
  , "embed-glm"
  # , "vtreat-cross"
  , "scikit-hashing"
  , "scikit-glmm"
  # , "scikit-Mestimate"
  # , "scikit-onehot"
  # , "scikit-ordinal"
  # , "scikit-target"
)

ENCODER.LIST.study1 <- c(
  "factor-encoding"
  , "integer-encoding"
  , "embed-glm"
  # , "embed-keras"
  , "vtreat-cross"
  , "vtreat-design"
  , "vtreat-dummy"
  , "scikit-backward"
  , "scikit-baseN"
  , "scikit-binary"
  , "scikit-catboost"
  # , "scikit-glmm" # error
  , "scikit-hashing" # error
  , "scikit-helmert"
  , "scikit-james-stein"
  # , "scikit-loo"
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
  # "scikit-loo",
  "scikit-Mestimate",
  "embed-glm",
  "embed-keras",
  "scikit-ordinal",
  "scikit-onehot"
)

# ENCODER.LIST.LIST <- ENCODER.LIST.test
ENCODER.LIST.LIST <- ENCODER.LIST.study1
# ENCODER <- "embed-keras"
# ENCODER <- "factor-ENCODER"
# ENCODER <- "scikit-binary"
# ENCODER <- "scikit-glmm"
# ENCODER <- "scikit-helmert" # reached elapsed time limit
# ENCODER <- "scikit-loo"
# ENCODER <- "scikit-Mestimate"
# ENCODER <- "scikit-ordinal"
# ENCODER <- "scikit-backward" # reached elapsed time limit
# ENCODER <- "scikit-james-stein" # ++2*ames
# ENCODER <- "scikit-polynomial" # ++3*ames
# ENCODER <- "scikit-onehot"
# ENCODER <- "scikit-target" # ++3*ames
# ENCODER <- "scikit-woe" # target must be binary
# ENCODER <- "vtreat-cross"
# ENCODER <- "vtreat-design"
# ENCODER <- "vtreat-dummy"

