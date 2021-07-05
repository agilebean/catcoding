################################################################################
#
# Script:       labels.R
# Output:       models_list_label, models_metrics_label, dataset_label
#
################################################################################
# use only cats
CATS.ONLY <- TRUE
# CATS.ONLY <- FALSE

# train/test split
TRAIN.SPLIT <- 1.0

CV.REPEATS <- 2
# CV.REPEATS <- 5
# CV.REPEATS <- 10
# CV.REPEATS <- 20

# CATS.ONLY <- FALSE
# TRANSFORM <- "pca"
# TRANSFORM <- "ica"
# TRANSFORM <- "YeoJohnson"
TRANSFORM <- NULL

output_dir <- function(..., file = "") {
  # create dir path
  dirs = file.path(...)
  # create dir (and subdirs) if they don't exist
  if(!dir.exists(dirs) & !file.exists(dirs)) { 
    dir.create(dirs, recursive = TRUE) 
  }
  # return filepath under dirs
  file.path(dirs, file)
}

models_list_label <- function(
  dataset_label, encoder, cv_repeats,
  prefix = "models.list", transform = TRANSFORM) {
  
  output_dir(
    "models", paste0("cv", cv_repeats),
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
  cv_repeats, 
  study,
  prefix = "benchmarks.datasets", 
  transform = TRANSFORM
) {
  output_dir(
    "benchmarks", STUDY, paste0("cv", cv_repeats),
    file  = output_filename(
      prefix = prefix, transform)  
  )
}

prep_label <- function(dataset_label, encoder) {
  
  output_filename(prefix = "models/prep", dataset_label, encoder)
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


DATASET.LABEL.LIST <- c(
  # "diamonds"
  "ames"
  , "designdim"
  , "timex"
  , "smartflow"
  # , "smartflow.scales"
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
  # "scikit-glmm" # error
  # , "scikit-hashing" # error
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



