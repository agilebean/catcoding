################################################################################
#
# Script:       _strings.R
# Output:       models_list_label, models_metrics_label, dataset_label
#
################################################################################
DATASET.LABEL.LIST <- c(
  "ames"
  , "diamonds"
  , "designdim"
  , "timex"
  , "smartflow"
)

ENCODER.LIST <- c(
  "no-encoding"
  , "vtreat-cross"
  , "vtreat-design"
  , "vtreat-dummy"
  , "scikit-target"
  , "scikit-ordinal"
  , "scikit-backward"
  , "scikit-helmert"
  , "scikit-james-stein"
  , "scikit-polynomial"
  , "scikit-binary"
  , "scikit-onehot"
)

models_list_label <- function(dataset_label, encoder) {
  output_filename(
    prefix = "models/models.list",
    dataset_label,
    train.test.split*100,
    encoder,
    paste0(CV.REPEATS, "repeats")
  )
}

models_metrics_label <- function() {
  output_filename(
    prefix = "models/models.metrics",
    DATASET.LABEL,
    train.test.split*100,
    ENCODING,
    paste0(CV.REPEATS, "repeats")
  )
}

dataset_filename <- function(dataset_label) {
  
  output_filename(
    prefix = "data/data",
    dataset_label,
    train.test.split*100
  )
}

prep_label <- function() {
  
  output_filename(
    prefix = "models/prep",
    DATASET.LABEL,
    ENCODING
  )
}

