################################################################################
#
# Script:       _strings.R
# Output:       models_list_label, models_metrics_label, dataset_label
#
################################################################################
models_list_label <- function() {
  output_filename(
    prefix = "models/models.list",
    DATASET.LABEL,
    train.test.split*100,
    ENCODING,
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

dataset_label <- function() {
  
  output_filename(
    prefix = "data/data",
    DATASET.LABEL,
    train.test.split*100,
    ENCODING,
    paste0(CV.REPEATS, "repeats")
  )
}

prep_label <- function() {
  
  output_filename(
    prefix = "models/prep",
    DATASET.LABEL,
    ENCODING
  )
}
