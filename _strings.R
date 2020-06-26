################################################################################
#
# Script:       _strings.R
# Output:       models_list_label, models_metrics_label, dataset_label
#
################################################################################
case_string <- function(case_variable) {
  return(case_variable)
}

treatment_string <- function() {
  case_string(TREATMENT)
}

models_list_label <- function() {
  output_filename(
    prefix = "models/models.list",
    DATASET.LABEL,
    train.test.split*100,
    TREATMENT,
    paste0(CV.REPEATS, "repeats")
  )
}

models_metrics_label <- function() {
  output_filename(
    prefix = "models/models.metrics",
    DATASET.LABEL,
    train.test.split*100,
    TREATMENT,
    paste0(CV.REPEATS, "repeats")
  )
}

dataset_label <- function() {
  
  output_filename(
    prefix = "data/data",
    DATASET.LABEL,
    train.test.split*100,
    TREATMENT,
    paste0(CV.REPEATS, "repeats")
  )
  
}
