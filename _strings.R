################################################################################
#
# Script:       _strings.R
# Output:       dataset, models_list_label, models_metrics_label 
# Treatment:    vtreat
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
    "data/models.list",
    paste0(CV.REPEATS, "repeats"),
    DATASET.LABEL,
    treatment_string()
  )
}

models_metrics_label <- function() {
  output_filename(
    "data/models.metrics",
    paste0(CV.REPEATS, "repeats"),
    DATASET.LABEL,
    treatment_string()
  )
}
