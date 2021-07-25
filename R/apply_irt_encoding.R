################################################################################
#
# Script:  R/apply_irt_encoding.R
# Output:  training.set, testing.set - no encoding
#
################################################################################
apply_irt_encoding <- function(
  encoding, training_set, testing_set, target_label) {
  
  # set feature labels
  features.labels <- training_set %>% select(-target_label) %>% names
  
  return(
    list(
      features.labels = features.labels,
      target.label = target_label,
      testing.set = testing_set,
      training.set = training_set
    )
  )
}