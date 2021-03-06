################################################################################
#
# Script:  encoders/factor-encoding.R
# Output:  training.set, testing.set - no encoding
#
################################################################################

apply_factor_encoder <- function(
  encoding, training_original, testing_original, target_label) {
  
  # set feature labels
  features.original <- training_original %>% select(-target_label) %>% names
  
  return(
    list(
      features.labels = features.original,
      target.label = target_label,
      training.set = training_original,
      testing.set = testing_original
    )
  )
}