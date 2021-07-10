################################################################################
#
# Script:  R/apply_integer_encoding.R
# Output:  training.set, testing.set - no encoding
#
################################################################################
apply_integer_encoding <- function(
  encoding, training_original, testing_original, target_label) {
  
  # set feature labels
  features.original <- training_original %>% select(-target_label) %>% names
  
  training.set.numeric <- training_original %>% 
    mutate(across(where(is.factor), as.numeric))
  
  if (!is.null(testing_original)) {
    testing.set.numeric <- testing_original %>% 
      mutate(across(where(is.factor), as.numeric))
    
  } else {
    testing.set.numeric <- NULL
  }
  
  return(
    list(
      features.labels = features.original,
      target.label = target_label,
      testing.set = testing.set.numeric,
      training.set = training.set.numeric
    )
  )
}