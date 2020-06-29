################################################################################
#
# Script:  encoders/vtreat-dummy.R
# Output:  training.set, testing.set - dummy-encoded
#
################################################################################

apply_vtreat_dummy <- function(
  encoding, training_original, testing_original, target_label) {
  
  features.labels <- training_original %>% select(-target_label) %>% names
  
  treatment.plan <- designTreatmentsZ(
    dframe = training_original,
    varlist = features.labels,
    minFraction = 0
  )
  
  # select features
  features.select <- treatment.plan$scoreFrame %>% 
    filter(code %in% VARTYPES.SELECT) %>% 
    pull(varName)
  
  training.set.encoded <- vtreat::prepare(
    treatment.plan,
    training_original,
    varRestriction = features.select
  )
  
  # set training.set
  training.set.select <- training.set.encoded %>% 
    select(features.select) %>% 
    cbind(., training_original[target_label])
  
  # set testing.set
  if (!is.null(testing_original)) {
    
    testing.set.encoded <- vtreat::prepare(
      treatment.plan,
      testing_original,
      varRestriction = features.select
    )
    testing.set.select <- testing.set.encoded %>% 
      select(features.select) %>% 
      cbind(., testing_original[target_label])
    
  } else {
    
    testing.set.select <- NULL
  }
  
  return(list(
    features.labels = features.select,
    target.label = target_label,
    training.set = training.set.select,
    testing.set = testing.set.select
  ))  
  
}



