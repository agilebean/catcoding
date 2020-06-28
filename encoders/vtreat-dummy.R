################################################################################
#
# Script:  encoders/vtreat-dummy.R
# Output:  training.set, testing.set - dummy-encoded
#
################################################################################

apply_vtreat_dummy <- function(
  encoding, training_original, testing_original, target_label) {

  column.labels <- training_original %>% names
  
  treatment.plan <- designTreatmentsZ(
    dframe = training_original,
    varlist = column.labels,
    minFraction = 0
  )
  
  # restrict to common variable types
  vartypes.selected <- if (CATS.ONLY) {
    
    print("feature selection: CATS ONLY")
    c("lev") 
    
  } else { 
    
    print("feature selection: ALL")
    c("lev", "clean", "isBAD")
  }
  
  # see vignette('vtreatVariableTypes', package = 'vtreat') for details
  features.select <- treatment.plan$scoreFrame %>% 
    filter(code %in% vartypes.selected) %>% 
    pull(varName)
  
  training.set.encoded <- vtreat::prepare(
    treatment.plan,
    training_original,
    scale = TRUE,
    varRestriction = features.select
  )
  
  if (!is.null(testing_original)) {
    
    testing.set.encoded <- vtreat::prepare(
      treatment.plan,
      testing_original,
      scale = TRUE,
      varRestriction = features.select
    ) 
  } else {
    
    testing.set.encoded <- NULL
  }
  
  # set training.set
  training.set.select <- training.set.encoded %>% 
    select(features.select, target_label)
  
  # set testing.set
  if (!is.null(testing.set.encoded)) {
    testing.set.select <- testing.set.encoded %>% 
      select(features.select, target_label)
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



