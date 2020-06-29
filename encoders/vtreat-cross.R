################################################################################
#
# Script:  encoders/vtreat-cross.R
# Output:  training.set, testing.set - vtreat::mkCrossFrame{N,C} Experiment
#
################################################################################

apply_vtreat_cross <- function(
  encoding, training_original, testing_original, target_label) {
  
  # decide regression or classification
  target <- training_original[[target_label]]
  if (is.numeric(target)) {
    
    treatment_function <- vtreat::mkCrossFrameNExperiment
    
  } else if (is.factor(target)) {
    
    treatment_function <- vtreat::mkCrossFrameCExperiment
  }
  
  features.original <- training_original %>% select(-target_label) %>% names
  
  clus <- clusterOn() # 8s/9s (11% faster)
  training.set.cross <- treatment_function(
    dframe = training_original, 
    varlist = features.original,
    outcomename = target_label,
    parallelCluster = clus
  )
  clusterOff(clus)
  
  # get treated training.set
  training.set.encoded <- training.set.cross$crossFrame %>% 
    as_tibble() %T>% print
  
  # select features
  features.select <- training.set.cross$treatments$scoreFrame %>%
    # code "clean":  a numerical variable with no NAs or NaNs
    # code "lev": an indicator variable for a specific level of the original categorical variable.
    # filter(code %in% c("clean", "lev")) %>%
    filter(code %in% VARTYPES.SELECT) %>%
    # vtreat recommendations to filter out useless variables
    filter(recommended == TRUE) %>%
    pull(varName) %T>% print
  
  if (!is.null(testing_original)) {
    testing.set.encoded <- vtreat::prepare(
      training.set.cross$treatments,
      testing_original,
      pruneSig = NULL
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


