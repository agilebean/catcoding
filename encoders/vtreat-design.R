################################################################################
#
# Script:  encoders/vtreat-design.R
# Output:  training.set, testing.set - vtreat::designTreatmentsN
#
################################################################################

apply_vtreat_design <- function(
  encoding, training_original, testing_original, target_label) {
  
  # to extract calibration.set from training.set, subset index by calibration.ratio 
  # tricky: subset too short: sample(nrow(training.original) * calibration.ratio)
  calib <- 0.2
  calibration.index <- 1:nrow(training_original) %>% 
    sample(length(.) * calib)

  # message on calibration set
  print(paste0("*** Removing ", calib*100, "% of training set for calibration!"))
  
  # extract calibration.set from training.set by calibration.ratio
  calibration.set <- training_original %>% slice(calibration.index)
  training.small <- training_original %>% slice(-calibration.index)
  
  features.original <- training_original %>% select(-target_label) %>% names
  
  # scoreFrame <- treatment.plan$scoreFrame %>%
  #   select(varName, origName, code) %T>% print

  success <- FALSE
  while(!success) {
    
    print("************ Calculate features with recommended == TRUE")
    treatment.plan <- designTreatmentsN(
      dframe = calibration.set,
      varlist = features.original,
      outcomename = target_label
    )
    features.select <- treatment.plan$scoreFrame %>%
      filter(code %in% VARTYPES.SELECT) %>%
      # vtreat recommendations to filter out useless variables
      filter(recommended == TRUE) %>%
      pull(varName)
    
    if (!is_empty(features.select)) success <- TRUE
  }

  print(features.select)
  
  training.set.encoded <- vtreat::prepare(
    treatment.plan,
    training.small,
    varRestriction = features.select
  )
  
  if (!is.null(testing_original)) {
    testing.set.encoded <-  vtreat::prepare(
      treatment.plan,
      testing_original,
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
