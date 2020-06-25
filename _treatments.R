################################################################################
#
# Script:       _treatments.R
# Purpose:      Provide different treatments of categorical features
#
################################################################################

# default case: whole dataset
train.index <- createDataPartition(
  dataset[[target.label]], p = train.test.split, list = FALSE
)
training.set <- dataset[train.index, ] 
testing.set <- dataset[-train.index, ]
if (nrow(testing.set) == 0) testing.set <- NULL

# no treatment
if (is.null(TREATMENT)) {
  
  # special case: only cats without treatment
  # cats must be converted in _getdata.R
  if (CATS.ONLY) {
    
    print("feature selection: CATS ONLY")
    # training.set with target and only cats
    training.set %<>% 
      select_if(is.factor) %>% 
      # add target.label column bec all factors
      mutate(!!target.label := training.set[[!!target.label]]) %>%
      select(target.label, everything())
    
    # testing.set with target and only cats
    if (!is.null(testing.set)) {
      
      testing.set %<>% 
        select_if(is.factor) %>% 
        mutate(!!target.label := testing.set[[!!target.label]]) %>% 
        select(target.label, everything())      
    } else {
      
      print("no testing set")
    }
    
  } else {
    
    print("feature selection: ALL")
  }
  
  # set feature labels
  features.labels <- training.set %>% select(-target.label) %>% names
  print(paste(
    "Categorical features:",
    ifelse(is_empty(features.labels), "None", features.labels)
  ))
  
} else { # TREATMENTS
  
  if (TREATMENT == "vtreat-design") {
    
    # to extract config.set from training.set
    training.original <- training.set

    # subset index by config.ratio 
    # tricky: subset too short: sample(nrow(training.original) * config.ratio)
    config.index <- 1:nrow(training.original) %>% 
      sample(length(.) * config.ratio)
    
    # extract config.set from training.set by config.ratio
    config.set <- training.original %>% slice(config.index)
    training.set <- training.original %>% slice(-config.index)
    
    treatment.plan <- designTreatmentsN(
      dframe = config.set,
      varlist = features.labels,
      # parallelCluster = clus, # 5% faster
      outcomename = target.label
    )
    
    scoreFrame <- treatment.plan$scoreFrame %>%
      select(varName, origName, code) %T>% print
    
    vartypes.selected <- if (CATS.ONLY) {
      
      print("feature selection: CATS ONLY")
      c("lev") 
      
    } else { 
      
      print("feature selection: ALL")
      c("lev", "clean")
    }
    
    features.treated <- treatment.plan$scoreFrame %>%
      # code "clean":  a numerical variable with no NAs or NaNs
      # code "lev": an indicator variable for a specific level of the original categorical variable.
      # filter(code %in% c("clean", "lev")) %>%
      filter(code %in% vartypes.selected) %>%
      pull(varName)
    
    training.set.scores <-  prepare(
      treatment.plan,
      training.set,
      scale = TRUE,
      varRestriction = features.treated
    )
    
    testing.set.scores <-  prepare(
      treatment.plan,
      testing.set,
      scale = TRUE,
      varRestriction = features.treated
    )
    
    training.set <- training.set.scores
    testing.set <- testing.set.scores
    features.labels <- features.treated
    print("TREATMENT: vtreat::designTreatmentsN")
    
  } else if (TREATMENT == "vtreat-cross") {
    
    training.set.cross <- vtreat::mkCrossFrameNExperiment(
      dframe = training.set, 
      varlist = features.labels,
      outcomename = target.label,
      # codeRestriction = c("lev"),
      # parallelCluster = clus, # % faster
      rareCount = 0,  # Note set this to something larger, like 5
      rareSig = c()
    ) 
    
    # get treated trainingset
    treatments <- training.set.cross$treatments %T>% print
    training.set.treated <- training.set.cross$crossFrame %>% 
      as_tibble() %T>% print
    
    vartypes.selected <- if (CATS.ONLY) {
      
      print("feature selection: CATS ONLY")
      c("lev")
      
    } else { 
      
      print("feature selection: ALL")
      c("lev", "clean")
    }
    
    # training.set.treated %>% select(-target.label) %>% names
    
    features.treated <- treatments$scoreFrame %>%
      # code "clean":  a numerical variable with no NAs or NaNs
      # code "lev": an indicator variable for a specific level of the original categorical variable.
      # filter(code %in% c("clean", "lev")) %>%
      filter(code %in% vartypes.selected) %>%
      filter(recommended == TRUE) %>% 
      pull(varName) %T>% print
    
    if (!is.null(testing.set)) {
      testing.set.treated <- vtreat::prepare(
        treatments,
        testing.set,
        pruneSig=c()
      )      
    }
    
    # set trainingset, testingset, features labels
    training.set <- training.set.treated
    if (!is.null(testing.set)) testing.set <- testing.set.treated  
    features.labels <- features.treated
    print("TREATMENT: vtreat::mkCrossFrameNExperiment")
    
  }
  
}
