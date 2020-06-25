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
  
} else {
  
  if (TREATMENT == "vtreat-design") {
    
    # sample by train.ratio from data outside testing.set
    train.index <- not.test.index %>% as.vector %>% sample(length(.)* train.ratio) 
    config.index <- not.test.index[!not.test.index %in% train.index]
    config.set <- dataset[config.index, ] 
    
    treatment.plan <- designTreatmentsN(
      dframe = config.set,
      varlist = features.labels,
      # parallelCluster = clus, # 5% faster
      outcomename = target.label
    )
    
    scoreFrame <- treatment.plan$scoreFrame %>%
      select(varName, origName, code) %T>% print
    
    vartypes.selected <- if (CATS.ONLY) {
      c("lev") 
      print("feature selection: CATS ONLY")
    } else { 
      c("clean", "lev")
    }
    
    features.treated <- treatment.plan$scoreFrame %>%
      # code "clean":  a numerical variable with no NAs or NaNs
      # code "lev": an indicator variable for a specific level of the original categorical variable.
      # filter(code %in% c("clean", "lev")) %>%
      filter(code %in% vartypes.selected) %>%
      pull(varName) %T>% print
    
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
