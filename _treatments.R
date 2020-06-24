
if (!is.null(TREATMENT)) {
  
  if (TREATMENT == "vtreat-design") {
    
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
    
    treatments <- training.set.cross$treatments %T>% print
    training.set.scores <- treatments$scoreFrame %T>% print
    training.set.treated <- training.set.cross$crossFrame %T>% print
    
    vartypes.selected <- if (CATS.ONLY) {
      
      print("feature selection: CATS ONLY")
      c("lev") 
      
    } else { 
      c("clean", "lev")
    }
    
    training.set.treated %>% select(-target.label) %>% names
    
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
    
  }
  
} else {
  
  if (CATS.ONLY) {
    
    # training.set with target and only cats
    training.set %<>% 
      select_if(is.factor) %>% 
      mutate(!!target.label := training.set[[!!target.label]]) %>% 
      select(target.label, everything())
    
    # training.set %>% 
    #   select_if(str_detect(names(.), target.label) | is.factor(.))
    
    # testing.set with target and only cats
    testing.set %<>% 
      select_if(is.factor) %>% 
      mutate(!!target.label := testing.set[[!!target.label]]) %>% 
      select(target.label, everything())
    
    features.labels <- training.set %>% select(-target.label) %>% names
    
    print("feature selection: CATS ONLY")
    
  }
  
}
