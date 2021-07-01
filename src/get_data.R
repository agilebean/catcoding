################################################################################
#
# Script:  plugins/getdata.R
# Output:  training.set, testing.set, features.labels
#
################################################################################

################################################################################
# 
# GOAL:   retrieve original dataset
# INPUT:  dataset_label
# OUTPUT: dataset, target.label, features.labels
# 
################################################################################
get_dataset_original <- function(dataset_label) {
  
  print("**********************************************************************")
  print("*************************************************")
  print(paste("***** DATASET:", dataset_label, "*****"))

  if (dataset_label == "pci") {
    
    target.label <- "job_score"
    features.labels <- pci %>% select(-target.label) %>% names
    
  } else  if (dataset_label == "diamonds") {
    
    # define target and features
    target.label <- "price"
    features.labels <- dataset %>% select(-target.label) %>% names
    
  } else if (dataset_label == "ames") {
    
    # define target and features
    target.label <- "Sale_Price"
    features.labels <- dataset %>% select(-target.label) %>% names
    
  } else if (dataset_label == "designdim") {
    
    target.label <- "NPS"
    features.labels <- designdim %>% select(-target.label) %>% names
    
  } else if (dataset_label == "timex") {
    
    # define target and features
    target.label <- "HAPPINESS"
    features.labels <- timex %>% 
      select(-target.label) %>% 
      select(-c(active:ashamed)) %>% #DV: PANAS-PA/-NA 
      select(-starts_with("lifesatis"), -happy) %>% #DV: lifesatis+happiness-global
      names
    
  } else if (dataset_label == "smartflow") {
    
    target.label <- "SMADDICTION"
    features.labels <- smartflow %>% 
      select(-target.label, -starts_with("addicted")) %>% names
    
  } else if (dataset_label == "smartflow.scales") {
    
    # define target and features
    target.label <- "SmartphoneAddiction"
    features.labels <- smartflow.scales %>% 
      select(-target.label, -SmartphoneHours) %>% names
    
  }
  
  # subset features & remove other DV items
  dataset %<>% select(target.label, features.labels)
  
  # put target as last column
  dataset %<>% relocate(target.label, .after = last_col())
  
  return(list(
    dataset = dataset,
    target.label = target.label,
    features.labels = features.labels
  ))
}

################################################################################
# # FUNCTION: split original dataset into training/testing.set
################################################################################
split_dataset_original <- function(
  dataset_original_object, train_test_split, cats_only = FALSE) {
  
  # retrieve dataset & labels
  dataset <- dataset_original_object$dataset
  target.label <- dataset_original_object$target.label
  features.labels <- dataset_original_object$features.labels
  
  ####################################################
  # create training/testing.set
  train.index <- createDataPartition(
    dataset[[target.label]], p = train_test_split, list = FALSE
  )
  
  training.set <- dataset %>% 
    # subset
    slice(train.index) %>% 
    # shuffle
    sample_n(nrow(.))
  
  if (train_test_split < 1.0) {
    
    testing.set <- dataset[-train.index, ]  
  } else { # default case: whole dataset
    
    print("no testing set")
    testing.set <- NULL
  }
  ####################################################
  # special case: only cats without treatment
  # cats must be converted in _getdata.R
  if (cats_only) {
    
    print("feature selection: CATS ONLY")
    # training.set with target and only cats
    training.set %<>% 
      select_if(is.factor) %>% 
      # add target.label column bec all factors
      mutate(!!target.label := training.set[[!!target.label]])
    
    # testing.set with target and only cats
    if (!is.null(testing.set)) {
      
      testing.set %<>% 
        select_if(is.factor) %>% 
        mutate(!!target.label := testing.set[[!!target.label]]) 
      
    }
    
  } else {
    
    print("feature selection: ALL")
  }
  ####################################################
  
  # create target
  target <- dataset[[target.label]]
  
  # get categorical features
  no.cats <- dataset %>% select(where(is.factor)) %>% ncol
  
  # get #features-original
  no.features.original <- length(features.labels)
  
  # inform about dataset type, #features, target
  if (is.numeric(target)) {
    
    print(
      paste(
        "This is a regression dataset with target *", target.label, "* and",
        no.cats, "categorical of", no.features.original, "features in total"
      )
    )
    
  } else if (is.factor(target)) {
    
    print(
      paste(
        "This is a classification dataset with target:", target.label, "and",
        no.cats, "categorical of", no.features.original, "features in total"
      )
    )
    
  }
  ####################################################
  return(
    list(
      training.set = training.set,
      testing.set = testing.set,
      target.label = target.label,
      features.labels = features.labels
    )
  )
  ##############################################################################
}

