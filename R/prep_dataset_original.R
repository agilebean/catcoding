################################################################################
# # FUNCTION: split original dataset into training/testing.set
################################################################################
prep_dataset_original <- function(
  dataset_label, train_test_split, cats_only = FALSE) {
  
  # get original dataset
  data <- get(dataset_label) %>% print
  
  # retrieve dataset & labels
  target.label <- data$target.label
  features.labels <- data$features.labels
  
  # subset features & remove other features or DVs
  dataset <- data$data %>% select(target.label, features.labels) 
  
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