################################################################################
#
# Script:       catcoding.R
# Benchmarking: lm, svmRadial, gbm, rf
# Treatment:    None
#
################################################################################
# common variables
rm(list = ls())


# DATASET.LABEL <- "diamonds"
DATASET.LABEL <- "ames"
# TREATMENT <- "vtreat-design"
# TREATMENT <- NULL
TREATMENT <- "vtreat-cross"

source("_common.R")
source("_strings.R")
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")

CATS.ONLY <- TRUE
# CATS.ONLY <- FALSE

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

################################################################################
################################################################################

NEW <- TRUE
# NEW <- FALSE
### continue
CV.REPEATS <- 2
# CV.REPEATS <- 10
TRY.FIRST <- 500
# TRY.FIRST <- NULL

models_list_label() 
models_metrics_label()

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)

if (!is.null(TREATMENT)) {
  
  if (TREATMENT == "vtreat-design") {
    
    training.set <- training.set.scores
    testing.set <- testing.set.scores
    features.labels <- features.treated
    print("dataset treatment: vtreat::designTreatmentsN")
    
  } else if (TREATMENT == "vtreat-cross") {
    
    training.set <- training.set.treated
    
    if (!is.null(testing.set)) {
      testing.set <- testing.set.treated  
    }
    
    features.labels <- features.treated
    print("dataset treatment: vtreat::mkCrossFrameNExperiment")
    
  }

}

algorithm.list <- c(
  "lm"
  # , "svmRadial"
  # "gbm"
  , "rf"
)


# models.list <- readRDS(models_list_label())
if (NEW) {
  system.time(
    models.list <- benchmark_algorithms(
      target_label = target.label,
      features_labels = features.labels,
      training_set = training.set,
      testing_set = testing.set,
      training_configuration = training.configuration,
      algorithm_list = algorithm.list,
      cv_repeats = CV.REPEATS,
      try_first = TRY.FIRST,
      models_list_name = models_list_label()
    )
  )
} 

if (NEW) {
  models.metrics <- models.list %>% get_model_metrics() %T>% print
  models.metrics %>% saveRDS(models_metrics_label())
  
  # models.metrics <- models.list %>% 
  #   purrr::list_modify("lm" = NULL) %>% 
  #   get_model_metrics() 
  
} else {
  
  models.list <- readRDS(models_list_label())
  models.metrics <- readRDS(models_metrics_label())
  
}

models.metrics

library(gbm)
models.list$gbm %>% varImp()
models.list$svmRadial %>% varImp()

models_list_label() 
models_metrics_label()
