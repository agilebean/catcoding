################################################################################
#
# Script:       catcoding.R
# Benchmarking: lm, svmRadial, gbm, rf
# Treatment:    None
#
################################################################################
# common variables
DATASET.LABEL <- "diamonds"
TREATMENT <- "vtreat"
# TREATMENT <- NULL
source("_common.R")
source("_strings.R")

# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")
# CV.REPEATS <- 2
CV.REPEATS <- 10
models_list_label() 
models_metrics_label()

TRY.FIRST <- 100
# TRY.FIRST <- NULL

# clus <- parallel::makeCluster(16)
# stopCluster(clus)

if (!is.null(TREATMENT)) {
  system.time(
    treatment.plan <- designTreatmentsN(
      dframe = config.set,
      varlist = features.labels,
      # parallelCluster = clus, # 5% faster
      outcomename = target.label
    )  
  )
  
  scoreFrame <- treatment.plan$scoreFrame %>%
    select(varName, origName, code) %T>% print
  
  features.treated <- treatment.plan$scoreFrame %>%
    # code "clean":  a numerical variable with no NAs or NaNs
    # code "lev": an indicator variable for a specific level of the original categorical variable.
    filter(code %in% c("clean", "lev")) %>%
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

  # mkCrossFrame*Experiment$crossFrame  
}

################################################################################
################################################################################

NEW <- TRUE
NEW <- FALSE
### continue
CV.REPEATS <- 2
CV.REPEATS <- 10
TRY.FIRST <- 1000
# TRY.FIRST <- NULL
TREATMENT <- "vtreat"
TREATMENT <- NULL

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)

if (!is.null(TREATMENT)) {
  training.set <- training.set.scores
  testing.set <- testing.set.scores
  features.labels <- features.treated
}

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

models_list_label() 
models_metrics_label()

if (NEW) {
  # models.list %>% saveRDS(models_list_label())  
  
  models.metrics <- models.list %>% get_model_metrics() %T>% print
  models.metrics %>% saveRDS(models_metrics_label())
  
  # models.metrics <- models.list %>% 
  #   purrr::list_modify("lm" = NULL) %>% 
  #   get_model_metrics() 
  
} else {
  
  models.metrics <- readRDS(models_metrics_label())
  
}
