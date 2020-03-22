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
source("_common.R")
source("_strings.R")

# CV.REPEATS <- 2
CV.REPEATS <- 10
models_list_label() 
models_metrics_label()

# TRY.FIRST <- 100
TRY.FIRST <- NULL

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)

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
    models_list_name = models.list.name()
  )
)

models_list_label() 
models_metrics_label()

models.list %>% saveRDS(models_list_label())

models.metrics <- models.list %>% get_model_metrics() %>% print

models.metrics <- models.list %>% 
  purrr::list_modify("lm" = NULL) %>% 
  get_model_metrics() 


models.metrics %>% saveRDS(models_metrics_label())


