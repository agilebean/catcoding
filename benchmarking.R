################################################################################
#
# Script:       benchamrk.R
# Benchmarking: lm, gbm, rf
#
################################################################################
packs <- c(
  "tidyverse",
  "magrittr",
  "vtreat",
  "caret",
  "machinelearningtools",
  "doParallel",
  "foreach"
)
sapply(packs, require, character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")
NEW <- TRUE
# NEW <- FALSE

source("plugins/strings.R")

####################################################
ENCODER.LIST <- c(
  "no-encoding"
  , "vtreat-cross"
  , "vtreat-design"
  # , "vtreat-dummy"
  , "scikit-target"
  , "scikit-ordinal"
  , "scikit-backward"
  , "scikit-helmert"
  , "scikit-james-stein"
  , "scikit-polynomial"
  , "scikit-binary"
  , "scikit-onehot"
)

################################################################################
CV.REPEATS <- 2
# CV.REPEATS <- 10
# TRY.FIRST <- 1000
TRY.FIRST <- NULL

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)

algorithm.list <- c(
  "lm"
  , "gbm"
  # , "rf"
)

################################################################################
# benchmarking
################################################################################
if (NEW) {
  system.time(
    benchmark.ALL.data.encoder.list <- DATASET.LABEL.LIST %>% 
      map(
        function(DATASET_LABEL) {
          
          data.list <- readRDS(dataset_filename(DATASET_LABEL))
          
          benchmark.encoder.list <- ENCODER.LIST %>% 
            map(
              function(ENCODER) {
                print(paste("ENCODER:", ENCODER))
                models.list <- benchmark_algorithms(
                  target_label = data.list[[ENCODER]]$target.label,
                  features_labels = data.list[[ENCODER]]$features.labels,
                  training_set = data.list[[ENCODER]]$training.set,
                  testing_set = data.list[[ENCODER]]$testing.set,
                  training_configuration = training.configuration,
                  algorithm_list = algorithm.list,
                  cv_repeats = CV.REPEATS,
                  try_first = TRY.FIRST,
                  models_list_name = models_list_label(DATASET_LABEL, ENCODER),
                  push = TRUE,
                  beep = TRUE
                )
              }
            ) %>% 
            set_names(ENCODER.LIST)
        }
      ) %>% 
      set_names(DATASET.LABEL.LIST)
  )
} 
# 429s >> EXP1 (60encoders, TRY.FIRST = 1000)
# 1183s = 19.7m >> EXP2 (60encoders, full datasets)
################################################################################

####################################################
# DATASET.LABEL <- "diamonds"
DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
# DATASET.LABEL <- "smartflow-scales"
####################################################
# ENCODING <- "no-encoding"
# ENCODING <- "vtreat-cross"
# ENCODING <- "vtreat-design"
# ENCODING <- "vtreat-dummy"
ENCODING <- "scikit-target" # ++3*ames
# ENCODING <- "scikit-ordinal"
# ENCODING <- "scikit-helmert" # reached elapsed time limit
# ENCODING <- "scikit-backward" # reached elapsed time limit
# ENCODING <- "scikit-james-stein" # ++2*ames
# ENCODING <- "scikit-polynomial" # ++3*ames
# ENCODING <- "scikit-binary"
# ENCODING <- "scikit-onehot"
# ENCODING <- "scikit-woe" # target must be binary
####################################################

models_list_label(DATASET.LABEL, ENCODING) 
models_metrics_label()
dataset_filename(DATASET.LABEL)

models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING))
models.metrics <- models.list %>% get_model_metrics() %T>% print

####################################################

################################################################################
# read all encoded datasets for one dataset
################################################################################
models.lists.dataset.labels <- dir("models/") %>% 
  startsWith(paste0("models.list.", DATASET.LABEL)) %>% 
  dir("models/")[.] %T>% print

system.time(
  models.lists.dataset <- models.lists.dataset.labels %>% 
    map(~paste0("models/", .x) %>% readRDS) %>% 
    set_names(paste0("models.list.", ENCODER.LIST))
) # 4.5s
models.lists.dataset %>% names

system.time(
  metrics.lists.dataset <- models.lists.dataset %>% 
    map(~ get_model_metrics(.x)) %>% 
    set_names(paste0("metrics.list.", ENCODER.LIST))
) # 0.7s
metrics.lists.dataset %>% names

benchmarks.all.dataset <- metrics.lists.dataset %>% 
  map(~ pluck(.x, "benchmark.all")) %T>% print

benchmark.all.dataset %>% 
  map_df(~ .x %>% filter(RMSE.mean == min(RMSE.mean))) %>% 
  mutate(encoder = ENCODER.LIST) %>% 
  select(encoder, everything()) %>% 
  arrange(RMSE.mean)

################################################################################
# read all encoded datasets for ALL datasets
################################################################################

system.time(
  benchmark.all.datasets.all <- DATASET.LABEL.LIST %>% 
    map(
      function(DATASET_LABEL) {
        
        models.lists.dataset.labels <- dir("models/") %>% 
          startsWith(paste0("models.list.", DATASET_LABEL)) %>% 
          dir("models/")[.] %T>% print
        
        system.time(
          models.lists.dataset <- models.lists.dataset.labels %>% 
            map(~paste0("models/", .x) %>% readRDS(.)) %>% 
            set_names(paste0("models.list.", ENCODER.LIST))
        ) # 4.5s
        models.lists.dataset %>% names
        
        system.time(
          metrics.lists.dataset <- models.lists.dataset %>% 
            map(~ get_model_metrics(.x)) %>% 
            set_names(paste0("metrics.list.", ENCODER.LIST))
        ) # 0.7s
        metrics.lists.dataset %>% names
        
        benchmarks.all.dataset <- metrics.lists.dataset %>% 
          map(~ pluck(.x, "benchmark.all")) %T>% print
        
        benchmarks.all.dataset %>% 
          map_df(~ .x %>% filter(RMSE.mean == min(RMSE.mean))) %>% 
          mutate(encoder = ENCODER.LIST) %>% 
          select(encoder, everything()) %>% 
          arrange(RMSE.mean)
      }
    ) %>% 
    set_names(paste0("benchmark.", DATASET.LABEL.LIST))
)
# 
# 57.6 >> EXP2 (60encoders, full datasets)

# benchmark.label <- paste0(
#   "output/benchmarks.all.datasets.all.cv", CV.REPEATS, "try1000.rds") %T>% print

benchmark.label <- paste0(
  "output/benchmarks.all.datasets.all.cv", CV.REPEATS, ".rds") %T>% print

benchmark.all.datasets.all %>% saveRDS(benchmark.label)
benchmark.all.datasets.all <- readRDS(benchmark.label) %T>% print


################################################################################











