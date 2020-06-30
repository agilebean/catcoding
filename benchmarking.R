################################################################################
#
# Script:       benchmarking.R
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
# NEW <- TRUE
NEW <- FALSE

source("plugins/strings.R")

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

algorithm.list <- c(
  "lm"
  , "gbm"
  , "rf"
)

################################################################################
# benchmarking
################################################################################

if (NEW) {
  system.time(
    benchmark.ALL.data.encoder.list <- DATASET.LABEL.LIST %>% 
      map(
        function(DATASET_LABEL) {
          
          print(paste("*** Dataset:", DATASET_LABEL))
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
# 1183s = 19.7m >> EXP2 (60encoders, 5 datasets)
# 20940 = 349m >> EXP3 
# 7552 = 126m >> EXP5 - diamonds
################################################################################

# benchmark.ALL.data.encoder.list$ames$`scikit-target` %>% 
benchmark.ALL.data.encoder.list$ames$`vtreat-dummy` %>% 
  get_model_metrics()

CORRECT <- benchmark.ALL.data.encoder.list %>% 
  map(
    ~.x %>% 
      # .$`scikit-target` %>%
      .$`vtreat-dummy` %>% 
      get_model_metrics(.) %>%
      .$benchmark.all %>%
      print
  )

CORRECT %>% map(~.x)
CORRECT %>% map_dfr(~.x, .id = "dataset")
CORRECT$ames %>% names

####################################################
# DATASET.LABEL <- "diamonds"
DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
# DATASET.LABEL <- "smartflow-scales"
####################################################
# ENCODING <- "factor-encoding"
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
create_benchmarks_all_datasets_all <- function(
  DATASET_LABEL_LIST, ENCODER_LIST) {
  
  benchmarks.all.datasets.all <- DATASET_LABEL_LIST %>% 
    map(
      function(DATASET_LABEL) {
        
        models.lists.dataset <- ENCODER_LIST %>% 
          map(~ models_list_label(DATASET_LABEL, .) %>% readRDS(.)) %>% 
          set_names(ENCODER_LIST)
        models.lists.dataset %>% names
        
        system.time(
          metrics.lists.dataset <- models.lists.dataset %>% 
            map(~ get_model_metrics(.x)) %>% 
            set_names(ENCODER_LIST)
          # set_names(paste0("metrics.list.", ENCODER.LIST))
        ) # 0.7s, 2.2s
        metrics.lists.dataset %>% names
        
        benchmarks.all.dataset <- metrics.lists.dataset %>% 
          map(~ pluck(.x, "benchmark.all")) %T>% print
        
        # benchmarks.full <- benchmarks.all.dataset %>% 
        #   # .id argument gets name
        #   map_df(~.x, .id = "encoder") %>% 
        #   arrange(RMSE.mean)
        
        # create list of the best single encoder per dataset
        benchmarks.top1 <- benchmarks.all.dataset %>% 
          map_df(~ .x %>% filter(RMSE.mean == min(RMSE.mean))) %>% 
          mutate(encoder = ENCODER.LIST) %>% 
          select(encoder, everything()) %>% 
          arrange(RMSE.mean)
        
        # return(list(
        #   benchmarks.full = benchmarks.full,
        #   benchmarks.top1 = benchmarks.top1
        # ))
        return(benchmarks.top1)
      }
    ) %>% 
    set_names(paste0("benchmark.", DATASET_LABEL_LIST))
  
}

system.time(
  benchmarks.all.datasets.all <- 
    create_benchmarks_all_datasets_all(DATASET.LABEL.LIST, ENCODER.LIST)
)
benchmarks.all.datasets.all
# 
# 57.6s >> EXP2 (60 encoders, full datasets)
# 46.5s >> EXP4 (76 encoders, 4 datasets)
# 39.4s >> EXP5 (76 encoders, 4 datasets)

# EXP2
# benchmark.label <- paste0(
#   "output/benchmarks.all.datasets.all.cv", CV.REPEATS, "try1000.rds") %T>% print
#
# EXP3
# benchmark.label <- paste0(
#   "output/benchmarks.all.datasets.all.cv", CV.REPEATS, ".rds") %T>% print
# all <- benchmark.all.datasets.all
# dia <- benchmark.all.datasets.all
# 
# benchmarks.all.datasets.all <- all %>% 
#   list_modify(benchmark.diamonds = dia$benchmark.diamonds)
# 
# benchmark.label <- paste0(
#   "output/benchmarks.all.datasets.all.cv", CV.REPEATS, ".folds5ml2.rds") %T>% print
#
# EXP4
# benchmark.label <- paste0(
#   "output/benchmarks.all.datasets.all.cv", CV.REPEATS, ".folds10.rds") %T>% print
# #
# EXP5
benchmark.label <- paste0(
  "output/benchmarks.all.datasets.all.cv", CV.REPEATS, ".folds5.rds") %T>% print

# benchmarks.all.datasets.all %>% saveRDS(benchmark.label)
benchmarks.all.datasets.all <- readRDS(benchmark.label) %T>% print

benchmarks.all.datasets.all

################################################################################
# DEBUG WRONG models.list
################################################################################
# ENCODER <- "scikit-target"
ENCODER <- "vtreat-dummy"
system.time(
  WRONG <- DATASET.LABEL.LIST %>% 
    map(
      function(DATASET_LABEL) {
        
        models.lists.dataset.label <- models_list_label(DATASET_LABEL, ENCODER)
        
        models.lists.dataset.label %>% print
        system.time(
          models.lists.dataset <- models.lists.dataset.label %>% readRDS(.)
        ) # 4.5s EXP2 > 22.6s EXP4
        
        # CORRECT[[DATASET_LABEL]] %>% 
        #   saveRDS(models.lists.dataset.label)
        models.lists.dataset %>%
          get_model_metrics(.) %>%
          pluck("benchmark.all") %>%
          filter(RMSE.mean == min(RMSE.mean))
      }
    ) %>% 
    set_names(DATASET.LABEL.LIST)
)
WRONG %>% names
WRONG %>% map(~.x)
WRONG %>% map_df(~.x, .id = "dataset")

CORRECT %>% names
CORRECT[[DATASET.LABEL]]
################################################################################
# SCRIBBLE
################################################################################
# utility function to change list element name
rename_list_element <- function() {
  DATASET.LABEL.LIST %>% 
    map(
      function(DATASET_LABEL) {
        data.list <- readRDS(dataset_filename(DATASET_LABEL))
        data.list %>% names
        names(data.list) <- str_replace(names(data.list), "no-encoding", "factor-encoding")
        data.list %>% saveRDS(dataset_filename(DATASET_LABEL))
      })
}

read_list_names <- function() {
  return.list <- DATASET.LABEL.LIST %>% 
    map(
      function(DATASET_LABEL) {
        data.list <- readRDS(dataset_filename(DATASET_LABEL))
        data.list
      }) %>% 
    set_names(DATASET.LABEL.LIST)
  return.list
}

object <- read_list_names()
object %>% names
object$ames$`vtreat-dummy`
object$ames$`factor-encoding`
object$ames$`scikit-onehot`







