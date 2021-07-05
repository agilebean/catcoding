################################################################################
#
# Script:       benchmarking.R
# Benchmarking: lm, gbm, rf
#
################################################################################
packs <- c(
  "catcoding",
  "tidyverse",
  "magrittr",
  "vtreat",
  "caret",
  "machinelearningtools",
  "doParallel",
  "foreach",
  "pushoverr"
)
sapply(packs, require, character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")

NEW <- TRUE
# NEW <- FALSE

if (getwd() == "/home/rstudio") {
  setwd("sync")
}

################################################################################
CV.REPEATS <- 2
# CV.REPEATS <- 5
# CV.REPEATS <- 10
# CV.REPEATS <- 20
# TRY.FIRST <- 100
TRY.FIRST <- NULL

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)

algorithm.list <- c(
  "lm"
  # , "knn"
  , "gbm"
  , "rf"
  # , "ranger"
)

# readRDS(dataset_filename(dataset_label = "designdim"))
models_list_label("diamonds", "encoding", CV.REPEATS)

################################################################################
# benchmarking
################################################################################

STUDY <- "study1"
# STUDY <- "study2"
# STUDY <- "study3"
encoder.list <- paste0("ENCODER.LIST.", STUDY) %>% get


system.time(
  benchmarks.datasets.encoders <- DATASET.LABEL.LIST %>%
    map(function(DATASET_LABEL) {
      
      print(paste("*** Dataset:", DATASET_LABEL))
      data.list <- readRDS(dataset_filename(DATASET_LABEL))
      # benchmark ml models for 1 dataset across all encoders
      benchmark.models.list <- encoder.list %>% 
        
        map(function(ENCODER) {
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
            models_list_name = models_list_label(DATASET_LABEL, ENCODER, CV.REPEATS),
            # models_list_name = NULL,
            preprocess_configuration = TRANSFORM,
            push = TRUE,
            # push = FALSE,
            beep = TRUE
            # beep = FALSE
          )
        }) %>%
        set_names(encoder.list)
    }) %>%
    set_names(DATASET.LABEL.LIST) 
  )

system.time(
  benchmark.datasets.encoders %>% 
    saveRDS(benchmark_filename(2, STUDY) %T>% print)
)

### NEW
# 709s cv2

#### OLD
# 429s >> EXP1 (60encoders, TRY.FIRST = 1000)
# 1183s = 19.7m >> EXP2 (60encoders, 5 datasets)
# 20940 = 349m >> EXP3 
# 7552 = 126m >> EXP5 - diamonds
# 1639s = 27.3m >> EXP6 (54 encoders, 3 datasets)
# 3773s = 62.9m >> EXP7 (54 encoders, 3 datasets, 4 algorithms)
# 5570s = 92.8m >> EXP9 (72 encoders, 4 datasets) PCA
# 1407s = 23.5m >> EXP10(72 encoders, 4 datasets) ICA
# 5475s = 91.3m >> EXP11(72 encoders, 4 datasets) YeoJohnson
# 5634s = 93.9m >> EXP1 (44 encoders, 2 datasets, cv10 = 11.000 runs)
# 7205s = 120.1m >> EXP2 (44 encoders, 4 datasets, cv20 = 6.400 runs)
# 6941s = 115.7m >> EXP2 with 80 N2 cpus 78c/m
################################################################################

# benchmark.ALL.data.ENCODER.LIST.study2$ames$`scikit-target` %>%
# benchmark.ALL.data.ENCODER.LIST.study2$ames$`vtreat-dummy` %>% 

benchmark.ALL.data.ENCODER.LIST.study2$ames$`scikit-loo` %>%
  get_model_metrics()






