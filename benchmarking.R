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
NEW <- TRUE
# NEW <- FALSE

if (getwd() == "/home/rstudio") {
  setwd("sync")
}
# ENCODER.LIST.study2 <- c("scikit-loo")
# ENCODER <- c("scikit-loo")

source("plugins/labels.R")

if (!is.null(PREPROCESS.OPTION) & PREPROCESS.OPTION != "none") {
  preprocess_string <- c("center", "scale", "zv", PREPROCESS.OPTION)  
} else {
  preprocess_string <- NULL
}
preprocess_string

################################################################################
CV.REPEATS <- 2
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
  , "knn"
  , "gbm"
  , "xgbTree"
  , "rf"
  , "ranger"
)

dataset_filename(dataset_label = "pci")

################################################################################
# benchmarking
################################################################################

if (NEW) {
  system.time(
    benchmark.ALL.data.ENCODER.LIST.study2 <- DATASET.LABEL.LIST %>% 
      map(
        function(DATASET_LABEL) {
          
          print(paste("*** Dataset:", DATASET_LABEL))
          data.list <- readRDS(dataset_filename(DATASET_LABEL))
          
          benchmark.ENCODER.LIST.study2 <- ENCODER.LIST.study2 %>% 
            map(
              function(ENCODER) {
                print(paste("ENCODER:", ENCODER))
                # print(models_list_label(DATASET_LABEL, ENCODER))
                # print(data.list[[ENCODER]])
                
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
                  # models_list_name = NULL,
                  preprocess_configuration = preprocess_string,
                  push = TRUE,
                  # push = FALSE,
                  beep = FALSE
                )
              }
            ) %>% 
            set_names(ENCODER.LIST.study2)
        }
      ) %>% 
      set_names(DATASET.LABEL.LIST)
  )
} 
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
# 
# CORRECT <- benchmark.ALL.data.ENCODER.LIST.study2 %>% 
#   map(
#     ~.x %>% 
#       # .$`scikit-target` %>%
#       # .$`vtreat-dummy` %>% 
#       .$`scikit-loo` %>% 
#       get_model_metrics(.) %>%
#       .$benchmark.all %>%
#       print
#   )
# 
# CORRECT %>% map(~.x)
# CORRECT %>% map_dfr(~.x, .id = "dataset")
# CORRECT$ames %>% names

################################################################################
# read all encoded datasets for ONE dataset
################################################################################

####################################################
DATASET.LABEL <- "pci"
# DATASET.LABEL <- "diamonds"
# DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
# DATASET.LABEL <- "smartflow-scales"
####################################################

# ENCODING <- "embed-keras"
# ENCODING <- "factor-encoding"
# ENCODING <- "scikit-binary"
# ENCODING <- "scikit-glmm"
# ENCODING <- "scikit-helmert" # reached elapsed time limit
# ENCODING <- "scikit-loo"
# ENCODING <- "scikit-Mestimate"
# ENCODING <- "scikit-ordinal"
# ENCODING <- "scikit-backward" # reached elapsed time limit
ENCODING <- "scikit-james-stein" # ++2*ames
# ENCODING <- "scikit-polynomial" # ++3*ames
# ENCODING <- "scikit-onehot"
# ENCODING <- "scikit-target" # ++3*ames
# ENCODING <- "scikit-woe" # target must be binary
# ENCODING <- "vtreat-cross"
# ENCODING <- "vtreat-design"
# ENCODING <- "vtreat-dummy"

####################################################
models_list_label(DATASET.LABEL, ENCODING)
# models_metrics_label()
# dataset_filename(DATASET.LABEL)
models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING))
models.metrics <- models.list %>% get_model_metrics() %T>% print
ggsave(
  dpi = 300, width = 6, height = 4,
  paste0("figures/study1-", DATASET.LABEL, "-", ENCODING, ".png") %T>% print)



####################################################
models.lists.dataset.labels <- dir("models/") %>% 
  startsWith(paste0("models.list.", DATASET.LABEL)) %>% 
  dir("models/")[.] %T>% print

system.time(
  models.lists.dataset <- models.lists.dataset.labels %>% 
    map(~paste0("models/", .x) %>% readRDS) %>% 
    set_names(
      gsub("models\\.list\\.(.+)\\.(.+)\\.(.*)\\.(.*)\\.rds", "\\3", 
           models.lists.dataset.labels)
    )
) # 4.5s 24.2s 21.7s
models.lists.dataset %>% names
models.lists.dataset

system.time(
  metrics.lists.dataset <- models.lists.dataset %>% 
    map(~ get_model_metrics(.x)) 
) # 0.7s 1.54s
metrics.lists.dataset %>% names

benchmarks.all.dataset <- metrics.lists.dataset %>% 
  map(~ pluck(.x, "benchmark.all")) %T>% print

benchmarks.all.dataset %>% 
  map_df(~ .x %>% filter(RMSE.mean == min(RMSE.mean))) %>% 
  mutate(encoder = names(benchmarks.all.dataset)) %>% 
  select(encoder, everything()) %>% 
  arrange(RMSE.mean)

################################################################################
# read all encoded datasets for ALL datasets
################################################################################
create_benchmarks_all_datasets_all <- function(
  DATASET_LABEL_LIST, ENCODER_LIST, median_sort = TRUE) {
  
  benchmarks.all.datasets.all <- DATASET_LABEL_LIST %>% 
    map(
      function(DATASET_LABEL) {
        
        DATASET_LABEL <- "ames"
        # DATASET_LABEL <- "designdim"
        # DATASET_LABEL <- "timex"
        # DATASET_LABEL <- "smartflow"
        # ENCODER_LIST <- ENCODER.LIST.study2
        ENCODER_LIST <- "scikit-loo"
        
        metric.sort <- ifelse(median_sort,
                              rlang::sym("RMSE.median"), 
                              rlang::sym("RMSE.mean")
                              )
        
        models.lists.dataset <- ENCODER_LIST %>% 
          map(~ models_list_label(DATASET_LABEL, .) %T>% print %>% readRDS(.)) %>%
          set_names(ENCODER_LIST)
        models.lists.dataset %>% names
        
        models.lists.dataset$`scikit-loo`$ranger$trainingData
        
        
        
        metrics.lists.dataset <- models.lists.dataset %>% 
          map(~ get_model_metrics(.x, median_sort = median_sort)) %>% 
          set_names(ENCODER_LIST)
        # set_names(paste0("metrics.list.", ENCODER.LIST.study2))
        # 0.7s, 2.2s
        metrics.lists.dataset %>% names
        
        benchmarks.all.dataset <- metrics.lists.dataset %>% 
          map(~ pluck(.x, "benchmark.all")) 
        
        benchmarks.full <- benchmarks.all.dataset %>%
          # .id argument gets name
          map_df(~.x, .id = "encoder") %>%
          arrange(!!metric.sort)
        
        # create list of the best single encoder per dataset
        benchmarks.top1 <- benchmarks.all.dataset %>% 
          # tricky tricky: create column with list element name!
          # https://stackoverflow.com/a/48255884/7769076
          imap(~ mutate(.x, encoder = .y)) %>% 
          map_df(~ .x %>% 
                   filter(!!metric.sort == min(!!metric.sort, na.rm = TRUE))) %>% 
          select(encoder, everything()) %>% 
          arrange(!!metric.sort) %T>% print
        
        benchmarks.top2 <- benchmarks.all.dataset %>% 
          # tricky tricky: create column with list element name!
          # https://stackoverflow.com/a/48255884/7769076
          imap(~ mutate(.x, encoder = .y)) %>% 
          map_df(~ .x %>% slice_min(order_by = !!metric.sort, n = 2)) %>% 
          select(encoder, everything()) %>% 
          arrange(!!metric.sort)

        return(benchmarks.top1)
      }
    ) %>% 
    set_names(DATASET_LABEL_LIST)
    # set_names(paste0("benchmark.", DATASET_LABEL_LIST))
}

system.time(
  benchmarks.all.datasets.all <- 
    create_benchmarks_all_datasets_all(
      DATASET.LABEL.LIST, ENCODER.LIST.study2, median_sort = FALSE)
) # 18.7s
benchmarks.all.datasets.all

benchmarks.all.datasets.all %>% 
  map(~select(.x, encoder:model))

benchmarks.all.datasets.all %>% 
  map(~select(.x, encoder:model)) %>% 
  # knitr::kable(format = "latex")
  knitr::kable()



# benchmarks.all.datasets.all %>% map_df(~.x, .id = "data")
# 
# 57.6s >> EXP2 (60 encoders, full datasets)
# 46.5s >> EXP4 (76 encoders, 4 datasets)
# 39.4s >> EXP5 (76 encoders, 4 datasets)
# 41.3s >> EXP6 (76 encoders, 4 datasets)
# 15.0s >> EXP7 (54 encoders, 3 datasets)
# 17.1s >> EXP1 ()
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
#   "output/benchmarks.all.datasets.all.cv", CV.REPEATS, ".ml2.rds") %T>% print
#
# EXP4
# benchmark.label <- paste0(
#   "output/benchmarks.all.datasets.all.cv", CV.REPEATS, ".folds10.rds") %T>% print
# #
# EXP5
# benchmark.label <- paste0(
#   "output/benchmarks.all.datasets.all.cv", CV.REPEATS, ".rds") %T>% print

# EXP6
benchmark.label <- "output/benchmarks.all.datasets.all.cv2.factor.rds"

# EXP7
benchmark.label <- "output/benchmarks.all.datasets.all.cv2.ordinal.rds" %T>% print

# EXP8
# benchmark.label <- paste0(
#   "output/benchmarks.all.datasets.all.cv", CV.REPEATS, ".ordinal4.rds") %T>% print

# EXP9 pca
benchmark.label <- "output/benchmarks.all.datasets.all.cv2.pca.rds"

# EXP10 ica
benchmark.label <- "output/benchmarks.all.datasets.all.cv2.ica.rds"

# EXP11 yeo
benchmark.label <- "output/benchmarks.all.datasets.all.cv2.yeo.rds"

# EXP1 final
benchmark.label <- "output/benchmarks.all.datasets.all.cv10.none.rds"

# benchmarks.all.datasets.all %>% saveRDS(benchmark.label)
benchmarks.all.datasets.all <- readRDS(benchmark.label) # %T>% print

# all alg
benchmarks.all.datasets.all
benchmarks.all.datasets.all$designdim %>% arrange(encoder) %>% print(n = 50)
benchmarks.all.datasets.all$timex %>% arrange(encoder) %>% print(n = 50)


benchmarks.all.datasets.all$designdim %>% 
  group_by(encoder, model) %>% 
  summarize(model, RMSE.median = min(RMSE.median))

# top2 alg
benchmarks.all.datasets.all %>% 
  imap(~ mutate(.x, dataset = .y)) %>% 
  map_df(~ .x %>% slice_min(order_by = RMSE.median, n = 2))

# top1 alg worse with ordinal
benchmarks.all.datasets.all %>% 
  imap(~ mutate(.x, dataset = .y)) %>% 
  map_df(~ .x %>% filter(RMSE.median == min(RMSE.median, na.rm = TRUE)))


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







