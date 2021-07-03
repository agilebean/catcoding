packs <- c(
  # "catcoding",
  "tidyverse",
  "magrittr",
  "vtreat",
  "caret",
  "machinelearningtools",
  "doParallel",
  "foreach"
)
sapply(packs, require, character.only = TRUE)
################################################################################
# read all encoded datasets for ONE dataset
################################################################################

####################################################
# DATASET.LABEL <- ""
DATASET.LABEL <- "diamonds"
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
# ENCODING <- "scikit-james-stein" # ++2*ames
# ENCODING <- "scikit-polynomial" # ++3*ames
# ENCODING <- "scikit-onehot"
# ENCODING <- "scikit-target" # ++3*ames
# ENCODING <- "scikit-woe" # target must be binary
# ENCODING <- "vtreat-cross"
# ENCODING <- "vtreat-design"
ENCODING <- "vtreat-dummy"

################################################################################

models.lists.dataset <- get_models_list_dataset(DATASET.LABEL, 2) %T>% print
models.lists.dataset %>% names

system.time(
  benchmarks.all.datasets.all <- 
    create_benchmarks_all_datasets_all(
      DATASET.LABEL.LIST, ENCODER.LIST.study2, median_sort = FALSE)
) # 38s
benchmarks.all.datasets.all

# only model
benchmarks.all.datasets.all %>% map(~select(.x, encoder:model)) %>% print
# knitr::kable(format = "latex")
# knitr::kable(format = "html")

# TRICKY: collapse list into df with data column = dataset (list element name)
benchmarks.all.datasets.all %>% map_df(~.x, .id = "data")
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

####################################################
# print benchmark for ONE dataset & ONE encoder
####################################################
if (NEW) {
  models_list_label(DATASET.LABEL, ENCODING)
  models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING))
  models.metrics <- models.list %>% get_model_metrics() %T>% print
  ggsave(dpi = 300, width = 6, height = 4,
         paste0("figures/study1-", DATASET.LABEL, "-", ENCODING, ".png") %T>% print)
}

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


object <- read_list_names()
object %>% names
object$ames$`vtreat-dummy`
object$ames$`factor-encoding`
object$ames$`scikit-onehot`


