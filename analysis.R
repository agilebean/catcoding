################################################################################
#
# Script:       analysis.R
# Purpose: generate plots for publication
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
devtools::install_github("agilebean/machinelearningtools")
unloadNamespace("machinelearningtools")
sapply(packs, require, character.only = TRUE)
NEW <- TRUE
# NEW <- FALSE

source("plugins/strings.R")

DATASET.LABEL <- "designdim"
ENCODING <- "scikit-loo"
models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING))

####################################################
models.lists.dataset.labels <- dir("models/") %>% 
  {grepl("pca", .) & startsWith(., paste0("models.list.", DATASET.LABEL))} %>% 
  dir("models/")[.] %T>% print

system.time(
  models.lists.dataset <- models.lists.dataset.labels %>% 
    map(~paste0("models/", .x) %>% readRDS) %>% 
    set_names(
      gsub("models\\.list\\.(.+)\\.(.+)\\.(.*)\\.pca\\.(.*)\\.rds", "\\3", 
           models.lists.dataset.labels)
    )
) # 4.5s 24.2s 21.7s

models.lists.dataset %>% names
models.lists.dataset$`scikit-onehot` 
models.lists.dataset$`factor-encoding` %>% 
  get_model_metrics(median_sort = TRUE) 


models.lists.dataset %>% 
  map(
    function(models_list) {
      models.metrics <- models_list %>% get_model_metrics(median_sort = TRUE) 
      
      best.model.label <- models.metrics$benchmark.all %>% 
        filter(RMSE.median == min(RMSE.median, na.rm = TRUE)) %>% 
        .$model
      
      best.model.sampling <- models.metrics$resamples.values %>% 
        select(starts_with(best.model.label))
    }
  )

# select the algorithm with lowest RMSE.median
get_sampling_models_list <- function(models_list, metric, median_sort = TRUE) {
  
  models.metrics <- models_list %>% get_model_metrics(median_sort) 
  
  best.model.label <- models.metrics$benchmark.all %>% 
    filter(RMSE.median == min(RMSE.median, na.rm = TRUE)) %>% 
    .$model
  
  best.model.sampling <- models.metrics$resamples.values %>% 
    select(starts_with(best.model.label))
  
  best.model.metric.sampling <- best.model.sampling %>%
    select(ends_with(metric)) %>%
    rename_with(~gsub(paste0("~", metric), "", .)) 

  best.model.metric.sampling
}

# return the sampling folds for the best algorithm
models.lists.dataset %>% 
  # imap(~ mutate(.x, name = .y))
  map(~ get_sampling_models_list(.x, "RMSE"))

# return the sampling folds for the best algorithm
models.lists.dataset %>% 
  # imap(~ mutate(.x, name = .y))
  map(~ get_sampling_models_list(.x, "RMSE")) %>% 
  # concatenate the sampling folds for all best algorithms
  imap_dfc(~ set_names(.x, .y), .id = "sta") 



