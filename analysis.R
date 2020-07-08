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
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")
sapply(packs, require, character.only = TRUE)
NEW <- TRUE
# NEW <- FALSE

source("plugins/strings.R")

DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"

# ENCODING <- "scikit-loo"
# models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING))

####################################################
# models.lists.dataset.labels <- dir("models/") %>% 
#   {
#     grepl("pca", .) &
#       startsWith(., paste0("models.list.", DATASET.LABEL))
#   } %>% 
#   dir("models/")[.] %T>% print
# 
# system.time(
#   models.lists.dataset <- models.lists.dataset.labels %>% 
#     map(~paste0("models/", .x) %>% readRDS) %>% 
#     set_names(
#       gsub("models\\.list\\.(.+)\\.(.+)\\.(.*)\\.pca\\.(.*)\\.rds", "\\3", 
#            models.lists.dataset.labels)
#     )
# ) # 4.5s 24.2s 21.7s 2.13s


get_models_list_dataset <- function(
  dataset_label, preprocess_option = NULL, cv_repeats) {
  
  models.lists.dataset.labels <- dir("models/") %>% 
    startsWith(., paste0("models.list.", dataset_label)) %>% 
    dir("models/")[.] 
  
  models.lists.dataset.preprocess.labels <- models.lists.dataset.labels %>% 
    { 
      if (!is.null(preprocess_option)) { 
        # get labels containing a preprocess option, e.g. "pca"
        grepl(preprocess_option, .) &
        grepl(paste0(cv_repeats, "repeats"), .)
        
      } else { 
        # get labels containing no preprocess option
        # !grepl("pca|ica|YeoJohnson", .)
        grepl("none", .)
        
      }
    } %>% 
    models.lists.dataset.labels[.]
  
  print("****************************************************")
  print("Reading...")
  print(models.lists.dataset.preprocess.labels)
  
  models.lists.dataset <- models.lists.dataset.preprocess.labels %>% 
    map(~paste0("models/", .x) %>% readRDS) %>%
    set_names(
      gsub("models\\.list\\.(.+)\\.([0-9]+)\\.(.+)\\.(.*)\\.(.*)\\.rds", "\\3", 
           models.lists.dataset.preprocess.labels)
    )
  models.lists.dataset %>% names
  
  print("****************************************************")
  print("Read successfully all above datasets...")
  print("****************************************************")
  
  models.lists.dataset
}

system.time(
  # models.lists.dataset <- get_models_list_dataset(DATASET.LABEL, "pca")
  models.lists.dataset <- get_models_list_dataset(DATASET.LABEL, "none", 20)
)

models.lists.dataset %>% names
models.lists.dataset$`embed-glm`


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
    # remove "~RMSE" or "~Accuracy" from column names
    rename_with(~gsub(paste0("~", metric), "", .)) 

  best.model.metric.sampling
}

# return the sampling folds for the best algorithm
sampling.folds <- models.lists.dataset %>% 
  # imap(~ mutate(.x, name = .y))
  map(~ get_sampling_models_list(.x, "RMSE")) %>% 
  # tricky tricky: concatenate the sampling folds for all best algorithms
  imap_dfc(~ set_names(.x, .y)) %T>% print



# visualize final benchmarking
visualize_sampling_models_list <- function(
  dataset_label, preprocess_option = "none", metric, cv_repeats,
  palette = "Set1", boxfill = "#DCDCDC") {
  
  models.lists.dataset <- get_models_list_dataset(
    DATASET.LABEL, preprocess_option, cv_repeats)
  
  # return the sampling folds for the best algorithm
  sampling.folds <- models.lists.dataset %>% 
    # imap(~ mutate(.x, name = .y))
    map(~ get_sampling_models_list(.x, metric)) %>% 
    # tricky tricky: concatenate the sampling folds for all best algorithms
    imap_dfc(~ set_names(.x, .y)) %T>% print
  
  
  sampling.folds.ordered <- sampling.folds %>% 
    pivot_longer(
      cols = everything(),
      names_to = "encoder",
      values_to = metric
    ) %>% 
    arrange(RMSE) %T>% print
  
  color.codes <- RColorBrewer::brewer.pal(8, palette)[-c(1:2)]
  color.values <- colorRampPalette(color.codes)(ncol(sampling.folds))
  
  plot.sampling.folds.ordered <- sampling.folds.ordered %>% 
    ggplot(aes(x = reorder(encoder, desc(RMSE)), y = RMSE)) +
    coord_flip() +
    # geom_boxplot(fill = "#778899") + # lightslategrey
    geom_boxplot(fill = boxfill) +
    # geom_point(aes(color = encoder), alpha = 0.25, size = 1.5) +
    # geom_point(aes(color = encoder), alpha = 1, size = 1.5, shape = 1) +
    geom_jitter(aes(color = encoder), alpha = 1, size = 1, shape = 1) +
    # scale_color_brewer(guide = "none", palette = palette) +
    scale_color_manual(guide = "none", values = color.values) +
    labs(
      title = paste("Dataset:", dataset_label),
      x = "model",
      y = "RMSE"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    )
  
  plot.sampling.folds.ordered
}

DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
visualize_sampling_models_list(DATASET.LABEL, PREPROCESS.OPTION, "RMSE", 20)
ggsave(
  dpi = 300, width = 6, height = 6,
  paste0("figures/study2-", DATASET.LABEL, ".png") %T>% print)


# visualize_sampling_models_list(models.lists.dataset, "RMSE", "Greys")
# visualize_sampling_models_list(
#   DATASET.LABEL, models.lists.dataset, "RMSE", "Blues", "#778899")

# DATASET.LABEL.LIST


# PREPROCESS.OPTION <- NULL
# system.time(
#   sampling.boxplots <- DATASET.LABEL.LIST %>% 
#     map(function(dataset_label) {
#       
#       get_models_list_dataset(
#         dataset_label, preprocess_option = PREPROCESS.OPTION
#       ) %>% 
#         visualize_sampling_models_list(
#           ., dataset_label, "RMSE", "Blues", "#778899"
#         )
#   }) %>% 
#     set_names(DATASET.LABEL.LIST)
# ) # 41.6s /4 datasets
# 
# sampling.boxplots$ames
# sampling.boxplots$designdim
# sampling.boxplots$timex
# sampling.boxplots$smartflow


PREPROCESS.OPTION.LIST <- c(NULL, "pca", "ica", "YeoJohnson")

system.time(
  
  sampling.boxplots.preprocess <- PREPROCESS.OPTION.LIST %>% 
    map(function(preprocess_option) {
      
      DATASET.LABEL.LIST %>% 
        map(function(dataset_label) {
          
          get_models_list_dataset(
            dataset_label, preprocess_option = preprocess_option
          ) %>% 
            visualize_sampling_models_list(
              ., dataset_label, "RMSE", "Blues", "#778899"
            )
        }) %>% 
        set_names(DATASET.LABEL.LIST)
    }) %>% 
    set_names(PREPROCESS.OPTION.LIST)

) #  /4 datasets x 4 preprocess options

sampling.boxplots
sampling.boxplots.preprocess$pca
sampling.boxplots.preprocess$ica
sampling.boxplots.preprocess$YeoJohnson
