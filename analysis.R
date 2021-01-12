################################################################################
#
# Script:  analysis.R
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
  "foreach",
  "RColorBrewer"
)
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")
sapply(packs, require, character.only = TRUE)
# NEW <- TRUE
NEW <- FALSE

source("plugins/labels.R")
source("plugins/get_models.R")


# DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
DATASET.LABEL <- "pci"
# CV.REPEATS <- ""
CV.REPEATS <- 2

ENCODING <- "scikit-loo"
# ENCODING <- "factor-encoding"

PREPROCESS.OPTION <- "none"

# visualize final benchmarking
visualize_sampling_models_list <- function(
  models_lists_dataset, metric, 
  palette = "Set1", boxfill = "#DCDCDC", dataset_label) {
  
  # models.lists.dataset <- get_models_list_dataset(
  #   DATASET.LABEL, preprocess_option, cv_repeats)
  
  # return the sampling folds for the best algorithm
  sampling.folds <- models_lists_dataset %>% 
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
    geom_jitter(aes(color = encoder), alpha = 1, size = 0.5, shape = 1) +
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


########################################################################
# Study 1
########################################################################
DATASET.LABEL <- "pci"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"

system.time(
  # models.lists.dataset <- get_models_list_dataset(DATASET.LABEL, "pca")
  models.lists.dataset <- get_models_list_dataset(
    DATASET.LABEL, 
    PREPROCESS.OPTION, 
    CV.REPEATS
  )
) # ~10.2s/11 encoders  

models.lists.dataset %>% names

# return the sampling folds for the best algorithm
system.time(
  sampling.folds <- models.lists.dataset %>% 
    # imap(~ mutate(.x, name = .y))
    map(~ get_sampling_models_list(.x, "RMSE")) %>% 
    # tricky tricky: concatenate the sampling folds for all best algorithms
    imap_dfc(~ set_names(.x, .y)) %>% 
    as_tibble() %T>% print  
) # 1.8s

sampling.folds %>% select(`scikit-loo`, `scikit-onehot`, `scikit-helmert`)

visualize_sampling_models_list(
  models.lists.dataset, "RMSE", 
  palette = "Blues", boxfill = "#778899", DATASET.LABEL)

ggsave(
  dpi = 300, width = 6, height = 9,
  paste0("figures/study1-", DATASET.LABEL, ".png") %T>% print)

########################################################################
# Study 2
########################################################################
DATASET.LABEL <- "pci"
# DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"

# PREPROCESS.OPTION.LIST <- c("none", "pca", "ica", "YeoJohnson")
PREPROCESS.OPTION.LIST <- "none"

DATASET.LABEL.LIST

system.time(
  
  sampling.boxplots.preprocess <- PREPROCESS.OPTION.LIST %>% 
    map(function(preprocess_option) {
      
      DATASET.LABEL.LIST %>% 
        map(function(dataset_label) {
          
            visualize_sampling_models_list(
              models.lists.dataset,
              metric = "RMSE", 
              palette = "Blues", 
              boxfill = "#778899",
              DATASET.LABEL
            )
        }) %>% 
        set_names(DATASET.LABEL.LIST)
    }) %>% 
    set_names(PREPROCESS.OPTION.LIST)

) #  11.0s/1 datasets x 4 preprocess options

sampling.boxplots.preprocess$none
# sampling.boxplots.preprocess$pca
# sampling.boxplots.preprocess$ica
# sampling.boxplots.preprocess$YeoJohnson
