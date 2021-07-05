################################################################################
#
# Script:  analysis.R
# Purpose: generate plots for publication
#
################################################################################
packs <- c(
  "tidyverse",
  "magrittr",
  "machinelearningtools",
  "RColorBrewer"
)
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")
sapply(packs, require, character.only = TRUE)

NEW <- TRUE
# NEW <- FALSE

########################################################################
# Study 1
########################################################################
# DATASET.LABEL <- "ames"
DATASET.LABEL <- "diamonds"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
# DATASET.LABEL <- ""

system.time(
  models.lists.dataset <- get_models_list_dataset(DATASET.LABEL, CV.REPEATS)
) # ~9.5s/11 encoders  

models.lists.dataset %>% names

visualize_multiple_models_lists(models.lists.dataset, "RMSE", 
  palette = "Blues", boxfill = "#778899", DATASET.LABEL,
  save_label = paste0("figures/study1-", DATASET.LABEL, ".png"))

### DETAILS
# return the sampling folds for the best algorithm
system.time(
  sampling.folds <- models.lists.dataset %>% 
    # imap(~ mutate(.x, name = .y))
    map(~ get_sampling_models_list(.x, "RMSE")) %>% 
    # tricky tricky: concatenate the sampling folds for all best algorithms
    imap_dfc(~ set_names(.x, .y)) %>% 
    as_tibble() %T>% print  
) # 1.8s
# sampling.folds

########################################################################
# Study 2
########################################################################
# DATASET.LABEL <- ""
# DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"

# dataset.label.list <- DATASET.LABEL.LIST
dataset.label.list <- c("ames", "diamonds")

system.time(
  
  sampling.boxplots.preprocess <- 
    dataset.label.list %>% 
        map(function(dataset_label) {
          
          models.lists.dataset <- get_models_list_dataset(dataset_label, CV.REPEATS)
          visualize_multiple_models_lists(
              models.lists.dataset,
              metric = "RMSE", 
              palette = "Blues", 
              boxfill = "#778899",
              dataset_label
            )
        }) %>% 
        set_names(dataset.label.list)

) #  11.0s/1 datasets x 4 preprocess options

sampling.boxplots.preprocess$ames
sampling.boxplots.preprocess$diamonds

