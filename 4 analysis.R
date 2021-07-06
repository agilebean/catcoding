################################################################################
#
# Script:  analysis.R
# Purpose: generate plots for publication
#
################################################################################
packs <- c(
  "catcoding",
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
# Main
########################################################################
# dataset.label.list <- ""
# dataset.label.list <- "ames"
# dataset.label.list <- "designdim"
# dataset.label.list <- "timex"
# dataset.label.list <- "smartflow"

# dataset.label.list.list <- dataset.label.list.LIST
dataset.label.list.list <- c("ames", "diamonds")

system.time(
  multiple.benchmarks.boxplots <- 
    
    dataset.label.list.list %>%
    
    map(function(dataset_label) {
      
      models.lists.dataset <-
        get_models_list_dataset(dataset_label, CV.REPEATS)
      
      visualize_multiple_models_lists(
        models.lists.dataset, metric = "RMSE", dataset_label,
        palette = "Blues",
        boxfill = "#778899"
      )
    }) %>%
    set_names(dataset.label.list.list)
  
) #  11.0s/1 datasets x 4 preprocess options

multiple.benchmarks.boxplots$ames
multiple.benchmarks.boxplots$diamonds

### DETAILS
### 
system.time(
  models.lists.dataset <- get_models_list_dataset(dataset.label.list, CV.REPEATS)
) # ~9.5s/11 encoders  

models.lists.dataset %>% names

aaa <- visualize_multiple_models_lists(models.lists.dataset, "RMSE", 
  palette = "Blues", boxfill = "#778899", dataset.label.list,
  save_label = paste0("figures/study1-", dataset.label.list, ".png"))

aaa
aaa %>% class
aaa %>% print

