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

# dataset.label.list <- dataset.label.list
dataset.label.list <- c("ames", "diamonds")

system.time(
  multiple.benchmarks.boxplots <- dataset.label.list %>%
    map(
      ~ visualize_benchmarks_dataset(
        metric = "RMSE",
        dataset_label,
        palette = "Blues",
        boxfill = "#778899",
        save = TRUE
      )
    ) %>%
    set_names(dataset.label.list)
  
) #  11.0s/1 datasets x 4 preprocess options

multiple.benchmarks.boxplots$ames
multiple.benchmarks.boxplots$diamonds

### 
### DETAILS
### 
DATASET.LABEL <- "diamonds"
system.time(
  models.lists.dataset <- get_models_list_dataset(DATASET.LABEL, CV.REPEATS)
) # 15s/11 encoders

models.lists.dataset %>% names

plot <- visualize_benchmarks_dataset(
  DATASET.LABEL, "RMSE", models.lists.dataset, 
  palette = "Blues", boxfill = "#778899",
  save = TRUE)

plot

