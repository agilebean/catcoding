################################################################################
#
# Script:  plugins/get_models.R
# Output:  training.set, testing.set, features.labels
#
################################################################################
get_models_list_dataset <- function(
  dataset_label, 
  cv_repeats, 
  preprocess_option = NULL, 
  models_dir = "models"
) {
  # filter all models for a specific dataset
  models.lists.dataset.labels <- 
    dir(file.path(models_dir, paste0("cv", cv_repeats))) %>% 
    keep(., startsWith(., paste0("models.list.", dataset_label))) %>% 
    {
      # filter all models with specific preprocess
      if (!is.null(preprocess_option)) {
        keep(grepl(preprocess_option, .))
      } else {
        .
      }
    }
  
  print("****************************************************")
  print("Reading...")
  print(models.lists.dataset.labels)
  
  models.lists.dataset <- models.lists.dataset.labels %>% 
    map(~ output_dir("models", paste0("cv", cv_repeats), file = .x) %>% 
          readRDS) %>%
    set_names(
      gsub("models\\.list\\.(.+)\\.([0-9]+)\\.(.+)\\.(.*)\\.(.*)\\.rds", "\\3", 
           models.lists.dataset.labels)
    )
  models.lists.dataset.labels %>% names
  
  print("****************************************************")
  print("Read successfully all above datasets...")
  print("****************************************************")
  
  models.lists.dataset
}


# select the algorithm with lowest RMSE.median
get_sampling_models_list <- function(models_list, metric, median_sort = TRUE) {
  
  require(machinelearningtools)
  
  models.metrics <- models_list %>% get_model_metrics(median_sort) 
  
  # TODO: RMSE.median = metric(RMSE/accuracy)+centrality_metric(mean or median)
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
