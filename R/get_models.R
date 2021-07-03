################################################################################
#
# Script:  plugins/get_models.R
# Output:  training.set, testing.set, features.labels
#
################################################################################
get_models_list_dataset <- function(
  dataset_label, 
  cv_repeats, 
  preprocess_option = "none", 
  models_dir = "models/"
  ) {
  
  # filter all models for a specific dataset
  models.lists.dataset.labels <- dir(models_dir) %>% 
    keep(., startsWith(., paste0("models.list.", dataset_label))) 
  
  # filter all models with specific preprocess and cv_repeats
  models.lists.dataset.preprocess.labels <- models.lists.dataset.labels %>% 
    keep(grepl(preprocess_option, .) & grepl(paste0("cv", cv_repeats), .))
  
  print("****************************************************")
  print("Reading...")
  print(models.lists.dataset.preprocess.labels)
  
  models.lists.dataset <- models.lists.dataset.preprocess.labels %>% 
    map(~paste0(models_dir, .x) %>% readRDS) %>%
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
