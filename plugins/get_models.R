################################################################################
#
# Script:  plugins/getdata.R
# Output:  training.set, testing.set, features.labels
#
################################################################################

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