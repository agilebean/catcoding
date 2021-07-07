################################################################################
# read all encoded datasets for ALL datasets
################################################################################
create_benchmarks_top_encoders <- function(
  study, DATASET_LABEL_LIST, ENCODER_LIST, median_sort = TRUE) {
  
  benchmarks.all.datasets.all <- DATASET_LABEL_LIST %>% 
    map(
      function(DATASET_LABEL) {
        
        metric.sort <- ifelse(median_sort,
                              rlang::sym("RMSE.median"), 
                              rlang::sym("RMSE.mean")
        )
        
        models.lists.dataset <- ENCODER_LIST %>% 
          map(~ models_list_label(study, DATASET_LABEL, ., CV.REPEATS) %T>% print %>% 
                readRDS(.)) %>% set_names(ENCODER_LIST)
        
        metrics.lists.dataset <- models.lists.dataset %>% 
          map(~ get_model_metrics(.x, median_sort = median_sort)) %>% 
          set_names(ENCODER_LIST)
        # 0.7s, 2.2s
        
        benchmarks.all.dataset <- metrics.lists.dataset %>% 
          map(~ pluck(.x, "benchmark.all")) 
        
        benchmarks.full <- benchmarks.all.dataset %>%
          # .id argument gets name
          map_df(~.x, .id = "encoder") %>%
          arrange(!!metric.sort)
        
        # create list of the best single encoder per dataset
        benchmarks.top1 <- benchmarks.all.dataset %>% 
          # tricky tricky: create column with list element name!
          # https://stackoverflow.com/a/48255884/7769076
          imap(~ mutate(.x, encoder = .y)) %>% 
          map_df(~ .x %>% 
                   filter(!!metric.sort == min(!!metric.sort, na.rm = TRUE))) %>% 
          select(encoder, everything()) %>% 
          arrange(!!metric.sort) %T>% print
        
        benchmarks.top2 <- benchmarks.all.dataset %>% 
          # tricky tricky: create column with list element name!
          # https://stackoverflow.com/a/48255884/7769076
          imap(~ mutate(.x, encoder = .y)) %>% 
          # make a df from the lowest 2 RMSE results from each encoded dataset
          map_df(~ .x %>% slice_min(order_by = !!metric.sort, n = 2)) %>% 
          select(encoder, everything()) %>% 
          arrange(!!metric.sort)
        
        return(benchmarks.top1)
      }
    ) %>% 
    set_names(DATASET_LABEL_LIST)
}

