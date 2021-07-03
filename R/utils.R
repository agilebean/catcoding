# utility function to change list element name
rename_list_element <- function() {
  DATASET.LABEL.LIST %>% 
    map(
      function(DATASET_LABEL) {
        data.list <- readRDS(dataset_filename(DATASET_LABEL))
        data.list %>% names
        names(data.list) <- str_replace(names(data.list), "no-encoding", "factor-encoding")
        data.list %>% saveRDS(dataset_filename(DATASET_LABEL))
      })
}

read_list_names <- function() {
  return.list <- DATASET.LABEL.LIST %>% 
    map(
      function(DATASET_LABEL) {
        data.list <- readRDS(dataset_filename(DATASET_LABEL))
        data.list
      }) %>% 
    set_names(DATASET.LABEL.LIST)
  return.list
}
