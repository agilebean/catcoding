################################################################################
#
# Script:  src/getdata.R
# Output:  training.set, testing.set, features.labels
#
################################################################################
# create split objects for ALL datasets
get_data_split_list <- function(dataset_label_list) {
  dataset_label_list %>% 
    map(~ get(.x)) %>% 
    map(~ split_dataset_original(., TRAIN.TEST.SPLIT, CATS.ONLY)) %>% 
    set_names(dataset_label_list)
}
