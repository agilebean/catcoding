# apply ALL encoders on 1 split object
apply_encoders_dataset <- function(data_prepped, encoder_list) {
  
  encoder_list %>% 
    map(~apply_encoder(data_prepped, .x)) %>%
    set_names(encoder_list)
}

# apply ALL encoders on ALL split objects
apply_encoders_data_list <- function(data_label_list, encoder_list, save = TRUE) {
  data_label_list %>%
    map( ~ prep_dataset_original(.x, TRAIN.SPLIT, CATS.ONLY) %>% 
           apply_encoders_dataset(., encoder_list) %>% 
           {
             if (save) {
               saveRDS(file = dataset_filename(dataset_label = .x))
             } else {
               .
             }
           }
    ) %>%
    set_names(data_label_list) %T>% print
}