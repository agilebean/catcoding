################################################################################
# apply encoding on dataset
apply_encoder <- function(data_prepped, encoding) {
  
  print("######################################################################")
  print(paste("ENCODING:", encoding))
  print("#####################")
  print("Encoding...")
  
  encoding_function <- case_when(
    startsWith(encoding, "embed") ~ "apply_embed_encoder",
    startsWith(encoding, "scikit") ~ "apply_scikit_encoder",
    # creates function name starting with "apply_" and underscores 
    TRUE ~ paste0("apply_", gsub("-", "_", encoding))
  ) %>% get()
  
  if (encoding == "irt-encoding") {
    
    training.set <- data_prepped$training.set.irt
    testing.set <- data_prepped$testing.set.irt
    
  } else {
    # apply encoding function
    training.set <- data_prepped$training.set
    testing.set <- data_prepped$testing.set
  }
  
  time.encoding <- system.time(
    data.encoded <- encoding_function(
      encoding, training.set, testing.set, data_prepped$target.label
    )
  ) %>% .["elapsed"] %>% round(., digits = 3)  
  
  # get categorical features
  no.cats <- training.set %>% select(where(is.factor)) %>% ncol
  
  # get #features-original (-1 for target)
  no.features.original <- training.set %>% ncol -1
  
  # get #features-encoded (-1 for target)
  no.features.encoded <- data.encoded$training.set %>% ncol -1
  
  # inform about feature generation stats
  print("#####################")
  print(paste("...finished encoding in:", time.encoding, "seconds"))
  print(paste(
    "From", no.cats, "categorical of", no.features.original,
    "original features in total, generated", 
    no.features.encoded, "features."
  ))
  print("######################################################################")
  
  return(data.encoded)
  
}

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
           apply_encoders_dataset(., encoder_list) %T>% 
           {
             if (save) {
               saveRDS(object = ., file = dataset_filename(dataset_label = .x))
             }
           }
    ) %>%
    set_names(data_label_list)
}