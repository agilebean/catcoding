################################################################################
# apply encoding on dataset
apply_encoder <- function(data_prepped, encoding) {
  
  print("######################################################################")
  print(paste("ENCODING:", encoding))
  print("#####################")
  print("Encoding...")
  
  # data_prepped <- data.prepped
  training.original <- data_prepped$training.set
  testing.original <- data_prepped$testing.set
  target.label <- data_prepped$target.label
  
  if (is.null(encoding) | encoding == "factor-encoding") {
    
    encoding_function <- apply_factor_encoder
    
  } else { # ENCODINGS
    
    if (encoding == "vtreat-cross") {
      
      encoding_function <- apply_vtreat_cross
      
    } else if (encoding == "vtreat-design") {
      
      encoding_function <- apply_vtreat_design
      
    } else if (encoding == "vtreat-dummy") { # DUMMY ENCODING
      
      encoding_function <- apply_vtreat_dummy
      
    } else if (startsWith(encoding, "embed")) {
      
      # PREP <- TRUE
      PREP <- FALSE
      encoding_function <- apply_embed_encoder
      
    } else if (startsWith(encoding, "scikit")) {
      
      encoding_function <-  apply_scikit_encoder
      
    } else if (encoding == "integer-encoding") {
      
      encoding_function <- apply_integer_encoder
      
    }
  }
  
  # apply encoding function
  time.encoding <- system.time(
    data.encoded <- encoding_function(
      encoding, training.original, testing.original, target.label
    )
  ) %>% .["elapsed"] %>% round(., digits = 3)
  
  # get categorical features
  no.cats <- training.original %>% select(where(is.factor)) %>% ncol
  
  # get #features-original (-1 for target)
  no.features.original <- training.original %>% ncol -1
  
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