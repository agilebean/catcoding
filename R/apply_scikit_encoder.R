################################################################################
#
# Script:  R/scikit-encoders.R
# Output:  training.set, testing.set - call scikit-learn category_encoders
#
################################################################################
apply_scikit_encoder <- function(
  encoding, training_original, testing_original, target_label) {
  
  # load python script
  reticulate::use_condaenv(condaenv = "reticulate", required = TRUE)
  source_python("src/scikit-encoders.py", convert = TRUE)
  
  CAT.labels <- training_original %>% 
    select(-target_label) %>% 
    select(where(is.factor)) %>% 
    names
  
  ## this passes the arguments literally
  # system("python _encoding_scikit-encoders.py encoding CAT_labels", wait = FALSE)
  ## this passes only string variables
  # system2("python", args = c("_encoding_scikit-encoders.py", encoding, CAT_labels))
  
  # get sckit encoder
  encoder <- get_scikit_encoder(encoding, CAT.labels)
  
  ### Note: replaced fit().transform() by fit_transform()
  ### see https://github.com/scikit-learn-contrib/category_encoders/issues/167#issuecomment-461489109
  ### Note: apply trained model on training.set >> creates "intercept"!
  # train encoder model on training.set & train encoder model on training.set
  training.set.transformed <- encoder$fit_transform(
    training_original, training_original[[target_label]]
  )
  
  
  features.labels.select <- training.set.transformed %>% 
    select(-target_label) %>% names
  
  if (!is.null(testing_original)) {
    testing.set.select <- testing_original %>% 
      select(target_label, features.labels.select)
  } else {
    testing.set.select <- NULL
  }
  
  return(list(
    features.labels = features.labels.select,
    target.label = target_label,
    testing.set = testing.set.select,
    training.set = training.set.transformed
  ))
}

