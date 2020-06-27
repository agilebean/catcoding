################################################################################
#
# Script:  encoders/scikit-encoders.R
# Output:  training.set, testing.set - call scikit-learn category_encoders
#
################################################################################
use_condaenv(condaenv = "reticulate", required = TRUE)
# py_config()
# import("category_encoders")

# load python script
source_python("encoders/scikit-encoders.py", convert = TRUE)

CAT.labels <- training.original %>% 
  select(-target.label) %>% 
  select(where(is.factor)) %>% 
  names %T>% print

## this passes the arguments literally
# system("python _encoding_scikit-encoders.py encoding CAT_labels", wait = FALSE)
## this passes only string variables
# system2("python", args = c("_encoding_scikit-encoders.py", encoding, CAT_labels))

# apply sckit encoder
encoder <- apply_scikit_encoder(ENCODING, CAT.labels)

### Note: replaced fit().transform() by fit_transform()
### see https://github.com/scikit-learn-contrib/category_encoders/issues/167#issuecomment-461489109
# train encoder model on training.set & train encoder model on training.set
training.set.transformed <- encoder$fit_transform(
  training.original, training.original[[target.label]]
)

# # train encoder model on training.set
# encoder$fit(training.set, training.set[[target.label]])
# # apply trained model on training.set >> creates "intercept"!
# training.set2 <- encoder$transform(training.set)
# 

training.set <- training.set.transformed
features.labels <- training.set %>% select(-target.label) %>% names

if (!is.null(testing.original)) {
  testing.set <- testing.original %>% select(features.selected)
} else {
  testing.set <- NULL
}

# return(list(training.set = training.set.transformed, 
#             testing.set = testing.set.transformed))
print("######################################################################")
print(paste("TREATMENT:", ENCODING))
print("######################################################################")
