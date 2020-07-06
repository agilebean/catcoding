################################################################################
#
# Script:  encoders/embed-steps.R
# Output:  training.set, testing.set - bayesian target encoding
#
################################################################################

# For outcome, only numeric and two-level factors are currently supported

apply_embed_encoder <- function(
  encoding, training_original, testing_original, target_label) {
  
  require(embed)
  
  if (encoding == "embed-bayes") {
    
    print("TREATMENT: embed-bayes")
    encoding_function <- step_lencode_bayes
    
  } else if (encoding == "embed-glm") {
    
    print("TREATMENT: embed-glm")
    encoding_function <- step_lencode_glm
    
  } else if (encoding == "embed-keras") {
    
    print("TREATMENT: embed-keras")
    library(reticulate)
    # use_python("/Users/chaehan/opt/miniconda3/bin/python")
    use_condaenv(condaenv = "reticulate", conda = "~/opt/miniconda3/bin/conda")
    # conda_python(envname = "sodeep", conda = "auto")
    
    encoding_function <- step_embed
  }
  
  features.labels <- training_original %>% 
    select(-target_label) %>% names %T>% print
  
  recipe.base <- features.labels %>% 
    paste(collapse = " + ") %>% 
    paste(target_label, "~", .) %>% 
    as.formula %>% 
    recipe(training_original)
    
  # add encoder to recipe
  recipe.encoding <- recipe.base %>% 
    encoding_function(all_nominal(), outcome = vars(target_label)) 
  
  recipe.encoding %>% print
  
  prep.encoding <- prep(recipe.encoding, training = training_original, retain = TRUE)
  
  training.set.juiced <- juice(prep.encoding) %T>% print
  
  features.labels.juiced <- training.set.juiced %>% 
    select(-target_label) %>% names
  
  if (!is.null(testing_original)) {
    testing.set.baked <- prep.encoding %>% bake(testing_original)
  } else {
    testing.set.baked <- NULL
  }
  
  return(list(
    features.labels = features.labels.juiced,
    target.label = target_label,
    training.set = training.set.juiced,
    testing.set = testing.set.baked
  ))
  
}


# # set feature labels
# features.labels <- training.set.juice %>% select(-target.label) %>% names
# 
# 
# # prep.bayes %>% tidy(number = 1) %>% select(-id) 
# # testing.set.bayes <- prep.bayes %>% bake(testing.set) %T>% print

# CV.REPEATS <- 2
# # CV.REPEATS <- 10
# TRY.FIRST <- 1000
# 
# training.configuration <- trainControl(
#   method = "repeatedcv",
#   number = 2,
#   repeats = CV.REPEATS,
#   savePredictions = "final"
# )
# 
# # use original training.set https://stackoverflow.com/a/55270581/7769076
# clus <- clusterOn()
# model.gbm <- caret::train(
#   x = recipe.encoding,
#   data = training.set,
#   method = "gbm",
#   trainControl = training.configuration
# )
# clusterOff(clus)
# 
# # workaround but doesn't avoid to prep()
# clus <- clusterOn()
# model.gbm <- caret::train(
#   x = recipe.base,
#   data = training.set.juice,
#   method = "gbm",
#   trainControl = training.configuration
# )
# clusterOff(clus)

