################################################################################
#
# Script:  _encoding embed-steps.R
# Output:  training.set, testing.set - bayesian target encoding
#
################################################################################
library(embed)
# For outcome, only numeric and two-level factors are currently supported

recipe.base <- features.labels %>% 
  paste(collapse = " + ") %>% 
  paste(target.label, "~", .) %>% 
  as.formula %>% 
  recipe(training.set)


if (ENCODING == "embed-bayes") {
  
  print("TREATMENT: embed-bayes")
  encoding_function <- step_lencode_bayes
  
} else if (ENCODING == "embed-glm") {
  
  print("TREATMENT: embed-glm")
  encoding_function <- step_lencode_glm
    
} else if (ENCODING == "embed-keras") {
  
  print("TREATMENT: embed-keras")
  library(reticulate)
  # use_python("/Users/chaehan/opt/miniconda3/bin/python")
  use_condaenv(condaenv = "reticulate", conda = "~/opt/miniconda3/bin/conda")
  # conda_python(envname = "sodeep", conda = "auto")
  
  encoding_function <- step_embed
}

recipe.encoding <- recipe.base %>% 
  encoding_function(all_nominal(), outcome = vars(target.label))

# PREP <- TRUE
PREP <- FALSE

if (PREP) {
  
  system.time(
    prep.encoding <- prep(recipe.encoding, training = training.set, retain = TRUE)
  )
  prep.encoding %>% saveRDS(prep_label())  
} else {
  
  prep.encoding <- readRDS(prep_label())  
}

training.set.juice <- juice(prep.encoding)
# # set feature labels
# features.labels <- training.set.juice %>% select(-target.label) %>% names
# 
# 
# # prep.bayes %>% tidy(number = 1) %>% select(-id) 
# # testing.set.bayes <- prep.bayes %>% bake(testing.set) %T>% print

# clus <- clusterOn()
# model.gbm <- caret::train(
#   x = recipe.base,
#   data = training.set.juice,
#   method = "gbm"
# )
# clusterOff(clus)

