

packs <- c(
  "tidyverse",
  "magrittr",
  "caret",
  "AmesHousing",
  "embed"
)
sapply(packs, require, character.only = TRUE)
dataset <- AmesHousing::make_ames()
target.label <- "Sale_Price"
target <- dataset[[target.label]]
features.labels <- dataset %>% select(-target.label) %>% names

train.index <- createDataPartition(target, p = 0.8, list = FALSE) %>% as.vector()
training.set <- dataset[train.index, ]
testing.set <- dataset[-train.index, ]

features.labels <-training.set %>% 
  select(-target.label) %>% names %T>% print

recipe.base <- features.labels %>% 
  paste(collapse = " + ") %>% 
  paste(target.label, "~", .) %>% 
  as.formula %>% 
  recipe(training.set)


recipe.encoding <- recipe.base %>% 
    step_lencode_bayes(all_nominal(), outcome = vars(target.label)) 

# this step takes forever
system.time(
  prep.encoding <- prep(recipe.encoding, 
                        training = training.set %>% slice(1:50), 
                        retain = TRUE)  
)

training.set.juiced <- juice(prep.encoding) %T>% print
testing.set.baked <- prep.encoding %>% bake(testing_original)