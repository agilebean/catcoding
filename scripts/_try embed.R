# devtools::install_github("tidymodels/embed")
library(tidyverse)
library(magrittr)
library(embed)
library(rpart.plot)
library(caret)
data(ptitanic)

NEW <- TRUE
# NEW <- FALSE

dataset <- ptitanic %>% as_tibble()
dataset %>% summary

target.label <- "survived"
target <- dataset[[target.label]]
features <- dataset %>% select(-target.label)
features.labels <- features %>% names

train.index <- createDataPartition(target, p = 0.8, list = FALSE)
training.set <- dataset[train.index, ]
testing.set <- dataset[-train.index, ]

# Step 1: create recipe = formula
recipe.this <- features.labels %>%
  paste(collapse = " + ") %>%
  paste(target.label, "~", .) %>%
  as.formula %>%
  recipe(., data = training.set) %>% print

# Step 2: add catencoding step
if (NEW) {
  recipe.glm <- recipe.this %>%
    step_lencode_glm(pclass, sex, outcome = vars(survived))
  
  recipe.bayes <- recipe.this %>%
    embed::step_lencode_bayes(pclass, sex, outcome = vars(survived))
  
  
  
} else {
  
  recipe.bayes <- readRDS("recipe.bayes.rds")
  
}

# Step 3: apply recipe to training data = prep
training.set.glm <- prep(recipe.glm, training = training.set, verbose = TRUE)
training.set.glm %>% summary
training.set.glm %>% juice

training.set.bayes <- prep(recipe.bayes, training = training.set, verbose = TRUE)

# testing.set.bayes <- training.set.bayes %>% bake(new_data = testing.set) %T>% print
# Step 4: apply recipe to testing data = bake
testing.set.glm <- training.set.glm %>% bake(new_data = testing.set) %T>% print

# Step 5: evaluate = compare predictions with ground truth
confusionMatrix(testing.set.glm[[target.label]], testing.set[[target.label]])
confusionMatrix(testing.set.bayes[[target.label]], testing.set[[target.label]])


data.frame(dummy = testing.set.bayes[[target.label]],
           real = testing.set[[target.label]]) %>%
  ggplot(aes(x = dummy, y = real)) +
  geom_jitter()
  # geom_point()
