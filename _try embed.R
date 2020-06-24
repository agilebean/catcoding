# devtools::install_github("tidymodels/embed")
# https://tidymodels.github.io/embed/articles/Applications/GLM.html
# https://tidymodels.github.io/embed/articles/Applications/Tensorflow.html
# all_nominal() from https://rdrr.io/cran/recipes/man/selections.html
# https://cran.r-project.org/web/packages/recipes/vignettes/Dummies.html
# https://konradsemsch.netlify.com/2019/08/caret-vs-tidymodels-comparing-the-old-and-new/
# 
library(tidyverse)
library(magrittr)
library(embed)
library(rpart.plot)
library(caret)
library(recipes)
data(ptitanic)

# NEW <- TRUE
NEW <- FALSE

dataset <- ptitanic %>% as_tibble()
dataset %>% summary
dataset %<>% na.omit()

target.label <- "survived"
target <- dataset[[target.label]]
features <- dataset %>% select(-target.label)
features.labels <- features %>% names

train.index <- createDataPartition(target, p = 0.8, list = FALSE) %>% as.vector()
training.set <- dataset[train.index, ]
testing.set <- dataset[-train.index, ]

formula1 <- features.labels %>%
  paste(collapse = " + ") %>%
  paste(target.label, "~", .) %>%
  as.formula %T>% print
# Step 1: create recipe = formula
recipe.this <- formula1 %>%
  recipe(., data = training.set) %T>% print

dataset

if (NEW) {
  
  # Step 2: add catencoding step
  encoding.onehot <- recipe.this %>% 
    step_dummy(pclass, sex)
    # step_dummy(all_nominal())
  encoding.onehot %>% saveRDS("data/encoding.onehot.rds")
  
  # encoding.onehot %>% summary
  # encoding.onehot %>% juice %>% distinct()
  
  encoding.glm <- recipe.this %>%
    step_lencode_glm(all_nominal(), outcome = vars(survived))
  encoding.glm %>% saveRDS("data/encoding.glm.rds")
  
  encoding.bayes <- recipe.this %>%
    step_lencode_bayes(all_nominal(), outcome = vars(survived))
  encoding.bayes %>% saveRDS("data/encoding.bayes.rds")
  
  # Step 3: apply recipe to training data = prep
  # 
  recipe.onehot <- prep(encoding.onehot, training.set, retain = TRUE)
  recipe.onehot %>% saveRDS("data/recipe.onehot.rds")
  
  recipe.glm <- prep(encoding.glm, training.set, verbose = TRUE)
  recipe.glm %>% summary
  recipe.glm %>% juice
  recipe.glm %>% saveRDS("data/recipe.glm.rds")
  
  # takes long!
  recipe.bayes <- prep(encoding.bayes, training = training.set, verbose = TRUE)
  recipe.bayes %>% saveRDS("data/recipe.bayes.rds")
  
  
} else {
  
  # Step 2: add catencoding step
  encoding.onehot <- readRDS("data/encoding.onehot.rds")
  encoding.glm <- readRDS("data/encoding.glm.rds")
  encoding.bayes <- readRDS("data/encoding.bayes.rds")
  
  # Step 3: apply recipe to training data = prep
  recipe.onehot <- readRDS("data/recipe.onehot.rds")
  recipe.glm <- readRDS("data/recipe.glm.rds")
  recipe.bayes <- readRDS("data/recipe.bayes.rds") 
}

recipe.onehot %>% tidy(number = 1) %>% select(-id) 
recipe.glm %>% tidy(number = 1) %>% select(-id) 
recipe.bayes %>% tidy(number = 1) %>% select(-id) 


# Step 4: apply recipe to testing data = bake
testing.set.onehot <- recipe.onehot %>% bake(testing.set) %T>% print
testing.set.glm <- recipe.glm %>% bake(testing.set) %T>% print
testing.set.bayes <- recipe.bayes %>% bake(testing.set) %T>% print

# Step 5: Train same models on the encoded training sets
gbm::gbm(formula = formula1, data = training.set)

model.gbm.onehot <- caret::train(
  form = formula1,
  data = training.set,
  method = "gbm"
) %T>% print

model.gbm.onehot2 <- caret::train(
  x = encoding.onehot,
  # data = training.set,
  data = juice(recipe.onehot) %>% distinct(),
  method = "gbm"
)
model.gbm.onehot2

# Step 6: evaluate = compare predictions with ground truth
confusionMatrix(testing.set.onehot[[target.label]], testing.set[[target.label]])
confusionMatrix(testing.set.glm[[target.label]], testing.set[[target.label]])
confusionMatrix(testing.set.bayes[[target.label]], testing.set[[target.label]])


plot_confusion <- function(data_set, data_set_encoded, target_label) {
  data.frame(encoded = data_set_encoded[[target_label]],
             real = data_set[[target_label]]) %>%
    ggplot(aes(x = encoded, y = real)) +
    geom_jitter()
  # geom_point()
}

plot_confusion(testing.set,testing.set.pred, target.label)
plot_confusion(testing.set,testing.set.bayes, target.label)
plot_confusion(testing.set,testing.set.glm, target.label)


