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
recipe.base <- formula1 %>%
  recipe(., data = training.set) %T>% print

dataset

if (NEW) {
  
  # Step 2: add catencoding step
  recipe.onehot <- recipe.base %>% 
    step_dummy(pclass, sex)
    # step_dummy(all_nominal())
  
  # recipe.onehot %>% summary
  # recipe.onehot %>% juice %>% distinct()
  
  recipe.glm <- recipe.base %>%
    step_lencode_glm(all_nominal(), outcome = vars(survived))
  
  recipe.bayes <- recipe.base %>%
    step_lencode_bayes(all_nominal(), outcome = vars(survived))
  
  # Step 3: apply recipe to training data = prep
  # 
  prep.onehot <- prep(recipe.onehot, training.set, retain = TRUE)
  
  prep.glm <- prep(recipe.glm, training.set, retain = TRUE)
  prep.glm %>% summary
  prep.glm %>% juice
  
  # takes long!
  prep.bayes <- prep(recipe.bayes, training = training.set, retain = TRUE)
  prep.bayes %>% saveRDS("data/prep.bayes.rds")
  
  
} else {
  
  # Step 2: add catencoding step
  recipe.bayes <- recipe.base %>%
    step_lencode_bayes(all_nominal(), outcome = vars(survived))
  
  # Step 3: apply recipe to training data = prep
  prep.bayes <- readRDS("data/prep.bayes.rds") 
}

prep.onehot %>% tidy(number = 1) %>% select(-id) 
prep.glm %>% tidy(number = 1) %>% select(-id) 
prep.bayes %>% tidy(number = 1) %>% select(-id) 


# Step 4: apply recipe to testing data = bake
testing.set.onehot <- prep.onehot %>% bake(testing.set) %T>% print
testing.set.glm <- prep.glm %>% bake(testing.set) %T>% print
testing.set.bayes <- prep.bayes %>% bake(testing.set) %T>% print

# Step 5: Train same models on the encoded training sets
gbm::gbm(formula = formula1, data = training.set)

model.gbm.onehot <- caret::train(
  form = formula1,
  data = training.set,
  method = "gbm"
) %T>% print

training.set.juice <- juice(prep.bayes)
clus <- clusterOn()
model.gbm.onehot2 <- caret::train(
  x = recipe.bayes,
  data = training.set,
  # data = training.set.juice,
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


