library(lightgbm)
library(tidyverse)
library(magrittr)
library(caret)

data(agaricus.train, package='lightgbm')
train <- agaricus.train %T>% print
dtrain <- lgb.Dataset(train$data, label=train$label) %T>% print

data(agaricus.test, package = "lightgbm")
test <- agaricus.test
dtest <- lgb.Dataset.create.valid(dtrain, test$data, label = test$label)

valids <- list(test = dtest)
params <- list(objective="regression", metric="l2")

model <- lgb.train(
  params = params
  , data = dtrain
  , nrounds = 10L
  , valids = valids
  , min_data = 1L
  , learning_rate = 1.0
  , early_stopping_rounds = 5L
)
model

predict(model, train$data)
preds <- predict(model, test$data)
preds
model %>% lgb.importance() %>% lgb.plot.importance()
model %>%
  lgb.interprete(data = test$data, idxset = 1:5) %>%
  .[[5]] %>%
  lgb.plot.interpretation()


model.cv <- lgb.cv(
  params = params
  , data = dtrain
  , nrounds = 10L
  , nfold = 10,
  , valids = valids
  , min_data = 1L
  , learning_rate = 1.0
  , early_stopping_rounds = 5L
)
model.cv

model.cv %>% attributes
model.cv %>% names

# boosters contains the lgb models for each cv run
model.cv$boosters
model.cv$boosters[[1]] %>% class
model.cv$boosters[[1]]$booster %>% class
model.cv$boosters[[1]]$booster %>% names
model.cv$boosters[[1]]$booster %>% predict(train$data)


################################################################################
# Lightgbm
################################################################################
dataset <- quakes %T>% print
target.label <- "mag"
features.labels <- dataset %>% select(-target.label) %>% names

set.seed(17)
training.set.index <- createDataPartition(
  dataset[[target.label]], p = 0.8, list = FALSE
)

training.set <- dataset[training.set.index,] %T>% print
testing.set <- dataset[-training.set.index,]

training.features.sparse <- training.set %>% select(features.labels) %>% as.matrix
training.target <- training.set[[target.label]]

testing.features.sparse <- testing.set %>% select(features.labels) %>% as.matrix
testing.target <- testing.set[[target.label]]

lgbm.training.set <- lgb.Dataset(
  data = training.features.sparse,
  label = training.target
)
lgbm.training.set

lgbm.testing.set <- lgb.Dataset.create.valid(
  dataset = lgbm.training.set,
  data = testing.features.sparse,
  label = testing.target
)
lgbm.testing.set


# valids <- list(train = lgbm.training.set, test = lgbm.testing.set)
valids <- list(test = lgbm.testing.set)
params <- list(objective = "regression", metric = "RMSE")

model.lgbm <- lgb.train(
  params = params
  , data = lgbm.training.set
  , num_leaves = 10 # important
  , nrounds = 100L
  , valids = valids
  , min_data = 1
  , learning_rate = 1
  , early_stopping_rounds = 5
  , nthreads = 2
)

model.lgbm %>% predict(testing.features.sparse) %>%
  RMSE(testing.set[[target.label]])

# if train is defined in valids, best_score is perf on training not testing
## tricky: then model predicts from best training test RMSE!
model.lgbm$best_score

model.lgbm %>% lgb.importance() %>% lgb.plot.importance()

model.lgbm %>% names

set.seed(17)
lgb.model.cv = lgb.cv(
  params = params,
  data = lgbm.training.set,
  valids = valids,
  learning_rate = .95,
  num_leaves = 15,
  num_threads = 2 ,
  nrounds = 1000,
  early_stopping_rounds = 10,
  eval_freq = 20,
  # eval = lgb.normalizedgini,
  # categorical_feature = categoricals.vec,
  nfold = 10,
  stratified = FALSE
)

lgb.model.cv %>% names
lgb.model.cv$best_score
