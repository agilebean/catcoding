
# install
# BINARY_URL <- "https://github.com/catboost/catboost/releases/download/v0.15.1/catboost-R-Darwin-0.15.1.tgz"
# devtools::install_url(BINARY_URL,args = c("--no-multiarch"))

library(catboost)

features <- data.frame(feature1 = c(1, 2, 3), feature2 = c('A', 'B', 'C'))
labels <- c(0, 0, 1)
train_pool <- catboost.load_pool(data = features, label = labels)

# The dataset is created from a synthetic data.frame called features in this example.
# The data argument can also reference a dataset file or a matrix of numerical features.

model <- catboost.train(train_pool,  NULL,
                        params = list(loss_function = 'Logloss',
                                      iterations = 100, metric_period=10))

# The second argument in this example (test_pool) is set to NULL.
# It can also be used to pass a test dataset (the labelled data used for estimating the prediction error while training).
# The params argument is used to specify the training parameters.


real_data <- data.frame(feature1 = c(2, 1, 3), feature2 = c('D', 'B', 'C'))
real_pool <- catboost.load_pool(real_data)

prediction <- catboost.predict(model, real_pool)
print(prediction)

################################################################################

# load libraries
library(mlbench)
library(tidyverse)

# attach the BostonHousing dataset
data(BostonHousing)

#caret library
library(caret)

# Split out validation dataset
# create a list of 80% of the rows in the original dataset we can use for training
set.seed(7)
validation_index <- createDataPartition(BostonHousing$medv, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- BostonHousing[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- BostonHousing[validation_index,]


y_train <- unlist(dataset[c('medv')])
X_train <- dataset %>% select(-medv)

y_valid <- unlist(validation[c('medv')])
X_valid <- validation %>% select(-medv)


train_pool <- catboost.load_pool(data = X_train, label = y_train)
test_pool <- catboost.load_pool(data = X_valid, label = y_valid)

params <- list(iterations=500,
               learning_rate=0.01,
               depth=10,
               loss_function='RMSE',
               eval_metric='RMSE',
               random_seed = 55,
               od_type='Iter',
               metric_period = 50,
               od_wait=20,
               use_best_model=TRUE)

model <- catboost.train(learn_pool = train_pool,params = params)

#predict
y_pred <- catboost.predict(model,test_pool)

#calculate error metrics
postResample(y_pred,validation$medv)
RMSE(y_pred,validation$medv)

