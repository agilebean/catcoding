################################################################################
#
# Script:       catcoding.R
# Benchmarking: lm, svmRadial, gbm, rf
# ENCODING:    None
#
################################################################################
packs <- c(
  "tidyverse",
  "magrittr",
  "vtreat",
  "caret",
  "machinelearningtools",
  "reticulate"
)
sapply(packs, require, character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")

####################################################
# common variables
# DATASET.LABEL <- "diamonds"
DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
# DATASET.LABEL <- "smartflow-scales"


####################################################

ENCODER.LIST <- c(
  "none",
  "embed-bayes",
  "embed-glm",
  "vtreat-cross",
  "vtreat-dummy",
  "scikit-target",
  "scikit-ordinal",
  "scikit-backward-difference",
  "scikit-helmert",
  "scikit-james-stein"
  "scikit-polynomial",
  "scikit-woe",
  "scikit-binary",
  "scikit-onehot"
)


####################################################
# data splits
train.test.split <- 1.0
# QUICK-TEST: use only cats to see whether it's worth catcoding 
# CATS.ONLY <- TRUE
CATS.ONLY <- FALSE

################################################################################
# PREP <- TRUE
PREP <- FALSE

# get dataset
source("plugins/getdata.R")
source("plugins/strings.R")

training.set %>% glimpse

################################################################################
# ENCODING <- NULL
# ENCODING <- "vtreat-design"
# ENCODING <- "vtreat-cross"
# ENCODING <- "vtreat-dummy"
# ENCODING <- "embed-bayes"
ENCODING <- "embed-glm"
# ENCODING <- "embed-keras"
# ENCODING <- "scikit-helmert"
################################################################################
# apply encoding on dataaset
if (is.null(ENCODING)) {

  source("encoders/no-encoding.R")
  
} else { # ENCODINGS
  
  if (ENCODING == "vtreat-cross") {
    
    source("encoders/vtreat-cross.R")
    
  } else if (ENCODING == "vtreat-design") {
    
    config.ratio <- 0.2  # only for vtreat-design
    source("encoders/vtreat-design.R")
    
  } else if (ENCODING == "vtreat-dummy") { # SUMMY ENCODING
    
    source("encoders/vtreat-dummy.R")
    
  } else if (ENCODING == "embed-bayes" | 
             ENCODING == "embed-glm" | 
             ENCODING == "embed-keras"
             ) {
    
    # PREP <- TRUE
    PREP <- FALSE
    source("encoders/embed-steps.R")
    
  } else if (startsWith(ENCODING, "scikit")) {
    
    use_condaenv(condaenv = "reticulate", required = TRUE)
    # py_config()
    # import("category_encoders")
    
    # load python script
    source_python("encoders/scikit-encoders.py", convert = TRUE)
    
    CAT.labels <- training.set %>% 
      select(-target.label) %>% 
      select(where(is.factor)) %>% 
      names %T>% print
    
    # script <- "_encoding_scikit-encoders.py"
    # training_set <- training.set
    # system("python _encoding_scikit-encoders.py ENCODING CAT_labels", wait = FALSE)
    # system2("python", args = c(script, ENCODING, CAT_labels))
    
    ENCODING = "scikit-target"
    # ENCODING = "scikit-ordinal"
    # ENCODING = "scikit-helmert"
    # ENCODING = "scikit-backward-difference"
    # ENCODING = "scikit-james-stein"
    # ENCODING = "scikit-polynomial"
    # ENCODING = "scikit-woe"
    # ENCODING = "scikit-binary"
    # ENCODING = "scikit-onehot"
    
    # apply sckit encoder
    encoder <- apply_scikit_encoder(ENCODING, CAT.labels)
    
    training.original <- training.set
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
    training.set.transformed %>% glimpse
    
    training.set <- training.set.transformed
    features.labels <- training.set %>% select(-target.label) %>% names
    
    if (!is.null(testing.set)) {
      testing.set <- testing.set.treated %>% select(features.selected)
    }

  }
}

training.set
# inform about feature generation stats
print(paste(
  "From", no.cats, "categorical of", no.features.original,
  "original features in total, generated", length(features.labels), "features."
))

CV.REPEATS <- 2
# CV.REPEATS <- 10
TRY.FIRST <- 1000

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)


################################################################################

dataset
target.label
features.labels
ENCODING

training.set %>% glimpse
training.set %>% dim
testing.set %>% glimpse
testing.set %>% dim

################################################################################
# NEW <- TRUE
NEW <- FALSE
### continue
CV.REPEATS <- 2
# CV.REPEATS <- 10
TRY.FIRST <- 500
# TRY.FIRST <- NULL

models_list_label() 
models_metrics_label()
dataset_label()
prep_label()

training.configuration <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = CV.REPEATS,
  savePredictions = "final"
)

algorithm.list <- c(
  "lm"
  , "gbm"
  , "rf"
)

if (NEW) {
  
  if (ENCODING == "embed-bayes" | 
      ENCODING == "embed-glm" | 
      ENCODING == "embed-keras"
  ) {
    

    
  } else {
    
    system.time(
      models.list <- benchmark_algorithms(
        target_label = target.label,
        features_labels = features.labels,
        training_set = training.set,
        testing_set = testing.set,
        training_configuration = training.configuration,
        algorithm_list = algorithm.list,
        cv_repeats = CV.REPEATS,
        try_first = TRY.FIRST,
        models_list_name = models_list_label()
      )
    )
  }

} else {
  
  models.list <- readRDS(models_list_label())
}

if (NEW) {
  models.metrics <- models.list %>% get_model_metrics() %T>% print
  models.metrics %>% saveRDS(models_metrics_label())
  
  # models.metrics <- models.list %>% 
  #   purrr::list_modify("lm" = NULL) %>% 
  #   get_model_metrics() 
  
} else {
  
  models.list <- readRDS(models_list_label())
  models.metrics <- readRDS(models_metrics_label())
  
}

models.metrics

library(gbm)
models.list$gbm %>% varImp()
models.list$svmRadial %>% varImp()

models_list_label() 
models_metrics_label()
