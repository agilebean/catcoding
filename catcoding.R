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
# data splits
train.test.split <- 1.0
calibration.ratio <- 0.3  # only for vtreat-design
# QUICK-TEST: use only cats to see whether it's worth catcoding 
# CATS.ONLY <- TRUE
CATS.ONLY <- FALSE

################################################################################
# PREP <- TRUE
PREP <- FALSE

################################################################################
# apply encoding on dataset
apply_encoder <- function(data_original_split, encoding) {
  
  print(encoding)
  # data_original_split <- data.original.split
  training.original <- data_original_split$training.set
  testing.original <- data_original_split$testing.set
  target.label <- data_original_split$target.label
  # encoding <- ENCODING
  
  if (is.null(encoding) | encoding == "no-encoding") {
    
    source("encoders/no-encoding.R")
    encoding_function <- apply_no_encoder
    
  } else { # ENCODINGS
    
    if (encoding == "vtreat-cross") {
      
      source("encoders/vtreat-cross.R")
      encoding_function <- apply_vtreat_cross
      
    } else if (encoding == "vtreat-design") {
      
      source("encoders/vtreat-design.R")
      encoding_function <- apply_vtreat_design
      
    } else if (encoding == "vtreat-dummy") { # DUMMY ENCODING
      
      source("encoders/vtreat-dummy.R")
      encoding_function <- apply_vtreat_dummy
      
    } else if (startsWith(encoding, "embed")) {
      
      # PREP <- TRUE
      PREP <- FALSE
      source("encoders/embed-steps.R")
      
    } else if (startsWith(encoding, "scikit")) {
      
      source("encoders/scikit-encoders.R")
      
      encoding_function <-  apply_scikit_encoder
    }
  }
  
  # apply encoding function
  data.encoded <- encoding_function(
    encoding, training.original, testing.original, target.label
  )
  
  # get categorical features
  no.cats <- training.original %>% select(where(is.factor)) %>% ncol
  
  # get #features-original
  no.features.original <- training.original %>% ncol -1
  
  # get #features-encoded
  no.features.encoded <- data.encoded$training.set %>% ncol -1
  
  # inform about feature generation stats
  print(paste(
    "From", no.cats, "categorical of", no.features.original,
    "original features in total, generated", 
    no.features.encoded, "features."
  ))
  
  return(data.encoded)
  
}
################################################################################
ENCODER.LIST <- c(
  "no-encoding"
  , "vtreat-cross"
  , "vtreat-design"
  , "vtreat-dummy"
  , "scikit-target"
  , "scikit-ordinal"
  , "scikit-backward"
  , "scikit-helmert"
  , "scikit-james-stein"
  , "scikit-polynomial"
  , "scikit-binary"
  , "scikit-onehot"
)


DATASET.LABEL.LIST <- c(
  "ames"
  , "diamonds"
  , "designdim"
  , "timex"
  , "smartflow"
)

####################################################
# dataset
# DATASET.LABEL <- "diamonds"
# DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
# DATASET.LABEL <- "smartflow-scales"
# 
####################################################
ENCODING <- "no-encoding"
ENCODING <- "vtreat-cross"
ENCODING <- "vtreat-design"
# ENCODING <- "vtreat-dummy"
# ENCODING <- "scikit-target"
# ENCODING <- "scikit-ordinal"
# ENCODING <- "scikit-helmert" # reached elapsed time limit
# ENCODING <- "scikit-backward" # reached elapsed time limit
# ENCODING <- "scikit-james-stein"
# ENCODING <- "scikit-polynomial"
# ENCODING <- "scikit-binary"
# ENCODING <- "scikit-onehot"
# ENCODING <- "scikit-woe" # target must be binary

# a <- apply_encoder(ENCODING, training.original, testing.original, target.label)

training.set %>% glimpse
####################################################
# ENCODING <- "embed-bayes"
# ENCODING <- "embed-glm"
# ENCODING <- "embed-keras"
################################################################################
# get dataset
source("plugins/get_data.R")
source("plugins/strings.R")

# get original dataset
system.time(
  data.original.object <- get_dataset_original(DATASET.LABEL)
) # 0.15s

# create split objects for 1 dataset
system.time(
  data.original.split <- split_dataset_original(
    data.original.object, train.test.split, CATS.ONLY
  )  
) # 0.03s


# create split objects for ALL datasets
get_data_split_list <- function() {
  DATASET.LABEL.LIST %>% 
    map(~ get_dataset_original(.x)) %>% 
    map(~ split_dataset_original(., train.test.split, CATS.ONLY)) %>% 
    set_names((DATASET.LABEL.LIST))
}
get_data_split_list() %>% names

# apply 1 encoder on 1 split object
system.time(
  data.encoded.split <- apply_encoder(data.original.split, ENCODING)  
) # 2.0s

microbenchmark::microbenchmark(
  apply_encoder(data.original.split, ENCODING),
  times = 10
)

# apply ALL encoders on 1 split object
apply_all_encoders <- function(data_original_split, encoder_list) {
  
  encoder_list %>% 
    map(~apply_encoder(data_original_split, .x)) %>%
    set_names(encoder_list)
}
system.time(
  data.encoded.list <- apply_all_encoders(data.original.split, ENCODER.LIST) %>% 
    set_names(ENCODER.LIST)
) # 24s ALL, 13s for 11 scikits, 8.2s for vtreat-cross, 2s for vtreat-design/-dummy


# apply ALL encoders on ALL split objects
get_data_encoded_list <- function() {
  DATASET.LABEL.LIST %>%
    map( ~ get_dataset_original(.x)) %>%
    map( ~ split_dataset_original(.x, train.test.split, CATS.ONLY)) %>%
    map( ~ apply_all_encoders(.x, ENCODER.LIST)) %>%
    set_names((DATASET.LABEL.LIST))
}
get_data_encoded_list() %>% names

get_data_encoded_list2 <- function() {
  DATASET.LABEL.LIST %>%
    map(
      ~ get_dataset_original(.x) %>%
        split_dataset_original(train.test.split, CATS.ONLY) %>%
        apply_all_encoders(ENCODER.LIST)
    ) %>%
    set_names(DATASET.LABEL.LIST)
} 
get_data_encoded_list2() %>% names

# # ERROR no usable vars with timex/smartflow+vtreat-design
# microbenchmark::microbenchmark(
#   get_data_encoded_list2(),
#   times = 10
# )

# microbenchmark::microbenchmark(
#   get_data_encoded_list(), get_data_encoded_list2(),
#   times = 1
# )
# 
################################################################################
################################################################################
# FINAL1: create list of encoded datasets
system.time(
  data.encoded.list <- get_data_encoded_list()  
) # 115s for ALL 5 datasets x 11 encoders

# create filenames for all datasets
dataset.filename.list <- DATASET.LABEL.LIST %>% 
    map(~print(dataset_filename(.x))) %T>% print

# FINAL2: save FINAL datasets
map2(data.encoded.list, dataset.filename.list,
     ~saveRDS(.x, .y))

################################################################################
################################################################################
