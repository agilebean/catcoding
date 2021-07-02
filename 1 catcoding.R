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
  "reticulate",
  "doParallel",
  "foreach",
  "catcoding"
)
sapply(packs, require, character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")
source("src/labels.R")
source("src/get_data.R")

####################################################
# run this for step_lencode_keras
# compareVersion("2.0", as.character(tensorflow::tf_version()))

################################################################################
PREP <- TRUE
# PREP <- FALSE

################################################################################
# apply encoding on dataset
apply_encoder <- function(data_prepped, encoding) {
  
  print("######################################################################")
  print(paste("ENCODING:", encoding))
  print("#####################")
  print("Encoding...")
  
  # data_prepped <- data.prepped
  training.original <- data_prepped$training.set
  testing.original <- data_prepped$testing.set
  target.label <- data_prepped$target.label
  
  if (is.null(encoding) | encoding == "factor-encoding") {
    
    source("encoders/factor-encoding.R")
    encoding_function <- apply_factor_encoder
    
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
      encoding_function <- apply_embed_encoder
      
    } else if (startsWith(encoding, "scikit")) {
      
      source("encoders/scikit-encoders.R")
      encoding_function <-  apply_scikit_encoder
      
    } else if (encoding == "integer-encoding") {
      
      source("encoders/integer-encoding.R")
      encoding_function <- apply_integer_encoder
      
    }
  }
  
  # apply encoding function
  time.encoding <- system.time(
    data.encoded <- encoding_function(
      encoding, training.original, testing.original, target.label
    )
  ) %>% .["elapsed"] %>% round(., digits = 3)
  
  # get categorical features
  no.cats <- training.original %>% select(where(is.factor)) %>% ncol
  
  # get #features-original (-1 for target)
  no.features.original <- training.original %>% ncol -1
  
  # get #features-encoded (-1 for target)
  no.features.encoded <- data.encoded$training.set %>% ncol -1
  
  # inform about feature generation stats
  print("#####################")
  print(paste("...finished encoding in:", time.encoding, "seconds"))
  print(paste(
    "From", no.cats, "categorical of", no.features.original,
    "original features in total, generated", 
    no.features.encoded, "features."
  ))
  print("######################################################################")
  
  return(data.encoded)
  
}


# apply ALL encoders on 1 split object
apply_all_encoders <- function(data_prepped, encoder_list) {
  
  encoder_list %>% 
    map(~apply_encoder(data_prepped, .x)) %>%
    set_names(encoder_list)
}

# apply ALL encoders on ALL split objects
get_data_ALL_encoded_list <- function(data_label_list) {
  data_label_list %>%
    map( ~ prep_dataset_original(.x, TRAIN.SPLIT, CATS.ONLY)) %>%
    map( ~ apply_all_encoders(.x, ENCODER.LIST.study1)) %>%
    set_names(data_label_list) %T>% 
    imap(~ .x %>% saveRDS(.y %>% dataset_filename))
    # imap(~print(.y))
}

################################################################################
# MAIN
################################################################################
# FINAL1: create list of encoded datasets
system.time(
  data.ALL.encoded.list <- get_data_ALL_encoded_list(DATASET.LABEL.LIST)  
)
#### NEW
# 407s for 100 encoders (5 x 20 encoders)
#### OLD
# 116.3s for 55 encoders (5 datasets x 11 encoders)
# 29.7s for 19 encoders (diamonds) 
# 29.5s for 22 encoders (pci)
# 178s for 76 encoders (4 datasets x 19 encoders) = ~2.3s
# 178s for 88 encoders (4 datasets x 22 encoders) = ~2.3s
data.ALL.encoded.list %>% names
data.ALL.encoded.list$pci$`embed-keras`
data.ALL.encoded.list$pci$`scikit-loo`
data.ALL.encoded.list$ames$`scikit-target`
data.ALL.encoded.list$ames$`scikit-loo`
data.ALL.encoded.list$ames$`scikit-loo`$target.label

################################################################################
# Tests on single dataset and single encoder
################################################################################

####################################################
# dataset
# DATASET.LABEL <- "diamonds"
# DATASET.LABEL <- "ames"
DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
# DATASET.LABEL <- "smartflow-scales"
# 
####################################################
# ENCODING <- "factor-encoding"
# ENCODING <- "vtreat-cross"
# ENCODING <- "vtreat-design"
# ENCODING <- "vtreat-dummy"
# ENCODING <- "scikit-target"
# ENCODING <- "scikit-loo"
# ENCODING <- "scikit-hashing" 
# ENCODING <- "scikit-ordinal"
# ENCODING <- "scikit-helmert" # reached elapsed time limit
# ENCODING <- "scikit-backward" # reached elapsed time limit
# ENCODING <- "scikit-james-stein"
# ENCODING <- "scikit-polynomial"
# ENCODING <- "scikit-binary"
ENCODING <- "scikit-onehot"
# ENCODING <- "scikit-woe" # target must be binary
# ENCODING <- "scikit-Mestimate"
####################################################
# ENCODING <- "embed-bayes"
# ENCODING <- "embed-glm"
# ENCODING <- "embed-keras"
# ENCODING <- "integer-encoding"
################################################################################

source("src/get_data.R")
# create split objects for 1 dataset
data.prepped <- prep_dataset_original(DATASET.LABEL, TRAIN.SPLIT, CATS.ONLY)  
# 0.06s

# show factors
data.prepped$training.set %>% select(where(is.factor)) %>% str
data.prepped$training.set %>% glimpse
data.prepped$training.set %>% summary

# apply 1 encoder on 1 split object ~ 1.1s
data.encoded <- apply_encoder(data.prepped, ENCODING) 

data.encoded$training.set %>% glimpse
data.encoded$training.set %>% summary
data.encoded$features.labels %>% length()

################################################################################
################################################################################

# # DEBUG ERROR no usable vars with timex/smartflow+vtreat-design
# microbenchmark::microbenchmark(
#   apply_encoder(data.prepped, ENCODING),
#   times = 10
# )

# system.time(
#   data.encoded.list <- apply_all_encoders(data.prepped, ENCODER.LIST.study1) %>%
#     set_names(ENCODER.LIST.study1)
# ) # 24s ALL, 13s for 11 scikits, 8.2s for vtreat-cross, 2s for vtreat-design/-dummy


################################################################################
################################################################################
# SCRIBBLE
################################################################################
# get_data_encoded_list2 <- function() {
#   DATASET.LABEL.LIST %>%
#     map(
#       ~ get_dataset_original(.x) %>%
#         prep_dataset_original(TRAIN.SPLIT, CATS.ONLY) %>%
#         apply_all_encoders(ENCODER.LIST.study1)
#     ) %>%
#     set_names(DATASET.LABEL.LIST)
# } 
# get_data_encoded_list2() %>% names

# # DEBUG ERROR no usable vars with timex/smartflow+vtreat-design
# microbenchmark::microbenchmark(
#   get_data_encoded_list2(),
#   times = 10
# )

# microbenchmark::microbenchmark(
#   get_data_encoded_list(), get_data_encoded_list2(),
#   times = 3
# )