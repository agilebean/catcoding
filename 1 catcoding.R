################################################################################
#
# Script:       catcoding.R
# Benchmarking: lm, svmRadial, gbm, rf
# ENCODING:    None
#
################################################################################
packs <- c(
  "catcoding",
  "tidyverse",
  "magrittr",
  "caret",
  "machinelearningtools",
  "reticulate",
  "doParallel",
  "foreach",
  "furrr"
)
sapply(packs, require, character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools")
# devtools::install_github("agilebean/catcoding")
# unloadNamespace("machinelearningtools")
options(future.fork.multithreading.enable = FALSE)

# source("src/labels.R")

TRAIN.SPLIT <- 1.0

####################################################
# run this for step_lencode_keras
# compareVersion("2.0", as.character(tensorflow::tf_version()))
################################################################################
# apply ALL encoders on 1 split object
apply_all_encoders <- function(data_prepped, encoder_list) {
  
  encoder_list %>% 
    map(~apply_encoder(data_prepped, .x)) %>%
    set_names(encoder_list)
}

SEED <- 171
# apply ALL encoders on ALL split objects
get_data_encoded_list <- function(data_label_list) {
  data_label_list %>%
    map( ~ prep_dataset_original(.x, TRAIN.SPLIT, CATS.ONLY) %>% 
            apply_all_encoders(., ENCODER.LIST.study1) %>% 
            save(file = dataset_filename(dataset_label = .x))
         # , options = furrr_options(seed = SEED,
                                        # packages = "catcoding")
         ) %>%
    set_names(data_label_list) %T>% print
}

# getOption("parallelly.fork.enable")
# options(parallelly.fork.enable = TRUE)
# future::supportsMulticore()
# future::availableCores()
# 
# plan(multicore (workers = 2))
# plan(multisession (workers = 15))

################################################################################
# MAIN
################################################################################
# FINAL1: create list of encoded datasets
clus <- clusterOn()
system.time(
  data.ALL.encoded.list <- get_data_encoded_list(DATASET.LABEL.LIST)  
)
clusterOff(clus)
#### NEW
# 263s in one chain 254 clusterOn()
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
DATASET.LABEL <- "diamonds"
# DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
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
# ENCODING <- "scikit-onehot"
# ENCODING <- "scikit-woe" # target must be binary
# ENCODING <- "scikit-Mestimate"
####################################################
# ENCODING <- "embed-bayes"
# ENCODING <- "embed-glm"
ENCODING <- "embed-keras"
# ENCODING <- "integer-encoding"
################################################################################

# create split objects for 1 dataset
data.prepped <- prep_dataset_original(DATASET.LABEL, TRAIN.SPLIT, CATS.ONLY)  
# 0.06s

# show factors
data.prepped$training.set %>% select(where(is.factor)) %>% str
data.prepped$training.set %>% glimpse
data.prepped$training.set %>% summary

# apply 1 encoder on 1 split object ~ 1.1s
data.encoded <- apply_encoder(data.prepped, ENCODING) 

encoding <- ENCODING


data.encoded$training.set %>% glimpse
data.encoded$training.set %>% summary
data.encoded$features.labels %>% length()

data.encoded %>% 
  save(file = dataset_filename(dataset_label = DATASET.LABEL))

unlink("data/diamonds.encoded.rda", recursive = TRUE) %>% print

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

# # DEBUG ERROR no usable vars with timex/smartflow+vtreat-design
# microbenchmark::microbenchmark(
#   get_data_encoded_list2(),
#   times = 10
# )

# microbenchmark::microbenchmark(
#   get_data_encoded_list(), get_data_encoded_list2(),
#   times = 3
# )