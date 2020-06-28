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
# data splits
train.test.split <- 1.0
calibration.ratio <- 0.2  # only for vtreat-design
# QUICK-TEST: use only cats to see whether it's worth catcoding 
# CATS.ONLY <- TRUE
CATS.ONLY <- FALSE

################################################################################
# PREP <- TRUE
PREP <- FALSE

################################################################################
# apply encoding on dataset
apply_encoder <- function(encoding, data_original_split) {
  
  print(encoding)
  # data_original_split <- data.original.split
  training.original <- data_original_split$training.set
  testing.original <- data_original_split$testing.set
  target.label <- data_original_split$target.label
  # encoding <- ENCODING
  
  if (is.null(encoding)) {
    
    source("encoders/no-encoding.R")
    
  } else { # ENCODINGS
    
    if (encoding == "vtreat-cross") {
      
      source("encoders/vtreat-cross.R")
      apply_vtreat_cross(
        encoding, training.original, testing.original, target.label
      )
      
    } else if (encoding == "vtreat-design") {
      
      source("encoders/vtreat-design.R")
      
    } else if (encoding == "vtreat-dummy") { # DUMMY ENCODING
      
      source("encoders/vtreat-dummy.R")
      
    } else if (startsWith(encoding, "embed")) {
      
      # PREP <- TRUE
      PREP <- FALSE
      source("encoders/embed-steps.R")
      
    } else if (startsWith(encoding, "scikit")) {
      
      source("encoders/scikit-encoders.R")
      
      data.encoded <- apply_scikit_encoder(
        encoding, training.original, testing.original, target.label
      )
    }
    
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
}
################################################################################
ENCODER.LIST <- c(
  "none",
  "vtreat-cross",
  "vtreat-dummy",
  "scikit-target",
  "scikit-ordinal",
  "scikit-backward",
  "scikit-helmert",
  "scikit-james-stein",
  "scikit-polynomial",
  "scikit-binary",
  "scikit-onehot"
)


DATASET.LABEL.LIST <- c(
  "diamonds",
  "ames",
  "designdim",
  "timex",
  "smartflow"
)
####################################################
# ENCODING <- NULL
# ENCODING <- "vtreat-design"
# ENCODING <- "vtreat-cross"
# ENCODING <- "vtreat-dummy"
# ENCODING <- "scikit-target"
# ENCODING <- "scikit-ordinal"
# ENCODING <- "scikit-helmert" # reached elapsed time limit
# ENCODING <- "scikit-backward" # reached elapsed time limit
# ENCODING <- "scikit-james-stein"
# ENCODING <- "scikit-polynomial"
# ENCODING <- "scikit-binary"
ENCODING <- "scikit-onehot"
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


system.time(
  # get original dataset
  data.original.object <- get_dataset_original(DATASET.LABEL)
) # 0.15s

system.time(
  # split original dataset into training/testing.set
  data.original.split <- split_dataset_original(
    data.original.object, train.test.split, CATS.ONLY
  )  
) # 0.03s

dataset_filename(DATASET.LABEL)
target <- training.original[[target.label]]


apply_all_encoders <- function(data_original_split, encoder_list) {
  
  print("************************************************")
  # print(paste("DATASET:", dataset_label))
  # print(paste("DATASET:", data_original_split))
  
  
  # # get original dataset
  # data.original.object <- get_dataset_original(dataset_label)
  
  sets <- encoder_list %>% 
    map(~apply_encoder(
      .x, data_original_split
    )) %>%
    set_names(encoder_list)

  sets  
  # map2(sets, encoder_list, 
  #      ~saveRDS(
  #        .x, paste(c("data/data", dataset_label, .y, "rds"), collapse = ".")))
}

# create split objects: DONE
system.time(
  ENCODER.LIST %>% 
    map(~ split_dataset_original(
      data.original.object, train.test.split, CATS.ONLY
    )) %>% 
    set_names((ENCODER.LIST))
) # 0.25s

# apply 1 encoder for 1 split object: DONE
system.time(
  data.encoded.split <- apply_encoder(ENCODING, data.original.split)  
) # 2.0s

# apply ALL encoders for 1 split object
system.time(
  apply_all_encoders(data.original.split, ENCODER.LIST)
)


system.time(
  DATASET.LABEL.LIST %>% 
    map(~print(dataset_filename(.x)))  
)

system.time(
  map2(
    DATASET.LABEL.LIST, ENCODER.LIST,
    ~apply_all_encoders(
      .x, ENCODER.LIST, training.original, testing.original, target.label)
  )
)


################################################################################
target.label
features.labels
ENCODING

training.set %>% glimpse
training.set %>% dim
# testing.set %>% glimpse
# testing.set %>% dim

################################################################################
