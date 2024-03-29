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
# credentials::set_github_pat()
# usethis::git_sitrep()
# devtools::install_github("briandconnelly/pushoverr")
# devtools::install_github("agilebean/catcoding", force = TRUE)
# unloadNamespace("machinelearningtools")

####################################################
# run this for step_lencode_keras
# compareVersion("2.0", as.character(tensorflow::tf_version()))
################################################################################

# CMD+SHIFT+L
Sys.setenv(RETICULATE_PYTHON = "~/miniforge3/envs/reticulate/bin/python")
options(reticulate.conda_binary = "~/miniforge3/condabin/conda")

################################################################################
# MAIN
################################################################################
# FINAL1: create list of encoded datasets
# DATASET.LABEL.TEST <- "smartflow"
# DATASET.LABEL.TEST <- DATASET.LABEL.LIST %>% discard(., grepl("smartflow", .))
system.time(
  data.encoded.list <- apply_encoders_data_list(
    # DATASET.LABEL.TEST, ENCODER.LIST.test
    DATASET.LABEL.LIST, ENCODER.LIST
    # , save = TRUE
    # , save = FALSE
    )  
) # 159s - 169/147m1

dataset.label <- "swbliss"
data.encoded.list[[dataset.label]]
data.encoded.list$swbliss$`irt-encoding`
#### NEW
# 240s after case_when - 263s in one chain 254 clusterOn()
# 407s for 100 encoders (5 x 20 encoders)
#### OLD
# 116.3s for 55 encoders (5 datasets x 11 encoders)
# 29.7s for 19 encoders (diamonds) 
# 29.5s for 22 encoders (pci)
# 178s for 76 encoders (4 datasets x 19 encoders) = ~2.3s
# 178s for 88 encoders (4 datasets x 22 encoders) = ~2.3s


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
ENCODING <- "irt-encoding"
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
# ENCODING <- "embed-keras"
# ENCODING <- "integer-encoding"
################################################################################

#####
## DEBUG ERROR no usable vars with smartflow-scales+vtreat-design
##### 
# DATASET.LABEL <- "diamonds"
# DATASET.LABEL <- "ames"
DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "smartflow"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "swbliss"
# DATASET.LABEL <- "swbsun"
# create split objects for 1 dataset
data.prepped <- prep_dataset_original(DATASET.LABEL, TRAIN.SPLIT, CATS.ONLY)  
# 0.06s

# # show factors
data.prepped$training.set %>% select(where(is.ordered)) %>% glimpse
data.prepped$training.set %>% glimpse
data.prepped$training.set.irt %>% glimpse
data.prepped$testing.set.irt
# ENCODING <- "vtreat-design"
# ENCODING <- "scikit-glmm"
# ENCODING <- "scikit-hashing"
ENCODING <- "irt-encoding"

# apply 1 encoder on 1 split object ~ 1.1s
system.time(
  data.encoded <- apply_encoder(data.prepped, ENCODING)  
)

data.encoded$training.set %>% glimpse
data.encoded$training.set %>% summary
data.encoded$features.labels %>% length()

# unlink("data/diamonds.encoded.rda", recursive = TRUE) %>% print

################################################################################
################################################################################
# SCRIBBLE
################################################################################



