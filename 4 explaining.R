################################################################################
#
# Script:       explaining.R
# Goal:         XAI on benchmarks
# Description:  None
#
################################################################################
packs <- c(
  "tidyverse",
  "magrittr",
  "caret",
  "furrr",
  "machinelearningtools"
)
devtools::install_github("agilebean/machinelearningtools", force = TRUE)
unloadNamespace("catcoding")
unloadNamespace("machinelearningtools")
sapply(packs, require, character.only = TRUE)
# options(future.fork.multithreading.enable = FALSE)
# "RhpcBLASctl"
################################################################################
#
################################################################################

NEW <- TRUE
# NEW <- FALSE

# dataset.label.list <- DATASET.LABEL.LIST
# dataset.label.list <- "timex"
dataset.label.list <- "swbsun"

xai.prefix <- paste0(
  output_dir(STUDY, "xai", paste0("cv", CV.REPEATS)),
  "models.explanation"
) %>% print

extract_xai_dataset <- function(
  dataset_label, study, encoder_list, cv_repeats) {
  
  encoder_list %>% 
    map(
      function(encoder) {
        
        models.list.label <- models_list_label(
          study, dataset_label, encoder, cv_repeats) %T>% print
        
        models.list <- readRDS(models.list.label) %>% 
          list_modify(target.label = NULL)
        
        get_xai_explanations(
          models.list,
          # save_path = xai.prefix,
          suffix = paste0(dataset_label, ".", encoder)
        )
      }
    ) %>% set_names(encoder_list)
}

# dataset.label <- "timex"
dataset.label <- "swbsun"
system.time(
  xai.list <- extract_xai_dataset(
    dataset.label, STUDY, ENCODER.LIST.XAI, CV.REPEATS)
) # 480s/3 encoders 1137s/study2

filename <- output_filename(xai.prefix, dataset.label) %>% print
if (NEW) {
  system.time(
    xai.list %>% saveRDS(filename)  
  ) # 13s, 22s/study2
} else {
  system.time(
    xai.list <- readRDS(filename)  
  ) # 5s
}


xai.list$`scikit-ordinal`$lm$DALEX.feature.importance.plot
xai.list$`scikit-ordinal`$gbm$DALEX.feature.importance.plot
xai.list$`scikit-ordinal`$svmRadial$DALEX.feature.importance.plot

xai.list$`scikit-onehot`$gbm$DALEX.distribution.plot
xai.list$`scikit-onehot`$gbm$DALEX.attribution.plot
xai.list$`scikit-onehot`$gbm$DALEX.pdp.plot %>% 
  ggsave(plot = ., filename = "study2/pdp.onehot.png", 
         width = 20, height = 15)


system.time(
  
  xai.list.all <- dataset.label.list %>% 
    map(
      ~ extract_xai_dataset(
        .x, STUDY, ENCODER.LIST, CV.REPEATS
      )
    ) %>% set_names(dataset.label.list)
)
# 1786s 


xai.list.all %>% 
  imap(function(xai_dataset, xai_dataset_label) {
    xai_dataset %>% 
    imap(function(encoder, encoder_label) {
      models.list.label <- models_list_label(
        STUDY, xai_dataset_label, encoder, CV.REPEATS) %T>% print
    })
  })


# RhpcBLASctl::blas_set_num_threads(8)
DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
# ENCODING <- "scikit-loo"
ENCODING <- "scikit-ordinal"

models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING, CV.REPEATS) %T>% print)
models.list.short <- models.list %>% list_modify(target.label = NULL)
models.list.short %>% names
model_object <- models.list.short$gbm

system.time(
  xai <- get_xai_explanations(
    models.list.short,
    save_path = xai.prefix,
    suffix = paste0(DATASET.LABEL, ".", ENCODING))
)

models_explanation_label(STUDY, CV.REPEATS, DATASET.LABEL, ENCODING, ) 

paste(
  c(save_path = xai.path, "DALEX.feature.importance.plot", 
    model_object$method,
    suffix = paste0(DATASET.LABEL, ".", ENCODING), "png"),
  collapse = ".")
# only if save_label
# ERROR:
# The process has forked and you cannot use this CoreFoundation functionality 
# safely. You MUST exec().
  
xai$gbm$DALEX.feature.importance.plot
xai$gbm$DALEX.residual.plot
xai$gbm$DALEX.pdp.plot
xai$gbm$DALEX.attribution.plot
xai$gbm$DALEX.attribution.uncertainty.plot
xai$gbm$DALEX.distribution.plot
xai$gbm$DALEX.shapley.plot
xai$gbm$LIME.features.plot
xai$gbm$LIME.explanations.plot
