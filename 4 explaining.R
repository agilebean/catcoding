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
sapply(packs, require, character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
# unloadNamespace("machinelearningtools")
# options(future.fork.multithreading.enable = FALSE)
# "RhpcBLASctl"
################################################################################
#
################################################################################

NEW <- TRUE
# NEW <- FALSE

# dataset.label.list <- DATASET.LABEL.LIST
dataset.label.list <- "timex"
# encoder.list <- ENCODER.LIST.study1
encoder.list <- ENCODER.LIST.test

xai.prefix <- paste0(
  output_dir(STUDY, "xai", paste0("cv", CV.REPEATS)),
  "models.explanation"
) %>% print

system.time(
  
  xai.list <- dataset.label.list %>% 
    map(
      function(DATASET_LABEL) {
        encoder.list %>% 
          map(
            function(ENCODER) {
              
              models.list.label <- models_list_label(
                STUDY, DATASET_LABEL, ENCODER, CV.REPEATS) %T>% print
              
              models.list <- readRDS(models.list.label) %>% 
                list_modify(target.label = NULL)
              
              get_xai_explanations(
                models.list,
                # save_path = xai.prefix,
                suffix = paste0(DATASET_LABEL, ".", ENCODER)
              )
            }
          ) %>% set_names(encoder.list)
      }
    ) %>% set_names(dataset.label.list)
)
# 1786s 

filename <- output_filename(xai.prefix, "timex") %>% print
if (NEW) {
  system.time(
    xai.list %>% saveRDS(filename)  
  ) # 33s
} else {
  system.time(
    xai.list <- readRDS(filename)  
  ) # 12s
}


xai.list %>% 
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

get_xai_explanations <- function(
  models_list,
  seed = 171,
  cutoff_greater = 0,
  n_features_lime = 5,
  local_obs = NULL,
  local_min_cutoff = 0.95,
  local_no = 6,
  random_case = NULL,
  save_path = NULL,
  suffix = NULL,
  width = 6, height = 6,
  # get_DALEX_explainer = TRUE,
  get_DALEX_residual_plot = TRUE,
  no_permutations = 50,
  get_DALEX_feature_importance = TRUE,
  get_DALEX_feature_importance_plot = TRUE,
  get_DALEX_pdp_plot = TRUE,
  get_DALEX_attribution_plot = TRUE,
  get_DALEX_attribution_text = TRUE,
  get_DALEX_attribution_uncertainty_plot = TRUE,
  get_DALEX_shapley_plot = TRUE,
  get_LIME_explainer = FALSE,
  get_LIME_explanations = FALSE,
  get_LIME_explanations_plot = FALSE,
  get_LIME_features_plot = FALSE
) {
  
  save_path = xai.prefix
  
  seed = 171
  cutoff_greater = 0
  n_features_lime = 5
  local_obs = NULL
  local_min_cutoff = 0.95
  local_no = 6
  random_case = NULL
  save_path = NULL
  suffix = NULL
  width = 6 
  height = 6
  
  get_DALEX_residual_plot = TRUE
  no_permutations = 50
  get_DALEX_feature_importance = TRUE
  get_DALEX_feature_importance_plot = TRUE
  get_DALEX_pdp_plot = TRUE
  get_DALEX_attribution_plot = TRUE
  get_DALEX_attribution_text = TRUE
  get_DALEX_attribution_uncertainty_plot = TRUE
  get_DALEX_shapley_plot = TRUE
  
  
  require(ggplot2) # ggsave
  require(dplyr)
  require(furrr)
  require(DALEX)
  require(iBreakDown)
  require(ingredients)
  if (get_LIME_explainer) require(lime)
  
  models_list <- models.list.short
  
  model_object <- models_list$gbm
  
  options(parallelly.fork.enable = TRUE)
  plan(multicore, workers = 8)
  
  xai.list <- models_list %>%
    
    future_map(function(model_object) {
      
      print(paste("*********", model_object$method))
      training.set <- model_object$trainingData %>%
        select(.outcome, everything())
      
      target <- training.set$.outcome
      print(paste("***target"))
      
      features <- training.set %>% select(-.outcome)
      
      # local observations for prediction
      local.obs <- if (!is.null(local_obs)) {
        local_obs
      } else {
        training.set %>%
          filter(
            .outcome >=
              get_percentile_from_model(model_object, local_min_cutoff)) %>%
          sample_n(local_no)
      }
      
      random.case <- if (!is.null(random_case)) {
        random_case
      } else {
        local.obs %>% sample_n(1)
      }
      
      print("*** DALEX.explainer")
      DALEX.explainer <- DALEX::explain(
        model = model_object,
        data = features,
        y = training.set$.outcome >= cutoff_greater,
        label = paste(model_object$method, " model"),
        colorize = TRUE
      )
      
      # for residual plots by plot(geom = "histogram")
      DALEX.performance <- DALEX.explainer %>%
        DALEX::model_performance()
      
      DALEX.residual.plot <- if (get_DALEX_residual_plot) {
        
        DALEX.performance %>% plot(geom = "histogram")
        
      } else {
        NULL
      }
      
      DALEX.feature.importance <- if (
        get_DALEX_feature_importance & !is.null(DALEX.explainer)) {
        
        print("*** DALEX.permutation.feature.importance")
        
        DALEX.explainer %>%
          model_parts(
            B = no_permutations,
            type = "ratio"
          )
        
      } else {
        NULL
      }
      
      DALEX.feature.importance.plot <- if (
        get_DALEX_feature_importance_plot &
        !is.null(DALEX.feature.importance)) {
        
        print("*** DALEX.feature.importance.plot")
        
        DALEX.feature.importance %>%
          plot(
            bar_width = 20 / log(ncol(features))
            , show_boxplots = FALSE
            , title = "Permutation Feature importance"
            , subtitle = ""
          ) +
          # reduce space to axis
          scale_y_continuous(expand = expansion()) %T>%
          {
            if (!is.null(save_path)) {
              print("AAAA")
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "DALEX.feature.importance.plot", model_object$method,
                    suffix, "png"),
                  collapse = ".")
              )
            }
          }
        
      } else {
        NULL
      }
      
      DALEX.pdp.plot <- if (get_DALEX_pdp_plot & !is.null(DALEX.explainer)) {
        
        print("*** DALEX.pdp.plot")
        
        DALEX.pdp <- DALEX.explainer %>% ingredients::partial_dependency()
        
        DALEX.pdp %>% plot %T>%
          {
            if (!is.null(save_path)) {
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "plot.pdp.DALEX", model_object$method,
                    suffix, "png"),
                  collapse = ".")
              )
            }
          }
      } else {
        NULL
      }
      
      DALEX.attribution <- DALEX.explainer %>%
        iBreakDown::local_attributions(
          local.obs,
          keep_distributions = TRUE
        )
      print("*** DALEX.attribution")
      
      DALEX.attribution.text <- if(get_DALEX_attribution_text) {
        
        print("*** DALEX.attribution.text")
        
        DALEX.attribution %>%
          iBreakDown::describe()
      } else {
        NULL
      }
      
      DALEX.attribution.plot <- if (get_DALEX_attribution_plot &
                                    !is.null(DALEX.explainer)) {
        
        print("*** DALEX.attribution.plot")
        
        DALEX.explainer %>%
          iBreakDown::local_attributions(
            local.obs,
            keep_distributions = TRUE
          ) %>%
          plot(
            shift_contributions = 0.03
          ) %T>%
          {
            if (!is.null(save_path)) {
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "DALEX.attribution.plot", model_object$method,
                    suffix, "png"),
                  collapse = ".")
              )
            }
          }
      } else {
        NULL
      }
      
      DALEX.attribution.uncertainty.plot <-
        if (get_DALEX_attribution_uncertainty_plot &
            !is.null(DALEX.explainer)) {
          
          print("*** DALEX.attribution.uncertainty.plot")
          DALEX.explainer %>%
            iBreakDown::break_down_uncertainty(local.obs) %>%
            plot %T>%
            {
              if (!is.null(save_path)) {
                ggsave(
                  width = width, height = height,
                  filename = paste(
                    c(save_path, "DALEX.attribution.uncertainty.plot",
                      model_object$method, suffix, "png"),
                    collapse = ".")
                )
              }
            }
        } else {
          NULL
        }
      
      
      DALEX.distribution.plot <- DALEX.attribution %>%
        plot(plot_distributions = TRUE)
      
      print("*** DALEX.distribution.plot")
      
      
      DALEX.shapley.plot <- if (
        get_DALEX_shapley_plot &
        !is.null(DALEX.explainer) & !is.null(random_case)) {
        
        print("*** DALEX.shapley.plot")
        
        DALEX.explainer %>%
          iBreakDown::shap(random_case,
                           B = no_permutations) %>%
          plot()
      }
      
      
      LIME.explainer <- if (get_LIME_explainer) {
        
        print("*** LIME.explainer")
        lime::lime(
          # tricky: features not training.set
          x = features,
          model = model_object
        )
      } else {
        NULL
      }
      
      LIME.explanations <- if (
        get_LIME_explanations & !is.null(LIME.explainer)) {
        
        print("***LIME.explanations")
        lime::explain(
          # tricky: features not training.set
          x = local.obs %>% select(-.outcome),
          explainer = LIME.explainer,
          n_features = n_features_lime
        ) %T>% print
      } else {
        NULL
      }
      
      LIME.explanations.plot <- if (
        get_LIME_explanations_plot & !is.null(LIME.explanations)) {
        
        print("***LIME.explanations.plot")
        lime::plot_explanations(
          LIME.explanations
        ) + ggtitle(model_object$method)  %T>%
          {
            if (!is.null(save_path)) {
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "LIME.explanations.plot",
                    model_object$method, suffix, "png"),
                  collapse = ".")
              )
            }
          }
        
      } else {
        NULL
      }
      
      LIME.features.plot <- if (
        get_LIME_features_plot & !is.null(LIME.explainer)) {
        
        print("***LIME.features.plot")
        lime::plot_features(
          LIME.explanations,
          ncol = 2
        ) + ggtitle(model_object$method)  %T>%
          {
            if (!is.null(save_path)) {
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "LIME.features.plot",
                    model_object$method, suffix, "png"),
                  collapse = ".")
              )
            }
          }
      } else {
        NULL
      }
      
      return(
        list(
          DALEX.explainer = DALEX.explainer
          , DALEX.performance = DALEX.performance
          , DALEX.feature.importance = DALEX.feature.importance
          , DALEX.feature.importance.plot = DALEX.feature.importance.plot
          , DALEX.residual.plot = DALEX.residual.plot
          , DALEX.pdp.plot = DALEX.pdp.plot
          , DALEX.attribution.text = DALEX.attribution.text
          , DALEX.attribution.plot = DALEX.attribution.plot
          , DALEX.attribution.uncertainty.plot = DALEX.attribution.uncertainty.plot
          , DALEX.distribution.plot = DALEX.distribution.plot
          , DALEX.shapley.plot = DALEX.shapley.plot
          , LIME.explainer = LIME.explainer
          , LIME.explanations = LIME.explanations
          , LIME.explanations.plot = LIME.explanations.plot
          , LIME.features.plot = LIME.features.plot
        )
      )
    },
    .options = furrr_options(
      seed = seed
      , packages = c("DALEX", "iBreakDown", "ingredients", "lime")
    ))
}

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
