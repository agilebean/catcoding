################################################################################
#
# Script:       explaining.R
# Goal:         XAI on benchmarks
# Description:  None
#
################################################################################
packs <- c(
  "caret",
  "furrr",
  "pdp",
  "vip",
  "ingredients",
  "iBreakDown",
  "DALEX",
  "machinelearningtools"
)
sapply(packs, require, character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
# unloadNamespace("machinelearningtools")


################################################################################
#
################################################################################

NEW <- TRUE
# NEW <- FALSE

source("plugins/strings.R")
source("plugins/get_models.R")

# CV.REPEATS <- 1
# CV.REPEATS <- 10
CV.REPEATS <- 20

DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"

####################################################
ENCODING <- "scikit-loo"
# ENCODING <- "scikit-Mestimate"
# ENCODING <- "embed-glm"
# ENCODING <- "scikit-james-stein"
# ENCODING <- "scikit-helmert" # reached elapsed time limit
# ENCODING <- "scikit-onehot"
####################################################

# if (NEW) {
#   system.time(
#     # models.lists.dataset <- get_models_list_dataset(DATASET.LABEL, "pca")
#     models.lists.dataset <- get_models_list_dataset(DATASET.LABEL, "none", 20)
#   )  
# } # ~3s
# models.lists.dataset %>% names

models_list_label(DATASET.LABEL, ENCODING)
models_metrics_label()

if (NEW) {
  
  models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING) %T>% print)
  
} else {
  
  models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING) %T>% print)
  models.metrics <- readRDS(models_metrics_label() %T>% print)
}

# prepare variables for explainers
target.label <- models.list$target.label
if (!is.null(models.list$testing.set)) {
  testing.set <- models.list$testing.set %>%
    # tricky: column order in local.obs is important for lime::explain
    select(target.label, everything())
}

models.list %>% names

local.obs <- models.list$ranger$trainingData %>% sample_n(6)
local.obs

models.list.short <- models.list %>%
  list_modify(target.label = NULL, testing.set = NULL)

models.list.short %>% names
models.list.short$xgbTree$trainingData %>% glimpse()
# plan(multicore) # falls back to sequential 34s
# future::plan(multiprocess) # falls back to multisession 30-34s
# plan(multisession)


# xai.list <- get_xai_explanations(
#   models.list.short,
#   get_varImp_DALEX = FALSE,
#   get_plot_varImp_DALEX = FALSE,
#   # get_pdp_plot_DALEX = TRUE,
#   get_plot_attribution_DALEX = TRUE,
#   get_plot_attribution_uncertainty_DALEX = TRUE
#   )

xai.list <- get_xai_explanations(models.list.short)
xai.list %>% names
xai.list$ranger$explainer.DALEX
xai.list$ranger$varImp.DALEX
xai.list$ranger$plot.varImp.DALEX
xai.list$ranger$plot.pdp.DALEX
xai.list$ranger$explainer.LIME %>% class
xai.list$ranger$explanation.LIME
xai.list$ranger$plot.features.LIME
xai.list$ranger$plot.attribution.DALEX
xai.list$ranger$plot.attribution.uncertainty.DALEX
xai.list$ranger$plot.features.LIME
xai.list$ranger$plot.explanations.LIME

system.time(
  xai.list <- models.list.short %>%
    # future_map(function(model_object) {
    map(function(model_object) {
      
      print(paste("*********", model_object$method))
      training.set <- model_object$trainingData %>%
        select(.outcome, everything())
      
      target <- training.set$.outcome
      print(paste("***target"))
      # print(target)
      
      features <- training.set %>% select(-.outcome)
      
      # training.set %>% glimpse
      
      explainer.DALEX <- DALEX::explain(
        model = model_object,
        data = features,
        y = training.set$.outcome >=4,
        label = paste(model_object$method, " model"),
        colorize = TRUE
      )
      print("*** explainer.DALEX")
      
      varImp.DALEX <- explainer.DALEX %>% variable_importance()
      plot.varImp.DALEX <- varImp.DALEX %>% plot
      print("*** plot.varImp.DALEX")
      
      pdp.DALEX <- explainer.DALEX %>% ingredients::partial_dependency()
      plot.pdp.DALEX <- pdp.DALEX %>% plot
      print("*** plot.pdp.DALEX")
      
      plot.attribution.DALEX <- explainer.DALEX %>%
        iBreakDown::local_attributions(local.obs) #%>%
      #plot
      print("*** plot.attribution.DALEX")
      
      # plot.attribution.uncertainty.DALEX <- explainer.DALEX %>%
      #   iBreakDown::break_down_uncertainty(local.obs) %>%
      #   plot
      # print("*** plot.attribution.uncertainty.DALEX")
      
      # lime explanations
      # explainer.LIME <- lime::lime(
      #   # tricky: features not training.set
      #   x = training.set,
      #   model = model_object
      # )
      # print("*** explainer.LIME")
      # print(training.set %>% names)
      
      # explanation.LIME <- lime::explain(
      #   x = local.obs,
      #   explainer = explainer.LIME,
      #   n_features = 5
      # ) %T>% print
      # print("***explanation.LIME")
      
      # plot.features.LIME <- lime::plot_features(
      #   explanation.LIME,
      #   ncol = 2
      # ) + ggtitle(model_object$method)
      # print("***plot.features.LIME")
      #
      # plot.explanations.LIME <- lime::plot_explanations(
      #   explanation.LIME
      # ) + ggtitle(model_object$method)
      # print("***plot.explanations.LIME")
      
      return(
        list(
          explainer.DALEX = explainer.DALEX
          , varImp.DALEX = varImp.DALEX
          , plot.varImp.DALEX = plot.varImp.DALEX
          , pdp.DALEX = pdp.DALEX
          , plot.pdp.DALEX = plot.pdp.DALEX
          # , plot.attribution.DALEX = plot.attribution.DALEX
          # , plot.attribution.uncertainty.DALEX = plot.attribution.uncertainty.DALEX
          # , explainer.LIME = explainer.LIME
          # , explanation.LIME = explanation.LIME
          # , plot.features.LIME = plot.features.LIME
          # , plot.explanations.LIME = plot.explanations.LIME
        )
      )
    })
) # 32s - 31s with multisession
#310s with all

xai.list %>% names


attributions <- xai.list$ranger$explainer.DALEX %>%
  iBreakDown::local_attributions(
    local.obs,
    keep_distributions = TRUE
  )

attributions %>%
  plot() +
  # plot(plot_distributions = TRUE) +
  theme(axis.text = element_text(size = 12)) +
  theme(legend.title=NULL)
# ggsave("output/Fig3.png", dpi = 1000, width = 225, height = 150, units = "mm")

models.list$ranger %>% .$modelInfo
models.list$ranger %>% .$modelInfo %>% names
models.list$ranger %>% varImp()

models.list$ranger %>% varImp() %>% visualize_importance()
xai.list$ranger$plot.varImp.DALEX # useless
xai.list$ranger$plot.pdp.DALEX
# ggsave("output/Fig4.png", dpi = 1000, width = 225, height = 150, units = "mm")

xai.list$ranger$plot.attribution.DALEX
xai.list$ranger$plot.attribution.uncertainty.DALEX
xai.list$ranger$plot.features.LIME
xai.list$ranger$plot.explanations.LIME

models.list$xgbTree %>% varImp() %>% visualize_importance()
xai.list$xgbTree$plot.varImp.DALEX
xai.list$xgbTree$plot.pdp.DALEX
xai.list$xgbTree$plot.attribution.DALEX
xai.list$xgbTree$plot.attribution.uncertainty.DALEX