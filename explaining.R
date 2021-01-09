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
  "machinelearningtools",
  "furrr"
)
sapply(packs, require, character.only = TRUE)
# devtools::install_github("agilebean/machinelearningtools", force = TRUE)
# unloadNamespace("machinelearningtools")


################################################################################
#
################################################################################

NEW <- TRUE
# NEW <- FALSE

source("plugins/labels.R")
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

# ENCODING <- "scikit-ordinal"
# ENCODING <- "scikit-onehot"
####################################################

# if (NEW) {
#   system.time(
#     # models.lists.dataset <- get_models_list_dataset(DATASET.LABEL, "pca")
#     models.lists.dataset <- get_models_list_dataset(DATASET.LABEL, "none", 20)
#   )  
# } # ~3s
# models.lists.dataset %>% names

# models_list_label(DATASET.LABEL, ENCODING)
# models_metrics_label()
# 
# if (NEW) {
#   
#   models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING) %T>% print)
#   
# } else {
#   
#   models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING) %T>% print)
#   models.metrics <- readRDS(models_metrics_label() %T>% print)
# }
# 
# # prepare variables for explainers
# target.label <- models.list$target.label
# if (!is.null(models.list$testing.set)) {
#   testing.set <- models.list$testing.set %>%
#     # tricky: column order in local.obs is important for lime::explain
#     select(target.label, everything())
# }
# 
# models.list %>% names
# 
# local.obs <- models.list$ranger$trainingData %>% sample_n(6)
# local.obs
# 
# models.list.short <- models.list %>%
#   list_modify(target.label = NULL, testing.set = NULL)
# 
# models.list.short %>% names
# models.list.short$xgbTree$trainingData %>% glimpse()


# xai.list <- get_xai_explanations(models.list.short)
plan(multiprocess)

system.time(
  
  DATASET.LABEL.LIST %>% 
    future_map(
      function(DATASET_LABEL) {
        ENCODER.LIST.study3 %>% 
          future_map(
            function(ENCODER) {
              
              models.list <- readRDS(models_list_label(DATASET_LABEL, ENCODER) %T>% print)
              models.list.short <- models.list %>%
                list_modify(target.label = NULL, testing.set = NULL)
              
              local.no <- 8
              local.obs <- models.list$ranger$trainingData %>% sample_n(local.no)
              
              get_xai_explanations(
                models.list.short,
                cutoff_greater = 4,
                local_no = local.no,
                local_obs = local.obs,
                save_path = "figures/study3",
                suffix = paste0(DATASET_LABEL, ".", ENCODER),
                # get_varImp_DALEX = TRUE,
                # get_plot_varImp_DALEX = TRUE,
                get_pdp_plot_DALEX = TRUE,
                get_plot_attribution_DALEX = TRUE,
                # get_plot_attribution_uncertainty_DALEX = TRUE,
                # get_explainer_LIME = TRUE,
                # get_explanation_LIME = TRUE,
                # get_plot_features_LIME = TRUE,
                get_plot_explanations_LIME = TRUE
              )
            },
            .progress = TRUE
          )
      },
      .progress = TRUE
    )
)

# 2429s = 40.5m

DATASET.LABEL <- "designdim"
DATASET.LABEL <- "ames"
ENCODING <- "scikit-loo"
models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING) %T>% print)
models.list.short <- models.list %>%
  list_modify(target.label = NULL, testing.set = NULL)

model_object <- models.list.short$ranger

training.set <- model_object$trainingData %>%
  select(.outcome, everything())

target <- training.set$.outcome

features <- training.set %>% select(-.outcome)

local.obs <-  training.set %>% sample_n(6)

explainer.LIME <- lime::lime(
  # tricky: features not training.set
  x = features,
  # x = training.set,
  model = model_object
)

cutoff_greater <- 4
explanation.LIME <- 
  lime::explain(
    x = local.obs %>% select(-.outcome),
    explainer = explainer.LIME,
    n_features = 5
  ) %T>% print

lime::plot_explanations(
  explanation.LIME
)


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

save_path <- "figures/study3-"
object <- xai.list$ranger$varImp.DALEX %>% plot %T>% 
  { if (!is.null(save_path)) ggsave(
    filename = paste0(
      save_path, "plot.varImp.DALEX.", xai.list$ranger$method, ".png")
  )}
object
 # 32s - 31s with multisession
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


