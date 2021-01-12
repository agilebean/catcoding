################################################################################
#
# Script:  analysis.R
# Purpose: generate plots for publication
#
################################################################################
packs <- c(
  "tidyverse",
  "magrittr",
  "vtreat",
  "caret",
  "machinelearningtools",
  "doParallel",
  "foreach",
  "RColorBrewer"
)
# devtools::install_github("agilebean/machinelearningtools")
# unloadNamespace("machinelearningtools")
sapply(packs, require, character.only = TRUE)
# NEW <- TRUE
NEW <- FALSE

source("plugins/labels.R")
source("plugins/get_models.R")


# DATASET.LABEL <- "ames"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"
# DATASET.LABEL <- "smartflow"
DATASET.LABEL <- "pci"
# CV.REPEATS <- ""
CV.REPEATS <- 2

ENCODING <- "scikit-loo"
# ENCODING <- "factor-encoding"

PREPROCESS.OPTION <- "none"

# models.list <- readRDS(models_list_label(DATASET.LABEL, ENCODING))

####################################################
# models.lists.dataset.labels <- dir("models/") %>% 
#   {
#     grepl("pca", .) &
#       startsWith(., paste0("models.list.", DATASET.LABEL))
#   } %>% 
#   dir("models/")[.] %T>% print
# 
# system.time(
#   models.lists.dataset <- models.lists.dataset.labels %>% 
#     map(~paste0("models/", .x) %>% readRDS) %>% 
#     set_names(
#       gsub("models\\.list\\.(.+)\\.(.+)\\.(.*)\\.pca\\.(.*)\\.rds", "\\3", 
#            models.lists.dataset.labels)
#     )
# ) # 4.5s 24.2s 21.7s 2.13s


# visualize final benchmarking
visualize_sampling_models_list <- function(
  models_lists_dataset, metric, 
  palette = "Set1", boxfill = "#DCDCDC", dataset_label) {
  
  # models.lists.dataset <- get_models_list_dataset(
  #   DATASET.LABEL, preprocess_option, cv_repeats)
  
  # return the sampling folds for the best algorithm
  sampling.folds <- models_lists_dataset %>% 
    # imap(~ mutate(.x, name = .y))
    map(~ get_sampling_models_list(.x, metric)) %>% 
    # tricky tricky: concatenate the sampling folds for all best algorithms
    imap_dfc(~ set_names(.x, .y)) %T>% print
  
  
  sampling.folds.ordered <- sampling.folds %>% 
    pivot_longer(
      cols = everything(),
      names_to = "encoder",
      values_to = metric
    ) %>% 
    arrange(RMSE) %T>% print
  
  color.codes <- RColorBrewer::brewer.pal(8, palette)[-c(1:2)]
  color.values <- colorRampPalette(color.codes)(ncol(sampling.folds))
  
  plot.sampling.folds.ordered <- sampling.folds.ordered %>% 
    ggplot(aes(x = reorder(encoder, desc(RMSE)), y = RMSE)) +
    coord_flip() +
    # geom_boxplot(fill = "#778899") + # lightslategrey
    geom_boxplot(fill = boxfill) +
    # geom_point(aes(color = encoder), alpha = 0.25, size = 1.5) +
    # geom_point(aes(color = encoder), alpha = 1, size = 1.5, shape = 1) +
    geom_jitter(aes(color = encoder), alpha = 1, size = 0.5, shape = 1) +
    # scale_color_brewer(guide = "none", palette = palette) +
    scale_color_manual(guide = "none", values = color.values) +
    labs(
      title = paste("Dataset:", dataset_label),
      x = "model",
      y = "RMSE"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    )
  
  plot.sampling.folds.ordered
}


########################################################################
# Study 1
########################################################################
DATASET.LABEL <- "pci"
# DATASET.LABEL <- "designdim"
# DATASET.LABEL <- "timex"

system.time(
  # models.lists.dataset <- get_models_list_dataset(DATASET.LABEL, "pca")
  models.lists.dataset <- get_models_list_dataset(
    DATASET.LABEL, 
    PREPROCESS.OPTION, 
    CV.REPEATS
  )
) # ~10.2s/11 encoders  

models.lists.dataset %>% names

# return the sampling folds for the best algorithm
system.time(
  sampling.folds <- models.lists.dataset %>% 
    # imap(~ mutate(.x, name = .y))
    map(~ get_sampling_models_list(.x, "RMSE")) %>% 
    # tricky tricky: concatenate the sampling folds for all best algorithms
    imap_dfc(~ set_names(.x, .y)) %>% 
    as_tibble() %T>% print  
) # 1.8s

mest.rf <- models.lis %>% ts.dataset$`scikit-Mestimate`$rf
glm.rf <- models.lists.dataset$`embed-glm`$rf
loo.rf <- models.lists.dataset$`scikit-loo`$rf

glm.rf %>% names
glm.rf$resample
loo.rf$resample

mest.data <- mest.rf$trainingData %>% as_tibble() 
glm.data <- glm.rf$trainingData %>% as_tibble() 
loo.data <- loo.rf$trainingData %>% as_tibble() 


mest.data$job_type %>% table 
mest.data$job_type %>% densityplot()

glm.data$job_type %>% table
glm.data$job_type %>% densityplot()

loo.data$job_type %>% table %>% as.data.frame 
loo.data$job_type %>% densityplot()
all.equal(glm.data, loo.data)

mest.short <- mest.data %>% select(.outcome, company, job_type, passed)
mest.short %>% cor
glm.short <- glm.data %>% select(.outcome, company, job_type, passed)
glm.short %>% cor
loo.short <- loo.data %>% select(.outcome, company, job_type, passed)
loo.short %>% cor

compare <- cbind(jobtype.loo = loo.short$job_type, 
                 jobtype.mestimate = glm.short$job_type, 
                 # job3 = mest.short$job_type,
                 groundtruth= loo.short$.outcome) %>% 
  as_tibble() %>% 
  # mutate(delta = abs(out1-out2)) %>% filter(delta > 0.00000001)
  mutate(delta12 = jobtype.loo-jobtype.mestimate, 
         # delta13 = job1-job3
  ) %>% 
  # filter(delta < -0.5 | delta > 0.5) %>% 
  print

plot(compare$job1, compare$out)
plot(compare$job2, compare$out)
plot(compare$job3, compare$out)
plot(compare$delta12, compare$groundtruth)
plot(compare$delta13, compare$out)

data.list <- 
  list(glm.short, loo.short) %>% 
  # list(glm.data, loo.data) %>% 
  set_names(c("glm.encoded", "loo.encoded"))

TRY.FIRST <- NULL
algorithm.list <- c("lm","rf")
preprocess_string <- c("center", "scale")
training.configuration <- trainControl(method = "repeatedcv",
                                       number = 5, repeats = 1)
models.list.list <- data.list %>% 
  map(
    ~ benchmark_algorithms(
      target_label = ".outcome",
      features_labels = .x %>% select(-.outcome) %>% names,
      training_set = .x,
      testing_set = NULL,
      training_configuration = training.configuration,
      algorithm_list = algorithm.list,
      cv_repeats = CV.REPEATS,
      try_first = TRY.FIRST,
      # models_list_name = models_list_label(DATASET_LABEL, ENCODER),
      models_list_name = NULL,
      preprocess_configuration = preprocess_string,
      # push = TRUE,
      push = FALSE,
      beep = FALSE    
    ))

glm.metrics <- models.list.list$glm.encoded %>% get_model_metrics()
loo.metrics <- models.list.list$loo.encoded %>% get_model_metrics()


glm.metrics$metric1.training
loo.metrics$metric1.training

glm.metrics$resamples.values
loo.metrics$resamples.values

glm.rf <- models.list.list$glm.encoded$rf
loo.rf <- models.list.list$loo.encoded$rf

glm.rf$finalModel$importance
loo.rf$finalModel$importance

glm.rf$trainingData %>% select(.outcome, everything()) %>% cor %>% 
  as.data.frame() %>% select(.outcome, work_commitment, teamwork)
loo.rf$trainingData %>% select(.outcome, everything()) %>% cor %>% 
  as.data.frame() %>% select(.outcome, work_commitment, teamwork)

glm.rf$finalModel %>% visualize_importance()
loo.rf$finalModel %>% visualize_importance() # wrong: job_type, passed

glm.rf$pred %>% as_tibble() %>% mutate(residual = sqrt((pred-obs)^2)) %>% .$residual  %>% sum
loo.rf$pred %>% as_tibble() %>% mutate(residual = sqrt((pred-obs)^2)) %>% .$residual  %>% sum

sampling.folds %>% select(`scikit-loo`, `scikit-onehot`, `scikit-helmert`)

visualize_sampling_models_list(
  models.lists.dataset, "RMSE", 
  palette = "Blues", boxfill = "#778899", DATASET.LABEL)

ggsave(
  dpi = 300, width = 6, height = 9,
  paste0("figures/study1-", DATASET.LABEL, ".png") %T>% print)
