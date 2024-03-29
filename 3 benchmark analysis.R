packs <- c(
  "catcoding",
  "tidyverse",
  "magrittr",
  "vtreat",
  "caret",
  "machinelearningtools",
  "doParallel",
  "foreach"
)
sapply(packs, require, character.only = TRUE)
# devtools::install_github("agilebean/catcoding")
# NEW <- TRUE
NEW <- FALSE
################################################################################

################################################################################
# extract top encoders from all encoded datasets
################################################################################
dataset.label.list <- DATASET.LABEL.LIST
# ENCODER.LIST <- c("factor-encoding", "integer-encoding", "embed-keras")

benchmark.filename <- benchmark_filename(STUDY, CV.REPEATS) %>% print
if (NEW) {
  system.time(
    benchmarks.top.encoders <- create_benchmarks_top_encoders(
      STUDY, dataset.label.list, ENCODER.LIST, median_sort = FALSE) %T>% 
      saveRDS(benchmark.filename %T>% print)
  ) # 40s, 10s study1
} else {
  benchmarks.top.encoders <- readRDS(benchmark.filename %T>% print)
}

# all alg
benchmarks.top.encoders

# all in one table
# TRICKY: collapse list into df with data column = dataset (list element name)
benchmarks.top.encoders %>% map_df(~.x, .id = "data")

# details
dataset.label <- "swbliss"
benchmarks.top.encoders[[dataset.label]] %>% 
  group_by(encoder, model) %>% 
  summarize(model, RMSE.median = min(RMSE.median)) %>% 
  arrange(RMSE.median)

# top2 alg
benchmarks.top.encoders %>% 
  imap(~ mutate(.x, dataset = .y)) %>% 
  map_df(~ .x %>% slice_min(order_by = RMSE.median, n = 2)) %>% 
  select(dataset, everything())

# top1 alg worse with ordinal
benchmarks.top.encoders %>% 
  imap(~ mutate(.x, dataset = .y)) %>% 
  map_df(~ .x %>% filter(RMSE.median == min(RMSE.median, na.rm = TRUE))) %>% 
  select(dataset, everything()) %>% 
  as.data.frame()


################################################################################
# visualize top encoders from all encoded datasets
################################################################################

system.time(
  multiple.benchmarks.boxplots <- dataset.label.list %>%
    map(
      ~ visualize_benchmarks_dataset(
        dataset_label = .,
        metric = "RMSE",
        # palette = "Blues",
        # boxfill = "#DCDCDC",
        save = TRUE
      )
    ) %>%
    set_names(dataset.label.list)
  
) # 44s/4 datasets x 20 preprocess options
multiple.benchmarks.boxplots

plot <- multiple.benchmarks.boxplots$swbliss
plot
plot + geom_boxplot(fill = "slategray1")
plot + geom_boxplot(fill = "slategray2")
plot + geom_boxplot(fill = "powderblue")
# not so good
plot + geom_boxplot(fill = "lightsteelblue")
plot + geom_boxplot(fill = "slategray4")
plot + geom_boxplot(fill = "slategray3")
plot + geom_boxplot(fill = "lightslategray")
################################################################################
# DETAILS
# system.time(
#   models.lists.dataset <- get_models_list_dataset(
#     STUDY, DATASET.LABEL, CV.REPEATS) %T>% print  
# ) # 16s diamonds
# models.lists.dataset %>% names

################################################################################
# DEBUG WRONG models.list
################################################################################
# ENCODER <- "scikit-target"
ENCODER <- "vtreat-dummy"
system.time(
  BLIST <- DATASET.LABEL.LIST %>% 
    map(
      function(DATASET_LABEL) {
        
        models.lists.dataset.label <- models_list_label(
          STUDY, DATASET_LABEL, ENCODER, CV.REPEATS)
        
        models.lists.dataset.label %>% print
        system.time(
          models.lists.dataset <- models.lists.dataset.label %>% readRDS(.)
        ) # 4.5s EXP2 > 22.6s EXP4
        
        # CORRECT[[DATASET_LABEL]] %>% 
        #   saveRDS(models.lists.dataset.label)
        models.lists.dataset %>%
          get_model_metrics(.) %>%
          pluck("benchmark.all") %>%
          filter(RMSE.mean == min(RMSE.mean))
      }
    ) %>% 
    set_names(DATASET.LABEL.LIST)
)
BLIST %>% names
BLIST %>% map(~.x)
BLIST %>% map_df(~.x, .id = "dataset")
BLIST[[DATASET.LABEL]]
################################################################################
# SCRIBBLE
################################################################################



