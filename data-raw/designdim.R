## code to prepare `designdim` dataset goes here

filepath <- system.file(
  "extdata",
  "designdim.items.rds",
  package = "catcoding"
) %>% print

dataset <- readRDS(filepath) %>% as_tibble()

# define target and features
target.label <- "NPS"
features.labels <- dataset %>% 
  select(-target.label) %>% names
features <- dataset %>% select(features.labels)

dataset %<>% 
  mutate(across(features.labels, as.factor)) %>% 
  # convert Likert to ordinal
  mutate(across(where(is.factor), as.ordered))

################################################################################
# IRT encoding
################################################################################

# NEW <- TRUE
NEW <- FALSE

# define labels
itemtype <- "graded"
model.label <- paste0(c("data/designdim.irt", itemtype, "rds"), collapse = ".") %>% print
model.fit.label <- paste0(c("data/designdim.fit", itemtype, "rds"), collapse = ".") %>% print

# define latent trait model
designdim.model <- "Novelty = 1-3
                    Simplicity = 4-6
                    Tool = 7-9
                    Energy = 10-12
                    COV = Novelty*Simplicity*Tool*Energy"

# create cluster
mirtCluster(4, omp_threads = 10, remove = TRUE)

# estimate irt model
system.time(
  model.mirt <- mirt(features, 
                     model = designdim.model,
                     itemtype = "graded", # Bo's tip
                     SE = TRUE,
                     method = "QMCEM" # MCEM doesn't work for SE, EM unstable
                     # method = "MHRM" # in tutorial & paper but slower
  )
) # 160s QMCEM, 146s MHRM

# model.mirt <- readRDS(model.label)

# assess model fit
model.fit <- M2(model.mirt, type = "M2*", calcNULL = TRUE, QMC=TRUE)
model.fit # RMSEA .086, CFI .943

# estimate factor scores
factor.scores <- fscores(model.mirt, method = "EAP", QMC = TRUE) %>% as_tibble()
factor.scores %>% head()

# # MAP scores should be used instead of EAP scores for higher dimensional models.
# fscores.MAP <- fscores(irt, method = "MAP", SE = TRUE, QMC = TRUE) %>% print()


################################################################################
# create data structure
################################################################################
designdim <- list()
designdim$target.label <- target.label
designdim$features.labels <- features.labels
designdim$data <- dataset
designdim$theta.scores <- factor.scores

usethis::use_data(designdim, overwrite = TRUE)
