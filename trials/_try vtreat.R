packs <- c(
  "tidyverse",
  "magrittr",
  "vtreat",
  "rpart.plot",
  "caret"
)
sapply(packs, require, character.only = TRUE)


data(ptitanic)
dataset <- ptitanic %>% as_tibble()

dataset %>% summary

target.label <- "survived"
target <- dataset[[target.label]]
features <- dataset %>% select(-target.label)
features.labels <- features %>% names

train.index <- createDataPartition(target, p = 0.8, list = FALSE)
training.set <- dataset[train.index, ]
testing.set <- dataset[-train.index, ]

?vtreat::prepare
?designTreatmentsC()

# clus <- parallel::makeCluster(16)

treatment.plan <- designTreatmentsC(
  dframe = training.set,
  varlist = features.labels,
  outcomename = target.label,
  outcometarget = "survived"
  # parallelCluster = clus
  )

# ggplot(data = treatment.plan$scoreFrame,aes(x=sig)) +
#   geom_density(adjust=0.2) +
#   geom_vline(xintercept=kddSig) +
#   ggtitle("distribution of variable significances")

scoreFrame <- treatment.plan$scoreFrame %>%
  select(varName, origName, code) %>% print

newvars <- treatment.plan$scoreFrame %>%
  # code "clean":  a numerical variable with no NAs or NaNs
  # code "lev": an indicator variable for a specific level of the original categorical variable.
  filter(code %in% c("clean", "lev")) %>%
  pull(varName) %T>% print


training.set.scores <-  prepare(
  treatment.plan,
  training.set,
  scale = TRUE,
  varRestriction = newvars
)

testing.set.scores <-  prepare(
  treatment.plan,
  testing.set,
  scale = TRUE,
  varRestriction = newvars
)

training.set.scores
testing.set.scores

model.rf <- randomForest::randomForest(
  x = training.set.scores,
  y = training.set.scores[[target.label]]
  )
model.rf

model.rf %>% predict(testing.set.scores) %>%
  confusionMatrix(testing.set.scores[[target.label]])



################################################################################
# correct practice
# https://winvector.github.io/vtreat/articles/vtreatOverfit.html#correct-practice-12-use-different-data-to-treat-and-train
# more reading
# https://github.com/WinVector/vtreat/blob/master/Examples/Regression/Regression.md
################################################################################
system.time(
  training.set.cross <- vtreat::mkCrossFrameNExperiment(
    dframe = training.set, 
    varlist = features.labels,
    outcomename = target.label,
    # parallelCluster = clus, # % faster
    rareCount = 0,  # Note set this to something larger, like 5
    rareSig = c()
  )  
)

unpack[
  transform = treatments,
  training.set.cross = crossFrame
  ] <- vtreat::mkCrossFrameNExperiment(
    dframe = training.set, 
    varlist = features.labels,
    outcomename = target.label,
    # parallelCluster = clus, # % faster
    rareCount = 0,  # Note set this to something larger, like 5
    rareSig = c()
  )  


treatments <- training.set.cross$treatments %T>% print

training.set.scores <- treatments$scoreFrame %T>% print

training.set.treated <- training.set.cross$crossFrame %T>% print

model.glm <- glm(
  price ~ cut_catP,
  data = training.set.treated
)
summary(model.glm)

training.set.treated$predictions <- predict(
  model.glm,
  newdata = training.set.treated, 
  type = 'response'
)

testing.set.treated <- vtreat::prepare(
  treatments,
  testing.set,
  pruneSig=c()
)

testing.set.treated$predictions <- predict(
  model.glm,
  newdata = testing.set.treated, 
  type = 'response'
)

plotRes(training.set.treated,'predictions','y','model.glm on train set')
plotRes(testing.set.treated,'predictions','y','model.glm on test set')


################################################################################





