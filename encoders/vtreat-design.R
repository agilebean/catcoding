################################################################################
#
# Script:  _encoding vtreat-design.R
# Output:  training.set, testing.set - vtreat::designTreatmentsN
#
################################################################################

# to extract config.set from training.set
training.original <- training.set

# subset index by config.ratio 
# tricky: subset too short: sample(nrow(training.original) * config.ratio)
config.index <- 1:nrow(training.original) %>% 
  sample(length(.) * config.ratio)

# extract config.set from training.set by config.ratio
config.set <- training.original %>% slice(config.index)
training.set <- training.original %>% slice(-config.index)

treatment.plan <- designTreatmentsN(
  dframe = config.set,
  varlist = features.labels,
  # parallelCluster = clus, # 5% faster
  outcomename = target.label
)

scoreFrame <- treatment.plan$scoreFrame %>%
  select(varName, origName, code) %T>% print

vartypes.selected <- if (CATS.ONLY) {
  
  print("feature selection: CATS ONLY")
  c("lev") 
  
} else { 
  
  print("feature selection: ALL")
  c("lev", "clean")
}

features.treated <- treatment.plan$scoreFrame %>%
  # code "clean":  a numerical variable with no NAs or NaNs
  # code "lev": an indicator variable for a specific level of the original categorical variable.
  # filter(code %in% c("clean", "lev")) %>%
  filter(code %in% vartypes.selected) %>%
  pull(varName)

training.set.scores <-  vtreat::prepare(
  treatment.plan,
  training.set,
  scale = TRUE,
  varRestriction = features.treated
)

if (!is.null(testing.set)) {
  testing.set.scores <-  vtreat::prepare(
    treatment.plan,
    testing.set,
    scale = TRUE,
    varRestriction = features.treated
  )
}

training.set <- training.set.scores
if (!is.null(testing.set)) {
  testing.set <- testing.set.scores 
}
features.labels <- features.treated
print("TREATMENT: vtreat::designTreatmentsN")
