################################################################################
#
# Script:  encoders/vtreat-design.R
# Output:  training.set, testing.set - vtreat::designTreatmentsN
#
################################################################################

# to extract calibration.set from training.set, subset index by calibration.ratio 
# tricky: subset too short: sample(nrow(training.original) * calibration.ratio)
calibration.index <- 1:nrow(training.original) %>% 
  sample(length(.) * calibration.ratio)

# extract calibration.set from training.set by calibration.ratio
calibration.set <- training.original %>% slice(calibration.index)
training.small <- training.original %>% slice(-calibration.index)

treatment.plan <- designTreatmentsN(
  dframe = calibration.set,
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
  c("lev", "clean", "isBad")
}

features.treated <- treatment.plan$scoreFrame %>%
  # code "clean":  a numerical variable with no NAs or NaNs
  # code "lev": an indicator variable for a specific level of the original categorical variable.
  # filter(code %in% c("clean", "lev")) %>%
  filter(code %in% vartypes.selected) %>%
  # vtreat recommendations to filter out useless variables
  filter(recommended == TRUE) %>%
  pull(varName)


training.set.scores <-  vtreat::prepare(
  treatment.plan,
  training.small,
  scale = TRUE,
  varRestriction = features.treated
)

if (!is.null(testing.original)) {
  testing.set.scores <-  vtreat::prepare(
    treatment.plan,
    testing.original,
    scale = TRUE,
    varRestriction = features.treated
  )
}

training.set <- training.set.scores
if (!is.null(testing.original)) {
  testing.set <- testing.set.scores 
}
features.labels <- features.treated
print("######################################################################")
print("TREATMENT: vtreat::designTreatmentsN")
print("######################################################################")
