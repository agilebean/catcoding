################################################################################
#
# Script:  encoders/vtreat-dummy.R
# Output:  training.set, testing.set - dummy-encoded
#
################################################################################

treatment.plan <- designTreatmentsZ(
  dframe = training.original,
  varlist = features.labels,
  minFraction = 0
  # parallelCluster = clus, # 5% faster
)

# restrict to common variable types
vartypes.selected <- if (CATS.ONLY) {
  
  print("feature selection: CATS ONLY")
  c("lev") 
  
} else { 
  
  print("feature selection: ALL")
  c("lev", "clean", "isBAD")
}

# see vignette('vtreatVariableTypes', package = 'vtreat') for details
features.treated <- treatment.plan$scoreFrame %>% 
  filter(code %in% vartypes.selected) %>% 
  pull(varName)

training.set.scores <- vtreat::prepare(
  treatment.plan,
  training.original,
  scale = TRUE,
  varRestriction = features.treated
)

if (!is.null(testing.original)) {
  
  testing.set.scores <- vtreat::prepare(
    treatment.plan,
    testing.original,
    scale = TRUE,
    varRestriction = features.treated
  ) 
}

# create output
training.set <- training.set.scores
testing.set <- if(is.null(testing.original)) { NULL } else { testing.set.scores }
features.labels <- features.treated
print("######################################################################")
print("TREATMENT: vtreat::designTreatmentsZ")
print("######################################################################")