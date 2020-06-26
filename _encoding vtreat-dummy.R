################################################################################
#
# Script:  _encoding vtreat-dummy.R
# Output:  training.set, testing.set - dummy-encoded
#
################################################################################

scoreFrame <- treatment.plan$scoreFrame %>%
  select(varName, origName, code) %T>% print

vartypes.selected <- if (CATS.ONLY) {
  
  print("feature selection: CATS ONLY")
  c("lev") 
  
} else { 
  
  print("feature selection: ALL")
  c("lev", "clean")
}
# get treated training.set
treatments <- training.set.cross$treatments %T>% print
training.set.treated <- training.set.cross$crossFrame %>% 
  as_tibble() %T>% print


treatment.plan <- vtreat::designTreatmentsZ(
  training.set, features.labels,
  minFraction = 0
)
# restrict to common variable types
vartypes.selected <- c("lev", "clean", "isBAD")
# see vignette('vtreatVariableTypes', package = 'vtreat') for details
features.treated <- treatment.plan$scoreFrame %>% 
  filter(code %in% vartypes.selected) %>% 
  pull(varName)

training.set.scores <-  prepare(
  treatment.plan,
  training.set,
  scale = TRUE,
  varRestriction = features.treated
)

if (!is.null(testing.set)) {
  
  testing.set.scores <-  prepare(
    treatment.plan,
    testing.set,
    scale = TRUE,
    varRestriction = features.treated
  ) 
}

# create output
training.set <- training.set.scores
testing.set <- if(is.null(testing.set)) { NULL } else { testing.set.scores }
features.labels <- features.treated
print("TREATMENT: vtreat::designTreatmentsZ")