################################################################################
#
# Script:  _encoding vtreat-cross.R
# Output:  training.set, testing.set - vtreat::mkCrossFrame{N,C} Experiment
#
################################################################################
# decide regression or classification
if (is.numeric(target)) {
  
  treatment_function <- vtreat::mkCrossFrameNExperiment
  
} else if (is.factor(target)) {
  
  treatment_function <- vtreat::mkCrossFrameCExperiment
}

clus <- clusterOn()
system.time(
  training.set.cross <- treatment_function(
    dframe = training.set, 
    varlist = features.labels,
    outcomename = target.label,
    # codeRestriction = c("lev"),
    parallelCluster = clus, # % faster
    rareCount = 0,  # Note set this to something larger, like 5
    rareSig = c()
  )       
)
clusterOff(clus)

# get treated training.set
treatments <- training.set.cross$treatments %T>% print
training.set.treated <- training.set.cross$crossFrame %>% 
  as_tibble() %T>% print

vartypes.selected <- if (CATS.ONLY) {
  
  print("feature selection: CATS ONLY")
  c("lev")
  
} else { 
  
  print("feature selection: ALL")
  c("lev", "clean")
}

# training.set.treated %>% select(-target.label) %>% names

features.selected <- treatments$scoreFrame %>%
  # code "clean":  a numerical variable with no NAs or NaNs
  # code "lev": an indicator variable for a specific level of the original categorical variable.
  # filter(code %in% c("clean", "lev")) %>%
  filter(code %in% vartypes.selected) %>%
  # vtreat recommendations to filter out useless variables
  filter(recommended == TRUE) %>%
  pull(varName) %T>% print
  
  if (!is.null(testing.set)) {
    testing.set.treated <- vtreat::prepare(
      treatments,
      testing.set,
      pruneSig=c()
    )      
  }
  
  # set trainingset, testingset, features labels
  training.set <- training.set.treated %>% select(features.selected)
  if (!is.null(testing.set)) {
    testing.set <- testing.set.treated  %>% select(features.selected)
  }
  features.labels <- features.selected
  
  print("TREATMENT: vtreat::mkCrossFrameNExperiment")