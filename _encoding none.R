################################################################################
#
# Script:  _encoding none.R
# Output:  training.set, testing.set - no encoding
#
################################################################################

# special case: only cats without treatment
# cats must be converted in _getdata.R
if (CATS.ONLY) {
  
  print("feature selection: CATS ONLY")
  # training.set with target and only cats
  training.set %<>% 
    select_if(is.factor) %>% 
    # add target.label column bec all factors
    mutate(!!target.label := training.set[[!!target.label]])
  
  # testing.set with target and only cats
  if (!is.null(testing.set)) {
    
    testing.set %<>% 
      select_if(is.factor) %>% 
      mutate(!!target.label := testing.set[[!!target.label]]) 
    
  } else {
    
    print("no testing set")
  }
  
} else {
  
  print("feature selection: ALL")
}

# set feature labels
features.labels <- training.set %>% select(-target.label) %>% names
print(paste(
  "Categorical features:",
  ifelse(is_empty(features.labels), "None", features.labels)
))

print("TREATMENT: NONE")

