################################################################################
#
# Script:  _encoding none.R
# Output:  training.set, testing.set - no encoding
#
################################################################################
# set feature labels
features.labels <- training.set %>% select(-target.label) %>% names
print(paste(
  "Categorical features:",
  ifelse(is_empty(features.labels), "None", features.labels)
))

print("TREATMENT: NONE")

