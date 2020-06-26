################################################################################
#
# Script:  _encoding none.R
# Output:  training.set, testing.set - no encoding
#
################################################################################
# set feature labels

features.labels <- training.set %>% select(-target.label) %>% names
print("TREATMENT: NONE")