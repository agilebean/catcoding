library(dplyr)
df <- data.frame(a = 1:4, b= 1:0, c=0:3)
df <- rbind(df, c(0,0,0))
df <- rbind(df, c(9,9,9))

df
# ok
df %>% filter(across(everything(.), ~. != 0)) 
# ok for all columns - default way
df %>% filter(across(everything(.), ~. == 0))
# works but weird why
df %>% filter(across(everything(.)) == 0)

# good! contains at least 1 zero column
df %>% 
  rowwise() %>% 
  filter(any(c_across(everything(.)) == 0))

# wrong!
df %>% filter(any(across(everything(.)) == 0))
