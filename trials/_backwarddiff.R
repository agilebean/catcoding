
library(tidyverse)
library(magrittr)

# https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/
hsb2 = read.table('https://stats.idre.ucla.edu/stat/data/hsb2.csv', header=T, sep=",")

hsb2 %<>% as_tibble

hsb2

hsb2$race.f = factor(hsb2$race, labels=c("Hispanic", "Asian", "African-Am", "Caucasian"))

hsb2

tapply(hsb2$write, hsb2$race.f, mean)


dataset <- hsb2 %>% 
  select(race, write) %>% 
  mutate(race = as.factor(race))

dataset %>% summary

dataset %>% 
  group_by(race) %>% 
  summarize(mean = mean(write))
