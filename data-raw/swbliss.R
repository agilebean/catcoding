## code to prepare `swbliss` dataset goes here
library(tidyverse)
filepath <- system.file(
  "extdata",
  "swbliss cp21m_EN_1.0p.sav",
  package = "catcoding",
  mustWork = TRUE
) %>% print

# import raw data
dataset <- sjlabelled::read_spss(filepath) 

dataset %<>% 
  select( # select 20 Mini-IPIP items from 50 Big5 item pool
    swls1 = cp21m014, # In most ways my life is close to my ideal
    swls2 = cp21m015, # The conditions of my life are excellent
    swls3 = cp21m016, # I am satisfied with my life
    swls4 = cp21m017, # So far I have gotten the important things I want in life
    swls5 = cp21m018, # If I could live my life over, I would change almost nothing
    big5e1 = cp21m020, # Am the life of the party
    big5e2 = cp21m050, # Talk to a lot of different people at parties
    big5e3 = cp21m025, # r Don't talk a lot.
    big5e4 = cp21m035, # r Keep in the background.
    big5a1 = cp21m036, # Sympathize with others' feelings
    big5a2 = cp21m061, # Feel others' emotions.
    big5a3 = cp21m051, # r Am not really interested in others.
    big5a4 = cp21m041, # r Am not interested in other people's problems
    big5c1 = cp21m042, # Get chores done right away.
    big5c2 = cp21m052, # Like order.
    big5c3 = cp21m047, # r Often forget to put things back in their proper place.
    big5c4 = cp21m037, # r Make a mess of things.
    big5n1 = cp21m058, # Have frequent mood swings.
    big5n2 = cp21m048, # Get upset easily.
    big5n3 = cp21m028, # r Am relaxed most of the time.
    big5n4 = cp21m038, # r Seldom feel blue.
    big5o1 = cp21m034, # Have a vivid imagination.
    big5o2 = cp21m029, # r Have difficulty understanding abstract ideas.
    big5o3 = cp21m039, # r Am not interested in abstract ideas.
    big5o4 = cp21m049 # r Do not have a good imagination.
  ) %>% 
  rowwise() %>% 
  mutate(
    SWB = mean(c_across(starts_with("swls"))),
    .keep = "unused",
    .before = "big5e1"
  ) %>% 
  ungroup() %>% 
  drop_na() %>% 
  mutate(across(-SWB, as_factor))

# define target and features
target.label <- "SWB"
features.labels <- dataset %>% select(-target.label) %>% names

# create data structure
swbliss <- list()
swbliss$target.label <- target.label
swbliss$features.labels <- features.labels
swbliss$data <- dataset

usethis::use_data(swbliss, overwrite = TRUE)
