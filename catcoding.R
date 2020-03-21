packs <- c(
  "tidyverse",
  "magrittr",
  "vtreat"
)
sapply(packs, require, character.only = TRUE)


diamonds
diamonds %>% str


