library(tidyverse)
library(microbenchmark)

df <- tibble(x = numeric(), y = character())
df
test1 <- function() {
  map_df(1:1e5, function(x) { df %>% add_row(x = x, y = toString(x)) })
}

test2 <- function() {
  map_dfr(1:1e5, function(x) { tibble(x = x, y = toString(x)) })
}

microbenchmark(
  test1(), test2(),
  times = 1
)

map_dfr(1:1e4, function(x) { df %>% add_row(x = x, y = toString(x)) })
map_dfr(1:1e4, function(x) { tibble(x = x, y = toString(x)) })
