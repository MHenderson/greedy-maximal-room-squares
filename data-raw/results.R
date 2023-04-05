library(dplyr)
library(here)
library(purrr)
library(readr)

X <- list.files(here("results"), full.names = TRUE)

XX <- map_dfr(X, read_rds)

write_rds(XX, here("data", "results.rds"))