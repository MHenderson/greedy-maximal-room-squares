library(dplyr)
library(here)
library(purrr)
library(readr)

source("/home/matthew/workspace/maximal-room-squares/R/avail.R")
source("/home/matthew/workspace/maximal-room-squares/R/empty_cells.R")
source("/home/matthew/workspace/maximal-room-squares/R/is_maximal_proom.R")
source("/home/matthew/workspace/maximal-room-squares/R/not_used_pairs.R")
source("/home/matthew/workspace/maximal-room-squares/R/see.R")
source("/home/matthew/workspace/maximal-room-squares/R/verify.R")

X_files <- list.files(here("results"), full.names = TRUE)

for(X_f in X_files) {
  
  XX <- read_rds(X_f)
  
  XXX <- XX %>%
    arrange(n) %>%
    head() %>%
    mutate(
      n_filled_v = map_dbl(cells, n_filled_cells),
      is_maximal_proom = map_lgl(cells, is_maximal_proom)
    )
  
  write_rds(XXX, here("results-valid", basename(X_f)))
  
}
