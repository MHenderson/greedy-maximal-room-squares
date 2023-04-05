library(dplyr)
library(ggplot2)
library(here)
library(purrr)
library(readr)
library(R6)
library(tictoc)
library(tidyr)

source(here("R", "all_pairs.R"))
source(here("R", "grid_lines.R"))
source(here("R", "is_subset.R"))
source(here("R", "remove_both.R"))
source(here("R", "roomr62.R"))

# seeds should be kept on disk somewhere so we can easily calculate new ones
# or simply calculated from the existing data
i <- 50

set.seed(i)

orders <- seq(10, 100, 2)

for(j in 1:length(orders)) {
  
  r <- Room$new(size = orders[j])
  
  tic()
  for(e in sample(r$empty_cells, length(r$empty_cells))) {
    for(p in sample(r$free_pairs, length(r$free_pairs))) {
      if(r$is_available(e, p)) {
        r$set(e, p)
        break()
      }
    }
  }
  toc()
  
  X <- tibble(
        seed = as.integer(i),
           n = as.integer(orders[j]),
    n_filled = as.integer(r$n_filled),
      volume = r$volume,
       cells = list(r$cells %>% select(row, col, first, second))
  )
  
  results_file <- here("results", paste0("results_", orders[j], ".rds"))
  
  # of course we shouldn't even do the experiment if we already have the results for the given
  # seed - it's incredibly wasteful to only decide at this point...? right?
  if(file.exists(results_file)) {
    X_existing <- read_rds(results_file)
    X <- distinct(bind_rows(X, X_existing))
  }
  
  saveRDS(X, results_file)
  
}
