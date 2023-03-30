library(dplyr)
library(ggplot2)
library(here)
library(purrr)
library(R6)
library(tictoc)
library(tidyr)

source(here("R", "grid_lines.R"))
source(here("R", "is_subset.R"))
source(here("R", "roomr62.R"))

n <- 10

r <- Room$new(size = n)

tic()
for(e in r$empty_cells()) {
  for(p in r$not_used_pairs()) {
    if(r$is_available(e, p)) {
      r$set(e, p)
    }
  }
}
toc()

ggplot(data = r$cells %>% pivot_wider(), aes(col, row)) +
  geom_segment(data = grid_lines(n - 1, n - 1), aes(x = x, y = y, xend = xend, yend = yend), size = .1) +
  geom_text(data = r$cells %>% pivot_wider() %>% filter(!is.na(first)), aes(label = paste(first, second, sep = ","))) +
  scale_y_reverse() +
  coord_fixed() + 
  theme_void() +
  theme(
    legend.position  = "none"
  )

