library(dplyr)
library(ggplot2)
library(purrr)
library(R6)
library(tidyr)

n <- 10

r <- Room$new(size = n)

for(e in r$empty_cells()) {
  for(p in r$not_used_pairs()) {
    if(p[1] %in% r$missing(row = e[1]) &&
       p[2] %in% r$missing(row = e[1]) &&
       p[1] %in% r$missing(col = e[2]) &&
       p[2] %in% r$missing(col = e[2])) {
      r$set(e, p)
    }
  }
}

ggplot(data = r$cells %>% pivot_wider(), aes(col, row)) +
  geom_segment(data = grid_lines(n - 1, n - 1), aes(x = x, y = y, xend = xend, yend = yend), size = .1) +
  geom_text(data = r$cells %>% pivot_wider() %>% filter(!is.na(first)), aes(label = paste(first, second, sep = ","))) +
  scale_y_reverse() +
  coord_fixed() + 
  theme_void() +
  theme(
    legend.position  = "none"
  )

