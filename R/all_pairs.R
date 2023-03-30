all_pairs <- function(x) {
  
  y <- combn(0:(x - 1), 2)
  
  tibble(
      first = y[1,],
     second = y[2,]
  ) %>%
  mutate(ffs = map2(first, second, c)) %>%
  pull(ffs)

}