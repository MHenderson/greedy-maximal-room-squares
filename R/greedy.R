greedy1 <- function(order) {
  
  r <- Room$new(size = order)
  
  for(e in r$empty_cells) {
    for(p in r$free_pairs) {
      if(r$is_available(e, p)) {
        r$set(e, p)
        break()
      }
    }
  }
  
  tibble(
    n = as.integer(order),
    n_filled = as.integer(r$n_filled),
    volume = r$volume,
    cells = list(r$cells |> select(row, col, first, second))
  )
  
}

greedy2 <- function(order) {
  
  r <- Room$new(size = order)
  
  for(p in r$free_pairs) {
    for(e in r$empty_cells) {
      if(r$is_available(e, p)) {
        r$set(e, p)
        break()
      }
    }
  }
  
  tibble(
    n = as.integer(order),
    n_filled = as.integer(r$n_filled),
    volume = r$volume,
    cells = list(r$cells |> select(row, col, first, second))
  )
  
}