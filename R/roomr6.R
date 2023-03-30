Room <- R6Class(
  classname = "Room",
  public = list(
    size = NULL,
    cells = NULL,
    symbols = NULL,
    initialize = function(size = NA) {
      self$size <- size
      self$symbols <- 0:(size - 1)
      self$cells <- expand_grid(row = 1:(self$size - 1), col = 1:(self$size - 1)) %>%
        mutate(first = as.integer(NA), second = as.integer(NA))
    },
    get = function(i, j) {
      self$cells %>%
        filter(row == i, col == j) %>%
        select(first, second)
    },
    set = function(i, j, k, l) {
      self$cells[self$cells$row == i & self$cells$col == j, "first"] <- k
      self$cells[self$cells$row == i & self$cells$col == j, "second"] <- l
    },
    used = function(row = NA, col = NA) {
      if(!is.na(row)) {
        first <- self$cells[self$cells$row == row, "first"]$first
        second <- self$cells[self$cells$row == row, "second"]$second
      }
      if(!is.na(col)) {
        first <- self$cells[self$cells$col == col, "first"]$first
        second <- self$cells[self$cells$col == col, "second"]$second
      }
      unique(c(first[!is.na(first)], second[!is.na(second)]))
    },
    missing = function(row = NA, col = NA) {
      if(!is.na(row)) {
        used <- self$used(row = row)
      }
      if(!is.na(col)) {
        used <- self$used(col = col)
      }
      setdiff(self$symbols, used)
    },
    empty_cells = function() {
      E <- self$cells[is.na(self$cells$first), ]
      E <- mapply(c, E$row, E$col, SIMPLIFY = FALSE)
      return(E)
    },
    not_used_pairs =  function() {
      #n <- max(self$cells$row) + 1
      # find the pairs already used
      used_pairs <- self$cells %>% select(first, second)
      # construct the set of pairs not used (namely all pairs minus used pairs)
      x <- combn(0:(self$size - 1), 2)
      
      all_pairs <- tibble(
        first = x[1,],
        second = x[2,]
      )
      
      anti_join(all_pairs, used_pairs, by = c("first", "second")) %>%
        mutate(ffs = map2(first, second, c)) %>%
        pull(ffs)
    }
  )
)

