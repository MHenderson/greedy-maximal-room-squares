library(R6)

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
    get = function(e) {
      self$cells %>%
        filter(row == e[1], col == e[2]) %>%
        select(first, second)
    },
    set = function(e, p) {
      self$cells[self$cells$row == e[1] & self$cells$col == e[2], "first"] <- p[1]
      self$cells[self$cells$row == e[1] & self$cells$col == e[2], "second"] <- p[2]
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
    missing_row = function(row = NA) {
      used <- self$used(row = row)
      setdiff(self$symbols, used)
    },
    missing_col = function(col = NA) {
      used <- self$used(col = col)
      setdiff(self$symbols, used)
    },
    is_available = function(e, p) {
      #is_subset(p, self$missing(row = e[1])) && is_subset(p, self$missing(col = e[2]))
      missing_row <- self$missing_row(row = e[1])
      missing_col <- self$missing_col(col = e[2])
      p[1] %in% missing_row && p[2] %in% missing_row && p[1] %in% missing_col && p[2] %in% missing_col
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

