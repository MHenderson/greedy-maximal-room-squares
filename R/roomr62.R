library(R6)

Room <- R6Class(
  classname = "Room",
  public = list(
       size = NULL,
      cells = NULL,
    symbols = NULL,
    free_pairs = NULL,
    empty_cells = NULL,
    
    initialize = function(size = NA) {
      self$size <- size
      self$symbols <- 0:(size - 1)
      self$cells <- expand_grid(row = 1:(self$size - 1), col = 1:(self$size - 1)) %>%
        mutate(first = as.integer(NA), second = as.integer(NA)) %>%
        mutate(avail = list(0:(n - 1)))
      self$free_pairs <- all_pairs(self$size)
      self$empty_cells <- all_ordered_pairs(self$size - 1)
    },
    
    get = function(e) {
      self$cells %>%
        filter(row == e[1], col == e[2]) %>%
        select(first, second)
    },
    
    set = function(e, p) {
      self$cells[self$cells$row == e[1] & self$cells$col == e[2], "first"] <- p[1]
      self$cells[self$cells$row == e[1] & self$cells$col == e[2], "second"] <- p[2]
      self$cells[self$cells$row == e[1], "avail"]$avail <- lapply(self$cells[self$cells$row == e[1], "avail"]$avail, remove_both, p)
      self$cells[self$cells$col == e[2], "avail"]$avail <- lapply(self$cells[self$cells$col == e[2], "avail"]$avail, remove_both, p)
      self$free_pairs <- self$free_pairs[-match(list(p), self$free_pairs)]
      self$empty_cells <- self$empty_cells[-match(list(e), self$empty_cells)]
    },
    
    used_row = function(row) {
      x <- self$cells[self$cells$row == row, c("first", "second")]
      unique(c(x$first[!is.na(x$first)], x$second[!is.na(x$second)]))
    },
    
    used_col = function(col) {
      x <- self$cells[self$cells$col == col, c("first", "second")]
      unique(c(x$first[!is.na(x$first)], x$second[!is.na(x$second)]))
    },
    
    missing_row = function(row = NA) {
      used <- self$used_row(row = row)
      setdiff(self$symbols, used)
    },
    
    missing_col = function(col = NA) {
      used <- self$used_col(col = col)
      setdiff(self$symbols, used)
    },
    
    is_available = function(e, p) {
      p[1] %in% self$cells[self$cells$row == e[1] & self$cells$col == e[2], "avail"]$avail[[1]] && p[2] %in% self$cells[self$cells$row == e[1] & self$cells$col == e[2], "avail"]$avail[[1]]
    }
  ),
  active = list(
    n_filled = function() {
      self$cells %>%
        filter(!is.na(first)) %>% 
        nrow()
    },
    volume = function() {
      round(self$n_filled/choose(max(self$cells$col) + 1, 2), 6)
    }
  )
)

