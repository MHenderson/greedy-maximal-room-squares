library(R6)

Room <- R6Class(
  classname = "Room",
  public = list(
       size = NULL,
      cells = NULL,
    symbols = NULL,
    not_used_pairs = NULL,
    
    initialize = function(size = NA) {
      self$size <- size
      self$symbols <- 0:(size - 1)
      self$cells <- expand_grid(row = 1:(self$size - 1), col = 1:(self$size - 1)) %>%
        mutate(first = as.integer(NA), second = as.integer(NA)) %>%
        mutate(avail = list(0:(n - 1)))
      self$not_used_pairs <- all_pairs(self$size)
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
      self$not_used_pairs <- self$not_used_pairs[-match(list(p), self$not_used_pairs)]
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
      #is_subset(p, self$missing(row = e[1])) && is_subset(p, self$missing(col = e[2]))
      missing_row <- self$missing_row(row = e[1])
      missing_col <- self$missing_col(col = e[2])
      p[1] %in% missing_row && p[2] %in% missing_row && p[1] %in% missing_col && p[2] %in% missing_col
    }
  ),
  active = list(
    empty_cells = function() {
      E <- self$cells[is.na(self$cells$first), ]
      E <- mapply(c, E$row, E$col, SIMPLIFY = FALSE)
      return(E)
    }
  )
)

