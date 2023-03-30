
<!-- README.md is generated from README.Rmd. Please edit that file -->

# more-maximal-room-squares

<!-- badges: start -->
<!-- badges: end -->

``` r
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
#> 2.295 sec elapsed
```

![](figure/plot-1.png)<!-- -->
