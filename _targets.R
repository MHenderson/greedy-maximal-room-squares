library(targets)

tar_option_set(
  packages = c("dplyr", "flextable", "ggplot2", "here", "purrr", "readr", "tibble", "tidyr", "wallis"),
  format = "rds",
  memory = "transient",
  garbage_collection = TRUE
)

tar_source()

list(
  tar_target(name = orders, command = seq(2, 80, 2)),
  tar_target(
    name = results,
    command = {
      tibble(
              n = as.integer(orders),
        greedy1 = list(greedy1(orders)),
        greedy2 = list(greedy2(orders))
      ) |>
      pivot_longer(-n) |>
      mutate(
          volume = map_dbl(value, "volume"),
        n_filled = map_dbl(value, "n_filled"),
           cells = map(value, "cells")
      ) |>
      select(-value)
    },
    pattern = map(orders)
  ),
  tar_target(
    name = final_results,
    command = {
      results |>
        select(-cells) |>
        pivot_wider(names_from = name, values_from = c(volume, n_filled)) |>
        rename(
          t1 = n_filled_greedy1,
          t2 = n_filled_greedy2,
          r1 = volume_greedy1,
          r2 = volume_greedy2
        ) |>
        select(n, t1, t2, r1, r2)
    }
  ),
  tar_target(
    name = plot_results,
    command = {
      ggplot(results, aes(n, volume, colour = name)) +
        geom_line() +
        theme_bw() +
        theme(
          legend.position = "bottom",
          legend.title = element_blank()
        )
    }
  ),
  tar_target(
    name = final_results_as_flextable,
    command = {
      set_flextable_defaults(
        font.size = 6,
          padding = 6,
        theme_fun = theme_zebra
      )
      flextable(final_results)
    }
  ),
  tar_target(
    name = save_plot_results,
    command = ggsave(plot = plot_results, filename = "png/plot-results.png", height = 1000, width = 1200, units = "px")
  ),
  tar_target(
       name = save_as_png,
    command = save_as_image(final_results_as_flextable, path = "png/final-results.png")
  )
)
