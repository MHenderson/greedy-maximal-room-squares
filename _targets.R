library(targets)

tar_option_set(
  packages = c("dplyr", "here", "purrr", "readr", "tibble", "wallis"),
  format = "rds"
)

tar_source()

list(
  tar_target(orders, seq(10, 22, 2)),
  tar_target(seeds, 42:48),
  tar_target(
    results,
    command = random_room(orders, seeds),
    pattern = cross(orders, seeds)
  ),
  tar_target(
    name = results_summary,
    command = results |>
      group_by(n) |>
      summarise(
        min_filled = min(n_filled),
        max_filled = max(n_filled)
      )
  )
)
