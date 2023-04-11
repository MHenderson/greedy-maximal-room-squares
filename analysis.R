
results <- read_rds(here("data", "results.rds"))
meszka <- read_rds(here("data", "meszka-rosa-table.rds"))

results_sum <- results %>%
  filter(n >= 50) %>%
  group_by(n) %>%
  summarise(t3 = max(n_filled))

X <- left_join(meszka, results_sum)

final_results <- X %>%
  mutate(
    `t3 - t1` = t3 - t1,
           r3 = t3/nc2
  ) 

write_csv(final_results, here("final-results.csv"))
