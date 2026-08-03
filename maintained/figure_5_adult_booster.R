# baxter-king_etal_2025/maintained/figure_5_adult_booster.R
# Output: output/figure_5_adult_booster.pdf, .png,
#         output/figure_5_adult_booster_levels.csv,
#         output/figure_5_adult_booster_aces.csv
# Depends on: helpers.R
# Description: Group means and average causal effects of the October 2022
# bivalent booster information experiment (I-3), overall and by seven-point party
# identification. Replaces figure_5.R.

source(here::here("maintained", "helpers.R"))

w8_adult_booster <- read_experiment("w8_adult_booster")

levels_df <- levels_data(w8_adult_booster)
aces_df <- aces_data(w8_adult_booster)

write_csv(levels_df, file.path(output_dir, "figure_5_adult_booster_levels.csv"))
write_csv(aces_df, file.path(output_dir, "figure_5_adult_booster_aces.csv"))

g <-
  (levels_plot(levels_df) +
     labs(y = "Somewhat likely to/very likely to/will definitely\nget the bivalent booster = 1, 0 otherwise")) +
  aces_plot(aces_df)

ggsave(file.path(output_dir, "figure_5_adult_booster.pdf"), g, width = 6.5, height = 3.0)
ggsave(file.path(output_dir, "figure_5_adult_booster.png"), g, width = 6.5, height = 3.0, dpi = 300)
