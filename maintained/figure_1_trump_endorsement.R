# baxter-king_etal_2025/maintained/figure_1_trump_endorsement.R
# Output: output/figure_1_trump_endorsement.pdf, .png,
#         output/figure_1_trump_endorsement_levels.csv,
#         output/figure_1_trump_endorsement_aces.csv
# Depends on: helpers.R
# Description: Group means and average causal effects of the Trump endorsement
# arm of E-1 (October 2020), overall and by seven-point party identification.
# Replaces figure_1.R.

source(here::here("maintained", "helpers.R"))

w3_endorse_trump <-
  read_experiment("w3_endorse") |>
  filter(Z %in% c("Control", "Trump"))

levels_df <- levels_data(w3_endorse_trump)
aces_df <- aces_data(w3_endorse_trump)

write_csv(levels_df, file.path(output_dir, "figure_1_trump_endorsement_levels.csv"))
write_csv(aces_df, file.path(output_dir, "figure_1_trump_endorsement_aces.csv"))

g <-
  (levels_plot(levels_df) +
     labs(y = '"Very likely" and "Somewhat likely"\nto get the vaccine = 1, 0 otherwise')) +
  aces_plot(aces_df)

ggsave(file.path(output_dir, "figure_1_trump_endorsement.pdf"), g, width = 6.5, height = 3.0)
ggsave(file.path(output_dir, "figure_1_trump_endorsement.png"), g, width = 6.5, height = 3.0, dpi = 300)
