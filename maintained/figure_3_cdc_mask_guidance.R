# baxter-king_etal_2025/maintained/figure_3_cdc_mask_guidance.R
# Output: output/figure_3_cdc_mask_guidance.pdf, .png,
#         output/figure_3_cdc_mask_guidance_levels.csv,
#         output/figure_3_cdc_mask_guidance_aces.csv
# Depends on: helpers.R
# Description: Group means and average causal effects of the June 2021 CDC mask
# guidance experiment (G-1), overall and by seven-point party identification.
# Replaces figure_3.R.

source(here::here("maintained", "helpers.R"))

w6_cdcmask <- read_experiment("w6_cdcmask")

levels_df <- levels_data(w6_cdcmask)
aces_df <- aces_data(w6_cdcmask)

write_csv(levels_df, file.path(output_dir, "figure_3_cdc_mask_guidance_levels.csv"))
write_csv(aces_df, file.path(output_dir, "figure_3_cdc_mask_guidance_aces.csv"))

g <-
  (levels_plot(levels_df) +
     labs(y = "Vaccinated people don't need wear masks\ninside of public places but unvaccinated\npeople do = 1, 0 otherwise")) +
  aces_plot(aces_df)

ggsave(file.path(output_dir, "figure_3_cdc_mask_guidance.pdf"), g, width = 6.5, height = 3.0)
ggsave(file.path(output_dir, "figure_3_cdc_mask_guidance.png"), g, width = 6.5, height = 3.0, dpi = 300)
