# baxter-king_etal_2025/maintained/figure_a1_a22_experiment_panels.R
# Output: output/figure_a1_a22_experiment_panels.csv
# Depends on: helpers.R
# Description: The estimates printed on appendix Figures S1 to S22, one
# two-panel figure per experimental contrast. Each panel prints a difference in
# means and a Lin-adjusted estimate, overall and within each level of
# seven-point party identification, in percentage points. Replaces the estimate
# half of appendix_figures_C1_C22.R.
#
# These are the same quantities main-text Figures 1, 3 and 5 carry, computed by
# the same helper, for the other nineteen contrasts as well. They appear nowhere
# else in the article: the appendix regression tables give one treatment by party
# interaction per contrast, not eight conditional estimates, so without this
# script 704 published numbers have nothing to be compared against.
#
# The panels themselves are not redrawn. What the article prints on them is the
# numbers, and those are written here.

source(here::here("maintained", "helpers.R"))

# One row per appendix figure, in the order the appendix numbers them ----
# An endorsement contrast is one treatment arm against the control group; the
# guidance and information experiments are two-armed and enter whole.
endorsement_contrast <- function(dataset, arm) {
  read_experiment(dataset) |> filter(Z %in% c("Control", arm))
}

contrasts <- tribble(
  ~float, ~contrast, ~dataset, ~arm,
  "Appendix Figure S1", "E-1: Trump", "w3_endorse", "Trump",
  "Appendix Figure S2", "E-1: Fauci", "w3_endorse", "Fauci",
  "Appendix Figure S3", "E-1: Trump and Fauci", "w3_endorse", "Trump + Fauci",
  "Appendix Figure S4", "E-1: Physician", "w3_endorse", "Personal Physician",
  "Appendix Figure S5", "E-1: Pharmacy", "w3_endorse", "Pharmacy",
  "Appendix Figure S6", "E-1: Insurance", "w3_endorse", "Health Insurance",
  "Appendix Figure S7", "E-1: Spiritual Leader", "w3_endorse", "Spiritual/Religious Leader",
  "Appendix Figure S8", "E-2: Trump", "w5_endorse", "Trump",
  "Appendix Figure S9", "E-2: Trump and Fauci", "w5_endorse", "Trump + Fauci",
  "Appendix Figure S10", "E-2: Fauci", "w5_endorse", "Fauci",
  "Appendix Figure S11", "E-2: Biden and Fauci", "w5_endorse", "Biden + Fauci",
  "Appendix Figure S12", "E-2: Biden", "w5_endorse", "Biden",
  "Appendix Figure S13", "E-2: Obama", "w5_endorse", "Obama",
  "Appendix Figure S14", "E-2: James", "w5_endorse", "Lebron James",
  "Appendix Figure S15", "E-2: Ramos", "w5_endorse", "Jorge Ramos",
  "Appendix Figure S16", "G-1: Less restrictive mask guidance", "w6_cdcmask", NA,
  "Appendix Figure S17", "G-3: More restrictive mask guidance", "w7_cdcmask", NA,
  "Appendix Figure S18", "I-1: Contagiousness", "w7_contagiousness", NA,
  "Appendix Figure S19", "I-2: Delta Variant", "w7_doctordelta", NA,
  "Appendix Figure S20", "I-3: Bivalent Booster - Adult", "w8_adult_booster", NA,
  "Appendix Figure S21", "I-4: Bivalent Booster - Child", "w8_child_booster", NA,
  "Appendix Figure S22", "I-5: Vaccince - Child", "w8_child_vaccine", NA
)

panels <-
  contrasts |>
  mutate(estimates = map2(dataset, arm, function(dataset, arm) {
    d <- if (is.na(arm)) read_experiment(dataset) else endorsement_contrast(dataset, arm)
    aces_data(d)
  })) |>
  select(float, contrast, estimates) |>
  unnest(estimates) |>
  select(float, contrast, subgroup, estimator, estimate, std.error, p.value,
         conf.low, conf.high, entry, covariates_trimmed)

stopifnot(nrow(panels) == nrow(contrasts) * 16)

write_csv(panels, file.path(output_dir, "figure_a1_a22_experiment_panels.csv"))

print(panels |> count(float, name = "cells"), n = nrow(contrasts))
