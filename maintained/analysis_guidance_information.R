# baxter-king_etal_2025/maintained/analysis_guidance_information.R
# Output: output/model_objects_gi/ (nine model objects)
# Depends on: helpers.R
# Description: Fits the Lin-adjusted weighted OLS models for the two CDC mask
# guidance experiments (G-1, G-3), the vaccine mandate vignettes (G-2, by
# activity and by whether the vignette was about the respondent or a friend) and
# the five information experiments (I-1 through I-5). Replaces the eight
# run_models_w6_*, run_models_w7_* and run_models_w8_* scripts.

source(here::here("maintained", "helpers.R"))

model_dir <- file.path(output_dir, "model_objects_gi")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

fit_treat <- function(data) {
  lm_lin(
    Y ~ treat,
    covariates = ols_formula_covariates,
    data = data,
    weights = weights,
    se_type = "HC1"
  )
}

# Guidance: G-1 and G-3 ----
write_model(fit_treat(read_experiment("w6_cdcmask")),
            file.path(model_dir, "w6_fit_cdcmask.rds"))
write_model(fit_treat(read_experiment("w7_cdcmask")),
            file.path(model_dir, "w7_fit_cdcmask.rds"))

# Mandate vignettes: G-2 ----
# Estimated separately for each of the four activities, once within each
# vignette version and once pooling the two versions. Fitted objects are saved
# rather than tidy summaries, so the appendix regression table can read sample
# sizes and R-squared out of the same objects the figure plots.
w6_vignette <- read_experiment("w6_vignette")

fit_vignette <- function(data) {
  lm_lin(
    Y ~ Z,
    covariates = ols_formula_covariates,
    data = data,
    weights = weights,
    se_type = "HC1"
  )
}

vignette_cells <-
  w6_vignette |>
  distinct(scenario_exp_activity, scenario_exp_anchored) |>
  arrange(scenario_exp_activity, scenario_exp_anchored)

vignette_fits <-
  vignette_cells |>
  mutate(fit = map2(scenario_exp_activity, scenario_exp_anchored, function(a, v) {
    fit_vignette(filter(w6_vignette, scenario_exp_activity == a, scenario_exp_anchored == v))
  }))

vignette_fits_pooled <-
  vignette_cells |>
  distinct(scenario_exp_activity) |>
  mutate(
    scenario_exp_anchored = "pooled",
    fit = map(scenario_exp_activity, function(a) {
      fit_vignette(filter(w6_vignette, scenario_exp_activity == a))
    })
  )

write_rds(strip_environments(vignette_fits), file.path(model_dir, "w6_fit_vignette.rds"))
write_rds(strip_environments(vignette_fits_pooled), file.path(model_dir, "w6_fit_vignette_pooled.rds"))

# Information: I-1 through I-5 ----
information <- c(
  w7_fit_contagiousness = "w7_contagiousness",
  w7_fit_doctordelta = "w7_doctordelta",
  w8_fit_adult_booster = "w8_adult_booster",
  w8_fit_child_booster = "w8_child_booster",
  w8_fit_child_vaccine = "w8_child_vaccine"
)

walk2(information, names(information), function(dataset, nm) {
  write_model(fit_treat(read_experiment(dataset)), file.path(model_dir, paste0(nm, ".rds")))
})
