# baxter-king_etal_2025/maintained/analysis_e2_endorse_wave5.R
# Output: output/model_objects_e2/ (eight lm_lin model RDS files)
# Depends on: helpers.R
# Description: Fits the Lin-adjusted weighted OLS model for every endorser arm of
# the April 2021 vaccine endorsement experiment (E-2), which was fielded among
# still-unvaccinated respondents. Replaces run_models_w5_endorse.R.

source(here::here("maintained", "helpers.R"))

model_dir <- file.path(output_dir, "model_objects_e2")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

w5_endorse <- read_experiment("w5_endorse")

arms <- c(
  trump = "Trump",
  fauci = "Fauci",
  trumpfauci = "Trump + Fauci",
  obama = "Obama",
  biden = "Biden",
  biden_fauci = "Biden + Fauci",
  ramos = "Jorge Ramos",
  james = "Lebron James"
)

fits <- map(arms, function(label) {
  lm_lin(
    Y ~ treat,
    covariates = ols_formula_covariates,
    data = make_arm(w5_endorse, label),
    weights = weights,
    se_type = "HC1"
  )
})

walk2(fits, names(fits), function(fit, nm) {
  write_model(fit, file.path(model_dir, paste0("w5_fit_", nm, ".rds")))
})
