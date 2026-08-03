# baxter-king_etal_2025/maintained/analysis_e1_endorse_wave3.R
# Output: output/model_objects_e1/ (21 lm_lin model RDS files)
# Depends on: helpers.R
# Description: Fits the Lin-adjusted weighted OLS model for every endorser arm of
# the October 2020 vaccine endorsement experiment (E-1), pooled across the
# Personal and Social framings and separately within each. Replaces
# run_models_w3_endorse.R and run_models_w3_endorse_by_arm.R.

source(here::here("maintained", "helpers.R"))

model_dir <- file.path(output_dir, "model_objects_e1")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

w3_endorse <- read_experiment("w3_endorse")

arms <- c(
  trump = "Trump",
  fauci = "Fauci",
  trumpfauci = "Trump + Fauci",
  spiritual = "Spiritual/Religious Leader",
  insurance = "Health Insurance",
  pharmacy = "Pharmacy",
  physician = "Personal Physician"
)

fit_endorser <- function(label, framing = NULL) {
  d <- make_arm(w3_endorse, label)
  if (!is.null(framing)) d <- filter(d, experiment_arm == framing)
  lm_lin(
    Y ~ treat,
    covariates = ols_formula_covariates,
    data = d,
    weights = weights,
    se_type = "HC1"
  )
}

# Pooled over the two framings ----
fits_pooled <- map(arms, fit_endorser)

# Within each framing ----
# experiment_arm is a deposited column of data_w3_endorse.rds, so the by-framing
# models are refit here rather than read from a previous run of anything.
fits_personal <- map(arms, fit_endorser, framing = "A (Personal)")
fits_social <- map(arms, fit_endorser, framing = "B (Social)")

walk2(fits_pooled, names(fits_pooled), function(fit, nm) {
  write_model(fit, file.path(model_dir, paste0("w3_fit_", nm, ".rds")))
})
walk2(fits_personal, names(fits_personal), function(fit, nm) {
  write_model(fit, file.path(model_dir, paste0("w3_fit_", nm, "_personal.rds")))
})
walk2(fits_social, names(fits_social), function(fit, nm) {
  write_model(fit, file.path(model_dir, paste0("w3_fit_", nm, "_social.rds")))
})
