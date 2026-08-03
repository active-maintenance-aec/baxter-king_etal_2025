# baxter-king_etal_2025/maintained/table_a1_endorsement_1.R
# Output: output/table_a1_endorsement_1.tex, .csv
# Depends on: helpers.R, analysis_e1_endorse_wave3.R
# Description: Appendix Table S1, the regression table behind the October 2020
# endorsement experiment (E-1), one column per endorser. Replaces table_A1.R.
# The CSV carries the cells unrounded so nothing downstream has to read a value
# back out of a formatted table.

source(here::here("maintained", "helpers.R"))

model_dir <- file.path(output_dir, "model_objects_e1")

endorsers <- c(
  "Trump" = "trump",
  "Fauci" = "fauci",
  "Trump and Fauci" = "trumpfauci",
  "Spiritual leader" = "spiritual",
  "Health Insurance" = "insurance",
  "Pharmacy" = "pharmacy",
  "Personal physician" = "physician"
)

fits <- map(endorsers, function(stem) {
  read_rds(file.path(model_dir, paste0("w3_fit_", stem, ".rds")))
})

extra_rows <- tibble(
  term = "Covariates",
  !!!set_names(as.list(rep("Yes", length(endorsers))), names(endorsers))
)

modelsummary(
  fits,
  gof_omit = "AIC|BIC|RMSE|Adj",
  coef_omit = "covariate",
  coef_rename = c(
    "treat" = "Treatment",
    "pid_7_c" = "Party ID (7-Point)",
    "treat:pid_7_c" = "Treatment x Party ID"
  ),
  stars = c("*" = 0.05),
  add_rows = extra_rows,
  title = "Endorsement experiment 1 (E-1)",
  escape = FALSE,
  output = file.path(output_dir, "table_a1_endorsement_1.tex")
)

write_csv(
  imap(fits, function(fit, nm) {
    tidy(fit) |>
      mutate(endorser = nm, nobs = fit$nobs, r.squared = fit$r.squared)
  }) |>
    bind_rows() |>
    filter(term %in% c("(Intercept)", "treat", "pid_7_c", "treat:pid_7_c")) |>
    select(endorser, term, estimate, std.error, p.value, conf.low, conf.high,
           nobs, r.squared),
  file.path(output_dir, "table_a1_endorsement_1.csv")
)
