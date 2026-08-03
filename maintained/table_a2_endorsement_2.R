# baxter-king_etal_2025/maintained/table_a2_endorsement_2.R
# Output: output/table_a2_endorsement_2.tex, .csv
# Depends on: helpers.R, analysis_e2_endorse_wave5.R
# Description: Appendix Table S2, the regression table behind the April 2021
# endorsement experiment (E-2), one column per endorser. Replaces table_A2.R.
# The CSV carries the cells unrounded.

source(here::here("maintained", "helpers.R"))

model_dir <- file.path(output_dir, "model_objects_e2")

endorsers <- c(
  "Trump" = "trump",
  "Fauci" = "fauci",
  "Trump and Fauci" = "trumpfauci",
  "Obama" = "obama",
  "Biden" = "biden",
  "Biden and Fauci" = "biden_fauci",
  "Jorge Ramos" = "ramos",
  "Lebron James" = "james"
)

fits <- map(endorsers, function(stem) {
  read_rds(file.path(model_dir, paste0("w5_fit_", stem, ".rds")))
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
  title = "Endorsement experiment 2 (E-2)",
  escape = FALSE,
  output = file.path(output_dir, "table_a2_endorsement_2.tex")
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
  file.path(output_dir, "table_a2_endorsement_2.csv")
)
