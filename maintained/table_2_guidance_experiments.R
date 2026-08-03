# baxter-king_etal_2025/maintained/table_2_guidance_experiments.R
# Output: output/table_2_guidance_experiments.tex, .csv
# Depends on: helpers.R, analysis_guidance_information.R
# Description: Regression table for the two CDC mask guidance experiments, G-1
# (June 2021) and G-3 (September 2021). Replaces table_2.R, which wrote its
# LaTeX through xtable.

source(here::here("maintained", "helpers.R"))

model_dir <- file.path(output_dir, "model_objects_gi")

fits <- list(
  "Less restrictive guidance (G-1)" = read_rds(file.path(model_dir, "w6_fit_cdcmask.rds")),
  "More restrictive guidance (G-3)" = read_rds(file.path(model_dir, "w7_fit_cdcmask.rds"))
)

extra_rows <- tribble(
  ~term, ~`Less restrictive guidance (G-1)`, ~`More restrictive guidance (G-3)`,
  "Covariates", "Yes", "Yes",
  "Sample", "All respondents", "All respondents"
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
  title = "Guidance experiments (G-1 and G-3)",
  escape = FALSE,
  output = file.path(output_dir, "table_2_guidance_experiments.tex")
)

write_csv(
  imap(fits, function(fit, nm) {
    tidy(fit) |>
      mutate(experiment = nm, nobs = fit$nobs, r.squared = fit$r.squared)
  }) |>
    bind_rows() |>
    filter(term %in% c("(Intercept)", "treat", "pid_7_c", "treat:pid_7_c")) |>
    select(experiment, term, estimate, std.error, p.value, conf.low, conf.high,
           nobs, r.squared),
  file.path(output_dir, "table_2_guidance_experiments.csv")
)
