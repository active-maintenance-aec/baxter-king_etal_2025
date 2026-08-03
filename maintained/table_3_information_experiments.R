# baxter-king_etal_2025/maintained/table_3_information_experiments.R
# Output: output/table_3_information_experiments.tex, .csv
# Depends on: helpers.R, analysis_guidance_information.R
# Description: Regression table for the five information experiments, I-1
# through I-5. Replaces table_3.R, which wrote its LaTeX through xtable.

source(here::here("maintained", "helpers.R"))

model_dir <- file.path(output_dir, "model_objects_gi")

fits <- list(
  "Contagiousness (I-1)" = read_rds(file.path(model_dir, "w7_fit_contagiousness.rds")),
  "Delta (I-2)" = read_rds(file.path(model_dir, "w7_fit_doctordelta.rds")),
  "Adult booster (I-3)" = read_rds(file.path(model_dir, "w8_fit_adult_booster.rds")),
  "Child booster (I-4)" = read_rds(file.path(model_dir, "w8_fit_child_booster.rds")),
  "Child vaccine (I-5)" = read_rds(file.path(model_dir, "w8_fit_child_vaccine.rds"))
)

extra_rows <- tibble(
  term = c("Covariates", "Sample"),
  `Contagiousness (I-1)` = c("Yes", "Unvacc. adults"),
  `Delta (I-2)` = c("Yes", "Unvacc. adults"),
  `Adult booster (I-3)` = c("Yes", "Vacc. adults, no booster"),
  `Child booster (I-4)` = c("Yes", "Vacc. kids in household"),
  `Child vaccine (I-5)` = c("Yes", "Unvacc. kids in household")
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
  title = "Information experiments (I-1 through I-5)",
  escape = FALSE,
  output = file.path(output_dir, "table_3_information_experiments.tex")
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
  file.path(output_dir, "table_3_information_experiments.csv")
)
