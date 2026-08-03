# baxter-king_etal_2025/maintained/table_a4_mandate_pooled.R
# Output: output/table_a4_mandate_pooled.tex, .csv
# Depends on: helpers.R, analysis_guidance_information.R
# Description: Appendix Table S4, the vaccine mandate vignette experiment (G-2)
# with the friend and solo versions of each activity pooled, one column per
# activity. The deposit has no script for this table. The CSV carries the cells
# unrounded.

source(here::here("maintained", "helpers.R"))

model_dir <- file.path(output_dir, "model_objects_gi")

pooled_fits <-
  read_rds(file.path(model_dir, "w6_fit_vignette_pooled.rds")) |>
  mutate(column = str_to_title(scenario_exp_activity)) |>
  arrange(scenario_exp_activity)

fits <- set_names(pooled_fits$fit, pooled_fits$column)

extra_rows <- tibble(
  term = "Covariates",
  !!!set_names(as.list(rep("Yes", nrow(pooled_fits))), pooled_fits$column)
)

modelsummary(
  fits,
  gof_omit = "AIC|BIC|RMSE|Adj",
  coef_omit = "covariate",
  coef_rename = c(
    "ZTreatment" = "Treatment",
    "pid_7_c" = "Party ID (7-Point)",
    "ZTreatment:pid_7_c" = "Treatment x Party ID"
  ),
  stars = c("*" = 0.05),
  add_rows = extra_rows,
  title = "Mandate experiment (G-2), friend and solo versions pooled",
  escape = FALSE,
  output = file.path(output_dir, "table_a4_mandate_pooled.tex")
)

write_csv(
  pooled_fits |>
    mutate(estimates = map(fit, tidy),
           nobs = map_int(fit, function(f) as.integer(f$nobs)),
           r.squared = map_dbl(fit, function(f) f$r.squared)) |>
    select(-fit) |>
    unnest(estimates) |>
    filter(term %in% c("(Intercept)", "ZTreatment", "pid_7_c", "ZTreatment:pid_7_c")) |>
    select(column, activity = scenario_exp_activity, term, estimate, std.error,
           p.value, conf.low, conf.high, nobs, r.squared),
  file.path(output_dir, "table_a4_mandate_pooled.csv")
)
