# baxter-king_etal_2025/maintained/table_a3_mandate_vignettes.R
# Output: output/table_a3_mandate_vignettes.tex, .csv
# Depends on: helpers.R, analysis_guidance_information.R
# Description: Appendix Table S3, the regression table behind the vaccine
# mandate vignette experiment (G-2), one column per activity and vignette
# version. Replaces table_A3.R. The CSV carries the cells unrounded.

source(here::here("maintained", "helpers.R"))

model_dir <- file.path(output_dir, "model_objects_gi")

vignette_fits <- read_rds(file.path(model_dir, "w6_fit_vignette.rds"))

columns <-
  vignette_fits |>
  mutate(
    column = paste0(str_to_title(scenario_exp_activity), " (",
                    str_to_upper(str_sub(scenario_exp_anchored, 1, 1)), ")"),
    version_order = if_else(scenario_exp_anchored == "friend", 1, 2)
  ) |>
  arrange(version_order, scenario_exp_activity)

fits <- set_names(columns$fit, columns$column)

extra_rows <- tibble(
  term = "Covariates",
  !!!set_names(as.list(rep("Yes", nrow(columns))), columns$column)
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
  title = "Mandate experiment (G-2). (F) and (S) are the friend and solo versions of the vignettes.",
  escape = FALSE,
  output = file.path(output_dir, "table_a3_mandate_vignettes.tex")
)

write_csv(
  columns |>
    mutate(estimates = map(fit, tidy),
           nobs = map_int(fit, function(f) as.integer(f$nobs)),
           r.squared = map_dbl(fit, function(f) f$r.squared)) |>
    select(-fit, -version_order) |>
    unnest(estimates) |>
    filter(term %in% c("(Intercept)", "ZTreatment", "pid_7_c", "ZTreatment:pid_7_c")) |>
    select(column, activity = scenario_exp_activity, version = scenario_exp_anchored,
           term, estimate, std.error, p.value, conf.low, conf.high, nobs, r.squared),
  file.path(output_dir, "table_a3_mandate_vignettes.csv")
)
