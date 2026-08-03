# baxter-king_etal_2025/maintained/text_guidance_information_claims.R
# Output: output/text_guidance_information_claims.csv
# Depends on: helpers.R, analysis_guidance_information.R
# Description: The quantities the article states in prose or in a footnote for the
# guidance, mandate and information experiments, including the two joint
# significance tests and the two chi-squared tests on the full outcome
# distributions.

source(here::here("maintained", "helpers.R"))

model_dir <- file.path(output_dir, "model_objects_gi")

coefficient <- function(file, which_term) {
  tidy(read_rds(file.path(model_dir, file))) |> filter(term == which_term)
}

g1_intercept <- coefficient("w6_fit_cdcmask.rds", "(Intercept)")
g1_treat <- coefficient("w6_fit_cdcmask.rds", "treat")
g3_intercept <- coefficient("w7_fit_cdcmask.rds", "(Intercept)")
g3_treat <- coefficient("w7_fit_cdcmask.rds", "treat")

i1_intercept <- coefficient("w7_fit_contagiousness.rds", "(Intercept)")
i2_intercept <- coefficient("w7_fit_doctordelta.rds", "(Intercept)")
i3_treat <- coefficient("w8_fit_adult_booster.rds", "treat")
i4_intercept <- coefficient("w8_fit_child_booster.rds", "(Intercept)")
i4_treat <- coefficient("w8_fit_child_booster.rds", "treat")

# Baseline willingness across the four mandate vignettes ----
g2_control_means <-
  read_rds(file.path(model_dir, "w6_fit_vignette_pooled.rds")) |>
  mutate(estimates = map(fit, tidy)) |>
  select(-fit) |>
  unnest(estimates) |>
  filter(term == "(Intercept)")

# Joint test that the solo and friend vignette versions have the same effects ----
w6_vignette <- read_experiment("w6_vignette")

g2_restricted <- lm_robust(
  Y ~ Z + scenario_exp_activity + pid_7 + Z * pid_7 + scenario_exp_activity * pid_7,
  data = w6_vignette, weights = weights
)

g2_unrestricted <- lm_robust(
  Y ~ Z + scenario_exp_activity + pid_7 + Z * scenario_exp_activity + Z * pid_7 +
    scenario_exp_activity * pid_7 + Z * scenario_exp_activity * pid_7,
  data = w6_vignette, weights = weights
)

g2_joint_p <- joint_test_p(g2_restricted, g2_unrestricted)

# Joint test that the information source does not matter in I-1 ----
w7_contagiousness <- read_experiment("w7_contagiousness")

i1_restricted <- lm_robust(
  Y ~ Z + contagious_exp_arm + pid_7 + Z * pid_7 + contagious_exp_arm * pid_7,
  data = w7_contagiousness, weights = weights
)

i1_unrestricted <- lm_robust(
  Y ~ Z + contagious_exp_arm + pid_7 + Z * contagious_exp_arm + Z * pid_7 +
    contagious_exp_arm * pid_7 + Z * contagious_exp_arm * pid_7,
  data = w7_contagiousness, weights = weights
)

i1_source_joint_p <- joint_test_p(i1_restricted, i1_unrestricted)

# Chi-squared tests on the undichotomized mask outcome ----
w6_cdcmask <- read_experiment("w6_cdcmask")
w7_cdcmask <- read_experiment("w7_cdcmask")

g1_chisq <-
  w6_cdcmask |>
  group_by(Z) |>
  summarize(
    stop_now = sum(mask_exp_combined_manual == "Everyone should stop doing this now regardless of vaccination status"),
    continue = sum(mask_exp_combined_manual == "Everyone should continue to do this for a little while longer regardless of vaccination status"),
    vaccinated_exempt = sum(mask_exp_combined_manual %in% c(
      "Following CDC recommendations, vaccinated people don't need to do this but unvaccinated people do",
      "Vaccinated people don't need to do this but unvaccinated people do"
    )),
    .groups = "drop"
  ) |>
  select(-Z) |>
  chisq.test()

g3_chisq <-
  w7_cdcmask |>
  group_by(Z) |>
  summarize(
    stop_now = sum(mask_exp_combined_manual == "Everyone should stop doing this now regardless of vaccination status"),
    vaccinated_exempt = sum(mask_exp_combined_manual == "Vaccinated people don't need to do this but unvaccinated people do"),
    continue = sum(mask_exp_combined_manual %in% c(
      "Following CDC recommendations, everyone should continue to do this for a little while longer regardless of vaccination status",
      "Everyone should continue to do this for a little while longer regardless of vaccination status"
    )),
    .groups = "drop"
  ) |>
  select(-Z) |>
  chisq.test()

claims <- tribble(
  ~claim, ~value,
  "G-1 covariate-adjusted control group mean", g1_intercept$estimate,
  "G-1 average treatment effect", g1_treat$estimate,
  "G-3 covariate-adjusted control group mean", g3_intercept$estimate,
  "G-3 average treatment effect", g3_treat$estimate,
  "G-2 lowest control group mean across the four vignettes", min(g2_control_means$estimate),
  "G-2 highest control group mean across the four vignettes", max(g2_control_means$estimate),
  "G-2 joint test of solo versus friend version, p-value", g2_joint_p,
  "I-1 covariate-adjusted control group mean", i1_intercept$estimate,
  "I-1 joint test of information source, p-value", i1_source_joint_p,
  "I-2 covariate-adjusted control group mean", i2_intercept$estimate,
  "I-3 average treatment effect", i3_treat$estimate,
  "I-3 standard error", i3_treat$std.error,
  "I-4 covariate-adjusted control group mean", i4_intercept$estimate,
  "I-4 average treatment effect", i4_treat$estimate,
  "I-4 standard error", i4_treat$std.error,
  "G-1 chi-squared statistic on the three-category outcome", unname(g1_chisq$statistic),
  "G-1 chi-squared p-value", g1_chisq$p.value,
  "G-3 chi-squared statistic on the three-category outcome", unname(g3_chisq$statistic),
  "G-3 chi-squared p-value", g3_chisq$p.value
)

write_csv(claims, file.path(output_dir, "text_guidance_information_claims.csv"))
print(claims, n = nrow(claims))
