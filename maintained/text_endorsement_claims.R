# baxter-king_etal_2025/maintained/text_endorsement_claims.R
# Output: output/text_endorsement_claims.csv
# Depends on: helpers.R, analysis_e1_endorse_wave3.R, analysis_e2_endorse_wave5.R
# Description: The quantities the article states in prose or in a footnote for the
# two endorsement experiments, and which no deposited script writes to a file.

source(here::here("maintained", "helpers.R"))

model_dir_e1 <- file.path(output_dir, "model_objects_e1")
model_dir_e2 <- file.path(output_dir, "model_objects_e2")

coefficient <- function(path, which_term) {
  tidy(read_rds(path)) |> filter(term == which_term)
}

e1_trump <- coefficient(file.path(model_dir_e1, "w3_fit_trump.rds"), "treat")
e1_trump_intercept <- coefficient(file.path(model_dir_e1, "w3_fit_trump.rds"), "(Intercept)")
e1_fauci <- coefficient(file.path(model_dir_e1, "w3_fit_fauci.rds"), "treat")

e2_biden <- coefficient(file.path(model_dir_e2, "w5_fit_biden.rds"), "treat")
e2_obama <- coefficient(file.path(model_dir_e2, "w5_fit_obama.rds"), "treat")

# Joint test that the endorsement effects are the same in the two E-1 framings ----
w3_endorse <- read_experiment("w3_endorse")

w3_restricted <- lm_robust(
  Y ~ experiment_treatment + experiment_arm + pid_7 +
    pid_7 * experiment_treatment + pid_7 * experiment_arm,
  data = w3_endorse, weights = weights
)

w3_unrestricted <- lm_robust(
  Y ~ experiment_treatment + experiment_arm + pid_7 +
    experiment_treatment * experiment_arm + pid_7 * experiment_treatment +
    pid_7 * experiment_arm + pid_7 * experiment_treatment * experiment_arm,
  data = w3_endorse, weights = weights
)

e1_arm_joint_p <- joint_test_p(w3_restricted, w3_unrestricted)

# Difference in the treatment-by-party interactions, Trump versus physician ----
# Estimated as a contrast within one unadjusted model so the two interactions
# share a control group and the covariance between them enters the standard
# error.
w3_pooled <-
  w3_endorse |>
  mutate(pid_7 = as.numeric(demo_pid7), Z = fct_relevel(factor(Z), "Control"))

interaction_fit <- lm_robust(Y ~ Z * pid_7, data = w3_pooled,
                             weights = weights, se_type = "HC1")

b <- coef(interaction_fit)
v <- vcov(interaction_fit)
trump_term <- "ZTrump:pid_7"
physician_term <- "ZPersonal Physician:pid_7"

dic_estimate <- b[[trump_term]] - b[[physician_term]]
dic_std_error <- sqrt(v[trump_term, trump_term] + v[physician_term, physician_term] -
                        2 * v[trump_term, physician_term])
dic_p_value <- 2 * pnorm(abs(dic_estimate / dic_std_error), lower.tail = FALSE)

# Intentions to vaccinate among the still unvaccinated, April 2021 ----
# Reported in the article for Democrats, Republicans and independents. The
# grouping that reproduces those three figures places leaners with independents.
w5_endorse <- read_experiment("w5_endorse")

e2_shares <-
  w5_endorse |>
  mutate(
    party = recode_values(
      as.character(demo_pid7),
      c("Strong Democrat", "Weak Democrat") ~ "Democrat",
      c("Lean Democrat", "Independent", "Lean Republican") ~ "Independent",
      c("Weak Republican", "Strong Republican") ~ "Republican"
    )
  ) |>
  group_by(party) |>
  summarize(share = weighted.mean(Y, weights), .groups = "drop")

e2_shares_leaners_with_party <-
  w5_endorse |>
  group_by(party = as.character(pid7_collapsed)) |>
  summarize(share = weighted.mean(Y, weights), .groups = "drop")

claims <- tribble(
  ~claim, ~value,
  "E-1 Trump endorsement, average treatment effect", e1_trump$estimate,
  "E-1 Trump endorsement, standard error", e1_trump$std.error,
  "E-1 Trump arm, covariate-adjusted control group mean", e1_trump_intercept$estimate,
  "E-1 Fauci endorsement, average treatment effect", e1_fauci$estimate,
  "E-1 Fauci endorsement, standard error", e1_fauci$std.error,
  "E-1 joint test of Personal versus Social framing, p-value", e1_arm_joint_p,
  "E-1 Trump minus physician treatment-by-party interaction", dic_estimate,
  "E-1 Trump minus physician interaction, standard error", dic_std_error,
  "E-1 Trump minus physician interaction, p-value", dic_p_value,
  "E-2 Biden endorsement, average treatment effect", e2_biden$estimate,
  "E-2 Biden endorsement, standard error", e2_biden$std.error,
  "E-2 Obama endorsement, average treatment effect", e2_obama$estimate,
  "E-2 Obama endorsement, standard error", e2_obama$std.error,
  "E-2 respondents in the deposited analysis file", nrow(w5_endorse),
  "E-2 share likely to vaccinate, Democrats (leaners with independents)",
    e2_shares$share[e2_shares$party == "Democrat"],
  "E-2 share likely to vaccinate, independents (leaners with independents)",
    e2_shares$share[e2_shares$party == "Independent"],
  "E-2 share likely to vaccinate, Republicans (leaners with independents)",
    e2_shares$share[e2_shares$party == "Republican"],
  "E-2 share likely to vaccinate, Democrats (leaners with party)",
    e2_shares_leaners_with_party$share[e2_shares_leaners_with_party$party == "Democrat"],
  "E-2 share likely to vaccinate, independents (leaners with party)",
    e2_shares_leaners_with_party$share[e2_shares_leaners_with_party$party == "Independent"],
  "E-2 share likely to vaccinate, Republicans (leaners with party)",
    e2_shares_leaners_with_party$share[e2_shares_leaners_with_party$party == "Republican"]
)

write_csv(claims, file.path(output_dir, "text_endorsement_claims.csv"))
print(claims, n = nrow(claims))
