# baxter-king_etal_2025/maintained/text_equivalence_claims.R
# Output: output/text_equivalence_claims.csv, output/text_weighting_claims.csv,
#         output/text_equivalence_counts.csv
# Depends on: helpers.R
# Description: Conditional average treatment effects among Democrats and among
# Republicans, the difference between them, and two one-sided equivalence tests
# at bands of 5 and 10 percentage points, for every experiment and endorser arm,
# plus the count of treatment-by-party interactions that change significance
# when the survey weights are dropped. These are the quantities behind the
# article's claims about how many experiments affirm equivalence, how many
# interactions turn significant unweighted, and behind the unnumbered
# equivalence tables of the appendix.
#
# The deposit computes the same quantities. Its standard error for the
# difference squares the Republican standard error twice and never uses the
# Democratic one, which is a typographical error rather than an analytical
# choice, so it is corrected here. See the errata section of the report.

source(here::here("maintained", "helpers.R"))

# Every experiment and endorser arm, with the data each is estimated on ----
w3_endorse <- read_experiment("w3_endorse")
w5_endorse <- read_experiment("w5_endorse")
w6_vignette <- read_experiment("w6_vignette")

e1_arms <- c(
  trump = "Trump", fauci = "Fauci", trumpfauci = "Trump + Fauci",
  spiritual = "Spiritual/Religious Leader", insurance = "Health Insurance",
  pharmacy = "Pharmacy", physician = "Personal Physician"
)

e2_arms <- c(
  trump = "Trump", fauci = "Fauci", trumpfauci = "Trump + Fauci",
  obama = "Obama", biden = "Biden", biden_fauci = "Biden + Fauci",
  ramos = "Jorge Ramos", james = "Lebron James"
)

vignette_activities <- c("concert", "restaurant", "team", "trip")

# interaction_key is the column label the corresponding regression table uses,
# so the difference-in-CATEs and the interaction term are matched on an
# identifier rather than on the order the two tables happen to come out in.
experiments <- bind_rows(
  tibble(
    experiment = paste0("E-1: ", e1_arms),
    family = "Endorsement",
    interaction_key = paste0("S1|", c("Trump", "Fauci", "Trump and Fauci", "Spiritual leader",
                                      "Health Insurance", "Pharmacy", "Personal physician")),
    data = map(e1_arms, function(label) make_arm(w3_endorse, label))
  ),
  tibble(
    experiment = paste0("E-2: ", e2_arms),
    family = "Endorsement",
    interaction_key = paste0("S2|", c("Trump", "Fauci", "Trump and Fauci", "Obama", "Biden",
                                      "Biden and Fauci", "Jorge Ramos", "Lebron James")),
    data = map(e2_arms, function(label) make_arm(w5_endorse, label))
  ),
  tibble(
    experiment = c("G-1", "G-3"),
    family = "Guidance",
    interaction_key = c("Less restrictive guidance (G-1)", "More restrictive guidance (G-3)"),
    data = map(c("w6_cdcmask", "w7_cdcmask"), read_experiment)
  ),
  tibble(
    experiment = paste("G-2", vignette_activities),
    family = "Mandate vignette",
    interaction_key = str_to_title(vignette_activities),
    data = map(vignette_activities, function(a) {
      w6_vignette |>
        filter(scenario_exp_activity == a) |>
        mutate(treat = as.numeric(Z == "Treatment"))
    })
  ),
  tibble(
    experiment = c("I-1", "I-2", "I-3", "I-4", "I-5"),
    family = "Information",
    interaction_key = c("Contagiousness (I-1)", "Delta (I-2)", "Adult booster (I-3)",
                        "Child booster (I-4)", "Child vaccine (I-5)"),
    data = map(c("w7_contagiousness", "w7_doctordelta", "w8_adult_booster",
                 "w8_child_booster", "w8_child_vaccine"), read_experiment)
  )
)

stopifnot(anyDuplicated(experiments$interaction_key) == 0)

# Party is held out of the covariate list because it is constant within each
# fit, matching the within-party specification used for the figures.
cate_by_party <- function(data, party) {
  fit <- lm_lin(
    Y ~ treat,
    covariates = ols_formula_within_party,
    data = filter(data, pid7_collapsed == party),
    weights = weights,
    se_type = "HC1"
  )
  tidy(fit) |> filter(term == "treat") |> select(estimate, std.error)
}

# Two one-sided tests against a symmetric equivalence band.
tost_p <- function(estimate, std.error, bound) {
  pmax(
    pnorm((estimate + bound) / std.error, lower.tail = FALSE),
    pnorm((estimate - bound) / std.error, lower.tail = TRUE)
  )
}

equivalence <-
  experiments |>
  mutate(
    democrat = map(data, cate_by_party, party = "Democrat"),
    republican = map(data, cate_by_party, party = "Republican")
  ) |>
  select(-data) |>
  mutate(
    cate_dem = map_dbl(democrat, "estimate"),
    se_dem = map_dbl(democrat, "std.error"),
    cate_rep = map_dbl(republican, "estimate"),
    se_rep = map_dbl(republican, "std.error")
  ) |>
  select(-democrat, -republican) |>
  mutate(
    dic = cate_rep - cate_dem,
    se_dic = sqrt(se_dem^2 + se_rep^2),
    p_dic = 2 * pnorm(abs(dic / se_dic), lower.tail = FALSE),
    eq_p_5pp = tost_p(dic, se_dic, 0.05),
    eq_p_10pp = tost_p(dic, se_dic, 0.10),
    affirms_5pp = eq_p_5pp <= 0.05,
    affirms_10pp = eq_p_10pp <= 0.05,
    dic_significant = p_dic <= 0.05,
    # The deposit's standard error for the difference, kept alongside so the
    # effect of the correction is measurable rather than asserted.
    se_dic_deposited = sqrt(2 * se_rep^2),
    affirms_5pp_deposited = tost_p(dic, se_dic_deposited, 0.05) <= 0.05,
    affirms_10pp_deposited = tost_p(dic, se_dic_deposited, 0.10) <= 0.05
  )

write_csv(equivalence, file.path(output_dir, "text_equivalence_claims.csv"))

# The counts the article states in prose ----
# Each row is the computed truth value of a claim about how many experiments
# behave a given way, so a claim with no number of its own still has one here.
interaction_terms <- bind_rows(
  read_csv(file.path(output_dir, "table_a1_endorsement_1.csv"), show_col_types = FALSE) |>
    filter(term == "treat:pid_7_c") |>
    transmute(interaction_key = paste0("S1|", endorser), interaction = estimate, interaction_p = p.value),
  read_csv(file.path(output_dir, "table_a2_endorsement_2.csv"), show_col_types = FALSE) |>
    filter(term == "treat:pid_7_c") |>
    transmute(interaction_key = paste0("S2|", endorser), interaction = estimate, interaction_p = p.value),
  read_csv(file.path(output_dir, "table_2_guidance_experiments.csv"), show_col_types = FALSE) |>
    filter(term == "treat:pid_7_c") |>
    transmute(interaction_key = experiment, interaction = estimate, interaction_p = p.value),
  read_csv(file.path(output_dir, "table_a4_mandate_pooled.csv"), show_col_types = FALSE) |>
    filter(term == "ZTreatment:pid_7_c") |>
    transmute(interaction_key = column, interaction = estimate, interaction_p = p.value),
  read_csv(file.path(output_dir, "table_3_information_experiments.csv"), show_col_types = FALSE) |>
    filter(term == "treat:pid_7_c") |>
    transmute(interaction_key = experiment, interaction = estimate, interaction_p = p.value)
)

# The article compares the sign and significance of each interaction term with
# the sign and significance of the corresponding difference-in-CATEs.
stopifnot(anyDuplicated(interaction_terms$interaction_key) == 0)
stopifnot(setequal(equivalence$interaction_key, interaction_terms$interaction_key))

# "Sign and significance match" is read here as: the two agree on whether the
# heterogeneity is detectable, and where both detect it they agree on its
# direction. Two null estimates of opposite sign are not a disagreement about
# anything the article claims. The comparison is also made against the deposit's
# own standard error for the difference, so the effect of correcting it is
# visible rather than assumed.
discordant <- function(interaction, interaction_p, dic, dic_p) {
  interaction_significant <- interaction_p <= 0.05
  dic_significant <- dic_p <= 0.05
  (interaction_significant != dic_significant) |
    (interaction_significant & dic_significant & sign(interaction) != sign(dic))
}

agreement <-
  equivalence |>
  left_join(interaction_terms, by = "interaction_key") |>
  mutate(
    same_sign = sign(interaction) == sign(dic),
    same_significance = (interaction_p <= 0.05) == dic_significant,
    agrees = same_sign & same_significance,
    discordant_corrected = discordant(interaction, interaction_p, dic, p_dic),
    discordant_deposited = discordant(
      interaction, interaction_p, dic,
      2 * pnorm(abs(dic / se_dic_deposited), lower.tail = FALSE)
    )
  )

# Interactions that turn significant when the weights are dropped ----
# The article states a count of these, and no other output of this pipeline
# fits an unweighted model.
interaction_p_value <- function(data, use_weights) {
  fit <- if (use_weights) {
    lm_lin(Y ~ treat, covariates = ols_formula_covariates, data = data,
           weights = weights, se_type = "HC1")
  } else {
    lm_lin(Y ~ treat, covariates = ols_formula_covariates, data = data,
           se_type = "HC1")
  }
  tidy(fit) |> filter(term == "treat:pid_7_c") |> pull(p.value)
}

weighting <-
  experiments |>
  mutate(
    p_weighted = map_dbl(data, interaction_p_value, use_weights = TRUE),
    p_unweighted = map_dbl(data, interaction_p_value, use_weights = FALSE),
    turns_significant = p_weighted > 0.05 & p_unweighted <= 0.05
  ) |>
  select(-data)

write_csv(weighting, file.path(output_dir, "text_weighting_claims.csv"))

counts <- tribble(
  ~claim, ~value,
  "Interaction terms not significant weighted that become significant unweighted",
    sum(weighting$turns_significant),
  "Guidance experiments affirming equivalence at 5 points",
    sum(equivalence$affirms_5pp[equivalence$family == "Guidance"]),
  "Guidance experiments tested",
    sum(equivalence$family == "Guidance"),
  "Mandate vignettes affirming equivalence at 10 points",
    sum(equivalence$affirms_10pp[equivalence$family == "Mandate vignette"]),
  "Mandate vignettes tested",
    sum(equivalence$family == "Mandate vignette"),
  "Information experiments with a significant difference-in-CATEs",
    sum(equivalence$dic_significant[equivalence$family == "Information"]),
  "Information experiments affirming equivalence at 10 points",
    sum(equivalence$affirms_10pp[equivalence$family == "Information"]),
  "Information experiments tested",
    sum(equivalence$family == "Information"),
  "Experiments where the interaction term and the difference-in-CATEs disagree in sign or significance",
    sum(!agreement$agrees),
  "Experiments where the two disagree on significance alone",
    sum(!agreement$same_significance),
  "Experiments where the interaction term and the difference-in-CATEs are discordant",
    sum(agreement$discordant_corrected),
  "Experiments discordant under the deposit's standard error",
    sum(agreement$discordant_deposited),
  "Experiments compared",
    nrow(agreement),
  "Mandate vignettes affirming equivalence at 10 points under the deposit's standard error",
    sum(equivalence$affirms_10pp_deposited[equivalence$family == "Mandate vignette"]),
  "Guidance experiments affirming equivalence at 5 points under the deposit's standard error",
    sum(equivalence$affirms_5pp_deposited[equivalence$family == "Guidance"]),
  "Information experiments affirming equivalence at 10 points under the deposit's standard error",
    sum(equivalence$affirms_10pp_deposited[equivalence$family == "Information"])
)

write_csv(counts, file.path(output_dir, "text_equivalence_counts.csv"))
print(counts, n = nrow(counts))
