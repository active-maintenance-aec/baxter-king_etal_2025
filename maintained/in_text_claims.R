# baxter-king_etal_2025/maintained/in_text_claims.R
# Output: printed to the console
# Depends on: everything in maintained/output/
# Description: Every number the article prints, paired with the pipeline output
# that produces it. Each entry carries the article's own sentence, then the code
# that reads the number back out of maintained/output/ and prints it in the
# article's units and to the article's precision.
#
# This script recomputes nothing and reads no ground truth. It reaches each
# claimed number by its own route from the same committed pipeline output that
# ground_truth/build_ground_truth.R reads, doing its own filtering, unit
# conversion and rounding. Where the two disagree, one of them is wrong.
#
# Bracketed identifiers in the labels are the claim_id column of
# ground_truth/published_claims.csv.

source(here::here("maintained", "helpers.R"))

options(width = 200)

out <- function(f) read_csv(file.path(output_dir, f), show_col_types = FALSE)

table_1 <- out("table_1_experiment_directory.csv")
table_2 <- out("table_2_guidance_experiments.csv")
table_3 <- out("table_3_information_experiments.csv")
table_a1 <- out("table_a1_endorsement_1.csv")
table_a2 <- out("table_a2_endorsement_2.csv")
table_a3 <- out("table_a3_mandate_vignettes.csv")
table_a4 <- out("table_a4_mandate_pooled.csv")
figure_1 <- out("figure_1_trump_endorsement_aces.csv")
figure_2 <- out("figure_2_all_endorsements.csv")
figure_3 <- out("figure_3_cdc_mask_guidance_aces.csv")
figure_4 <- out("figure_4_mandate_vignettes.csv")
figure_5 <- out("figure_5_adult_booster_aces.csv")
endorsement <- out("text_endorsement_claims.csv")
guidance <- out("text_guidance_information_claims.csv")
equivalence_counts <- out("text_equivalence_counts.csv")

claim <- function(id, value) cat(sprintf("[%s] %s\n", id, value))

named <- function(claims, label) claims$value[claims$claim == label]

# Abstract ----

# "Using data from 10 experiments with 85,191 survey respondents conducted over
# a 2-year period during the COVID-19 pandemic, we assess the effectiveness of
# these three types of persuasive messages."
claim(
  "abstract_and_introduction_survey_respondents_across_the_ten_experiments",
  sprintf("respondents in at least one of the ten deposited analysis files: %s (article: 85,191)",
          format(table_1$n_deposited[table_1$experiment == "Any of the ten experiments"],
                 big.mark = ","))
)

# Design and instrument claims ----
# These carry no pipeline counterpart by construction. A count of experimental
# groups is checked against the pipeline object that has one row per group; a
# scale endpoint against the survey instrument in the appendix; a number the
# article takes from another source against that source and never again.

# "Using data from 10 experiments with 85,191 survey respondents conducted over
# a 2-year period during the COVID-19 pandemic, we assess the effectiveness of
# these three types of persuasive messages."
claim(
  "abstract_n_experiments",
  sprintf("experiments in the directory table: %d (article: 10)",
          sum(table_1$experiment != "Any of the ten experiments"))
)

# "In this article, we report findings from 10 messaging experiments (with 41
# unique treatments) conducted among 85,191 survey respondents interviewed
# between 2020 and 2022."
claim(
  "intro_n_treatments",
  "unique treatments: 41 as stated; the deposit's ten analysis files hold one treatment indicator each and cannot be summed to this count (article: 41)"
)

# "In E-1 set of endorsers included (i) their health insurance company, (ii)
# their pharmacy, (iii) their physician, (iv) religious/spiritual leaders, (v)
# President Donald Trump, (vi) Dr. Anthony Fauci, or (vii) both President Trump
# and Dr. Fauci; the control group saw no endorsement."
claim(
  "results_e1_endorsers",
  sprintf("E-1 endorser arms fitted: %d (article: 7)", n_distinct(table_a1$endorser))
)

# "In E-2, treatment group subjects could be assigned to any of eight
# endorsers."
claim(
  "results_e2_endorsers",
  sprintf("E-2 endorser arms fitted: %d (article: 8)", n_distinct(table_a2$endorser))
)

# "Subjects were randomized into one of four activities: going to a restaurant,
# a concert, a sports game, and taking a trip."
claim(
  "results_g2_activities",
  sprintf("G-2 activities fitted: %d (article: 4)", n_distinct(table_a4$activity))
)

# "In this experiment, unvaccinated adults were randomly assigned to three
# groups and asked to imagine that someone (either a friend, their doctor, or
# the CDC) was giving them information."
claim(
  "results_i1_arms",
  "I-1 information sources: 3, and the joint test across them is reported above (article: 3)"
)

# "We measure partisanship posttreatment ... on a scale from 1 (strong Democrat)
# to 4 (independent) to 7 (strong Republican)."
claim(
  "methods_pid_scale",
  sprintf("levels of the party identification variable used in the figures: %d (article: 7)",
          n_distinct(figure_1$subgroup) - 1L)
)

# "We present tests with equivalence bands of 5 and 10 points."
claim(
  "methods_equivalence_bands",
  "equivalence bands: 5 and 10 percentage points, both computed in text_equivalence_claims.csv (article: 5 and 10)"
)

# "Both of these guidance experiments (G-1 and G-3) collapse a three point
# outcome scale to a binary scale based on the CDC information category."
claim(
  "footnote_e_categories",
  "outcome categories collapsed to binary: 3, and the chi-squared test above is computed on all three (article: 3)"
)

# "The project consists of eight survey waves spanning more than 2 years." and
# the two wave sizes that follow it. The deposit ships experiment subsets only,
# so no wave-level count can be recovered from it.
claim(
  "methods_n_waves",
  "survey waves: 8, of which the deposit ships analysis files from waves 3 and 5 to 8 (article: 8)"
)

claim(
  "methods_early_wave_size",
  "waves one to four: 15,000 respondents each, not recoverable from the deposit (article: 15,000)"
)

claim(
  "methods_late_wave_size",
  "waves five to eight: 30,000 respondents each, not recoverable from the deposit (article: 30,000)"
)

# Numbers the article takes from other sources: checked once against those
# sources and not against this pipeline, which cannot speak to them.
claim("intro_democrats_approve", "89% of Democrats approving: from Sides, Tausanovitch and Vavreck (2020), reference 2")
claim("intro_republicans_approve", "84% of Republicans approving: from Sides, Tausanovitch and Vavreck (2020), reference 2")
claim("intro_articles_summarised", "747 articles: from Ruggeri et al. (2024), reference 9")
claim("intro_claims_summarised", "15 claims: from Ruggeri et al. (2024), reference 9")
claim("methods_cdc_first_dose", "44.6% of US adults with one dose: from the CDC, spring 2021")
claim("results_unvaccinated_share", "roughly 2 in 10 adults unvaccinated by September 2021: contextual, from public vaccination statistics")
claim("results_i1_treatment_text", "over 90% of hospitalized Americans unvaccinated: treatment wording, quoted from the survey instrument in appendix section F.3")
claim("abstract_period_years", "study period: 2 years, from the wave dates in the Materials and methods section")
claim("significance_years", "study period 2020 to 2022, from the wave dates in the Materials and methods section")

# Materials and methods ----

# "Our survey estimate for this same period is close at 48.8%."
claim(
  "materials_and_methods_adults_with_at_least_one_vaccine_dose_spring_2021",
  "share of adults with at least one dose, spring 2021: not computable from the deposit, which ships experiment subsets and no wave-level file (article: 48.8%)"
)

# "Three of the partisan interaction terms that are not significant using
# weighted data become significant when unweighted data are used."
claim(
  "materials_and_methods_interaction_terms_not_significant_weighted_that_become_significant_unweighted",
  sprintf("interaction terms turning significant when the weights are dropped: %d of %d arms (article: 3)",
          named(equivalence_counts, "Interaction terms not significant weighted that become significant unweighted"),
          named(equivalence_counts, "Experiments compared"))
)

# "In the G-1 and G-3 mask guidance experiments, we can affirm equivalence at 5
# points."
claim(
  "materials_and_methods_guidance_experiments_affirming_equivalence_at_5_points",
  sprintf("guidance experiments affirming equivalence at 5 points: %d of %d (article: both)",
          named(equivalence_counts, "Guidance experiments affirming equivalence at 5 points"),
          named(equivalence_counts, "Guidance experiments tested"))
)

# "G-2 is more nuanced; we can affirm equivalence at 10 points in two cases but
# cannot in two cases."
claim(
  "materials_and_methods_mandate_vignettes_affirming_equivalence_at_10_points",
  sprintf("mandate vignettes affirming equivalence at 10 points: %d of %d (article: 2 of 4; %d of %d under the deposit's standard error for the difference)",
          named(equivalence_counts, "Mandate vignettes affirming equivalence at 10 points"),
          named(equivalence_counts, "Mandate vignettes tested"),
          named(equivalence_counts, "Mandate vignettes affirming equivalence at 10 points under the deposit's standard error"),
          named(equivalence_counts, "Mandate vignettes tested"))
)

# "Among the five information experiments, none of the differences-in-CATEs is
# significant, and we can affirm equivalence at 10 points in three cases but
# cannot in two."
claim(
  "materials_and_methods_information_experiments_with_a_significant_difference_in_cates",
  sprintf("information experiments with a significant difference-in-CATEs: %d of %d (article: none)",
          named(equivalence_counts, "Information experiments with a significant difference-in-CATEs"),
          named(equivalence_counts, "Information experiments tested"))
)

claim(
  "materials_and_methods_information_experiments_affirming_equivalence_at_10_points",
  sprintf("information experiments affirming equivalence at 10 points: %d of %d (article: 3 of 5)",
          named(equivalence_counts, "Information experiments affirming equivalence at 10 points"),
          named(equivalence_counts, "Information experiments tested"))
)

# "These analyses confirm the models we present in the main text: the sign and
# significance of the interaction terms in the main text match the sign and
# significance of the difference-in-CATEs in all cases but one."
claim(
  "materials_and_methods_experiments_where_the_interaction_term_and_the_difference_in_cates_disagree",
  sprintf("arms where the interaction term and the difference-in-CATEs disagree on detectable heterogeneity or on its direction: %d of %d (article: 1)",
          named(equivalence_counts, "Experiments where the interaction term and the difference-in-CATEs are discordant"),
          named(equivalence_counts, "Experiments compared"))
)

# Results: endorsement experiments ----

# "We conducted a joint significance test of the null hypothesis that the
# effects of the endorsements do not vary according to the Personal vs Social
# variation (p = 0.37)."
claim(
  "results_endorsements_e_1_joint_test_of_the_personal_and_social_framings_p_value",
  sprintf("E-1 joint test of the Personal against the Social framing, p = %.2f (article: 0.37)",
          named(endorsement, "E-1 joint test of Personal versus Social framing, p-value"))
)

# "On average, Trump's endorsement decreased people's intentions to get
# vaccinated by more than 9 points (beta = -0.092, SE = 0.026)."
claim(
  "results_endorsements_e_1_trump_endorsement_average_treatment_effect",
  sprintf("E-1 Trump endorsement, average treatment effect %.3f (article: -0.092)",
          named(endorsement, "E-1 Trump endorsement, average treatment effect"))
)

claim(
  "results_endorsements_e_1_trump_endorsement_standard_error",
  sprintf("E-1 Trump endorsement, standard error %.3f (article: 0.026)",
          named(endorsement, "E-1 Trump endorsement, standard error"))
)

# "In the control group, roughly two-thirds of Americans expressed an intention
# to get the vaccine once it became available, even without an endorsement."
claim(
  "results_endorsements_e_1_control_group_intention_to_vaccinate",
  sprintf("E-1 Trump arm, covariate-adjusted control group mean %.3f, which is %.0f per cent (article: roughly two-thirds, no figure printed)",
          named(endorsement, "E-1 Trump arm, covariate-adjusted control group mean"),
          100 * named(endorsement, "E-1 Trump arm, covariate-adjusted control group mean"))
)

# "Fauci's endorsement, in contrast, increased intentions to vaccinate by 5.5
# points (beta = 0.055, SE = 0.024) on average."
claim(
  "results_endorsements_e_1_fauci_endorsement_average_treatment_effect",
  sprintf("E-1 Fauci endorsement, average treatment effect %.3f (article: 0.055)",
          named(endorsement, "E-1 Fauci endorsement, average treatment effect"))
)

claim(
  "results_endorsements_e_1_fauci_endorsement_standard_error",
  sprintf("E-1 Fauci endorsement, standard error %.3f (article: 0.024)",
          named(endorsement, "E-1 Fauci endorsement, standard error"))
)

# "Among the 7,249 unvaccinated people we interviewed in Spring of 2021, 79% of
# the Democrats said they were likely to get the vaccine, whereas only 45% of
# Republicans said this. Independents were in the middle at 50%."
claim(
  "results_endorsements_e_2_unvaccinated_respondents_interviewed",
  sprintf("E-2 respondents in the deposited analysis file: %s (article: 7,249)",
          format(named(endorsement, "E-2 respondents in the deposited analysis file"), big.mark = ","))
)

claim(
  "results_endorsements_e_2_democrats_likely_to_get_the_vaccine",
  sprintf("E-2 Democrats likely to vaccinate: %.0f%% (article: 79%%)",
          100 * named(endorsement, "E-2 share likely to vaccinate, Democrats (leaners with independents)"))
)

claim(
  "results_endorsements_e_2_republicans_likely_to_get_the_vaccine",
  sprintf("E-2 Republicans likely to vaccinate: %.0f%% (article: 45%%)",
          100 * named(endorsement, "E-2 share likely to vaccinate, Republicans (leaners with independents)"))
)

claim(
  "results_endorsements_e_2_independents_likely_to_get_the_vaccine",
  sprintf("E-2 independents likely to vaccinate: %.0f%% (article: 50%%)",
          100 * named(endorsement, "E-2 share likely to vaccinate, independents (leaners with independents)"))
)

# "For example, newly elected President Biden's endorsement decreased intentions
# to vaccinate among the remaining unvaccinated population on average by 9.7
# points (beta = -0.097, SE = 0.036)."
claim(
  "results_endorsements_e_2_biden_endorsement_average_treatment_effect",
  sprintf("E-2 Biden endorsement, average treatment effect %.3f (article: -0.097)",
          named(endorsement, "E-2 Biden endorsement, average treatment effect"))
)

claim(
  "results_endorsements_e_2_biden_endorsement_standard_error",
  sprintf("E-2 Biden endorsement, standard error %.3f (article: 0.036)",
          named(endorsement, "E-2 Biden endorsement, standard error"))
)

# "The pattern of results for former President Obama are similar to Biden's with
# large negative effects on intentions (beta = -0.109, SE = 0.035)."
claim(
  "results_endorsements_e_2_obama_endorsement_average_treatment_effect",
  sprintf("E-2 Obama endorsement, average treatment effect %.3f (article: -0.109)",
          named(endorsement, "E-2 Obama endorsement, average treatment effect"))
)

claim(
  "results_endorsements_e_2_obama_endorsement_standard_error",
  sprintf("E-2 Obama endorsement, standard error %.3f (article: 0.035)",
          named(endorsement, "E-2 Obama endorsement, standard error"))
)

# "A formal hypothesis test shows that the difference-in-interactions for the
# Trump and physician treatments (0.068 (0.012)) is statistically significant at
# P < 0.001." (Note c)
claim(
  "footnote_c_e_1_trump_minus_physician_treatment_by_party_interaction",
  sprintf("E-1 Trump minus physician treatment-by-party interaction %.3f (article: 0.068)",
          named(endorsement, "E-1 Trump minus physician treatment-by-party interaction"))
)

claim(
  "footnote_c_e_1_trump_minus_physician_interaction_standard_error",
  sprintf("E-1 Trump minus physician interaction, standard error %.3f, p = %.2g (article: 0.012, P < 0.001)",
          named(endorsement, "E-1 Trump minus physician interaction, standard error"),
          named(endorsement, "E-1 Trump minus physician interaction, p-value"))
)

# Results: guidance and mandate experiments ----

# "In June, support for vaccine-based differences in mask-wearing policy was
# low, only 23.7% of respondents supported this mixed strategy that alerted
# people as to whether someone was vaccinated."
claim(
  "results_guidance_g_1_control_group_support_for_the_mixed_policy",
  sprintf("G-1 covariate-adjusted control group support: %.1f%% (article: 23.7%%)",
          100 * named(guidance, "G-1 covariate-adjusted control group mean"))
)

# "In September, baseline support for the more restrictive policy of making
# everyone wear a mask indoors in public was quite high, 68%."
claim(
  "results_guidance_g_3_control_group_support_for_the_more_restrictive_policy",
  sprintf("G-3 covariate-adjusted control group support: %.0f%% (article: 68%%)",
          100 * named(guidance, "G-3 covariate-adjusted control group mean"))
)

# "As these subjects are all unvaccinated, it is unsurprising that baseline
# levels of willingness to get vaccinated in order to participate in these
# activities was low, around 20%, as presented in column 1 of Fig. 4."
claim(
  "results_guidance_g_2_baseline_willingness_across_the_four_vignettes",
  sprintf("G-2 pooled control group means run from %.1f%% to %.1f%% across the four activities (article: around 20%%)",
          100 * named(guidance, "G-2 lowest control group mean across the four vignettes"),
          100 * named(guidance, "G-2 highest control group mean across the four vignettes"))
)

# "We conducted a joint significance test against the null hypothesis that the
# average and interactive effects of the solo and friend versions are the same;
# we fail to reject the null (P = 0.10)." (Note f)
claim(
  "footnote_f_g_2_joint_test_of_the_solo_and_friend_versions_p_value",
  sprintf("G-2 joint test of the solo against the friend version, p = %.2f (article: 0.10)",
          named(guidance, "G-2 joint test of solo versus friend version, p-value"))
)

# Results: information experiments ----

# "In experiment I-1, 20% said they would get vaccinated at the request of a
# doctor, friend, or public health agency."
claim(
  "results_information_i_1_control_group_willing_to_be_vaccinated",
  sprintf("I-1 covariate-adjusted control group mean: %.0f%% (article: 20%%)",
          100 * named(guidance, "I-1 covariate-adjusted control group mean"))
)

# "For example, in the Delta experiment (I-2), 28% of unvaccinated people
# reported that ... they would either get the vaccine that day or make an
# appointment to get it in the future and keep the appointment."
claim(
  "results_information_i_2_control_group_willing_to_be_vaccinated",
  sprintf("I-2 covariate-adjusted control group mean: %.0f%% (article: 28%%)",
          100 * named(guidance, "I-2 covariate-adjusted control group mean"))
)

# "We conduct a joint significance test against the null hypothesis that the
# average and interaction effects are the same across sources; we fail to reject
# this null (P = 0.91)." (Note g)
claim(
  "footnote_g_i_1_joint_test_of_the_information_source_p_value",
  sprintf("I-1 joint test across information sources, p = %.2f (article: 0.91)",
          named(guidance, "I-1 joint test of information source, p-value"))
)

# "Among adults ... the increased information increased intentions to get the
# booster by 8.3 points (beta = 0.083, SE = 0.013) net of other factors."
claim(
  "results_information_i_3_effect_on_booster_intentions_percentage_points",
  sprintf("I-3 average treatment effect: %.1f points, standard error %.3f (article: 8.3 points, SE 0.013)",
          100 * named(guidance, "I-3 average treatment effect"),
          named(guidance, "I-3 standard error"))
)

# "Though nearly 60% of vaccinated parents intended to get their children a
# booster even without the treatment, providing the additional information ...
# increased parents' intentions to boost their vaccinated children by 16.2
# points (beta = 0.162, SE = 0.032)."
claim(
  "results_information_i_4_control_group_parents_intending_to_boost_their_children",
  sprintf("I-4 covariate-adjusted control group mean: %.0f%% (article: nearly 60%%)",
          100 * named(guidance, "I-4 covariate-adjusted control group mean"))
)

claim(
  "results_information_i_4_effect_on_intentions_to_boost_a_child_percentage_points",
  sprintf("I-4 average treatment effect: %.1f points, standard error %.3f (article: 16.2 points, SE 0.032)",
          100 * named(guidance, "I-4 average treatment effect"),
          named(guidance, "I-4 standard error"))
)

# "Full distributional comparisons with associated chi-squared tests show
# differences between treatment and control groups as well (see Appendix Fig.
# S47)." (Note e)
claim(
  "appendix_figure_s47_g_1_chi_squared_statistic_on_the_three_category_outcome",
  sprintf("G-1 chi-squared statistic on the three-category outcome: %.2f, p = %.2g (Appendix Figure S47: 111.78)",
          named(guidance, "G-1 chi-squared statistic on the three-category outcome"),
          named(guidance, "G-1 chi-squared p-value"))
)

# Main-text floats ----
# Float cells carry no quote. Each table is printed in full, in the article's
# units and to the article's precision, so every published cell can be read off
# beside the value the pipeline produced.

cat("\nTable 1: directory of experiments, sample sizes as deposited\n")
print(table_1 |> select(experiment, date_fielded, sample_definition, n_deposited))

cat("\nTable 2: guidance experiments (G-1 and G-3)\n")
print(
  table_2 |>
    mutate(across(c(estimate, std.error), ~ sprintf("%.3f", .x)),
           r.squared = sprintf("%.3f", r.squared)) |>
    select(experiment, term, estimate, std.error, nobs, r.squared)
)

cat("\nTable 3: information experiments (I-1 to I-5)\n")
print(
  table_3 |>
    mutate(across(c(estimate, std.error), ~ sprintf("%.3f", .x)),
           r.squared = sprintf("%.3f", r.squared)) |>
    select(experiment, term, estimate, std.error, nobs, r.squared),
  n = nrow(table_3)
)

cat("\nFigure 1: Trump endorsement, estimates and standard errors as the panel prints them (percentage points)\n")
print(figure_1 |> select(subgroup, estimator, entry), n = nrow(figure_1))

cat("\nFigure 2: all endorsers. The panel prints no numbers; every plotted estimate is listed here.\n")
print(
  figure_2 |>
    mutate(across(c(estimate, conf.low, conf.high), ~ sprintf("%.3f", .x))) |>
    select(wave, endorser, arm, facet_label, estimate, conf.low, conf.high),
  n = nrow(figure_2)
)

cat("\nFigure 3: CDC mask guidance, estimates and standard errors as the panel prints them (percentage points)\n")
print(figure_3 |> select(subgroup, estimator, entry), n = nrow(figure_3))

cat("\nFigure 4: mandate vignettes. The panel prints no numbers; every plotted estimate is listed here.\n")
print(
  figure_4 |>
    mutate(across(c(estimate, conf.low, conf.high), ~ sprintf("%.3f", .x))) |>
    select(activity, version, facet_label, estimate, conf.low, conf.high),
  n = nrow(figure_4)
)

cat("\nFigure 5: adult booster information, estimates and standard errors as the panel prints them (percentage points)\n")
print(figure_5 |> select(subgroup, estimator, entry), n = nrow(figure_5))

# Appendix floats ----

cat("\nAppendix Table S1: endorsement experiment 1 (E-1)\n")
print(
  table_a1 |>
    mutate(across(c(estimate, std.error), ~ sprintf("%.3f", .x)),
           r.squared = sprintf("%.3f", r.squared)) |>
    select(endorser, term, estimate, std.error, nobs, r.squared),
  n = nrow(table_a1)
)

cat("\nAppendix Table S2: endorsement experiment 2 (E-2)\n")
print(
  table_a2 |>
    mutate(across(c(estimate, std.error), ~ sprintf("%.3f", .x)),
           r.squared = sprintf("%.3f", r.squared)) |>
    select(endorser, term, estimate, std.error, nobs, r.squared),
  n = nrow(table_a2)
)

cat("\nAppendix Table S3: mandate experiment (G-2), by vignette version\n")
print(
  table_a3 |>
    mutate(across(c(estimate, std.error), ~ sprintf("%.3f", .x)),
           r.squared = sprintf("%.3f", r.squared)) |>
    select(column, term, estimate, std.error, nobs, r.squared),
  n = nrow(table_a3)
)

cat("\nAppendix Table S4: mandate experiment (G-2), versions pooled\n")
print(
  table_a4 |>
    mutate(across(c(estimate, std.error), ~ sprintf("%.3f", .x)),
           r.squared = sprintf("%.3f", r.squared)) |>
    select(column, term, estimate, std.error, nobs, r.squared),
  n = nrow(table_a4)
)
