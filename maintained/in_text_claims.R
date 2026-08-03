# baxter-king_etal_2025/maintained/in_text_claims.R
# Output: printed to the console
# Depends on: everything in maintained/output/, plus the deposited clean data
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
# Claims that describe the design rather than a result get no pipeline output
# and none should be created for them. A count of experimental groups is checked
# against the deposited assignment column that defines those groups, a scale
# against its own levels, and a number the article takes from another work
# against that work. Those blocks read maintained/output/ or the deposited clean
# data and never fit anything.
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
section_c <- out("figure_a1_a22_experiment_panels.csv")
endorsement <- out("text_endorsement_claims.csv")
guidance <- out("text_guidance_information_claims.csv")
equivalence <- out("text_equivalence_claims.csv")
equivalence_counts <- out("text_equivalence_counts.csv")

# The deposited analysis files, read for the design claims only. Reading clean
# data is what lets a claim about the design be checked rather than restated.
w3_endorse <- read_experiment("w3_endorse")
w5_endorse <- read_experiment("w5_endorse")
w6_cdcmask <- read_experiment("w6_cdcmask")
w6_vignette <- read_experiment("w6_vignette")
w7_cdcmask <- read_experiment("w7_cdcmask")
w7_contagiousness <- read_experiment("w7_contagiousness")

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

claim(
  "abstract_n_experiments",
  sprintf("experiments in the directory table: %d (article: 10)",
          sum(table_1$experiment != "Any of the ten experiments"))
)

# The same sentence dates the study period. The deposited analysis files carry
# no field dates, so the two years are read from the wave dates the Materials
# and methods section prints and are not recoverable from the deposit.
claim(
  "abstract_period_years",
  "study period: 2 years, from the wave dates in the Materials and methods section; the deposited analysis files carry no field date"
)

# Significance Statement ----

# "Ten experiments conducted between 2020 and 2022 with 85,191 respondents on
# intentions to vaccinate and wear a mask show factual information and guidance
# can successfully encourage prosocial behavior among subjects from all partisan
# backgrounds but endorsements from political leaders and celebrities too
# frequently cause unintended decreases in prosocial behavior."
claim(
  "significance_years",
  "study period 2020 to 2022, from the wave dates in the Materials and methods section; the deposited analysis files carry no field date"
)

# Introduction ----

# "In this article, we report findings from 10 messaging experiments (with 41
# unique treatments) conducted among 85,191 survey respondents interviewed
# between 2020 and 2022."
#
# The deposit records the assignment of every respondent, so the treatment
# conditions can be counted rather than restated. The count depends on one
# reading: I-1 crosses three information sources with a detailed and a limited
# version of the message, and the deposited treat indicator marks only the three
# detailed cells as treated.
arms_deposited <- tribble(
  ~experiment, ~treatment_conditions,
  "E-1 endorser by Personal or Social framing",
  n_distinct(paste(w3_endorse$experiment_treatment_short, w3_endorse$experiment_arm)[w3_endorse$Z != "Control"]),
  "E-2 endorser",
  n_distinct(as.character(w5_endorse$Z)[w5_endorse$Z != "Control"]),
  "G-1 CDC guidance",
  n_distinct(as.character(w6_cdcmask$Z)[w6_cdcmask$Z != "Control"]),
  "G-2 activity by solo or friend framing",
  n_distinct(paste(w6_vignette$arm_activity, w6_vignette$arm_anchor_self_friend)[w6_vignette$Z == "Treatment"]),
  "G-3 CDC guidance",
  n_distinct(as.character(w7_cdcmask$Z)[w7_cdcmask$Z != "Control"]),
  "I-1 information source, detailed message only",
  n_distinct(as.character(w7_contagiousness$contagious_exp_assign_descriptive)[w7_contagiousness$treat == 1]),
  "I-2 Delta conversation", 1L,
  "I-3 adult booster information", 1L,
  "I-4 child booster information", 1L,
  "I-5 child vaccine information", 1L
)

i1_all_cells <- n_distinct(as.character(w7_contagiousness$contagious_exp_assign_descriptive))

print(arms_deposited)

claim(
  "intro_n_treatments",
  sprintf(paste("distinct treatment conditions in the deposited assignment columns: %d,",
                "or %d counting each of I-1's %d source-by-message cells as its own condition.",
                "The deposit records nothing that resolves the difference (article: 41)"),
          sum(arms_deposited$treatment_conditions),
          sum(arms_deposited$treatment_conditions) -
            arms_deposited$treatment_conditions[arms_deposited$experiment == "I-1 information source, detailed message only"] +
            i1_all_cells,
          i1_all_cells)
)

# "For example, 89% of Democrats and 84% of Republicans approved of canceling
# large gatherings and similarly high percentages approved of restricting travel
# (2)."
claim("intro_democrats_approve",
      "89% of Democrats approving of canceling large gatherings: from Sides, Tausanovitch and Vavreck (2020), reference 2, and outside this pipeline")
claim("intro_republicans_approve",
      "84% of Republicans approving of canceling large gatherings: from the same source, reference 2")

# "In December of 2023, an interdisciplinary group of scholars summarized the
# insights from 747 articles written about behavioral science and COVID-19
# policy-making (9). Our results are in line with their broad conclusions, and
# we provide detailed evidence regarding one of their 15 claims, namely that
# 'Identifying trusted sources ... can be effective in increasing intentions to
# engage in recommended health behaviors.'"
claim("intro_articles_summarised",
      "747 articles summarized: from Ruggeri et al. (2024), reference 9, and outside this pipeline")
claim("intro_claims_summarised",
      "15 claims: from the same source, reference 9")

# Materials and methods ----

# "The project consists of eight survey waves spanning more than 2 years. The
# first four waves, consisting of 15,000 respondents each, were conducted
# between 2020 May 11 and 24; 2020 July 9 and 22; 2020 October 1 and 17; and
# 2020 December 4 and 16. Waves five through eight, which consist of 30,000
# respondents each, were conducted 2021 March 25 to April 13, 2021 June 17 to
# July 6, 2021 September 3 to October 4, and 2022 October 24 to December 20."
#
# The deposit ships experiment subsets rather than waves, so no wave-level count
# can be read off it directly. What it does give is a lower bound: the largest
# deposited file drawn from each wave.
deposited_by_wave <-
  tibble(file = list.files(data_dir, pattern = "^data_w\\d_.*\\.rds$")) |>
  mutate(
    wave = str_extract(file, "(?<=^data_w)\\d"),
    n = map_int(file, function(f) nrow(read_rds(file.path(data_dir, f))))
  ) |>
  group_by(wave) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(wave, largest_deposited_file = file, n)

print(deposited_by_wave)

claim(
  "methods_n_waves",
  sprintf("survey waves: 8, of which the deposit ships analysis files from %d (waves %s)",
          nrow(deposited_by_wave), paste(deposited_by_wave$wave, collapse = ", "))
)

claim(
  "methods_early_wave_size",
  sprintf("waves one to four: 15,000 respondents each. The deposit reaches waves 3 and 4 only, whose largest deposited files hold %s and %s respondents, both lower bounds on the wave (article: 15,000)",
          format(deposited_by_wave$n[deposited_by_wave$wave == "3"], big.mark = ","),
          format(deposited_by_wave$n[deposited_by_wave$wave == "4"], big.mark = ","))
)

claim(
  "methods_late_wave_size",
  sprintf("waves five to eight: 30,000 respondents each. The largest deposited file from each is %s, and every one is an experiment subset rather than the wave (article: 30,000)",
          paste(sprintf("w%s %s", deposited_by_wave$wave[deposited_by_wave$wave >= "5"],
                        format(deposited_by_wave$n[deposited_by_wave$wave >= "5"], big.mark = ",")),
                collapse = ", "))
)

# "To get a sense of the representativeness of our data about COVID-19
# mitigation, in Spring of 2021, the CDC estimated that 44.6% of the adult US
# population had received at least one dose of a COVID-19 vaccine. Our survey
# estimate for this same period is close at 48.8%."
claim("methods_cdc_first_dose",
      "44.6% of US adults with at least one dose: the CDC's estimate for spring 2021, quoted by the article and outside this pipeline")

claim(
  "materials_and_methods_adults_with_at_least_one_vaccine_dose_spring_2021",
  "share of adults with at least one dose, spring 2021: not computable from the deposit, which ships experiment subsets and no wave-level file carrying vaccination status for a whole wave (article: 48.8%)"
)

# "Three of the partisan interaction terms that are not significant using
# weighted data become significant when unweighted data are used."
claim(
  "materials_and_methods_interaction_terms_not_significant_weighted_that_become_significant_unweighted",
  sprintf("interaction terms turning significant when the weights are dropped: %d of %d arms (article: 3)",
          named(equivalence_counts, "Interaction terms not significant weighted that become significant unweighted"),
          named(equivalence_counts, "Experiments compared"))
)

# "We measure partisanship posttreatment (on posttreatment placement, see (13))
# using the standard American National Election Study branching question that
# categorizes partisans on a scale from 1 (strong Democrat) to 4 (independent)
# to 7 (strong Republican)."
pid_levels <- levels(w6_cdcmask$demo_pid7)

claim(
  "methods_pid_scale",
  sprintf("party identification levels in the deposited data: %d, running 1 = %s, 4 = %s, 7 = %s (article: 1 strong Democrat to 4 independent to 7 strong Republican)",
          length(pid_levels), pid_levels[1], pid_levels[4], pid_levels[7])
)

# "We present tests with equivalence bands of 5 and 10 points."
equivalence_bands <-
  names(equivalence) |>
  str_subset("^eq_p_") |>
  str_remove("^eq_p_") |>
  str_remove("pp$")

claim(
  "methods_equivalence_bands",
  sprintf("equivalence bands tested in text_equivalence_claims.csv: %s percentage points (article: 5 and 10)",
          paste(equivalence_bands, collapse = " and "))
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

# "In E-1 set of endorsers included (i) their health insurance company, (ii)
# their pharmacy, (iii) their physician, (iv) religious/spiritual leaders, (v)
# President Donald Trump, (vi) Dr. Anthony Fauci, or (vii) both President Trump
# and Dr. Fauci; the control group saw no endorsement."
claim(
  "results_e1_endorsers",
  sprintf("E-1 endorser arms in the deposited assignment column: %d, and %d fitted (article: 7)",
          n_distinct(as.character(w3_endorse$Z)[w3_endorse$Z != "Control"]),
          n_distinct(table_a1$endorser))
)

# "In E-2, treatment group subjects could be assigned to any of eight endorsers:
# President Trump, Dr. Fauci, Trump and Fauci, NBA star LeBron James, Univision
# news anchor Jorge Ramos, President Barack Obama, President Joe Biden, and
# Biden and Fauci."
claim(
  "results_e2_endorsers",
  sprintf("E-2 endorser arms in the deposited assignment column: %d, and %d fitted (article: 8)",
          n_distinct(as.character(w5_endorse$Z)[w5_endorse$Z != "Control"]),
          n_distinct(table_a2$endorser))
)

# "We conducted a joint significance test of the null hypothesis that the
# effects of the endorsements do not vary according to the Personal vs Social
# variation (p = 0.37)."
claim(
  "results_endorsements_e_1_joint_test_of_the_personal_and_social_framings_p_value",
  sprintf("E-1 joint test of the Personal against the Social framing, p = %.2f (article: 0.37)",
          named(endorsement, "E-1 joint test of Personal versus Social framing, p-value"))
)

# "As discussed above, Trump's endorsement decreased intentions to vaccinate by
# more than 9 points (beta = -0.092, SE = 0.026) on average and polarized
# intentions by party identification."
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
# points (beta = 0.055, SE = 0.024) on average but, as a nonpolitical, medical
# expert, his endorsement did not have differential effects across partisan
# groups."
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
# points (beta = -0.097, SE = 0.036), with slightly more negative effects among
# Republicans and slightly less negative effects among Democrats."
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

# "Subjects were randomized into one of four activities: going to a restaurant,
# a concert, a sports game, and taking a trip. We further varied whether the
# respondent was asked to consider the activity for themselves, or for a friend
# who would really enjoy the activity."
claim(
  "results_g2_activities",
  sprintf("G-2 activities in the deposited assignment column: %d, and %d fitted (article: 4)",
          n_distinct(w6_vignette$arm_activity), n_distinct(table_a4$activity))
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

# "Both of these guidance experiments (G-1 and G-3) collapse a three point
# outcome scale to a binary scale based on the CDC information category." (Note e)
#
# The deposited outcome column carries four labels in each experiment, two of
# which are the same substantive category with and without the CDC preamble the
# treatment adds. Stripping that preamble is what leaves the three points.
mask_categories <- function(data) {
  data$mask_exp_combined_manual |>
    as.character() |>
    str_remove("^Following CDC recommendations, ") |>
    str_to_lower() |>
    n_distinct()
}

claim(
  "footnote_e_categories",
  sprintf("outcome categories once the CDC preamble is stripped: %d in G-1 and %d in G-3, from %d and %d raw labels (article: three point scale)",
          mask_categories(w6_cdcmask), mask_categories(w7_cdcmask),
          n_distinct(w6_cdcmask$mask_exp_combined_manual),
          n_distinct(w7_cdcmask$mask_exp_combined_manual))
)

# Results: information experiments ----

# "Roughly 2 in 10 adults remained unvaccinated and finding ways to reach this
# resistant population became a priority of policymakers and medical
# professionals."
claim("results_unvaccinated_share",
      "roughly 2 in 10 adults unvaccinated by September 2021: contextual, from public vaccination statistics, and outside this pipeline")

# "In this experiment, unvaccinated adults were randomly assigned to three
# groups and asked to imagine that someone (either a friend, their doctor, or
# the CDC) was giving them information about the effects of COVID-19 and its
# contagiousness."
claim(
  "results_i1_arms",
  sprintf("I-1 information sources in the deposited assignment column: %d (%s) (article: 3)",
          n_distinct(w7_contagiousness$contagious_exp_arm),
          paste(sort(unique(as.character(w7_contagiousness$contagious_exp_arm))), collapse = ", "))
)

# "In the treatment group, we offered respondents a more detailed description of
# the contagiousness, asking, 'Imagine [a friend, your doctor, the CDC] mentions
# that over 90% of Americans in the hospital right now due to COVID-19 are
# unvaccinated.'"
claim("results_i1_treatment_text",
      "over 90% of hospitalized Americans unvaccinated: treatment wording, checked against the survey instrument in appendix section F.3 and not a quantity this pipeline estimates")

# "In experiment I-1, 20% said they would get vaccinated at the request of a
# doctor, friend, or public health agency."
claim(
  "results_information_i_1_control_group_willing_to_be_vaccinated",
  sprintf("I-1 covariate-adjusted control group mean: %.0f%% (article: 20%%)",
          100 * named(guidance, "I-1 covariate-adjusted control group mean"))
)

# "For example, in the Delta experiment (I-2), 28% of unvaccinated people
# reported that if they were in their doctor's office and the doctor was urging
# them to get vaccinated, they would either get the vaccine that day or make an
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

# "Among adults, as discussed earlier in the detailed figure, the increased
# information increased intentions to get the booster by 8.3 points (beta =
# 0.083, SE = 0.013) net of other factors, with no evidence of heterogeneity by
# party."
claim(
  "results_information_i_3_effect_on_booster_intentions_percentage_points",
  sprintf("I-3 average treatment effect: %.1f points, standard error %.3f (article: 8.3 points, SE 0.013)",
          100 * named(guidance, "I-3 average treatment effect"),
          named(guidance, "I-3 standard error"))
)

# "Though nearly 60% of vaccinated parents intended to get their children a
# booster even without the treatment, providing the additional information about
# the expected upcoming Winter surge increased parents' intentions to boost
# their vaccinated children by 16.2 points (beta = 0.162, SE = 0.032)."
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

# Appendix ----

# "Full distributional comparisons with associated chi-squared tests show
# differences between treatment and control groups as well (see Appendix Fig.
# S47)." (Note e)
claim(
  "appendix_figure_s47_g_1_chi_squared_statistic_on_the_three_category_outcome",
  sprintf("G-1 chi-squared statistic on the three-category outcome: %.2f, p = %.2g (Appendix Figure S47: 111.78)",
          named(guidance, "G-1 chi-squared statistic on the three-category outcome"),
          named(guidance, "G-1 chi-squared p-value"))
)

cat("\nAppendix Figures S1 to S22: one two-panel figure per experimental contrast. Each prints these estimates and standard errors in percentage points, and they appear in no table.\n")
print(
  section_c |>
    select(float, contrast, subgroup, estimator, entry),
  n = nrow(section_c)
)

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
