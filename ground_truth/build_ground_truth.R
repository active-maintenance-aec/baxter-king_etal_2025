# baxter-king_etal_2025/ground_truth/build_ground_truth.R
# Output: ground_truth/baxter-king_etal_2025_ground_truth.csv,
#         ground_truth/float_coverage.csv
# Depends on: ground_truth/archive_values.csv (extract_archive_values.R),
#             maintained/output/ (run run_all.R first)
# Description: Assemble the ground truth table. Every published value here was
#   read from the article PDF or its online appendix and is used only as a
#   comparison target; no published number is an input to any computation here
#   or anywhere in maintained/. value_script is read back from what the
#   deposited code produced and value_rewrite from maintained/output/, so
#   neither column can drift from the code that made it.
#
#   value_paper is a character column holding the digits the article prints, in
#   the article's own units, because a double does not record how many decimals
#   were printed and 0.80 and 0.8 are the same double. A value agrees when the
#   pipeline's number, printed to the page's precision, gives the same digits.
#   Thousands separators are written as commas. Tables 2 and 3 of the article
#   print theirs as full stops (30.751 for 30,751), which no parser can tell
#   from a decimal point; that typesetting is recorded in the notes of the rows
#   concerned rather than carried into the comparison.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

options(width = 200)

paper_id <- "baxter-king_etal_2025"

archive <- read_csv(here::here("ground_truth", "archive_values.csv"), show_col_types = FALSE)

out <- function(f) read_csv(here::here("maintained", "output", f), show_col_types = FALSE)

table_1_rewrite <- out("table_1_experiment_directory.csv")
table_2_rewrite <- out("table_2_guidance_experiments.csv")
table_3_rewrite <- out("table_3_information_experiments.csv")
table_a1_rewrite <- out("table_a1_endorsement_1.csv")
table_a2_rewrite <- out("table_a2_endorsement_2.csv")
table_a3_rewrite <- out("table_a3_mandate_vignettes.csv")
table_a4_rewrite <- out("table_a4_mandate_pooled.csv")
figure_1_rewrite <- out("figure_1_trump_endorsement_aces.csv")
figure_2_rewrite <- out("figure_2_all_endorsements.csv")
figure_3_rewrite <- out("figure_3_cdc_mask_guidance_aces.csv")
figure_4_rewrite <- out("figure_4_mandate_vignettes.csv")
figure_5_rewrite <- out("figure_5_adult_booster_aces.csv")
text_endorsement <- out("text_endorsement_claims.csv")
text_guidance <- out("text_guidance_information_claims.csv")
text_equivalence <- out("text_equivalence_counts.csv")

# Selectors ----
# A ground-truth row is always selected by an identifier, never by the value the
# article prints, so a selector cannot guarantee its own match.
archive_value <- function(object_name, term_name, column) {
  row <- archive |> filter(object == object_name, term == term_name)
  stopifnot(nrow(row) <= 1)
  if (nrow(row) == 0) return(NA_real_)
  row[[column]][1]
}

text_value <- function(claims, claim_text) {
  hit <- claims$value[claims$claim == claim_text]
  stopifnot(length(hit) == 1)
  hit
}

# The one join used everywhere in this file. It asserts the key is unique on the
# right, which is what stops a join multiplying rows; that the row count
# survives; and that every published row found a pipeline counterpart. A
# published value that silently finds nothing is indistinguishable from a
# quantity the pipeline does not produce, and that is the failure this guard
# exists to make impossible. unique_x = FALSE is for a lookup, where the left
# side repeats its key by construction (a table column has four terms).
join_checked <- function(x, y, by, unique_x = TRUE) {
  if (unique_x) stopifnot(anyDuplicated(x[by]) == 0)
  stopifnot(anyDuplicated(y[by]) == 0)
  joined <- left_join(x, y, by = by)
  stopifnot(nrow(joined) == nrow(x))
  stopifnot(nrow(semi_join(x, y, by = by)) == nrow(x))
  joined
}

# Agreement at the precision the article prints ----
paper_numeric <- function(paper) as.numeric(str_remove_all(paper, "[,%[:space:]]"))

paper_decimals <- function(paper) {
  fraction <- str_extract(str_remove_all(paper, "[,%[:space:]]"), "(?<=\\.)\\d+$")
  if_else(is.na(fraction), 0L, nchar(fraction))
}

print_to_paper <- function(value, paper) {
  decimals <- replace_na(paper_decimals(paper), 0L)
  printed <- sprintf(paste0("%.", decimals, "f"), value)
  printed[is.na(value) | is.na(paper)] <- NA_character_
  # A rounded negative that lands on zero prints as -0.000 and must not count as
  # a disagreement with a published 0.000.
  str_replace(printed, "^-(0\\.?0*)$", "\\1")
}

agrees <- function(value, paper) {
  case_when(
    is.na(value) | is.na(paper) ~ NA_real_,
    print_to_paper(value, paper) == print_to_paper(paper_numeric(paper), paper) ~ 1,
    .default = 0
  )
}

# Table 1: directory of experiments ----
# The article's N column counts respondents enrolled in each experiment. The
# deposited analysis files are smaller, and no deposited script assembles the
# table, so value_script is empty throughout.
table_1_paper <- tribble(
  ~experiment, ~dataset, ~value_paper,
  "E-1: Vaccine endorsement 1", "w3_endorse", "14,946",
  "E-2: Vaccine endorsement 2", "w5_endorse", "7,249",
  "G-1: CDC mask guidance 1", "w6_cdcmask", "30,857",
  "G-2: Vaccine mandate vignettes", "w6_vignette", "10,298",
  "G-3: CDC mask guidance 2", "w7_cdcmask", "33,088",
  "I-1: Contagiousness conversation", "w7_contagiousness", "8,710",
  "I-2: Delta variant conversation", "w7_doctordelta", "8,710",
  "I-3: Bivalent booster information", "w8_adult_booster", "10,700",
  "I-4: Bivalent booster information (children)", "w8_child_booster", "1,628",
  "I-5: Holiday surge information (children)", "w8_child_vaccine", "1,715"
)

table_1_rows <-
  table_1_paper |>
  join_checked(table_1_rewrite |> select(dataset, n_deposited), by = "dataset") |>
  transmute(
    table_figure = "Table 1",
    claim = paste0(experiment, ", N"),
    value_script = NA_real_,
    value_paper,
    value_rewrite = n_deposited,
    expect_rewrite = TRUE,
    defect_locus = "archive",
    notes = "The deposit contains no script that assembles Table 1, and its analysis file for this experiment holds fewer rows than the published enrollment count."
  )

headline_n_row <-
  tibble(
    table_figure = "Abstract and Introduction",
    claim = "Survey respondents across the ten experiments",
    value_script = NA_real_,
    value_paper = "85,191",
    value_rewrite = table_1_rewrite$n_deposited[table_1_rewrite$experiment == "Any of the ten experiments"],
    expect_rewrite = TRUE,
    defect_locus = "archive"
  ) |>
  mutate(
    # The direction is computed rather than typed, because it is the substantive
    # point and it runs opposite to every other sample-size row in this table:
    # the deposit holds more respondents than the headline count, not fewer.
    notes = paste0(
      "No deposited script computes the headline count. The union of the respondent identifiers in the ten deposited analysis files is ",
      if_else(value_rewrite > paper_numeric(value_paper), "larger", "smaller"),
      " than the published figure, so the deposit does not contain the file from which it was counted."
    )
  )

# Regression tables ----
# Tables 2, 3, S1, S2, S3 and S4 all have the same shape: a coefficient and a
# standard error for four terms, then a sample size and an R-squared. One
# assembler builds all six from a published transcription keyed on column label
# and term.
term_label <- c(
  "(Intercept)" = "Intercept",
  "treat" = "Treatment",
  "ZTreatment" = "Treatment",
  "pid_7_c" = "Party ID (7-Point)",
  "treat:pid_7_c" = "Treatment x Party ID",
  "ZTreatment:pid_7_c" = "Treatment x Party ID"
)

regression_rows <- function(paper, key, rewrite, label, note = "") {
  coefficients <-
    paper |>
    pivot_longer(c(estimate, std.error), names_to = "quantity", values_to = "value_paper") |>
    join_checked(key, by = "column", unique_x = FALSE) |>
    join_checked(
      rewrite |>
        pivot_longer(c(estimate, std.error), names_to = "quantity", values_to = "value_rewrite") |>
        select(rewrite_label, term, quantity, value_rewrite),
      by = c("rewrite_label", "term", "quantity")
    ) |>
    mutate(
      claim = paste0(column, ", ", term_label[term], ", ",
                     if_else(quantity == "estimate", "estimate", "standard error")),
      value_script = pmap_dbl(list(archive_object, archive_prefix, term, quantity),
                              function(o, p, t, q) {
        archive_value(o, str_squish(paste(p, t)), if_else(q == "estimate", "estimate", "std.error"))
      })
    )

  fit_statistics <-
    key |>
    cross_join(tibble(quantity = c("nobs", "r.squared"))) |>
    join_checked(
      rewrite |>
        distinct(rewrite_label, nobs, r.squared) |>
        pivot_longer(c(nobs, r.squared), names_to = "quantity", values_to = "value_rewrite"),
      by = c("rewrite_label", "quantity")
    ) |>
    join_checked(
      gof_paper |> filter(table == label) |> select(column, quantity, value_paper),
      by = c("column", "quantity")
    ) |>
    mutate(
      claim = paste0(column, ", ", if_else(quantity == "nobs", "Num.Obs.", "R2")),
      value_script = pmap_dbl(list(archive_object, gof_term, quantity), function(o, t, q) {
        archive_value(o, t, if_else(q == "nobs", "nobs", "r.squared"))
      })
    )

  bind_rows(coefficients, fit_statistics) |>
    mutate(
      table_figure = label,
      expect_rewrite = TRUE,
      defect_locus = NA_character_,
      notes = note
    ) |>
    select(table_figure, claim, value_script, value_paper, value_rewrite,
           expect_rewrite, defect_locus, notes)
}

thousands_note <- "The published table prints the sample size with a full stop as the thousands separator."

gof_paper <- tribble(
  ~table, ~column, ~quantity, ~value_paper,
  "Table 2", "G-1", "nobs", "30,751",
  "Table 2", "G-1", "r.squared", "0.073",
  "Table 2", "G-3", "nobs", "32,933",
  "Table 2", "G-3", "r.squared", "0.153",
  "Table 3", "I-1", "nobs", "8,659",
  "Table 3", "I-1", "r.squared", "0.165",
  "Table 3", "I-2", "nobs", "8,690",
  "Table 3", "I-2", "r.squared", "0.207",
  "Table 3", "I-3", "nobs", "10,677",
  "Table 3", "I-3", "r.squared", "0.197",
  "Table 3", "I-4", "nobs", "1,626",
  "Table 3", "I-4", "r.squared", "0.198",
  "Table 3", "I-5", "nobs", "1,711",
  "Table 3", "I-5", "r.squared", "0.146",
  "Appendix Table S1", "Trump", "nobs", "3680",
  "Appendix Table S1", "Trump", "r.squared", "0.179",
  "Appendix Table S1", "Fauci", "nobs", "3665",
  "Appendix Table S1", "Fauci", "r.squared", "0.202",
  "Appendix Table S1", "Trump and Fauci", "nobs", "3725",
  "Appendix Table S1", "Trump and Fauci", "r.squared", "0.176",
  "Appendix Table S1", "Spiritual leader", "nobs", "3678",
  "Appendix Table S1", "Spiritual leader", "r.squared", "0.166",
  "Appendix Table S1", "Health Insurance", "nobs", "3733",
  "Appendix Table S1", "Health Insurance", "r.squared", "0.170",
  "Appendix Table S1", "Pharmacy", "nobs", "3735",
  "Appendix Table S1", "Pharmacy", "r.squared", "0.170",
  "Appendix Table S1", "Personal physician", "nobs", "3766",
  "Appendix Table S1", "Personal physician", "r.squared", "0.163",
  "Appendix Table S2", "Trump", "nobs", "1604",
  "Appendix Table S2", "Trump", "r.squared", "0.165",
  "Appendix Table S2", "Fauci", "nobs", "1596",
  "Appendix Table S2", "Fauci", "r.squared", "0.230",
  "Appendix Table S2", "Trump and Fauci", "nobs", "1543",
  "Appendix Table S2", "Trump and Fauci", "r.squared", "0.228",
  "Appendix Table S2", "Obama", "nobs", "1613",
  "Appendix Table S2", "Obama", "r.squared", "0.244",
  "Appendix Table S2", "Biden", "nobs", "1563",
  "Appendix Table S2", "Biden", "r.squared", "0.233",
  "Appendix Table S2", "Biden and Fauci", "nobs", "1567",
  "Appendix Table S2", "Biden and Fauci", "r.squared", "0.229",
  "Appendix Table S2", "Jorge Ramos", "nobs", "1586",
  "Appendix Table S2", "Jorge Ramos", "r.squared", "0.263",
  "Appendix Table S2", "Lebron James", "nobs", "1634",
  "Appendix Table S2", "Lebron James", "r.squared", "0.243",
  "Appendix Table S3", "Concert (F)", "nobs", "1270",
  "Appendix Table S3", "Concert (F)", "r.squared", "0.244",
  "Appendix Table S3", "Restaurant (F)", "nobs", "1322",
  "Appendix Table S3", "Restaurant (F)", "r.squared", "0.173",
  "Appendix Table S3", "Team (F)", "nobs", "1234",
  "Appendix Table S3", "Team (F)", "r.squared", "0.199",
  "Appendix Table S3", "Trip (F)", "nobs", "1316",
  "Appendix Table S3", "Trip (F)", "r.squared", "0.172",
  "Appendix Table S3", "Concert (S)", "nobs", "1283",
  "Appendix Table S3", "Concert (S)", "r.squared", "0.229",
  "Appendix Table S3", "Restaurant (S)", "nobs", "1362",
  "Appendix Table S3", "Restaurant (S)", "r.squared", "0.194",
  "Appendix Table S3", "Team (S)", "nobs", "1224",
  "Appendix Table S3", "Team (S)", "r.squared", "0.213",
  "Appendix Table S3", "Trip (S)", "nobs", "1264",
  "Appendix Table S3", "Trip (S)", "r.squared", "0.151",
  "Appendix Table S4", "Concert", "nobs", "2553",
  "Appendix Table S4", "Concert", "r.squared", "0.215",
  "Appendix Table S4", "Restaurant", "nobs", "2684",
  "Appendix Table S4", "Restaurant", "r.squared", "0.153",
  "Appendix Table S4", "Team", "nobs", "2458",
  "Appendix Table S4", "Team", "r.squared", "0.174",
  "Appendix Table S4", "Trip", "nobs", "2580",
  "Appendix Table S4", "Trip", "r.squared", "0.129"
) |>
  select(table, column, quantity, value_paper)

# Table 2: guidance experiments ----
table_2_paper <- tribble(
  ~column, ~term, ~estimate, ~std.error,
  "G-1", "(Intercept)", "0.237", "0.005",
  "G-1", "treat", "0.060", "0.007",
  "G-1", "pid_7_c", "0.003", "0.002",
  "G-1", "treat:pid_7_c", "-0.001", "0.003",
  "G-3", "(Intercept)", "0.680", "0.005",
  "G-3", "treat", "0.015", "0.007",
  "G-3", "pid_7_c", "-0.053", "0.002",
  "G-3", "treat:pid_7_c", "0.000", "0.003"
)

table_2_key <- tibble(
  column = c("G-1", "G-3"),
  archive_object = c("w6_fit_cdcmask", "w7_fit_cdcmask"),
  archive_prefix = "",
  gof_term = "treat",
  rewrite_label = c("Less restrictive guidance (G-1)", "More restrictive guidance (G-3)")
)

table_2_rows <- regression_rows(
  table_2_paper, table_2_key,
  table_2_rewrite |> rename(rewrite_label = experiment),
  "Table 2"
)

# Table 3: information experiments ----
table_3_paper <- tribble(
  ~column, ~term, ~estimate, ~std.error,
  "I-1", "(Intercept)", "0.197", "0.009",
  "I-1", "treat", "-0.010", "0.013",
  "I-1", "pid_7_c", "-0.032", "0.006",
  "I-1", "treat:pid_7_c", "0.008", "0.008",
  "I-2", "(Intercept)", "0.277", "0.010",
  "I-2", "treat", "0.016", "0.014",
  "I-2", "pid_7_c", "-0.045", "0.006",
  "I-2", "treat:pid_7_c", "0.009", "0.009",
  "I-3", "(Intercept)", "0.556", "0.010",
  "I-3", "treat", "0.083", "0.013",
  "I-3", "pid_7_c", "-0.061", "0.005",
  "I-3", "treat:pid_7_c", "0.000", "0.006",
  "I-4", "(Intercept)", "0.598", "0.024",
  "I-4", "treat", "0.162", "0.032",
  "I-4", "pid_7_c", "-0.058", "0.013",
  "I-4", "treat:pid_7_c", "0.019", "0.017",
  "I-5", "(Intercept)", "0.391", "0.024",
  "I-5", "treat", "0.057", "0.034",
  "I-5", "pid_7_c", "-0.056", "0.011",
  "I-5", "treat:pid_7_c", "-0.001", "0.017"
)

table_3_key <- tibble(
  column = c("I-1", "I-2", "I-3", "I-4", "I-5"),
  archive_object = c("w7_fit_contagiousness", "w7_fit_doctordelta", "w8_fit_adult_booster",
                     "w8_fit_child_booster", "w8_fit_child_vaccine"),
  archive_prefix = "",
  gof_term = "treat",
  rewrite_label = c("Contagiousness (I-1)", "Delta (I-2)", "Adult booster (I-3)",
                    "Child booster (I-4)", "Child vaccine (I-5)")
)

table_3_rows <- regression_rows(
  table_3_paper, table_3_key,
  table_3_rewrite |> rename(rewrite_label = experiment),
  "Table 3"
)

# Appendix Table S1: endorsement experiment 1 ----
table_a1_paper <- tribble(
  ~column, ~term, ~estimate, ~std.error,
  "Trump", "(Intercept)", "0.653", "0.018",
  "Trump", "treat", "-0.092", "0.026",
  "Trump", "pid_7_c", "-0.008", "0.008",
  "Trump", "treat:pid_7_c", "0.069", "0.011",
  "Fauci", "(Intercept)", "0.650", "0.018",
  "Fauci", "treat", "0.055", "0.024",
  "Fauci", "pid_7_c", "-0.008", "0.008",
  "Fauci", "treat:pid_7_c", "-0.013", "0.011",
  "Trump and Fauci", "(Intercept)", "0.652", "0.018",
  "Trump and Fauci", "treat", "-0.035", "0.025",
  "Trump and Fauci", "pid_7_c", "-0.008", "0.008",
  "Trump and Fauci", "treat:pid_7_c", "0.022", "0.012",
  "Spiritual leader", "(Intercept)", "0.652", "0.018",
  "Spiritual leader", "treat", "-0.047", "0.025",
  "Spiritual leader", "pid_7_c", "-0.008", "0.008",
  "Spiritual leader", "treat:pid_7_c", "0.022", "0.011",
  "Health Insurance", "(Intercept)", "0.650", "0.018",
  "Health Insurance", "treat", "0.033", "0.025",
  "Health Insurance", "pid_7_c", "-0.008", "0.008",
  "Health Insurance", "treat:pid_7_c", "0.008", "0.011",
  "Pharmacy", "(Intercept)", "0.645", "0.018",
  "Pharmacy", "treat", "0.039", "0.025",
  "Pharmacy", "pid_7_c", "-0.008", "0.008",
  "Pharmacy", "treat:pid_7_c", "-0.006", "0.011",
  "Personal physician", "(Intercept)", "0.649", "0.018",
  "Personal physician", "treat", "0.024", "0.025",
  "Personal physician", "pid_7_c", "-0.008", "0.008",
  "Personal physician", "treat:pid_7_c", "0.003", "0.011"
)

table_a1_key <- tibble(
  column = c("Trump", "Fauci", "Trump and Fauci", "Spiritual leader",
             "Health Insurance", "Pharmacy", "Personal physician"),
  archive_object = paste0("w3_fit_", c("trump", "fauci", "trumpfauci", "spiritual",
                                       "insurance", "pharmacy", "physician")),
  archive_prefix = "",
  gof_term = "treat"
) |>
  mutate(rewrite_label = column)

table_a1_rows <- regression_rows(
  table_a1_paper, table_a1_key,
  table_a1_rewrite |> rename(rewrite_label = endorser),
  "Appendix Table S1"
)

# Appendix Table S2: endorsement experiment 2 ----
table_a2_paper <- tribble(
  ~column, ~term, ~estimate, ~std.error,
  "Trump", "(Intercept)", "0.636", "0.025",
  "Trump", "treat", "-0.047", "0.036",
  "Trump", "pid_7_c", "-0.061", "0.012",
  "Trump", "treat:pid_7_c", "0.033", "0.018",
  "Fauci", "(Intercept)", "0.629", "0.026",
  "Fauci", "treat", "-0.032", "0.035",
  "Fauci", "pid_7_c", "-0.061", "0.012",
  "Fauci", "treat:pid_7_c", "0.014", "0.018",
  "Trump and Fauci", "(Intercept)", "0.640", "0.025",
  "Trump and Fauci", "treat", "-0.064", "0.036",
  "Trump and Fauci", "pid_7_c", "-0.061", "0.012",
  "Trump and Fauci", "treat:pid_7_c", "0.001", "0.017",
  "Obama", "(Intercept)", "0.627", "0.026",
  "Obama", "treat", "-0.109", "0.035",
  "Obama", "pid_7_c", "-0.061", "0.012",
  "Obama", "treat:pid_7_c", "0.001", "0.017",
  "Biden", "(Intercept)", "0.630", "0.026",
  "Biden", "treat", "-0.097", "0.036",
  "Biden", "pid_7_c", "-0.061", "0.012",
  "Biden", "treat:pid_7_c", "-0.019", "0.018",
  "Biden and Fauci", "(Intercept)", "0.624", "0.026",
  "Biden and Fauci", "treat", "-0.056", "0.036",
  "Biden and Fauci", "pid_7_c", "-0.061", "0.012",
  "Biden and Fauci", "treat:pid_7_c", "-0.006", "0.018",
  "Jorge Ramos", "(Intercept)", "0.633", "0.025",
  "Jorge Ramos", "treat", "-0.055", "0.036",
  "Jorge Ramos", "pid_7_c", "-0.061", "0.012",
  "Jorge Ramos", "treat:pid_7_c", "-0.012", "0.018",
  "Lebron James", "(Intercept)", "0.623", "0.026",
  "Lebron James", "treat", "-0.030", "0.035",
  "Lebron James", "pid_7_c", "-0.061", "0.012",
  "Lebron James", "treat:pid_7_c", "0.006", "0.017"
)

table_a2_key <- tibble(
  column = c("Trump", "Fauci", "Trump and Fauci", "Obama", "Biden",
             "Biden and Fauci", "Jorge Ramos", "Lebron James"),
  archive_object = paste0("w5_fit_", c("trump", "fauci", "trumpfauci", "obama", "biden",
                                       "biden_fauci", "ramos", "james")),
  archive_prefix = "",
  gof_term = "treat"
) |>
  mutate(rewrite_label = column)

table_a2_rows <- regression_rows(
  table_a2_paper, table_a2_key,
  table_a2_rewrite |> rename(rewrite_label = endorser),
  "Appendix Table S2"
)

# Appendix Table S3: mandate vignettes by version ----
table_a3_paper <- tribble(
  ~column, ~term, ~estimate, ~std.error,
  "Concert (F)", "(Intercept)", "0.204", "0.023",
  "Concert (F)", "ZTreatment", "0.006", "0.030",
  "Concert (F)", "pid_7_c", "-0.052", "0.015",
  "Concert (F)", "ZTreatment:pid_7_c", "0.008", "0.019",
  "Restaurant (F)", "(Intercept)", "0.165", "0.022",
  "Restaurant (F)", "ZTreatment", "0.056", "0.032",
  "Restaurant (F)", "pid_7_c", "-0.020", "0.013",
  "Restaurant (F)", "ZTreatment:pid_7_c", "-0.022", "0.019",
  "Team (F)", "(Intercept)", "0.201", "0.022",
  "Team (F)", "ZTreatment", "0.059", "0.032",
  "Team (F)", "pid_7_c", "-0.036", "0.011",
  "Team (F)", "ZTreatment:pid_7_c", "-0.042", "0.017",
  "Trip (F)", "(Intercept)", "0.238", "0.026",
  "Trip (F)", "ZTreatment", "0.074", "0.036",
  "Trip (F)", "pid_7_c", "-0.018", "0.015",
  "Trip (F)", "ZTreatment:pid_7_c", "-0.007", "0.021",
  "Concert (S)", "(Intercept)", "0.172", "0.020",
  "Concert (S)", "ZTreatment", "-0.007", "0.027",
  "Concert (S)", "pid_7_c", "-0.052", "0.012",
  "Concert (S)", "ZTreatment:pid_7_c", "0.014", "0.015",
  "Restaurant (S)", "(Intercept)", "0.142", "0.019",
  "Restaurant (S)", "ZTreatment", "0.071", "0.030",
  "Restaurant (S)", "pid_7_c", "-0.046", "0.013",
  "Restaurant (S)", "ZTreatment:pid_7_c", "0.039", "0.019",
  "Team (S)", "(Intercept)", "0.160", "0.018",
  "Team (S)", "ZTreatment", "0.040", "0.027",
  "Team (S)", "pid_7_c", "-0.038", "0.010",
  "Team (S)", "ZTreatment:pid_7_c", "-0.008", "0.016",
  "Trip (S)", "(Intercept)", "0.221", "0.023",
  "Trip (S)", "ZTreatment", "0.058", "0.034",
  "Trip (S)", "pid_7_c", "-0.035", "0.013",
  "Trip (S)", "ZTreatment:pid_7_c", "-0.010", "0.020"
)

table_a3_key <- tibble(
  column = c("Concert (F)", "Restaurant (F)", "Team (F)", "Trip (F)",
             "Concert (S)", "Restaurant (S)", "Team (S)", "Trip (S)"),
  activity = rep(c("concert", "restaurant", "team", "trip"), 2),
  version = rep(c("friend", "solo"), each = 4)
) |>
  mutate(
    archive_object = "w6_fit_vignette",
    archive_prefix = paste(activity, version),
    gof_term = paste(activity, version, "ZTreatment"),
    rewrite_label = column
  ) |>
  select(column, archive_object, archive_prefix, gof_term, rewrite_label)

table_a3_rows <- regression_rows(
  table_a3_paper,
  table_a3_key,
  table_a3_rewrite |> rename(rewrite_label = column),
  "Appendix Table S3"
)

# Appendix Table S4: mandate vignettes pooled over versions ----
table_a4_paper <- tribble(
  ~column, ~term, ~estimate, ~std.error,
  "Concert", "(Intercept)", "0.188", "0.015",
  "Concert", "ZTreatment", "0.001", "0.021",
  "Concert", "pid_7_c", "-0.053", "0.010",
  "Concert", "ZTreatment:pid_7_c", "0.013", "0.012",
  "Restaurant", "(Intercept)", "0.154", "0.015",
  "Restaurant", "ZTreatment", "0.060", "0.022",
  "Restaurant", "pid_7_c", "-0.033", "0.010",
  "Restaurant", "ZTreatment:pid_7_c", "0.006", "0.014",
  "Team", "(Intercept)", "0.179", "0.015",
  "Team", "ZTreatment", "0.054", "0.022",
  "Team", "pid_7_c", "-0.032", "0.008",
  "Team", "ZTreatment:pid_7_c", "-0.033", "0.012",
  "Trip", "(Intercept)", "0.228", "0.018",
  "Trip", "ZTreatment", "0.072", "0.025",
  "Trip", "pid_7_c", "-0.027", "0.011",
  "Trip", "ZTreatment:pid_7_c", "-0.008", "0.015"
)

table_a4_key <- tibble(
  column = c("Concert", "Restaurant", "Team", "Trip"),
  archive_object = "w6_fit_vignette_pooled",
  archive_prefix = paste(c("concert", "restaurant", "team", "trip"), "pooled"),
  gof_term = paste(c("concert", "restaurant", "team", "trip"), "pooled ZTreatment"),
  rewrite_label = c("Concert", "Restaurant", "Team", "Trip")
)

table_a4_rows <- regression_rows(
  table_a4_paper, table_a4_key,
  table_a4_rewrite |> rename(rewrite_label = column),
  "Appendix Table S4"
)

# The archive saves the vignette models as tidy summaries rather than fitted
# objects, so it records neither a sample size nor an R-squared for Tables S3
# and S4. That is a property of the deposit, and it leaves value_script empty on
# those rows rather than filled in from somewhere else.
table_a3_rows <- table_a3_rows |>
  mutate(notes = if_else(
    str_detect(claim, "Num.Obs.|R2"),
    "The deposit saves this model as a tidy table of coefficients, so it records no sample size or R-squared.",
    notes
  ))

table_a4_rows <- table_a4_rows |>
  mutate(notes = if_else(
    str_detect(claim, "Num.Obs.|R2"),
    "The deposit saves this model as a tidy table of coefficients, so it records no sample size or R-squared. The deposit also has no script that assembles Table S4.",
    notes
  ))

# Figures 1, 3 and 5: every estimate and standard error printed on the panel ----
# Values are percentage points, as printed. Read off rendered pages of the
# article: the figures are rasterized and these numbers appear nowhere else.
figure_1_paper <- tribble(
  ~subgroup, ~dim_estimate, ~dim_std.error, ~ols_estimate, ~ols_std.error,
  "Full sample", "-9.5", "2.8", "-9.2", "2.6",
  "Strong Republican", "16.2", "5.3", "14.7", "4.4",
  "Weak Republican", "3.0", "7.7", "5.8", "6.3",
  "Lean Republican", "1.4", "11.2", "-10.6", "10.1",
  "Independent", "-5.2", "7.2", "-1.3", "6.1",
  "Lean Democrat", "-19.6", "9.1", "-18.5", "7.4",
  "Weak Democrat", "-31.5", "8.0", "-34.0", "6.6",
  "Strong Democrat", "-29.4", "5.6", "-27.2", "5.0"
)

figure_3_paper <- tribble(
  ~subgroup, ~dim_estimate, ~dim_std.error, ~ols_estimate, ~ols_std.error,
  "Full sample", "6.0", "0.7", "6.0", "0.7",
  "Strong Republican", "4.6", "1.7", "4.9", "1.6",
  "Weak Republican", "4.5", "2.3", "5.2", "2.2",
  "Lean Republican", "12.4", "2.7", "11.5", "2.6",
  "Independent", "6.6", "1.7", "6.6", "1.7",
  "Lean Democrat", "9.4", "2.7", "8.8", "2.7",
  "Weak Democrat", "2.9", "2.2", "3.6", "2.1",
  "Strong Democrat", "5.2", "1.4", "5.3", "1.4"
)

figure_5_paper <- tribble(
  ~subgroup, ~dim_estimate, ~dim_std.error, ~ols_estimate, ~ols_std.error,
  "Full sample", "9.1", "1.5", "8.3", "1.3",
  "Strong Republican", "5.3", "3.4", "5.5", "3.2",
  "Weak Republican", "10.1", "4.0", "9.7", "3.5",
  "Lean Republican", "8.6", "4.8", "7.9", "4.2",
  "Independent", "6.1", "3.8", "4.8", "3.3",
  "Lean Democrat", "12.3", "4.5", "12.5", "3.8",
  "Weak Democrat", "15.7", "3.8", "17.1", "3.5",
  "Strong Democrat", "6.3", "2.5", "4.6", "2.3"
)

figure_rows <- function(paper, rewrite, archive_object, label) {
  paper |>
    pivot_longer(-subgroup, names_to = c("estimator", "quantity"), names_sep = "_",
                 values_to = "value_paper") |>
    mutate(estimator = str_to_upper(estimator)) |>
    join_checked(
      rewrite |>
        pivot_longer(c(estimate, std.error), names_to = "quantity", values_to = "value_rewrite") |>
        select(subgroup, estimator, quantity, value_rewrite),
      by = c("subgroup", "estimator", "quantity")
    ) |>
    mutate(
      table_figure = label,
      claim = paste0(subgroup, ", ", estimator, ", ",
                     if_else(quantity == "estimate", "estimate (pp)", "standard error (pp)")),
      value_script = pmap_dbl(list(subgroup, estimator, quantity), function(s, e, q) {
        archive_value(archive_object, paste(s, e), if_else(q == "estimate", "estimate", "std.error"))
      }),
      expect_rewrite = TRUE,
      defect_locus = NA_character_,
      notes = ""
    ) |>
    select(table_figure, claim, value_script, value_paper, value_rewrite,
           expect_rewrite, defect_locus, notes)
}

figure_1_rows <- figure_rows(figure_1_paper, figure_1_rewrite, "figure_1_aces", "Figure 1")
figure_3_rows <- figure_rows(figure_3_paper, figure_3_rewrite, "figure_3_aces", "Figure 3")
figure_5_rows <- figure_rows(figure_5_paper, figure_5_rewrite, "figure_5_aces", "Figure 5")

# Appendix Figures S1 to S22: every number the section C panels print ----
# Section C draws one two-panel figure per experimental contrast, in the same
# form as Figures 1, 3 and 5, and each prints eight subgroups by two estimators
# by an estimate and a standard error. Those 704 numbers are in no table: the
# appendix regression tables carry one treatment by party interaction per
# contrast, not eight conditional estimates. They are parsed once by
# ground_truth/extract_published_appendix_values.R and summarised here, one row
# per figure reading cells reproduced of cells printed.
published_appendix <- read_csv(here::here("ground_truth", "published_appendix_values.csv"),
                               col_types = cols(.default = "c"))

section_c_rewrite <- out("figure_a1_a22_experiment_panels.csv")

# The parse is checked against three independent transcriptions before it is
# used. Appendix Figures S1, S16 and S20 are the same panels as main-text
# Figures 1, 3 and 5, whose values were read off rendered pages by hand above,
# so all 96 of those cells must agree digit for digit or the parse is wrong.
parse_check <-
  bind_rows(
    figure_1_paper |> mutate(float = "Appendix Figure S1"),
    figure_3_paper |> mutate(float = "Appendix Figure S16"),
    figure_5_paper |> mutate(float = "Appendix Figure S20")
  ) |>
  pivot_longer(-c(subgroup, float), names_to = c("estimator", "quantity"),
               names_sep = "_", values_to = "transcribed") |>
  mutate(estimator = str_to_upper(estimator)) |>
  join_checked(
    published_appendix |>
      pivot_longer(c(value_estimate, value_std.error),
                   names_to = "quantity", values_to = "parsed") |>
      mutate(quantity = str_remove(quantity, "^value_")) |>
      select(float, subgroup, estimator, quantity, parsed),
    by = c("float", "subgroup", "estimator", "quantity")
  )

stopifnot(nrow(parse_check) == 3 * 8 * 2 * 2, parse_check$parsed == parse_check$transcribed)

section_c_cells <-
  published_appendix |>
  pivot_longer(c(value_estimate, value_std.error),
               names_to = "quantity", values_to = "value_paper") |>
  mutate(quantity = str_remove(quantity, "^value_")) |>
  join_checked(
    section_c_rewrite |>
      pivot_longer(c(estimate, std.error), names_to = "quantity", values_to = "value_rewrite") |>
      select(float, subgroup, estimator, quantity, value_rewrite),
    by = c("float", "subgroup", "estimator", "quantity")
  ) |>
  mutate(
    value_script = pmap_dbl(list(float, subgroup, estimator, quantity),
                            function(f, s, e, q) {
      archive_value(paste0("figure_", str_to_lower(str_remove(f, "Appendix Figure ")), "_aces"),
                    paste(s, e), if_else(q == "estimate", "estimate", "std.error"))
    }),
    cell_agrees = agrees(value_rewrite, value_paper),
    cell_agrees_script = agrees(value_script, value_paper)
  )

write_csv(section_c_cells |> select(float, contrast, subgroup, estimator, quantity,
                                    value_paper, value_rewrite, cell_agrees),
          here::here("ground_truth", "section_c_cells.csv"))

# Cells whose published value depends on which collinear column was dropped ----
# In the Strong Republican cell of each E-2 endorsement contrast the AAPI
# category is empty in one of the two arms. That makes four columns of the Lin
# design exactly collinear, because the centred AAPI covariate and its treatment
# interaction satisfy AAPI_c - Z * AAPI_c = -mean(AAPI) * (1 - Z) to the last
# bit. The fit is the same whichever column is dropped, with identical fitted
# values and an identical R squared, but the treatment coefficient is not, so
# the printed estimate is whichever column the decomposition happened to alias.
# estimatr 1.0.6 produced the published numbers and dropped the covariate main
# effect; 2.0 follows stats::lm() and drops the later column, the interaction.
# Neither is wrong: the quantity lm_lin reports, the effect at the covariate
# means, is not identified in these cells under any version. The locus is
# environment because the published value records a version of the software
# rather than a decision in the article or in the deposit.
section_c_unidentified <-
  section_c_cells |>
  filter(!is.na(cell_agrees), cell_agrees == 0) |>
  group_by(float) |>
  summarize(
    moved = paste(paste(subgroup, estimator, quantity), collapse = ", "),
    .groups = "drop"
  )

section_c_rows <-
  section_c_cells |>
  group_by(float, contrast) |>
  summarize(
    cells = n(),
    cells_rewrite = sum(cell_agrees, na.rm = TRUE),
    cells_script = sum(cell_agrees_script, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(float_number = as.integer(str_extract(float, "\\d+"))) |>
  arrange(float_number) |>
  left_join(section_c_unidentified, by = "float") |>
  transmute(
    table_figure = float,
    claim = paste0(contrast, ", printed estimates and standard errors reproduced"),
    value_script = cells_script,
    value_paper = as.character(cells),
    value_rewrite = cells_rewrite,
    expect_rewrite = TRUE,
    defect_locus = if_else(is.na(moved), NA_character_, "environment"),
    notes = if_else(
      is.na(moved),
      "The panel prints eight subgroups by two estimators, each as an estimate and a standard error in percentage points, and those numbers appear in no table. Cells are compared one by one in section_c_cells.csv.",
      paste0("The panel prints eight subgroups by two estimators, each as an estimate and a standard error in percentage points, and those numbers appear in no table. Cells are compared one by one in section_c_cells.csv. These cells do not match: ",
             moved,
             ". The AAPI category is empty in one arm of this contrast's Strong Republican cell, which makes the centred AAPI covariate, its treatment interaction, the intercept and the treatment indicator exactly collinear. The fit is the same whichever column is dropped, with identical fitted values and R squared, but the treatment coefficient is not, so the effect at the covariate means is not identified here. estimatr 1.0.6 produced the published number and dropped the covariate main effect; 2.0 follows stats::lm() and drops the interaction.")
    )
  )

# Figures with no printed numbers ----
# Both panels are coefficient plots without labels, so there is no published
# number to compare against. The rewrite writes every plotted estimate to a CSV,
# and the coefficients they show are the ones the appendix regression tables
# print, which the Table S1 to S4 rows above verify one by one.
unlabelled_figures <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~expect_rewrite, ~defect_locus, ~notes,
  "Figure 2", "Plotted coefficients and confidence intervals", NA, NA, NA, FALSE, NA,
    paste0("The panel prints no numbers. All ", nrow(figure_2_rewrite),
           " plotted estimates are written to figure_2_all_endorsements.csv; the coefficients are those of appendix Tables S1 and S2."),
  "Figure 2", "Series labels for the Personal and Social framings", NA, NA, NA, FALSE, "archive",
    "The deposited by-arm models name the 'A (Personal)' subset prosocial and the 'B (Social)' subset not_prosocial, and the deposited figure script then relabels prosocial as Social and not_prosocial as Personal, so the published panel attaches each framing's label to the other framing's estimates. The rewrite labels each series by the deposited experiment_arm value it was fitted on.",
  "Figure 4", "Plotted coefficients and confidence intervals", NA, NA, NA, FALSE, NA,
    paste0("The panel prints no numbers. All ", nrow(figure_4_rewrite),
           " plotted estimates are written to figure_4_mandate_vignettes.csv; the coefficients are those of appendix Tables S3 and S4.")
)

# Text and footnotes ----
text_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~expect_rewrite, ~defect_locus, ~notes,
  "Results, endorsements", "E-1 Trump endorsement, average treatment effect",
    archive_value("w3_fit_trump", "treat", "estimate"), "-0.092",
    text_value(text_endorsement, "E-1 Trump endorsement, average treatment effect"), TRUE, NA, "",
  "Results, endorsements", "E-1 Trump endorsement, standard error",
    archive_value("w3_fit_trump", "treat", "std.error"), "0.026",
    text_value(text_endorsement, "E-1 Trump endorsement, standard error"), TRUE, NA, "",
  "Results, endorsements", "E-1 control group intention to vaccinate",
    archive_value("w3_fit_trump", "(Intercept)", "estimate"), NA,
    text_value(text_endorsement, "E-1 Trump arm, covariate-adjusted control group mean"), TRUE, NA,
    "The article says roughly two-thirds and prints no figure, so there is nothing to compare against.",
  "Results, endorsements", "E-1 Fauci endorsement, average treatment effect",
    archive_value("w3_fit_fauci", "treat", "estimate"), "0.055",
    text_value(text_endorsement, "E-1 Fauci endorsement, average treatment effect"), TRUE, NA, "",
  "Results, endorsements", "E-1 Fauci endorsement, standard error",
    archive_value("w3_fit_fauci", "treat", "std.error"), "0.024",
    text_value(text_endorsement, "E-1 Fauci endorsement, standard error"), TRUE, NA, "",
  "Results, endorsements", "E-1 joint test of the Personal and Social framings, p-value",
    NA, "0.37",
    text_value(text_endorsement, "E-1 joint test of Personal versus Social framing, p-value"), TRUE, NA,
    "The deposited script computes this test and prints it to the console; it writes the fitted objects but not the p-value.",
  "Footnote c", "E-1 Trump minus physician treatment-by-party interaction",
    NA, "0.068",
    text_value(text_endorsement, "E-1 Trump minus physician treatment-by-party interaction"), TRUE, NA,
    "No deposited script computes the contrast. It reproduces as the difference between the two interaction terms of a single unadjusted model of the outcome on endorser and party.",
  "Footnote c", "E-1 Trump minus physician interaction, standard error",
    NA, "0.012",
    text_value(text_endorsement, "E-1 Trump minus physician interaction, standard error"), TRUE, NA, "",
  "Results, endorsements", "E-2 Biden endorsement, average treatment effect",
    archive_value("w5_fit_biden", "treat", "estimate"), "-0.097",
    text_value(text_endorsement, "E-2 Biden endorsement, average treatment effect"), TRUE, NA, "",
  "Results, endorsements", "E-2 Biden endorsement, standard error",
    archive_value("w5_fit_biden", "treat", "std.error"), "0.036",
    text_value(text_endorsement, "E-2 Biden endorsement, standard error"), TRUE, NA, "",
  "Results, endorsements", "E-2 Obama endorsement, average treatment effect",
    archive_value("w5_fit_obama", "treat", "estimate"), "-0.109",
    text_value(text_endorsement, "E-2 Obama endorsement, average treatment effect"), TRUE, NA, "",
  "Results, endorsements", "E-2 Obama endorsement, standard error",
    archive_value("w5_fit_obama", "treat", "std.error"), "0.035",
    text_value(text_endorsement, "E-2 Obama endorsement, standard error"), TRUE, NA, "",
  "Results, endorsements", "E-2 unvaccinated respondents interviewed",
    NA, "7,249",
    text_value(text_endorsement, "E-2 respondents in the deposited analysis file"), TRUE, "archive",
    "Same gap as the Table 1 row for E-2: the deposited analysis file holds fewer rows than the published enrollment count.",
  "Results, endorsements", "E-2 Democrats likely to get the vaccine",
    NA, "79%",
    100 * text_value(text_endorsement, "E-2 share likely to vaccinate, Democrats (leaners with independents)"), TRUE, NA,
    "Reproduces only when leaners are grouped with independents rather than with their party. The deposited pid7_collapsed column groups leaners with their party.",
  "Results, endorsements", "E-2 independents likely to get the vaccine",
    NA, "50%",
    100 * text_value(text_endorsement, "E-2 share likely to vaccinate, independents (leaners with independents)"), TRUE, NA,
    "Same grouping as the row above.",
  "Results, endorsements", "E-2 Republicans likely to get the vaccine",
    NA, "45%",
    100 * text_value(text_endorsement, "E-2 share likely to vaccinate, Republicans (leaners with independents)"), TRUE, NA,
    "Same grouping as the row above.",
  "Results, guidance", "G-1 control group support for the mixed policy",
    100 * archive_value("w6_fit_cdcmask", "(Intercept)", "estimate"), "23.7%",
    100 * text_value(text_guidance, "G-1 covariate-adjusted control group mean"), TRUE, NA, "",
  "Results, guidance", "G-3 control group support for the more restrictive policy",
    100 * archive_value("w7_fit_cdcmask", "(Intercept)", "estimate"), "68%",
    100 * text_value(text_guidance, "G-3 covariate-adjusted control group mean"), TRUE, NA, "",
  "Footnote f", "G-2 joint test of the solo and friend versions, p-value",
    NA, "0.10",
    text_value(text_guidance, "G-2 joint test of solo versus friend version, p-value"), TRUE, NA,
    "The deposited script computes this test and prints it to the console.",
  "Footnote g", "I-1 joint test of the information source, p-value",
    NA, "0.91",
    text_value(text_guidance, "I-1 joint test of information source, p-value"), TRUE, NA,
    "No deposited script computes this test.",
  "Results, information", "I-1 control group willing to be vaccinated",
    100 * archive_value("w7_fit_contagiousness", "(Intercept)", "estimate"), "20%",
    100 * text_value(text_guidance, "I-1 covariate-adjusted control group mean"), TRUE, NA, "",
  "Results, information", "I-2 control group willing to be vaccinated",
    100 * archive_value("w7_fit_doctordelta", "(Intercept)", "estimate"), "28%",
    100 * text_value(text_guidance, "I-2 covariate-adjusted control group mean"), TRUE, NA, "",
  "Results, information", "I-3 effect on booster intentions (percentage points)",
    100 * archive_value("w8_fit_adult_booster", "treat", "estimate"), "8.3",
    100 * text_value(text_guidance, "I-3 average treatment effect"), TRUE, NA, "",
  "Results, information", "I-4 effect on intentions to boost a child (percentage points)",
    100 * archive_value("w8_fit_child_booster", "treat", "estimate"), "16.2",
    100 * text_value(text_guidance, "I-4 average treatment effect"), TRUE, NA, "",
  "Results, information", "I-4 control group parents intending to boost their children",
    100 * archive_value("w8_fit_child_booster", "(Intercept)", "estimate"), "60%",
    100 * text_value(text_guidance, "I-4 covariate-adjusted control group mean"), TRUE, NA,
    "The article says nearly 60 per cent, so the published figure is an upper bound rather than a rounded value.",
  "Results, guidance", "G-2 baseline willingness across the four vignettes",
    NA, NA,
    100 * text_value(text_guidance, "G-2 lowest control group mean across the four vignettes"), TRUE, NA,
    paste0("The article says willingness was low, around 20 per cent, and prints no figure. The four pooled control group means run from ",
           sprintf("%.1f", 100 * text_value(text_guidance, "G-2 lowest control group mean across the four vignettes")),
           " to ",
           sprintf("%.1f", 100 * text_value(text_guidance, "G-2 highest control group mean across the four vignettes")),
           " per cent."),
  "Appendix Figure S47", "G-1 chi-squared statistic on the three-category outcome",
    NA, "111.78",
    text_value(text_guidance, "G-1 chi-squared statistic on the three-category outcome"), TRUE, NA,
    "The deposited script hardcodes this statistic into the figure's annotation and computes the test separately, printing it to the console.",
  "Materials and methods", "Adults with at least one vaccine dose, spring 2021",
    NA, "48.8%", NA, FALSE, "archive",
    "The deposit contains no wave-level file from which this share could be computed; its ten analysis files are all experiment subsets."
)

# Equivalence claims ----
# The article states these as counts of experiments rather than as printed
# numbers, so value_paper is the count the sentence asserts. The reading each
# one is tested under is recorded in the note.
equivalence_rows <- tribble(
  ~table_figure, ~claim, ~value_script, ~value_paper, ~value_rewrite, ~expect_rewrite, ~defect_locus, ~notes,
  "Materials and methods", "Interaction terms not significant weighted that become significant unweighted",
    NA, "3",
    text_value(text_equivalence, "Interaction terms not significant weighted that become significant unweighted"), TRUE, NA,
    "Counted over all 26 experiment arms, comparing the treatment-by-party interaction fitted with and without the survey weights.",
  "Materials and methods", "Guidance experiments affirming equivalence at 5 points",
    NA, "2",
    text_value(text_equivalence, "Guidance experiments affirming equivalence at 5 points"), TRUE, NA,
    "Both of the two guidance experiments, as the article says.",
  "Materials and methods", "Mandate vignettes affirming equivalence at 10 points",
    text_value(text_equivalence, "Mandate vignettes affirming equivalence at 10 points under the deposit's standard error"),
    "2",
    text_value(text_equivalence, "Mandate vignettes affirming equivalence at 10 points"), TRUE, "archive",
    "The deposit's standard error for a difference in conditional average treatment effects squares the Republican standard error twice and never uses the Democratic one. Under that standard error two of the four vignettes affirm equivalence, as the article says; under the corrected one, one does.",
  "Materials and methods", "Information experiments with a significant difference-in-CATEs",
    NA, "0",
    text_value(text_equivalence, "Information experiments with a significant difference-in-CATEs"), TRUE, NA, "",
  "Materials and methods", "Information experiments affirming equivalence at 10 points",
    text_value(text_equivalence, "Information experiments affirming equivalence at 10 points under the deposit's standard error"),
    "3",
    text_value(text_equivalence, "Information experiments affirming equivalence at 10 points"), TRUE, NA, "",
  "Materials and methods", "Experiments where the interaction term and the difference-in-CATEs disagree",
    text_value(text_equivalence, "Experiments discordant under the deposit's standard error"),
    "1",
    text_value(text_equivalence, "Experiments where the interaction term and the difference-in-CATEs are discordant"), TRUE, "paper_internal",
    "Read as: the two disagree when one detects heterogeneity and the other does not, or when both do and their signs differ. Two of the 26 experiment arms are discordant on that reading, and the count does not change under the deposit's own standard error."
)

# Assemble ----
gt <- bind_rows(
  table_1_rows,
  headline_n_row,
  table_2_rows,
  table_3_rows,
  table_a1_rows,
  table_a2_rows,
  table_a3_rows,
  table_a4_rows,
  figure_1_rows,
  figure_3_rows,
  figure_5_rows,
  section_c_rows,
  unlabelled_figures,
  text_rows,
  equivalence_rows
)

gt <-
  gt |>
  mutate(
    paper_id = paper_id,
    match = agrees(value_script, value_paper),
    match_rewrite = agrees(value_rewrite, value_paper),
    # The verdict clause is built from the same printed comparison that sets
    # match_rewrite, so a note cannot name a value its own verdict contradicts.
    verdict_clause = if_else(
      is.na(match_rewrite),
      NA_character_,
      paste0("Rewrite prints ",
             print_to_paper(value_rewrite, value_paper),
             if_else(str_detect(value_paper, "%"), "%", ""),
             " against the article's ", value_paper, ".")
    ),
    notes = str_squish(paste(notes, replace_na(verdict_clause, ""))),
    defect_locus = if_else(!is.na(match_rewrite) & match_rewrite == 0 & is.na(defect_locus),
                           "unresolved", defect_locus),
    defect_locus = if_else(!is.na(match_rewrite) & match_rewrite == 1, NA_character_, defect_locus)
  )

# Gates ----
# A published value that expects a pipeline counterpart and did not find one is
# a broken label, not an unverifiable quantity, and must stop the build.
missing_rewrite <- gt |> filter(expect_rewrite, !is.na(value_paper), is.na(value_rewrite))

if (nrow(missing_rewrite) > 0) {
  stop("Published values with no rewrite counterpart: ",
       paste(missing_rewrite$claim, collapse = "; "), ".")
}

allowed_locus <- c("paper_internal", "archive", "environment", "rewrite", "unresolved")

unmatched_locus <- gt |> filter(!is.na(match_rewrite), match_rewrite == 0,
                                is.na(defect_locus) | !defect_locus %in% allowed_locus)

if (nrow(unmatched_locus) > 0) {
  stop("Rows with match_rewrite == 0 and no usable defect_locus: ",
       paste(unmatched_locus$claim, collapse = "; "), ".")
}

# Coverage of the published float list ----
# Enumerated from the article and its appendix rather than from what the
# pipeline happens to produce. Every float is either covered by at least one row
# above or carries a reason here.
floats <- bind_rows(
  tibble(float = c("Table 1", "Table 2", "Table 3",
                   "Figure 1", "Figure 2", "Figure 3", "Figure 4", "Figure 5")),
  tibble(float = paste("Appendix Table S", 1:4, sep = "")),
  tibble(float = paste0("Appendix Figure S", 1:62))
)

uncovered_reasons <- bind_rows(
  tibble(float = "Appendix Figure S23",
         reason = "A variable importance plot from the random forest that predicts vaccination status at the final wave. The rewrite does not refit that model."),
  tibble(float = paste0("Appendix Figure S", 24:45),
         reason = "Section D of the appendix repeats the section C panels among respondents a machine-learning model predicts will remain unvaccinated. Those panels print estimates and standard errors of their own, which nothing here reproduces: the rewrite does not refit the prediction model that defines the subgroup, and the deposited script that draws these figures fails. This is the largest uncovered group in the paper."),
  tibble(float = "Appendix Figure S46",
         reason = "A comparison of weighted with unweighted estimates. The rewrite fits the weighted models the main text reports and does not refit every model unweighted."),
  tibble(float = paste0("Appendix Figure S", 48:61),
         reason = "Section E of the appendix plots conditional average treatment effects and differences in conditional average treatment effects by party for the mandate vignettes. The rewrite does not redraw them; the deposited script that draws them is never sourced by the deposit's own code/main.R."),
  tibble(float = "Appendix Figure S62",
         reason = "A meta-analysis collecting the average treatment effects and interaction terms of every experiment. The deposited script that draws it fails.")
)

coverage <-
  floats |>
  left_join(gt |> count(float = table_figure, name = "ground_truth_rows"), by = "float") |>
  mutate(ground_truth_rows = replace_na(ground_truth_rows, 0L)) |>
  left_join(uncovered_reasons, by = "float") |>
  mutate(reason = replace_na(reason, ""))

write_csv(coverage, here::here("ground_truth", "float_coverage.csv"))

uncounted <- coverage |> filter(ground_truth_rows == 0, reason == "")

if (nrow(uncounted) > 0) {
  stop("Published floats with neither a ground-truth row nor a stated reason: ",
       paste(uncounted$float, collapse = ", "), ".")
}

gt <-
  gt |>
  mutate(claim_id = str_remove_all(
    str_replace_all(str_to_lower(paste(table_figure, claim)), "[^a-z0-9]+", "_"),
    "^_|_$"
  )) |>
  select(paper_id, claim_id, table_figure, claim, value_script, value_paper, match,
         value_rewrite, match_rewrite, defect_locus, notes)

stopifnot(anyDuplicated(gt$claim_id) == 0)

# The published extraction ----
# published_claims.csv is every numeric token in the article and its appendix,
# classified by hand. It governs coverage for this file and for
# maintained/in_text_claims.R: a pipeline or descriptive claim must have a row
# here and a block there, and a claim with neither stops the build.
published_claims <- read_csv(here::here("ground_truth", "published_claims.csv"),
                             show_col_types = FALSE)

stopifnot(anyDuplicated(published_claims$claim_id) == 0)
stopifnot(all(published_claims$claim_type %in%
                c("pipeline", "definitional", "structural", "transcribed", "descriptive")))

needs_computing <- published_claims |> filter(claim_type %in% c("pipeline", "descriptive"))

no_ground_truth_row <- setdiff(needs_computing$claim_id, gt$claim_id)

if (length(no_ground_truth_row) > 0) {
  stop("Published claims with no ground-truth row: ",
       paste(no_ground_truth_row, collapse = ", "), ".")
}

# A claim is covered by a block in in_text_claims.R when the file names its
# identifier, or, for a float cell, when the file prints that float.
in_text_source <- read_lines(here::here("maintained", "in_text_claims.R"))

covered_in_text <- map_lgl(seq_len(nrow(needs_computing)), function(i) {
  any(str_detect(in_text_source, fixed(needs_computing$claim_id[i]))) ||
    any(str_detect(in_text_source, fixed(paste0(needs_computing$location[i], ":"))))
})

if (any(!covered_in_text)) {
  stop("Published claims with no block in maintained/in_text_claims.R: ",
       paste(needs_computing$claim_id[!covered_in_text], collapse = ", "), ".")
}

# The extraction was tokenized from the published PDF and the ground truth's
# published values were transcribed from rendered pages. Two routes to the same
# digits, so a disagreement is a transcription slip in one of them.
transcription_check <-
  needs_computing |>
  select(claim_id, extracted = value_paper) |>
  inner_join(gt |> select(claim_id, transcribed = value_paper), by = "claim_id") |>
  filter(!is.na(extracted), !is.na(transcribed), extracted != transcribed)

if (nrow(transcription_check) > 0) {
  stop("published_claims.csv and the ground truth disagree on the published value for: ",
       paste(transcription_check$claim_id, collapse = ", "), ".")
}

write_csv(gt, here::here("ground_truth", paste0(paper_id, "_ground_truth.csv")))

print(gt |> select(table_figure, claim, value_script, value_paper, match,
                   value_rewrite, match_rewrite, defect_locus),
      n = nrow(gt))

print(str_glue(
  "rows: {nrow(gt)}  ",
  "match=1: {sum(gt$match == 1, na.rm = TRUE)}  match=0: {sum(gt$match == 0, na.rm = TRUE)}  ",
  "match=NA: {sum(is.na(gt$match))}  ",
  "match_rewrite=1: {sum(gt$match_rewrite == 1, na.rm = TRUE)}  ",
  "match_rewrite=0: {sum(gt$match_rewrite == 0, na.rm = TRUE)}  ",
  "match_rewrite=NA: {sum(is.na(gt$match_rewrite))}"
))

print(str_glue(
  "floats: {nrow(coverage)}  covered: {sum(coverage$ground_truth_rows > 0)}  ",
  "uncovered with a stated reason: {sum(coverage$ground_truth_rows == 0)}"
))
