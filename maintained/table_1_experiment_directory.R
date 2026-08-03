# baxter-king_etal_2025/maintained/table_1_experiment_directory.R
# Output: output/table_1_experiment_directory.csv, .tex
# Depends on: helpers.R
# Description: Sample sizes for the ten experiments in the article's directory
# table, counted from the deposited analysis files. No deposited script builds
# Table 1, so this rebuilds the one column of it that the data can speak to.

source(here::here("maintained", "helpers.R"))

directory <- tribble(
  ~experiment, ~date_fielded, ~sample_definition, ~dataset,
  "E-1: Vaccine endorsement 1", "10/2020", "All respondents", "w3_endorse",
  "E-2: Vaccine endorsement 2", "04/2021", "Unvaccinated", "w5_endorse",
  "G-1: CDC mask guidance 1", "06/2021", "All respondents", "w6_cdcmask",
  "G-2: Vaccine mandate vignettes", "06/2021", "Unvaccinated", "w6_vignette",
  "G-3: CDC mask guidance 2", "09/2021", "All respondents", "w7_cdcmask",
  "I-1: Contagiousness conversation", "09/2021", "Unvaccinated", "w7_contagiousness",
  "I-2: Delta variant conversation", "09/2021", "Unvaccinated", "w7_doctordelta",
  "I-3: Bivalent booster information", "10/2022", "Unboosted", "w8_adult_booster",
  "I-4: Bivalent booster information (children)", "10/2022", "Children unboosted", "w8_child_booster",
  "I-5: Holiday surge information (children)", "10/2022", "Children unvaccinated", "w8_child_vaccine"
)

experiment_data <- map(directory$dataset, read_experiment)

table_1 <-
  directory |>
  mutate(
    n_deposited = map_int(experiment_data, nrow),
    n_distinct_respondents = map_int(experiment_data, function(d) n_distinct(d$respondent_id))
  )

# Respondents appearing in at least one of the ten experiments. The article
# reports a single headline count of respondents across the whole project.
n_respondents_any_experiment <-
  experiment_data |>
  map(function(d) d$respondent_id) |>
  reduce(union) |>
  length()

table_1 <- table_1 |>
  bind_rows(tibble(
    experiment = "Any of the ten experiments",
    date_fielded = "",
    sample_definition = "",
    dataset = "",
    n_deposited = n_respondents_any_experiment,
    n_distinct_respondents = n_respondents_any_experiment
  ))

write_csv(table_1, file.path(output_dir, "table_1_experiment_directory.csv"))

table_1 |>
  select(Experiment = experiment, `Date fielded` = date_fielded,
         `Sample definition` = sample_definition, N = n_deposited) |>
  kable(format = "latex", booktabs = TRUE, format.args = list(big.mark = ","),
        caption = "Directory of experiments, sample sizes as deposited") |>
  write_lines(file.path(output_dir, "table_1_experiment_directory.tex"))
