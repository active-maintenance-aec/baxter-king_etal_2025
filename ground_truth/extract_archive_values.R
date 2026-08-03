# baxter-king_etal_2025/ground_truth/extract_archive_values.R
# Output: ground_truth/archive_values.csv, ground_truth/archive_run_status.csv,
#         ground_truth/archive_overwrites.csv
# Depends on: original/ (run download_original.R first)
# Description: Run the deposited code and record both what it produces and
#   whether it runs, so the ground truth can compare the article against the
#   archive as well as against the maintained rewrite.
#
#   Nothing here writes to original/. Every run happens in a throwaway copy
#   whose location comes from the ARCHIVE_RUN_DIR environment variable and
#   defaults to a directory under tempdir(), so the script is runnable by anyone
#   who has cloned this repository and has no path in it that is specific to any
#   one machine.
#
#   The deposit ships code/, data/ and metadata/ and no output/ directory, so a
#   copy stripped to data plus code is byte-identical to the deposit as
#   downloaded. That makes the usual "does a script pass only because the
#   deposit shipped its input" test vacuous on the file listing and non-vacuous
#   on execution, so it is run as execution: the as_shipped pass runs the
#   scripts in the order code/main.R sources them and lets each one see what the
#   ones before it wrote, and the stripped pass runs each script against a copy
#   holding only deposited files. A script that passes in the first and fails in
#   the second depends on another script's output.

library(here)
library(tidyverse)
library(estimatr)

here::i_am("ground_truth/extract_archive_values.R")

archive_source <- here::here("original", "replication_archive")

archive_run_dir <- Sys.getenv("ARCHIVE_RUN_DIR", unset = file.path(tempdir(), "archive_run"))
stopifnot(is.character(archive_run_dir), length(archive_run_dir) == 1, nzchar(archive_run_dir))

# The deposited scripts, in the order the deposit runs them ----
# Read out of code/main.R rather than typed, so a script the deposit adds or
# drops cannot go unnoticed. Scripts under code/ that main.R never sources are
# appended and counted, because the deposit's README describes some of them.
main_lines <- read_lines(file.path(archive_source, "code", "main.R"))

sourced <-
  main_lines |>
  str_subset("^\\s*source\\(") |>
  str_match('source\\("([^"]+)"\\)') |>
  (\(m) m[, 2])()

all_scripts <-
  list.files(file.path(archive_source, "code"), pattern = "\\.R$",
             recursive = TRUE, full.names = FALSE) |>
  (\(f) file.path("code", f))()

not_sourced <- setdiff(all_scripts, c(sourced, "code/main.R", "code/helpers.R"))

deposited_scripts <- c(sourced, not_sourced)

print(str_glue("code/main.R sources {length(sourced)} scripts; ",
               "{length(not_sourced)} further scripts under code/ are never sourced by it: ",
               "{paste(not_sourced, collapse = ', ')}."))

# Running the deposit ----
prepare_copy <- function(destination) {
  unlink(destination, recursive = TRUE)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  file.copy(archive_source, destination, recursive = TRUE)
  root <- file.path(destination, "replication_archive")
  walk(c("output", "output/model_objects", "output/model_objects_cate_party",
         "output/w6_vignette_extra_plots"),
       function(d) dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE))
  root
}

# Elapsed time is measured and printed but never written. The two CSVs this
# script commits are read by the report and diffed by anyone re-running the
# pipeline, so a column that changes on every run would bury a real change in
# noise. Everything written here is a property of the deposit, not of the run.
run_one <- function(root, script) {
  code <- str_glue('setwd("{root}"); source("{script}")')
  started <- Sys.time()
  console <- suppressWarnings(system2("Rscript", c("-e", shQuote(code)),
                                      stdout = TRUE, stderr = TRUE))
  failed <- !is.null(attr(console, "status"))
  message_line <- if (failed) {
    str_squish(tail(str_subset(console, "^Error"), 1))
  } else {
    ""
  }
  tibble(
    script = script,
    status = if_else(failed, "error", "ok"),
    seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    message = if_else(length(message_line) == 0, "", message_line)
  )
}

# Pass 1: the deposit as shipped, in main.R order, outputs accumulating ----
as_shipped_root <- prepare_copy(file.path(archive_run_dir, "as_shipped"))

deposited_files <- list.files(as_shipped_root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
mtime_before <- file.mtime(file.path(as_shipped_root, deposited_files))

as_shipped_status <-
  map(deposited_scripts, function(s) run_one(as_shipped_root, s)) |>
  bind_rows() |>
  mutate(pass = "as_shipped")

# Which deposited files did the run overwrite? ----
overwrites <- tibble(
  file = deposited_files,
  overwritten = file.mtime(file.path(as_shipped_root, deposited_files)) > mtime_before
)

write_csv(overwrites, here::here("ground_truth", "archive_overwrites.csv"))

print(str_glue("The deposit holds {nrow(overwrites)} files. A full run overwrites ",
               "{sum(overwrites$overwritten)} of them."))

# Pass 2: each script against a copy holding only deposited files ----
stripped_root <- prepare_copy(file.path(archive_run_dir, "stripped"))

stripped_status <-
  map(deposited_scripts, function(s) {
    walk(c("output/model_objects", "output/model_objects_cate_party",
           "output/w6_vignette_extra_plots", "output"),
         function(d) {
           inside <- list.files(file.path(stripped_root, d), full.names = TRUE)
           unlink(inside[!dir.exists(inside)])
         })
    run_one(stripped_root, s)
  }) |>
  bind_rows() |>
  mutate(pass = "stripped")

run_status <- bind_rows(as_shipped_status, stripped_status)

write_csv(run_status |> select(pass, script, status, message),
          here::here("ground_truth", "archive_run_status.csv"))

print(
  run_status |>
    count(pass, status) |>
    pivot_wider(names_from = status, values_from = n, values_fill = 0)
)

print(
  run_status |>
    group_by(pass) |>
    summarize(scripts = n(), total_minutes = round(sum(seconds) / 60, 1), .groups = "drop")
)

# Values the deposit produces ----
# Read out of the as-shipped copy, which is the configuration the deposit is
# meant to run in. The deposited figure helpers return a ggplot whose data slot
# is the table of estimates the figure prints, so the numbers the published
# figures carry can be read from the deposited code rather than recomputed.
original_wd <- getwd()
setwd(as_shipped_root)

helper_env <- new.env()
source("code/helpers.R", local = helper_env)

# The deposited figure scripts set the control group as the reference before
# calling this helper, which matters because data_w5_endorse.rds stores Z as a
# factor whose first level is Obama. Reading the archive means running it the way
# it runs itself: without the relevel every E-2 estimate would come back with the
# sign reversed and the archive would appear not to reproduce its own figure.
figure_data <- function(dataset, treatment_labels = NULL) {
  d <- read_rds(file.path("data/clean/rds", paste0("data_", dataset, ".rds")))
  if (!is.null(treatment_labels)) {
    d <- d |>
      filter(Z %in% treatment_labels) |>
      mutate(Z = factor(Z, levels = c("Control", setdiff(treatment_labels, "Control"))))
  }
  helper_env$aces_plot(d)$data
}

figure_1_aces <- figure_data("w3_endorse", c("Control", "Trump"))
figure_3_aces <- figure_data("w6_cdcmask")
figure_5_aces <- figure_data("w8_adult_booster")

# Appendix Figures S1 to S22 draw the same two panels for every experimental
# contrast, so the same deposited helper gives the numbers those panels print.
section_c_contrasts <- tribble(
  ~float, ~dataset, ~arm,
  "S1", "w3_endorse", "Trump",
  "S2", "w3_endorse", "Fauci",
  "S3", "w3_endorse", "Trump + Fauci",
  "S4", "w3_endorse", "Personal Physician",
  "S5", "w3_endorse", "Pharmacy",
  "S6", "w3_endorse", "Health Insurance",
  "S7", "w3_endorse", "Spiritual/Religious Leader",
  "S8", "w5_endorse", "Trump",
  "S9", "w5_endorse", "Trump + Fauci",
  "S10", "w5_endorse", "Fauci",
  "S11", "w5_endorse", "Biden + Fauci",
  "S12", "w5_endorse", "Biden",
  "S13", "w5_endorse", "Obama",
  "S14", "w5_endorse", "Lebron James",
  "S15", "w5_endorse", "Jorge Ramos",
  "S16", "w6_cdcmask", NA,
  "S17", "w7_cdcmask", NA,
  "S18", "w7_contagiousness", NA,
  "S19", "w7_doctordelta", NA,
  "S20", "w8_adult_booster", NA,
  "S21", "w8_child_booster", NA,
  "S22", "w8_child_vaccine", NA
)

section_c_values <-
  section_c_contrasts |>
  mutate(panel = map2(dataset, arm, function(dataset, arm) {
    figure_data(dataset, if (is.na(arm)) NULL else c("Control", arm))
  })) |>
  select(float, panel) |>
  unnest(panel) |>
  transmute(object = paste0("figure_", str_to_lower(float), "_aces"),
            term = paste(as.character(subgroup), estimator),
            estimate, std.error, nobs = NA_real_, r.squared = NA_real_)

model_files <- list.files("output/model_objects", full.names = TRUE)
models <- set_names(model_files, str_remove(basename(model_files), "\\.rds$"))

model_values <-
  models[!str_detect(names(models), "heterogeneity|waldtest|vignette")] |>
  imap(function(path, nm) {
    fit <- read_rds(path)
    tidy(fit) |>
      mutate(object = nm, nobs = fit$nobs, r.squared = fit$r.squared)
  }) |>
  bind_rows() |>
  select(object, term, estimate, std.error, nobs, r.squared)

# The vignette experiment is saved as a table of tidy estimates rather than as
# fitted objects, so it is read straight through and carries no nobs or
# R-squared. Those two rows of the published Table S3 therefore have no archive
# counterpart, which the ground truth records rather than fills in.
vignette_values <-
  c(w6_fit_vignette = "output/model_objects/w6_fit_vignette.rds",
    w6_fit_vignette_pooled = "output/model_objects/w6_fit_vignette_pooled.rds") |>
  imap(function(path, nm) {
    read_rds(path) |>
      transmute(object = nm,
                term = paste(scenario_exp_activity, scenario_exp_anchored, term),
                estimate, std.error, nobs = NA_real_, r.squared = NA_real_)
  }) |>
  bind_rows()

figure_values <-
  list(figure_1_aces = figure_1_aces,
       figure_3_aces = figure_3_aces,
       figure_5_aces = figure_5_aces) |>
  imap(function(d, nm) {
    d |>
      transmute(object = nm,
                term = paste(as.character(subgroup), estimator),
                estimate, std.error, nobs = NA_real_, r.squared = NA_real_)
  }) |>
  bind_rows()

setwd(original_wd)
stopifnot(identical(getwd(), original_wd))

archive_values <- bind_rows(model_values, vignette_values, figure_values, section_c_values)

stopifnot(!anyDuplicated(archive_values[c("object", "term")]))

write_csv(archive_values, here::here("ground_truth", "archive_values.csv"))

unlink(archive_run_dir, recursive = TRUE)

print(str_glue("Recorded {nrow(archive_values)} values from ",
               "{n_distinct(archive_values$object)} deposited objects."))
