# baxter-king_etal_2025/ground_truth/extract_published_appendix_values.R
# Output: ground_truth/published_appendix_values.csv
# Depends on: the published appendix PDF and pdftotext (poppler)
# Description: Parse the estimates and standard errors printed on appendix
#   Figures S1 to S22 into a committed CSV, so the ground truth can compare all
#   704 of them without every reader needing the PDF.
#
#   This script is not part of run_all.R. It is run by hand when the parse needs
#   redoing, because the appendix is frozen and its numbers can never change,
#   and because the PDF is a published document this repository does not
#   redistribute. Point it at a copy with the APPENDIX_PDF environment variable.
#
#   Section C of the appendix draws one two-panel figure per experimental
#   contrast, in the same form as main-text Figures 1, 3 and 5: a difference in
#   means and a Lin-adjusted estimate, overall and within each level of
#   seven-point party identification, each printed as "estimate (standard
#   error)" in percentage points. Those numbers appear nowhere else in the
#   article, so a ground truth built from the regression tables alone cannot
#   reach them.
#
#   The parse is positional, never by reading order. pdftotext emits words in
#   the order the PDF draws them, which interleaves axis labels with the
#   estimates and does not run down the panel, so each number is matched to a
#   subgroup by its vertical position: the estimate sits just above its
#   subgroup label and the Lin-adjusted value just below it. Reading order would
#   silently mis-map a row, which is the failure this project has manufactured
#   before.
#
#   The parse is checked, not trusted. Appendix Figures S1, S16 and S20 are the
#   same panels as main-text Figures 1, 3 and 5, whose numbers were transcribed
#   independently from rendered pages in build_ground_truth.R. Those three
#   figures are asserted to agree cell by cell before anything is written.

library(here)
library(tidyverse)

here::i_am("ground_truth/extract_published_appendix_values.R")

appendix_pdf <- Sys.getenv(
  "APPENDIX_PDF",
  unset = file.path(path.expand("~"), "Downloads", "baxter-king_etal_2025_appendix.pdf")
)

stopifnot(file.exists(appendix_pdf))

# Section C contrasts, in the order the appendix numbers them ----
section_c <- tribble(
  ~float, ~contrast,
  "Appendix Figure S1", "E-1: Trump",
  "Appendix Figure S2", "E-1: Fauci",
  "Appendix Figure S3", "E-1: Trump and Fauci",
  "Appendix Figure S4", "E-1: Physician",
  "Appendix Figure S5", "E-1: Pharmacy",
  "Appendix Figure S6", "E-1: Insurance",
  "Appendix Figure S7", "E-1: Spiritual Leader",
  "Appendix Figure S8", "E-2: Trump",
  "Appendix Figure S9", "E-2: Trump and Fauci",
  "Appendix Figure S10", "E-2: Fauci",
  "Appendix Figure S11", "E-2: Biden and Fauci",
  "Appendix Figure S12", "E-2: Biden",
  "Appendix Figure S13", "E-2: Obama",
  "Appendix Figure S14", "E-2: James",
  "Appendix Figure S15", "E-2: Ramos",
  "Appendix Figure S16", "G-1: Less restrictive mask guidance",
  "Appendix Figure S17", "G-3: More restrictive mask guidance",
  "Appendix Figure S18", "I-1: Contagiousness",
  "Appendix Figure S19", "I-2: Delta Variant",
  "Appendix Figure S20", "I-3: Bivalent Booster - Adult",
  "Appendix Figure S21", "I-4: Bivalent Booster - Child",
  "Appendix Figure S22", "I-5: Vaccince - Child"
)

subgroups <- c("Full sample", "Strong Republican", "Weak Republican", "Lean Republican",
               "Independent", "Lean Democrat", "Weak Democrat", "Strong Democrat")

# Positioned words ----
positioned_words <- function(pdf) {
  bbox <- tempfile(fileext = ".xhtml")
  system2("pdftotext", c("-bbox-layout", shQuote(pdf), shQuote(bbox)))
  lines <- read_lines(bbox)

  page_breaks <- cumsum(str_detect(lines, "<page "))

  tibble(line = lines, page = page_breaks) |>
    filter(str_detect(line, "<word ")) |>
    mutate(
      y = as.numeric(str_match(line, 'yMin="([\\d.]+)"')[, 2]),
      x = as.numeric(str_match(line, 'xMin="([\\d.]+)"')[, 2]),
      word = str_match(line, ">([^<]*)</word>")[, 2]
    ) |>
    select(page, x, y, word)
}

words <- positioned_words(appendix_pdf)

# The figure caption fixes the region each float occupies ----
# Two figures share a page through most of section C, so a page is not a float.
# A figure's region runs from its own caption down to the next caption on the
# same page, or to the foot of the page where it is the last one.
#
# The number must be anchored on the word "Figure" beside it. "S1:" alone also
# opens "Table S1:", and matching that sends three of the floats to the wrong
# page entirely.
caption_regions <-
  words |>
  arrange(page, y, x) |>
  filter(str_detect(word, "^S\\d+:$"), lag(word) == "Figure") |>
  transmute(float = paste("Appendix Figure", str_remove(word, ":$")), page, y_caption = y) |>
  distinct(float, .keep_all = TRUE) |>
  arrange(page, y_caption) |>
  group_by(page) |>
  mutate(y_end = lead(y_caption, default = Inf)) |>
  ungroup()

# An estimate is printed as two words, "-9.5" then "(2.8)", side by side on one
# line. They are paired on the y coordinate and ordered on x.
estimates_on_page <- function(page_words) {
  numbers <-
    page_words |>
    # The panels typeset a negative with the Unicode minus sign, not a hyphen,
    # written here as an escape so this file stays plain ASCII.
    mutate(word = str_replace_all(word, "\u2212", "-")) |>
    filter(str_detect(word, "^-?\\d+\\.\\d$") | str_detect(word, "^\\(\\d+\\.\\d\\)$")) |>
    arrange(y, x)

  paired <-
    numbers |>
    mutate(kind = if_else(str_starts(word, "\\("), "se", "estimate")) |>
    group_by(y = round(y)) |>
    filter(n() == 2, all(c("estimate", "se") %in% kind)) |>
    summarize(
      estimate = as.numeric(word[kind == "estimate"]),
      std.error = as.numeric(str_remove_all(word[kind == "se"], "[()]")),
      .groups = "drop"
    )

  labels <-
    page_words |>
    filter(word %in% c("sample", "Republican", "Independent", "Democrat")) |>
    arrange(y)

  # One label per subgroup, in panel order, so the label's y anchors its pair.
  stopifnot(nrow(labels) == length(subgroups))

  label_positions <- tibble(subgroup = subgroups, y_label = labels$y)

  # The difference in means is drawn above the label and the Lin-adjusted
  # estimate below it, so nearest-above and nearest-below assign the estimator.
  assign_estimator <- function(y_label) {
    above <- paired |> filter(y < y_label) |> slice_max(y, n = 1)
    below <- paired |> filter(y > y_label) |> slice_min(y, n = 1)
    stopifnot(nrow(above) == 1, nrow(below) == 1)
    bind_rows(
      above |> mutate(estimator = "DIM"),
      below |> mutate(estimator = "OLS")
    )
  }

  label_positions |>
    mutate(values = map(y_label, assign_estimator)) |>
    unnest(values) |>
    select(subgroup, estimator, estimate, std.error)
}

published_appendix_values <-
  section_c |>
  left_join(caption_regions, by = "float") |>
  mutate(values = pmap(list(page, y_caption, y_end), function(p, y0, y1) {
    estimates_on_page(filter(words, page == p, y > y0, y < y1))
  })) |>
  select(float, contrast, values) |>
  unnest(values) |>
  # Carried as the digits the page prints, in the page's units, so a comparison
  # is between printed strings rather than between doubles.
  mutate(
    value_estimate = sprintf("%.1f", estimate),
    value_std.error = sprintf("%.1f", std.error)
  ) |>
  select(float, contrast, subgroup, estimator, value_estimate, value_std.error)

stopifnot(nrow(published_appendix_values) == 22 * 8 * 2)

# The parse is checked against three independent transcriptions ----
# S1, S16 and S20 print the same panels as main-text Figures 1, 3 and 5, whose
# values were read off rendered pages by hand in build_ground_truth.R.
main_text_twins <- tribble(
  ~float, ~subgroup, ~estimator, ~value_estimate, ~value_std.error,
  "Appendix Figure S1", "Full sample", "DIM", "-9.5", "2.8",
  "Appendix Figure S1", "Full sample", "OLS", "-9.2", "2.6",
  "Appendix Figure S1", "Strong Republican", "DIM", "16.2", "5.3",
  "Appendix Figure S1", "Strong Democrat", "OLS", "-27.2", "5.0",
  "Appendix Figure S16", "Full sample", "DIM", "6.0", "0.7",
  "Appendix Figure S16", "Lean Republican", "DIM", "12.4", "2.7",
  "Appendix Figure S16", "Strong Democrat", "OLS", "5.3", "1.4",
  "Appendix Figure S20", "Full sample", "DIM", "9.1", "1.5",
  "Appendix Figure S20", "Weak Democrat", "OLS", "17.1", "3.5",
  "Appendix Figure S20", "Strong Democrat", "OLS", "4.6", "2.3"
)

parse_check <-
  main_text_twins |>
  left_join(published_appendix_values,
            by = c("float", "subgroup", "estimator"),
            suffix = c("_expected", "_parsed"))

stopifnot(
  nrow(parse_check) == nrow(main_text_twins),
  !anyNA(parse_check$value_estimate_parsed),
  parse_check$value_estimate_parsed == parse_check$value_estimate_expected,
  parse_check$value_std.error_parsed == parse_check$value_std.error_expected
)

write_csv(published_appendix_values,
          here::here("ground_truth", "published_appendix_values.csv"))

print(str_glue("Parsed {nrow(published_appendix_values)} printed estimate and standard error ",
               "pairs from {n_distinct(published_appendix_values$float)} appendix figures, ",
               "checked against {nrow(main_text_twins)} values transcribed independently."))
