# baxter-king_etal_2025/maintained/helpers.R
# Output: none
# Depends on: nothing
# Description: Packages, palettes, the shared covariate formula and the plotting
# helpers used by every script in maintained/. Sourced first by all of them.

library(here)
library(tidyverse)
library(estimatr)
library(patchwork)
library(modelsummary)
library(knitr)
library(kableExtra)
library(scales)

here::i_am("maintained/helpers.R")

options(modelsummary_format_numeric_latex = "plain")

data_dir <- here::here("original", "replication_archive", "data", "clean", "rds")
output_dir <- here::here("maintained", "output")

read_experiment <- function(name) {
  read_rds(file.path(data_dir, paste0("data_", name, ".rds")))
}

# Palettes carried over from the deposited code/helpers.R.
bpr_colors <- c("#1F3A93", "#7C2C55", "#D91E18")
bpr_ramp <- colorRampPalette(bpr_colors[c(1, 3)])
colors <- c(bpr_ramp(7), "black")
four_colors <- c("#008080", "#FFA500", "black", "#6A0DAD")

# Covariates entering every Lin-adjusted model. The seven-point party
# identification score is included alongside the demographic and COVID-19
# covariates, as in the deposited code.
ols_formula_covariates <- as.formula(
  ~ pid_7 +
    covariate_pre_lucid_race_hispanic_binned +
    covariate_pre_lucid_age +
    covariate_pre_lucid_gender +
    covariate_pre_lucid_household_income_imputed_three_groups +
    covariate_pre_lucid_education_binned +
    covariate_pre_ucla_covid_worried_about_continuous_0_1_imputed +
    covariate_pre_ucla_covid_risk_month_continuous_0_1_imputed +
    covariate_pre_ucla_covid_risk_comparative_continuous_0_1_imputed +
    covariate_pre_ucla_covid_risk_lifetime_continuous_0_1_imputed +
    covariate_pre_ucla_flu_last_year_yes_binary_imputed
)

# The same list without party identification, used for the within-party
# conditional effects where party is constant by construction.
ols_formula_within_party <- as.formula(
  ~ covariate_pre_lucid_race_hispanic_binned +
    covariate_pre_lucid_age +
    covariate_pre_lucid_gender +
    covariate_pre_lucid_household_income_imputed_three_groups +
    covariate_pre_lucid_education_binned +
    covariate_pre_ucla_covid_worried_about_continuous_0_1_imputed +
    covariate_pre_ucla_covid_risk_month_continuous_0_1_imputed +
    covariate_pre_ucla_covid_risk_comparative_continuous_0_1_imputed +
    covariate_pre_ucla_covid_risk_lifetime_continuous_0_1_imputed +
    covariate_pre_ucla_flu_last_year_yes_binary_imputed
)

# Save a fitted model. The terms object of a fit carries a pointer to the
# environment it was built in, which serializes differently in every session, so
# two runs of the same script write bytes that differ while every number in them
# is identical. Dropping that pointer is what lets a reader diff a fresh run of
# the pipeline against the committed output.
write_model <- function(fit, path) {
  attr(fit$terms, ".Environment") <- baseenv()
  write_rds(fit, path)
}

# The same for a table whose fit column holds several fitted models.
strip_environments <- function(fits) {
  fits |> mutate(fit = map(fit, function(f) {
    attr(f$terms, ".Environment") <- baseenv()
    f
  }))
}

# Restrict a multi-arm experiment to one treatment arm plus the control group.
make_arm <- function(data, treatment_label) {
  data |>
    filter(Z %in% c("Control", treatment_label)) |>
    mutate(
      pid_7 = as.numeric(demo_pid7),
      treat = as.numeric(Z == treatment_label)
    )
}

# Data behind the left panel of each of Figures 1, 3 and 5: weighted control
# and treatment group means, overall and within each level of party ID.
levels_data <- function(data) {
  levels_pid_7 <-
    data |>
    group_by(demo_pid7, Z) |>
    reframe(tidy(lm_robust(Y ~ 1, weights = weights)))

  levels_all <-
    data |>
    group_by(Z) |>
    reframe(tidy(lm_robust(Y ~ 1, weights = weights)))

  bind_rows(levels_pid_7, levels_all) |>
    mutate(subgroup = fct_na_value_to_level(demo_pid7, level = "Full sample")) |>
    select(subgroup, Z, estimate, std.error, conf.low, conf.high, df)
}

# Fitting the Lin specification where the cell can carry it ----
# The covariates whose treatment interaction was dropped while their own main
# effect survived. A covariate that loses both halves of lm_lin()'s expansion
# is redundant, and what remains is the same specification on the covariates
# that are left; only a covariate that keeps its main effect and loses its
# interaction costs the estimand.
lost_interactions <- function(fit, covs) {
  na_terms <- fit$term[is.na(fit$coefficients)]
  owner <- function(term) {
    term <- str_remove(term, "^[^:]*:")
    hits <- covs[str_detect(term, fixed(covs))]
    if (length(hits) == 0) NA_character_ else hits[which.max(nchar(hits))]
  }
  interactions <- na_terms[str_detect(na_terms, ":")]
  main_effects <- setdiff(na_terms, interactions)
  setdiff(
    unique(na.omit(map_chr(interactions, owner))),
    unique(na.omit(map_chr(main_effects, owner)))
  )
}

# lm_lin() reports the effect at the covariate means, and that quantity exists
# only while every covariate is interacted with the treatment. In a thin party
# identification cell a race category can be empty in one arm, and because
# lm_lin() centres before interacting, x_c - Z * x_c equals -mean(x) * (1 - Z):
# the intercept, the treatment indicator, the centred covariate and its
# interaction are then exactly collinear. One of the four is dropped, the fit
# is the same whichever it is, and the treatment coefficient is not, so the
# published number in such a cell records which column the decomposition
# aliased rather than an effect. That is 21 of the 154 conditional cells behind
# the appendix panels, on 11 of the 22 figures, and the empty category is AAPI
# in 9 of them, Other in 8 and Black in 4.
#
# The covariate is therefore removed from the cells that cannot identify it,
# and from those cells only. Every other cell keeps the covariate list the
# article used, which is what makes this different from respecifying the whole
# analysis: dropping race everywhere would move 64 of the 133 identified cells
# by more than a percentage point and one of them by 14. The specification each
# cell actually used travels out with its estimate in covariates_trimmed, so a
# reader is never left to recover it by refitting.
lin_cell <- function(cell_data, covariates) {
  covs <- all.vars(covariates)
  trimmed <- character(0)
  repeat {
    fit <- lm_lin(Y ~ Z, covariates = reformulate(covs), weights = weights,
                  se_type = "HC1", data = cell_data)
    lost <- lost_interactions(fit, covs)
    if (length(lost) == 0) break
    trimmed <- c(trimmed, lost)
    covs <- setdiff(covs, lost)
    stopifnot(length(covs) > 0)
  }
  tidy(fit) |> mutate(covariates_trimmed = paste(sort(trimmed), collapse = "; "))
}

# Data behind the right panel of each of Figures 1, 3 and 5: difference-in-means
# and Lin-adjusted effects, overall and within each level of party ID. Estimates
# are on the percentage-point scale, as the published panels are.
aces_data <- function(data) {
  # The control group is the reference, always. data_w5_endorse.rds stores Z as a
  # factor whose first level is Obama rather than Control, so a contrast taken
  # from that file without this line is estimated the other way round and every
  # estimate comes back with the wrong sign while its standard error looks
  # right. The other analysis files already have Control first, or store Z as a
  # character vector, where it sorts first anyway, so this changes nothing else.
  data <- data |> mutate(Z = fct_relevel(factor(Z), "Control"))

  cates_pid_7 <-
    data |>
    group_by(demo_pid7) |>
    reframe(tidy(lm_robust(Y ~ Z, weights = weights, se_type = "HC1"))) |>
    filter(term != "(Intercept)")

  ate <-
    data |>
    reframe(tidy(lm_robust(Y ~ Z, weights = weights, se_type = "HC1"))) |>
    filter(term != "(Intercept)")

  cates_pid_7_ols <-
    data |>
    group_by(demo_pid7) |>
    reframe(lin_cell(pick(everything()), ols_formula_within_party)) |>
    filter(term != "(Intercept)", !str_detect(term, "covariate"))

  ate_ols <-
    data |>
    mutate(pid_7 = as.numeric(demo_pid7)) |>
    reframe(lin_cell(pick(everything()), ols_formula_covariates)) |>
    filter(term != "(Intercept)",
           !str_detect(term, "covariate"),
           !str_detect(term, "pid_7"))

  bind_rows(cates_pid_7, ate, cates_pid_7_ols, ate_ols, .id = "model") |>
    mutate(
      estimator = if_else(model %in% c("1", "2"), "DIM", "OLS"),
      across(c(estimate, std.error, conf.low, conf.high), ~ .x * 100),
      subgroup = fct_na_value_to_level(demo_pid7, level = "Full sample"),
      entry = sprintf("%.1f (%.1f)", estimate, std.error),
      covariates_trimmed = replace_na(covariates_trimmed, "")
    ) |>
    select(subgroup, estimator, term, estimate, std.error, p.value,
           conf.low, conf.high, entry, covariates_trimmed)
}

levels_plot <- function(gg_df) {
  pos <- position_dodge(width = 0.2)

  ggplot(
    gg_df,
    aes(Z, estimate,
        color = subgroup, group = subgroup,
        ymin = conf.low, ymax = conf.high)
  ) +
    geom_point(position = pos) +
    geom_line(position = pos) +
    geom_ribbon(aes(fill = subgroup, color = NULL), alpha = 0.2, position = pos) +
    geom_linerange(position = pos) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.title.y = element_text(size = 7),
      axis.title.x = element_text(size = 7)
    ) +
    labs(x = "Randomly assigned treatment")
}

# The deposited version dodged with coefplot::position_dodgev(). coefplot is
# unmaintained and its position class is built on ggplot2 3.x internals, so the
# dodge here is position_dodge() on the discrete y axis, which ggplot2 handles
# natively.
aces_plot <- function(gg_df) {
  pos <- position_dodge(width = -0.4)

  ggplot(gg_df, aes(estimate, subgroup,
                    color = subgroup, shape = estimator, group = estimator)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5, linewidth = 0.25) +
    geom_point(position = pos) +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = pos) +
    geom_text(aes(label = entry),
              position = position_dodge(width = -1.8),
              hjust = 0, size = 2.5) +
    coord_cartesian(xlim = c(-35, 35)) +
    theme_bw() +
    scale_color_manual(values = colors) +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 7),
      panel.grid.minor = element_blank()
    ) +
    labs(x = "Average causal effect estimate")
}

# Joint test that a set of interactions adds nothing to a nested weighted model.
# Used for the three joint significance tests reported in the article's text and
# footnotes.
joint_test_p <- function(restricted, unrestricted) {
  lmtest::waldtest(restricted, unrestricted)[["Pr(>Chisq)"]][2]
}

# Blank a figure PDF's embedded timestamps ----
# R's pdf() device stamps /CreationDate and /ModDate with the wall clock, so an
# otherwise deterministic pipeline writes a different file on every run. The epoch
# string is the same width as what it replaces, which keeps the cross-reference byte
# offsets valid, and a file with no timestamp is left alone.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("D:[0-9]{14}", raw_pdf, all = TRUE)
  if (length(hits) == 0) return(invisible(path))
  for (h in hits) raw_pdf[h:(h + length(epoch) - 1L)] <- epoch
  writeBin(raw_pdf, path)
  invisible(path)
}
