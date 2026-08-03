# baxter-king_etal_2025/maintained/figure_4_mandate_vignettes.R
# Output: output/figure_4_mandate_vignettes.pdf, .png,
#         output/figure_4_mandate_vignettes.csv
# Depends on: helpers.R, analysis_guidance_information.R
# Description: Control group means, average treatment effects and
# treatment-by-party interactions for the four vaccine mandate vignettes (G-2),
# by vignette version and pooled. Replaces figure_4.R.

source(here::here("maintained", "helpers.R"))

model_dir <- file.path(output_dir, "model_objects_gi")

facet_levels <- c("Control group mean", "Average Treatment Effect", "Treatment X 7-pt PID")

gg_df <-
  bind_rows(
    read_rds(file.path(model_dir, "w6_fit_vignette.rds")),
    read_rds(file.path(model_dir, "w6_fit_vignette_pooled.rds"))
  ) |>
  mutate(estimates = map(fit, tidy)) |>
  select(-fit) |>
  unnest(estimates) |>
  filter(term %in% c("(Intercept)", "ZTreatment", "ZTreatment:pid_7_c")) |>
  mutate(
    activity = str_to_title(scenario_exp_activity),
    version = str_to_title(scenario_exp_anchored),
    version = fct_relevel(version, "Friend", "Solo", "Pooled"),
    facet_label = recode_values(
      term,
      "(Intercept)" ~ "Control group mean",
      "ZTreatment" ~ "Average Treatment Effect",
      "ZTreatment:pid_7_c" ~ "Treatment X 7-pt PID"
    ),
    facet_label = factor(facet_label, levels = facet_levels)
  )

write_csv(
  gg_df |>
    select(activity, version, facet_label, term, estimate, std.error,
           p.value, conf.low, conf.high) |>
    arrange(activity, version, facet_label),
  file.path(output_dir, "figure_4_mandate_vignettes.csv")
)

lines_df <- tibble(
  facet_label = factor(facet_levels, levels = facet_levels),
  xintercept = c(0.5, 0.0, 0.0)
)

blank_df <-
  tibble(
    facet_label = rep(facet_levels, each = 2),
    activity = "Trip",
    version = factor("Solo", levels = c("Friend", "Solo", "Pooled")),
    estimate = c(0.45, 0.85, -0.20, 0.20, -0.20, 0.20)
  ) |>
  mutate(facet_label = factor(facet_label, levels = facet_levels))

pos <- position_dodge(width = 0.5)

g <-
  ggplot(gg_df, aes(estimate, activity,
                    group = version, color = version, shape = version)) +
  geom_blank(data = blank_df) +
  geom_point(position = pos) +
  geom_text(
    data = gg_df |> filter(facet_label == "Control group mean", activity == "Trip"),
    aes(label = version, x = 0.37),
    size = 2.8,
    position = position_dodge(width = 1)
  ) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = pos) +
  geom_vline(data = lines_df, aes(xintercept = xintercept), linetype = "dashed") +
  scale_color_manual(values = four_colors) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 9),
    legend.position = "none"
  ) +
  facet_grid(cols = vars(facet_label), scales = "free") +
  labs(x = "Coefficient estimate and 95% confidence interval")

ggsave(file.path(output_dir, "figure_4_mandate_vignettes.pdf"), g, width = 6.5, height = 4)
ggsave(file.path(output_dir, "figure_4_mandate_vignettes.png"), g, width = 6.5, height = 4, dpi = 300)
