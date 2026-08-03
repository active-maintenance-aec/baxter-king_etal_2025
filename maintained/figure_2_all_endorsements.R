# baxter-king_etal_2025/maintained/figure_2_all_endorsements.R
# Output: output/figure_2_all_endorsements.pdf, .png,
#         output/figure_2_all_endorsements.csv
# Depends on: helpers.R, analysis_e1_endorse_wave3.R, analysis_e2_endorse_wave5.R
# Description: Average treatment effects and treatment-by-party interactions for
# every endorser in E-1 (October 2020, pooled and within each framing) and E-2
# (April 2021). Replaces figure_2.R.
#
# The framing series are labelled from the deposited experiment_arm column:
# "A (Personal)" is plotted as Personal and "B (Social)" as Social. The deposited
# figure script attaches the opposite labels; see the report.

source(here::here("maintained", "helpers.R"))

model_dir_e1 <- file.path(output_dir, "model_objects_e1")
model_dir_e2 <- file.path(output_dir, "model_objects_e2")

e1_labels <- c(
  trump = "Trump (w3)",
  fauci = "Fauci (w3)",
  trumpfauci = "Trump + Fauci (w3)",
  spiritual = "Spiritual leader (w3)",
  insurance = "Health Insurance (w3)",
  pharmacy = "Pharmacy (w3)",
  physician = "Personal physician (w3)"
)

e2_labels <- c(
  trump = "Trump (w5)",
  fauci = "Fauci (w5)",
  trumpfauci = "Trump + Fauci (w5)",
  obama = "Obama (w5)",
  biden = "Biden (w5)",
  biden_fauci = "Biden + Fauci (w5)",
  ramos = "Jorge Ramos (w5)",
  james = "Lebron James (w5)"
)

spec <- bind_rows(
  tibble(endorser = e1_labels, arm = "Pooled",
         path = file.path(model_dir_e1, paste0("w3_fit_", names(e1_labels), ".rds"))),
  tibble(endorser = e1_labels, arm = "Personal",
         path = file.path(model_dir_e1, paste0("w3_fit_", names(e1_labels), "_personal.rds"))),
  tibble(endorser = e1_labels, arm = "Social",
         path = file.path(model_dir_e1, paste0("w3_fit_", names(e1_labels), "_social.rds"))),
  tibble(endorser = e2_labels, arm = "Pooled",
         path = file.path(model_dir_e2, paste0("w5_fit_", names(e2_labels), ".rds")))
)

endorser_order <- rev(c(unname(e1_labels), unname(e2_labels)))
facet_levels <- c("Control group mean", "Average Treatment Effect", "Treatment X 7-pt PID")

gg_df <-
  spec |>
  mutate(estimates = map(path, function(p) tidy(read_rds(p)))) |>
  select(-path) |>
  unnest(estimates) |>
  filter(term %in% c("(Intercept)", "treat", "treat:pid_7_c")) |>
  mutate(
    facet_label = recode_values(
      term,
      "(Intercept)" ~ "Control group mean",
      "treat" ~ "Average Treatment Effect",
      "treat:pid_7_c" ~ "Treatment X 7-pt PID"
    ),
    facet_label = factor(facet_label, levels = facet_levels),
    arm = factor(arm, levels = c("Personal", "Social", "Pooled")),
    endorser = factor(endorser, levels = endorser_order),
    wave = if_else(str_detect(endorser, "w3"), "October 2020", "April 2021"),
    wave = factor(wave, levels = c("October 2020", "April 2021"))
  )

write_csv(
  gg_df |>
    select(endorser, arm, wave, facet_label, term, estimate, std.error,
           p.value, conf.low, conf.high) |>
    arrange(wave, endorser, arm, facet_label),
  file.path(output_dir, "figure_2_all_endorsements.csv")
)

lines_df <- tibble(
  facet_label = factor(facet_levels, levels = facet_levels),
  xintercept = c(0.5, 0.0, 0.0)
)

# Fixes the panel ranges so the two waves share an x scale.
blank_df <-
  tibble(
    facet_label = rep(facet_levels, each = 2),
    endorser = "Trump (w3)",
    wave = factor("October 2020", levels = c("October 2020", "April 2021")),
    estimate = c(0.45, 0.85, -0.20, 0.20, -0.20, 0.20)
  ) |>
  mutate(facet_label = factor(facet_label, levels = facet_levels))

pos <- position_dodge(width = 0.7)

p1 <-
  ggplot(
    gg_df |> filter(wave == "October 2020", facet_label != "Control group mean"),
    aes(x = estimate, y = endorser, color = arm, shape = arm, group = arm)
  ) +
  geom_blank(data = blank_df |>
               filter(facet_label != "Control group mean") |>
               mutate(arm = factor("Pooled", levels = c("Personal", "Social", "Pooled")))) +
  geom_text(
    data = gg_df |> filter(wave == "October 2020",
                           facet_label == "Average Treatment Effect",
                           endorser == "Trump (w3)"),
    aes(label = arm, color = arm, x = 0.025),
    hjust = 0,
    position = position_dodge(width = 1),
    size = 2.8
  ) +
  geom_point(position = pos) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = pos) +
  geom_vline(data = lines_df |> filter(facet_label != "Control group mean"),
             aes(xintercept = xintercept), linetype = "dashed") +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_blank()
  ) +
  facet_grid(cols = vars(facet_label), scales = "free", space = "free") +
  labs(x = "Coefficient estimate and 95% confidence interval", title = "October 2020") +
  scale_y_discrete(labels = function(x) str_remove(x, " \\(w[35]\\)")) +
  scale_color_manual(values = four_colors)

p2 <-
  ggplot(
    gg_df |> filter(wave != "October 2020", facet_label != "Control group mean"),
    aes(estimate, endorser)
  ) +
  geom_blank(data = blank_df |>
               filter(facet_label != "Control group mean") |>
               mutate(endorser = "Trump (w5)")) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(data = lines_df |> filter(facet_label != "Control group mean"),
             aes(xintercept = xintercept), linetype = "dashed") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_blank()
  ) +
  facet_grid(cols = vars(facet_label), scales = "free", space = "free") +
  labs(x = "Coefficient estimate and 95% confidence interval", title = "April 2021") +
  scale_y_discrete(labels = function(x) str_remove(x, " \\(w[35]\\)")) +
  scale_color_manual(values = four_colors)

g <- p1 / p2 + plot_layout(heights = c(2.1, 0.9))

ggsave(file.path(output_dir, "figure_2_all_endorsements.pdf"), g, width = 8, height = 7)
ggsave(file.path(output_dir, "figure_2_all_endorsements.png"), g, width = 8, height = 7, dpi = 300)
