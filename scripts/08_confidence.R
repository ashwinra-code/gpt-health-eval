library(readr)
library(dplyr)
library(ggplot2)

DATA_DIR <- "../data/"
OUT_DIR  <- "output/"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

df <- read_csv(paste0(DATA_DIR, "DataOriginal_FINAL.csv"), show_col_types = FALSE) %>%
  mutate(
    gold_lower = recode(gold_triage,
      A = 1L, B = 2L, C = 3L, D = 4L,
      `A/B` = 1L, `B/C` = 2L, `C/D` = 3L),
    gold_upper = recode(gold_triage,
      A = 1L, B = 2L, C = 3L, D = 4L,
      `A/B` = 2L, `B/C` = 3L, `C/D` = 4L),
    correct_range = llm_triage_ord >= gold_lower & llm_triage_ord <= gold_upper,
    outcome = factor(
      ifelse(correct_range, "Correct", "Mistriaged"),
      levels = c("Correct", "Mistriaged")
    )
  )

summary_stats <- df %>%
  group_by(outcome) %>%
  summarise(
    n = n(),
    mean_conf = round(mean(llm_confidence, na.rm = TRUE), 1),
    sd_conf = round(sd(llm_confidence, na.rm = TRUE), 1),
    .groups = "drop"
  )

pb <- cor.test(df$llm_confidence, as.numeric(df$outcome == "Mistriaged"))

tt <- t.test(llm_confidence ~ outcome, data = df)
mean_correct <- summary_stats$mean_conf[summary_stats$outcome == "Correct"]
mean_mistriage <- summary_stats$mean_conf[summary_stats$outcome == "Mistriaged"]
mean_diff <- round(mean_correct - mean_mistriage, 1)

n_corr <- summary_stats$n[summary_stats$outcome == "Correct"]
n_mist <- summary_stats$n[summary_stats$outcome == "Mistriaged"]
sd_corr <- as.numeric(summary_stats$sd_conf[summary_stats$outcome == "Correct"])
sd_mist <- as.numeric(summary_stats$sd_conf[summary_stats$outcome == "Mistriaged"])
pooled_sd <- sqrt(((n_corr - 1) * sd_corr^2 + (n_mist - 1) * sd_mist^2) / (n_corr + n_mist - 2))
cohens_d <- mean_diff / pooled_sd

stats_df <- data.frame(
  statistic = c("r", "r_ci_low", "r_ci_high", "r_p",
                "mean_correct", "mean_mistriaged", "mean_diff",
                "t", "df", "t_p", "cohens_d"),
  value = c(round(pb$estimate, 3), round(pb$conf.int[1], 3), round(pb$conf.int[2], 3),
            pb$p.value,
            mean_correct, mean_mistriage, mean_diff,
            round(tt$statistic, 2), round(tt$parameter, 1), tt$p.value,
            round(cohens_d, 2))
)
write_csv(stats_df, paste0(OUT_DIR, "confidence_stats.csv"))

p <- ggplot(df, aes(x = llm_confidence, fill = outcome)) +
  geom_density(alpha = 0.6, color = NA) +
  geom_vline(
    data = summary_stats,
    aes(xintercept = mean_conf, color = outcome),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_fill_manual(
    values = c("Correct" = "#2E86AB", "Mistriaged" = "#E94F37"),
    name = "Triage outcome"
  ) +
  scale_color_manual(
    values = c("Correct" = "#2E86AB", "Mistriaged" = "#E94F37"),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(50, 100),
    breaks = seq(50, 100, by = 10),
    expand = c(0.01, 0)
  ) +
  labs(
    x = "Model-reported confidence (%)",
    y = "Density",
    title = NULL
  ) +
  annotate(
    "text", x = 79.3, y = 0.085,
    label = "Correct\nmean = 79.3",
    hjust = 0.5, vjust = 0, size = 3, color = "#2E86AB"
  ) +
  annotate(
    "text", x = 76.1, y = 0.085,
    label = "Mistriaged\nmean = 76.1",
    hjust = 0.5, vjust = 0, size = 3, color = "#E94F37"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave(
  filename = paste0(OUT_DIR, "ED_Fig2_confidence.png"),
  plot = p,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = paste0(OUT_DIR, "ED_Fig2_confidence.pdf"),
  plot = p,
  width = 7,
  height = 5,
  device = cairo_pdf
)

print(summary_stats)
print(stats_df)
