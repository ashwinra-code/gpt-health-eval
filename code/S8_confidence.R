library(readr)
library(dplyr)

df <- read_csv("/data/DataOriginal_FINAL.csv", show_col_types = FALSE) %>%
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
write_csv(stats_df, "/results/confidence_stats.csv")
