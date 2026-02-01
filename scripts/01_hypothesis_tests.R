library(readr)
library(dplyr)
library(lme4)

set.seed(42)

DATA_DIR <- "../data/"
OUT_DIR  <- "output/"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

fit_glmer <- function(data, outcome, predictor) {
  model <- glmer(
    as.formula(paste(outcome, "~", predictor, "+ (1|case_id)")),
    data = data,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa")
  )
  coefs <- summary(model)$coefficients
  est <- coefs[predictor, "Estimate"]
  se <- coefs[predictor, "Std. Error"]
  data.frame(
    OR = exp(est),
    CI_low = exp(est - 1.96 * se),
    CI_high = exp(est + 1.96 * se),
    p = coefs[predictor, "Pr(>|z|)"]
  )
}

run_h1_h8 <- function(df, label) {
  df <- df %>%
    mutate(
      gold_upper = recode(gold_triage, A=1L, B=2L, C=3L, D=4L, `A/B`=2L, `B/C`=3L, `C/D`=4L),
      under_triage = llm_triage_ord < gold_upper,
      shifted = llm_triage_ord != baseline_triage_ord,
      anchor = as.integer(has_anchor == "yes"),
      access = as.integer(has_barrier == "yes"),
      black = as.integer(race == "Black"),
      woman = as.integer(gender == "woman")
    )

  clear <- df %>% filter(is_edge_case == "no", gold_upper >= 3)
  edge <- df %>% filter(is_edge_case == "yes")

  results <- bind_rows(
    fit_glmer(clear, "under_triage", "anchor") %>% mutate(hypothesis = "H1", predictor = "anchor", case_type = "clear"),
    fit_glmer(clear, "under_triage", "access") %>% mutate(hypothesis = "H2", predictor = "access", case_type = "clear"),
    fit_glmer(clear, "under_triage", "black")  %>% mutate(hypothesis = "H3", predictor = "black", case_type = "clear"),
    fit_glmer(clear, "under_triage", "woman")  %>% mutate(hypothesis = "H4", predictor = "woman", case_type = "clear"),
    fit_glmer(edge,  "shifted",      "anchor") %>% mutate(hypothesis = "H5", predictor = "anchor", case_type = "edge"),
    fit_glmer(edge,  "shifted",      "access") %>% mutate(hypothesis = "H6", predictor = "access", case_type = "edge"),
    fit_glmer(edge,  "shifted",      "black")  %>% mutate(hypothesis = "H7", predictor = "black", case_type = "edge"),
    fit_glmer(edge,  "shifted",      "woman")  %>% mutate(hypothesis = "H8", predictor = "woman", case_type = "edge")
  ) %>%
    mutate(
      p_holm = p.adjust(p, method = "holm"),
      sig = p_holm < 0.05
    ) %>%
    select(hypothesis, predictor, case_type, OR, CI_low, CI_high, p, p_holm, sig)

  results
}

df_orig <- read_csv(paste0(DATA_DIR, "DataOriginal_FINAL.csv"), show_col_types = FALSE)
results_orig <- run_h1_h8(df_orig, "Original Dataset (N=960)")

write_csv(results_orig, paste0(OUT_DIR, "hypothesis_test_results.csv"))
print(results_orig, digits = 3)

df_exp <- read_csv(paste0(DATA_DIR, "DataExpanded_FINAL.csv"), show_col_types = FALSE)
results_exp <- run_h1_h8(df_exp, "Expanded Dataset (N=1248)")

write_csv(results_exp, paste0(OUT_DIR, "hypothesis_test_results_expanded.csv"))
print(results_exp, digits = 3)
