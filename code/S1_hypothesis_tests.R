library(readr)
library(dplyr)
library(lme4)

df <- read_csv("/data/DataOriginal_FINAL.csv", show_col_types = FALSE) %>%
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

results <- bind_rows(
  fit_glmer(clear, "under_triage", "anchor") %>% mutate(hypothesis = "H1", predictor = "anchor"),
  fit_glmer(clear, "under_triage", "access") %>% mutate(hypothesis = "H2", predictor = "access"),
  fit_glmer(clear, "under_triage", "black")  %>% mutate(hypothesis = "H3", predictor = "black"),
  fit_glmer(clear, "under_triage", "woman")  %>% mutate(hypothesis = "H4", predictor = "woman"),
  fit_glmer(edge,  "shifted",      "anchor") %>% mutate(hypothesis = "H5", predictor = "anchor"),
  fit_glmer(edge,  "shifted",      "access") %>% mutate(hypothesis = "H6", predictor = "access"),
  fit_glmer(edge,  "shifted",      "black")  %>% mutate(hypothesis = "H7", predictor = "black"),
  fit_glmer(edge,  "shifted",      "woman")  %>% mutate(hypothesis = "H8", predictor = "woman")
) %>%
  mutate(
    p_holm = p.adjust(p, method = "holm"),
    sig = p_holm < 0.05
  ) %>%
  select(hypothesis, predictor, OR, CI_low, CI_high, p, p_holm, sig)

write_csv(results, "/results/hypothesis_test_results.csv")
print(results, digits = 3)
