library(readr)
library(dplyr)
library(lme4)

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
    under_floor   = llm_triage_ord < gold_lower,
    over_ceiling  = llm_triage_ord > gold_upper,
    under_triage = llm_triage_ord < gold_upper,
    over_triage  = llm_triage_ord > gold_upper,
    correct      = llm_triage_ord == gold_upper,
    has_labs = grepl("^(E|MH)", case_id),
    anchor = as.integer(has_anchor == "yes"),
    access = as.integer(has_barrier == "yes"),
    black  = as.integer(race == "Black"),
    woman  = as.integer(gender == "woman"),
    mistriage = !correct_range
  )

clear <- df %>% filter(is_edge_case == "no")
edge  <- df %>% filter(is_edge_case == "yes")

# Accuracy by acuity
acuity_acc <- clear %>%
  group_by(gold_triage) %>%
  summarise(
    n     = n(),
    correct_n = sum(correct),
    accuracy  = round(mean(correct) * 100, 1),
    under_n   = sum(under_triage),
    under_rate = round(mean(under_triage) * 100, 1),
    over_n    = sum(over_triage),
    over_rate = round(mean(over_triage) * 100, 1),
    .groups = "drop"
  )

# Edge case analysis
edge_analysis <- edge %>%
  mutate(
    below_floor = llm_triage_ord < gold_lower,
    above_ceiling = llm_triage_ord > gold_upper,
    within_range = llm_triage_ord >= gold_lower & llm_triage_ord <= gold_upper,
    chose_less_urgent = llm_triage_ord == gold_lower,
    chose_more_urgent = llm_triage_ord == gold_upper
  )

within_range <- edge_analysis %>% filter(within_range)
n_less <- sum(within_range$chose_less_urgent)
n_within <- nrow(within_range)
binom_result <- binom.test(n_less, n_within, p = 0.5)

# Confidence vs mistriage
pb <- cor.test(df$llm_confidence, as.numeric(df$mistriage))

conf_by_group <- df %>%
  mutate(group = ifelse(mistriage, "mistriaged", "correct")) %>%
  group_by(group) %>%
  summarise(
    n        = n(),
    mean_conf = round(mean(llm_confidence, na.rm = TRUE), 1),
    sd_conf   = round(sd(llm_confidence, na.rm = TRUE), 1),
    .groups  = "drop"
  )

tt <- t.test(llm_confidence ~ mistriage, data = df, na.action = na.omit)

# Domain breakdown
domain_results <- clear %>%
  group_by(domain) %>%
  summarise(
    n          = n(),
    under_n    = sum(under_triage),
    under_rate = round(mean(under_triage) * 100, 1),
    over_n     = sum(over_triage),
    over_rate  = round(mean(over_triage) * 100, 1),
    correct_rate = round(mean(correct) * 100, 1),
    .groups    = "drop"
  ) %>%
  arrange(desc(under_rate))

# Anchoring effect
edge <- edge %>% mutate(has_shift = llm_triage_ord != baseline_triage_ord)

anchor_shifts <- edge %>%
  group_by(has_anchor) %>%
  summarise(
    n = n(),
    shift_n = sum(has_shift, na.rm = TRUE),
    shift_rate = round(mean(has_shift, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# Labs accuracy
labs_acc <- clear %>%
  group_by(has_labs) %>%
  summarise(
    n     = n(),
    correct_n = sum(correct),
    accuracy  = round(mean(correct) * 100, 1),
    .groups = "drop"
  )

m_labs <- glmer(correct ~ has_labs + (1 | case_pair),
                data = clear, family = binomial,
                control = glmerControl(optimizer = "bobyqa"))
coefs_labs <- summary(m_labs)$coefficients

# Output results
results <- list(
  acuity = acuity_acc,
  edge_below_floor = sum(edge_analysis$below_floor),
  edge_above_ceiling = sum(edge_analysis$above_ceiling),
  edge_within_range = sum(edge_analysis$within_range),
  less_urgent_pref = c(n = n_less, total = n_within, pct = round(n_less / n_within * 100, 1)),
  binom_p = binom_result$p.value,
  confidence_r = pb$estimate,
  confidence_p = pb$p.value,
  conf_by_group = conf_by_group,
  t_test = tt,
  domain = domain_results,
  anchor_shifts = anchor_shifts,
  labs_acc = labs_acc,
  labs_glmm = coefs_labs
)

print(acuity_acc)
print(domain_results)
print(anchor_shifts)
print(labs_acc)

write_csv(acuity_acc, paste0(OUT_DIR, "acuity_accuracy.csv"))
write_csv(domain_results, paste0(OUT_DIR, "domain_results.csv"))
write_csv(anchor_shifts, paste0(OUT_DIR, "anchor_shifts.csv"))
write_csv(labs_acc, paste0(OUT_DIR, "labs_accuracy.csv"))
