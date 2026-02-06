library(readr)
library(dplyr)

df <- read_csv("/data/DataOriginal_FINAL.csv", show_col_types = FALSE) %>%
  mutate(
    gold_upper = recode(gold_triage, A=1L, B=2L, C=3L, D=4L, `A/B`=2L, `B/C`=3L, `C/D`=4L),
    under_triage = llm_triage_ord < gold_upper,
    over_triage  = llm_triage_ord > gold_upper,
    correct      = llm_triage_ord == gold_upper
  )

clear <- df %>% filter(is_edge_case == "no")
edge  <- df %>% filter(is_edge_case == "yes")

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

edge <- edge %>% mutate(has_shift = llm_triage_ord != baseline_triage_ord)

anchor_shifts <- edge %>%
  group_by(has_anchor) %>%
  summarise(
    n = n(),
    shift_n = sum(has_shift, na.rm = TRUE),
    shift_rate = round(mean(has_shift, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

write_csv(acuity_acc, "/results/acuity_accuracy.csv")
write_csv(domain_results, "/results/domain_results.csv")
write_csv(anchor_shifts, "/results/anchor_shifts.csv")
