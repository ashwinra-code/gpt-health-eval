library(readr)
library(dplyr)
library(tidyr)

df <- read_csv("/data/DataExpanded_FINAL.csv", show_col_types = FALSE)

psych <- df %>%
  filter(grepl("^(MH|NH)", case_id)) %>%
  mutate(
    scenario_num = as.integer(gsub("^(MH|NH)", "", case_id)),
    prompt_version = ifelse(grepl("^MH", case_id), "With labs", "Without labs")
  ) %>%
  filter(scenario_num != 3)

scenario_summary <- psych %>%
  group_by(case_id, scenario_num, prompt_version, diagnosis) %>%
  summarise(
    n = n(),
    crisis_n = sum(guardrail_triggered == 1, na.rm = TRUE),
    crisis_pct = round(mean(guardrail_triggered == 1, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(scenario_num, desc(prompt_version))

table_s8 <- scenario_summary %>%
  select(scenario_num, diagnosis, prompt_version, crisis_n, n) %>%
  mutate(
    crisis_str = paste0(crisis_n, "/", n)
  ) %>%
  select(scenario_num, diagnosis, prompt_version, crisis_str) %>%
  pivot_wider(
    names_from = prompt_version,
    values_from = crisis_str
  ) %>%
  rename(
    Scenario = scenario_num,
    Diagnosis = diagnosis,
    `Labs (MH)` = `With labs`,
    `No Labs (NH)` = `Without labs`
  )

by_version <- psych %>%
  group_by(prompt_version) %>%
  summarise(
    n_responses = n(),
    crisis_n = sum(guardrail_triggered == 1, na.rm = TRUE),
    crisis_pct = round(mean(guardrail_triggered == 1, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

write_csv(table_s8, "/results/Table_S8_guardrail.csv")
write_csv(scenario_summary, "/results/guardrail_scenario_summary.csv")
