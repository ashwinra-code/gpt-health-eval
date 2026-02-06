library(readr)
library(dplyr)
library(lme4)

df <- read_csv("/data/DataOriginal_FINAL.csv", show_col_types = FALSE) %>%
  mutate(
    gold_lower = recode(gold_triage,
      A = 1L, B = 2L, C = 3L, D = 4L,
      `A/B` = 1L, `B/C` = 2L, `C/D` = 3L),
    gold_upper = recode(gold_triage,
      A = 1L, B = 2L, C = 3L, D = 4L,
      `A/B` = 2L, `B/C` = 3L, `C/D` = 4L),
    correct = llm_triage_ord >= gold_lower & llm_triage_ord <= gold_upper,
    over_triage = llm_triage_ord > gold_upper,
    under_triage = llm_triage_ord < gold_lower,
    has_labs = grepl("^(E|MH)", case_id)
  )

clear <- df %>% filter(is_edge_case == "no")

overall <- clear %>%
  group_by(has_labs) %>%
  summarise(
    n = n(),
    correct_n = sum(correct),
    accuracy = round(mean(correct) * 100, 1),
    .groups = "drop"
  )

m_labs <- glmer(correct ~ has_labs + (1 | case_pair),
                data = clear, family = binomial,
                control = glmerControl(optimizer = "bobyqa"))
coefs <- summary(m_labs)$coefficients
est <- coefs["has_labsTRUE", "Estimate"]
se  <- coefs["has_labsTRUE", "Std. Error"]
pval <- coefs["has_labsTRUE", "Pr(>|z|)"]
or_overall  <- exp(est)
ci_lo_overall <- exp(est - 1.96 * se)
ci_hi_overall <- exp(est + 1.96 * se)

acuity_levels <- c("A", "B", "C", "D")

results <- data.frame(
  Acuity = character(),
  Metric = character(),
  N_Labs = integer(),
  Rate_Labs = numeric(),
  N_NoLabs = integer(),
  Rate_NoLabs = numeric(),
  Diff_pp = numeric(),
  OR = numeric(),
  CI_lo = numeric(),
  CI_hi = numeric(),
  p_value = character(),
  stringsAsFactors = FALSE
)

for (lvl in acuity_levels) {
  sub <- clear %>% filter(gold_triage == lvl)

  labs_yes <- sub %>% filter(has_labs == TRUE)
  labs_no  <- sub %>% filter(has_labs == FALSE)

  n_labs <- nrow(labs_yes)
  n_nolabs <- nrow(labs_no)

  if (lvl == "A") {
    metric <- "Over-triage"
    outcome_labs <- sum(labs_yes$over_triage)
    outcome_nolabs <- sum(labs_no$over_triage)
  } else if (lvl == "D") {
    metric <- "Under-triage"
    outcome_labs <- sum(labs_yes$under_triage)
    outcome_nolabs <- sum(labs_no$under_triage)
  } else {
    metric <- "Accuracy"
    outcome_labs <- sum(labs_yes$correct)
    outcome_nolabs <- sum(labs_no$correct)
  }

  rate_labs <- round(outcome_labs / n_labs * 100, 1)
  rate_nolabs <- round(outcome_nolabs / n_nolabs * 100, 1)
  diff_pp <- round(rate_labs - rate_nolabs, 1)

  tbl <- matrix(
    c(outcome_labs, n_labs - outcome_labs,
      outcome_nolabs, n_nolabs - outcome_nolabs),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("Labs", "NoLabs"), c("Outcome", "NoOutcome"))
  )

  ft <- fisher.test(tbl)

  results <- rbind(results, data.frame(
    Acuity = lvl,
    Metric = metric,
    N_Labs = n_labs,
    Rate_Labs = rate_labs,
    N_NoLabs = n_nolabs,
    Rate_NoLabs = rate_nolabs,
    Diff_pp = diff_pp,
    OR = round(ft$estimate, 2),
    CI_lo = round(ft$conf.int[1], 2),
    CI_hi = round(ft$conf.int[2], 2),
    p_value = formatC(ft$p.value, format = "g", digits = 3),
    stringsAsFactors = FALSE
  ))
}

d_clear <- clear %>% mutate(gold_fac = factor(gold_triage, levels = c("A", "B", "C", "D")))

m_additive <- glmer(correct ~ has_labs + gold_fac + (1 | case_pair),
                    family = binomial, data = d_clear,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

m_interact <- glmer(correct ~ has_labs * gold_fac + (1 | case_pair),
                    family = binomial, data = d_clear,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

lrt <- anova(m_additive, m_interact, test = "Chisq")

write_csv(results, "/results/labs_accuracy_by_acuity.csv")
