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
    correct = llm_triage_ord == gold_upper,
    has_labs = grepl("^(E|MH)", case_id)
  )

clear <- df %>% filter(is_edge_case == "no")

acuity_levels <- c("A", "B", "C", "D")

results <- data.frame(
  Acuity       = character(),
  N_Labs       = integer(),
  Correct_Labs = integer(),
  Acc_Labs     = numeric(),
  N_NoLabs     = integer(),
  Correct_NoLabs = integer(),
  Acc_NoLabs   = numeric(),
  Diff_pp      = numeric(),
  OR           = numeric(),
  CI_lo        = numeric(),
  CI_hi        = numeric(),
  p_value      = character(),
  stringsAsFactors = FALSE
)

for (lvl in acuity_levels) {
  sub <- clear %>% filter(gold_triage == lvl)

  labs_yes <- sub %>% filter(has_labs == TRUE)
  labs_no  <- sub %>% filter(has_labs == FALSE)

  n_labs    <- nrow(labs_yes)
  n_nolabs  <- nrow(labs_no)
  corr_labs   <- sum(labs_yes$correct)
  corr_nolabs <- sum(labs_no$correct)
  acc_labs    <- round(corr_labs / n_labs * 100, 1)
  acc_nolabs  <- round(corr_nolabs / n_nolabs * 100, 1)
  diff_pp     <- round(acc_labs - acc_nolabs, 1)

  tbl <- matrix(
    c(corr_labs, n_labs - corr_labs,
      corr_nolabs, n_nolabs - corr_nolabs),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("Labs", "NoLabs"), c("Correct", "Incorrect"))
  )

  ft <- fisher.test(tbl)

  results <- rbind(results, data.frame(
    Acuity       = lvl,
    N_Labs       = n_labs,
    Correct_Labs = corr_labs,
    Acc_Labs     = acc_labs,
    N_NoLabs     = n_nolabs,
    Correct_NoLabs = corr_nolabs,
    Acc_NoLabs   = acc_nolabs,
    Diff_pp      = diff_pp,
    OR           = round(ft$estimate, 2),
    CI_lo        = round(ft$conf.int[1], 2),
    CI_hi        = round(ft$conf.int[2], 2),
    p_value      = formatC(ft$p.value, format = "g", digits = 3),
    stringsAsFactors = FALSE
  ))
}

# Overall GLMM
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

# Labs x Acuity interaction
d_clear <- clear %>% mutate(gold_fac = factor(gold_triage, levels = c("A", "B", "C", "D")))

m_additive <- glmer(correct ~ has_labs + gold_fac + (1 | case_pair),
                    family = binomial, data = d_clear,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

m_interact <- glmer(correct ~ has_labs * gold_fac + (1 | case_pair),
                    family = binomial, data = d_clear,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

lrt <- anova(m_additive, m_interact, test = "Chisq")

# Overall row
overall_row <- data.frame(
  Acuity       = "Overall",
  N_Labs       = sum(clear$has_labs),
  Correct_Labs = sum(clear$correct[clear$has_labs]),
  Acc_Labs     = round(mean(clear$correct[clear$has_labs]) * 100, 1),
  N_NoLabs     = sum(!clear$has_labs),
  Correct_NoLabs = sum(clear$correct[!clear$has_labs]),
  Acc_NoLabs   = round(mean(clear$correct[!clear$has_labs]) * 100, 1),
  Diff_pp      = round(mean(clear$correct[clear$has_labs]) * 100 -
                        mean(clear$correct[!clear$has_labs]) * 100, 1),
  OR           = round(or_overall, 2),
  CI_lo        = round(ci_lo_overall, 2),
  CI_hi        = round(ci_hi_overall, 2),
  p_value      = formatC(pval, format = "g", digits = 3),
  stringsAsFactors = FALSE
)

full_table <- rbind(results, overall_row)

print(full_table)
write_csv(full_table, paste0(OUT_DIR, "labs_accuracy_by_acuity.csv"))
