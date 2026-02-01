library(readr)
library(dplyr)
library(irr)

DATA_DIR <- "../data/"
OUT_DIR  <- "output/"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

ratings <- read_csv(paste0(DATA_DIR, "irr_ratings.csv"), show_col_types = FALSE)

stopifnot(
  nrow(ratings) == 60,
  all(c("case_id", "rater_1", "rater_2", "rater_3") %in% names(ratings))
)

main <- read_csv(paste0(DATA_DIR, "DataOriginal_FINAL.csv"), show_col_types = FALSE) %>%
  distinct(case_id, .keep_all = TRUE) %>%
  mutate(
    vignette_type = ifelse(
      grepl("^(E|MH)", case_id), "Labs", "No labs"
    )
  ) %>%
  select(case_id, is_edge_case, vignette_type)

ratings <- ratings %>% left_join(main, by = "case_id")

make_matrix <- function(df) {
  as.matrix(df %>% select(rater_1, rater_2, rater_3))
}

strict_agree <- function(df) {
  n <- nrow(df)
  agree <- sum(df$rater_1 == df$rater_2 & df$rater_2 == df$rater_3)
  list(agree = agree, n = n, pct = round(100 * agree / n, 1))
}

expand_triage <- function(x) {
  switch(as.character(x),
    "A"   = "A",
    "B"   = "B",
    "C"   = "C",
    "D"   = "D",
    "A/B" = c("A", "B"),
    "B/C" = c("B", "C"),
    "C/D" = c("C", "D"),
    as.character(x)
  )
}

overlap_agree_row <- function(r1, r2, r3) {
  s1 <- expand_triage(r1)
  s2 <- expand_triage(r2)
  s3 <- expand_triage(r3)
  length(intersect(s1, s2)) > 0 &
    length(intersect(s2, s3)) > 0 &
    length(intersect(s1, s3)) > 0
}

partial_agree <- function(df) {
  n <- nrow(df)
  agree <- sum(mapply(overlap_agree_row, df$rater_1, df$rater_2, df$rater_3))
  list(agree = agree, n = n, pct = round(100 * agree / n, 1))
}

landis_koch <- function(k) {
  if (k < 0)    return("Poor")
  if (k <= 0.20) return("Slight")
  if (k <= 0.40) return("Fair")
  if (k <= 0.60) return("Moderate")
  if (k <= 0.80) return("Substantial")
  return("Almost perfect")
}

compute_fleiss <- function(df) {
  mat <- make_matrix(df)
  fk <- kappam.fleiss(mat)
  k <- fk$value
  se <- (1 - k) / sqrt(nrow(df) * (ncol(mat) - 1))
  ci_low <- k - 1.96 * se
  ci_high <- k + 1.96 * se
  list(
    kappa = round(k, 3),
    ci_low = round(ci_low, 3),
    ci_high = round(ci_high, 3),
    p = fk$p.value,
    interpretation = landis_koch(k)
  )
}

# Overall
sa <- strict_agree(ratings)
pa <- partial_agree(ratings)
fk <- compute_fleiss(ratings)

results <- data.frame(
  subset = "Overall",
  n = sa$n,
  strict_agree = sa$agree,
  strict_pct = sa$pct,
  partial_agree = pa$agree,
  partial_pct = pa$pct,
  kappa = fk$kappa,
  ci_low = fk$ci_low,
  ci_high = fk$ci_high,
  interpretation = fk$interpretation
)

# By vignette type
for (vt in c("Labs", "No labs")) {
  sub <- ratings %>% filter(vignette_type == vt)
  sa_sub <- strict_agree(sub)
  pa_sub <- partial_agree(sub)
  fk_sub <- compute_fleiss(sub)
  results <- rbind(results, data.frame(
    subset = vt,
    n = sa_sub$n,
    strict_agree = sa_sub$agree,
    strict_pct = sa_sub$pct,
    partial_agree = pa_sub$agree,
    partial_pct = pa_sub$pct,
    kappa = fk_sub$kappa,
    ci_low = fk_sub$ci_low,
    ci_high = fk_sub$ci_high,
    interpretation = fk_sub$interpretation
  ))
}

# By case type
for (ec in c("yes", "no")) {
  label <- ifelse(ec == "yes", "Edge cases", "Clear cases")
  sub <- ratings %>% filter(is_edge_case == ec)
  sa_sub <- strict_agree(sub)
  pa_sub <- partial_agree(sub)
  fk_sub <- compute_fleiss(sub)
  results <- rbind(results, data.frame(
    subset = label,
    n = sa_sub$n,
    strict_agree = sa_sub$agree,
    strict_pct = sa_sub$pct,
    partial_agree = pa_sub$agree,
    partial_pct = pa_sub$pct,
    kappa = fk_sub$kappa,
    ci_low = fk_sub$ci_low,
    ci_high = fk_sub$ci_high,
    interpretation = fk_sub$interpretation
  ))
}

print(results)
write_csv(results, paste0(OUT_DIR, "irr_results.csv"))
