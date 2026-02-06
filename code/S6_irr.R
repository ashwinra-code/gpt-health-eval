library(readr)
library(dplyr)
library(irr)

ratings <- read_csv("/data/IRR_rating_FINAL.csv", show_col_types = FALSE)

stopifnot(
  nrow(ratings) == 60,
  all(c("case_id", "rater_1", "rater_2", "rater_3") %in% names(ratings))
)

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

write_csv(results, "/results/irr_results.csv")
