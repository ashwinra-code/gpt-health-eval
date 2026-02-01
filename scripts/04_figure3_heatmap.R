library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

DATA_DIR <- "../data/"
OUT_DIR  <- "output/"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

COL_CORRECT <- "#00A087"
COL_OVER    <- "#3C5488"
COL_UNDER   <- "#E64B35"
COL_TEXT    <- "#2C3E50"

blend_white <- function(hex, fraction) {
  rgb_col <- col2rgb(hex)[, 1]
  blended <- round(255 - fraction * (255 - rgb_col))
  rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
}

theme_heatmap <- function(base_size = 12, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title    = element_blank(),
      plot.subtitle = element_blank(),
      plot.tag      = element_text(face = "bold", size = base_size * 1.4),
      plot.tag.position = c(0.0, 1.02),
      axis.title    = element_text(face = "bold"),
      axis.text     = element_text(face = "bold", color = "black", size = base_size + 1),
      panel.grid    = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text  = element_text(size = base_size),
      legend.key.height = grid::unit(0.5, "cm"),
      legend.key.width  = grid::unit(0.8, "cm"),
      plot.margin  = margin(10, 12, 10, 12)
    )
}

axis_labels <- c(
  "A" = "Home (A)",
  "B" = "Routine (B)",
  "C" = "Urgent (C)",
  "D" = "ED now (D)"
)

df <- read_csv(paste0(DATA_DIR, "DataOriginal_FINAL.csv"), show_col_types = FALSE) %>%
  filter(is_edge_case == "no") %>%
  mutate(
    gold_ord = recode(gold_triage, A = 1L, B = 2L, C = 3L, D = 4L),
    llm_ord  = recode(llm_triage,  A = 1L, B = 2L, C = 3L, D = 4L)
  )

confusion <- df %>%
  mutate(
    gold_label = factor(gold_triage, levels = c("A", "B", "C", "D")),
    llm_label  = factor(llm_triage,  levels = c("A", "B", "C", "D"))
  ) %>%
  count(gold_label, llm_label, .drop = FALSE) %>%
  group_by(gold_label) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    gold_i = as.numeric(gold_label),
    llm_i  = as.numeric(llm_label),
    outcome = case_when(
      gold_i == llm_i ~ "Correct",
      llm_i  > gold_i ~ "Over-triage",
      llm_i  < gold_i ~ "Under-triage"
    )
  )

outcome_colors <- c(
  "Correct"      = COL_CORRECT,
  "Over-triage"  = COL_OVER,
  "Under-triage" = COL_UNDER
)

confusion <- confusion %>%
  mutate(
    base_hex  = outcome_colors[outcome],
    alpha_val = ifelse(n == 0, 0, pmax(pct / 100, 0.15)),
    fill_hex  = mapply(function(hex, a) {
      if (a == 0) return("#F5F5F5")
      blend_white(hex, a)
    }, base_hex, alpha_val),
    text_color = ifelse(alpha_val >= 0.35, "white", COL_TEXT),
    cell_label = sprintf("%d\n(%.1f%%)", n, pct)
  )

conf_nonzero <- confusion %>% filter(n > 0)
conf_zero    <- confusion %>% filter(n == 0)

highlight_cell <- conf_nonzero %>%
  filter(outcome == "Under-triage") %>%
  slice_max(pct, n = 1, with_ties = FALSE)

p <- ggplot() +
  geom_tile(
    data = conf_zero,
    aes(x = gold_label, y = llm_label),
    fill = "#F5F5F5", color = "#E0E0E0", linewidth = 0.5
  ) +
  geom_tile(
    data = conf_nonzero,
    aes(x = gold_label, y = llm_label),
    fill = conf_nonzero$fill_hex,
    color = "white", linewidth = 1.0
  ) +
  geom_tile(
    data = highlight_cell,
    aes(x = gold_label, y = llm_label),
    fill = NA, color = "black", linewidth = 1.5
  ) +
  geom_text(
    data = conf_nonzero,
    aes(x = gold_label, y = llm_label, label = cell_label),
    color = conf_nonzero$text_color,
    size = 4.2,
    fontface = "bold",
    lineheight = 0.9
  ) +
  geom_point(
    data = data.frame(
      gold_label = factor(c("A", "A", "A"), levels = c("A", "B", "C", "D")),
      llm_label  = factor(c("A", "A", "A"), levels = c("A", "B", "C", "D")),
      outcome    = factor(c("Correct", "Over-triage", "Under-triage"),
                          levels = c("Correct", "Over-triage", "Under-triage"))
    ),
    aes(x = gold_label, y = llm_label, color = outcome),
    alpha = 0, size = 0
  ) +
  scale_color_manual(
    values = c(
      "Correct"      = COL_CORRECT,
      "Over-triage"  = COL_OVER,
      "Under-triage" = COL_UNDER
    ),
    name = "Outcome",
    guide = guide_legend(
      override.aes = list(
        alpha = 1, shape = 15, size = 5
      )
    )
  ) +
  scale_x_discrete(position = "top", limits = c("A", "B", "C", "D"), labels = axis_labels) +
  scale_y_discrete(limits = c("A", "B", "C", "D"), labels = axis_labels) +
  coord_fixed(clip = "off") +
  labs(
    tag = "A",
    x = "Gold Standard Triage",
    y = "AI Recommendation"
  ) +
  theme_heatmap()

ggsave(paste0(OUT_DIR, "Figure3_Heatmap.png"), p,
       width = 6.5, height = 5.5, dpi = 400, bg = "white")
ggsave(paste0(OUT_DIR, "Figure3_Heatmap.pdf"), p,
       width = 6.5, height = 5.5, device = cairo_pdf)
