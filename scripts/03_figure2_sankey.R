library(readr)
library(dplyr)
library(ggplot2)
library(ggalluvial)

DATA_DIR <- "../data/"
OUT_DIR  <- "output/"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

colors <- list(
  over   = "#0072B2",
  over2  = "#56B4E9",
  under  = "#D55E00",
  text   = "#2C3E50"
)

theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(color = colors$text),
      axis.title = element_text(face = "bold"),
      axis.line = element_line(color = colors$text, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

df_clear <- read_csv(paste0(DATA_DIR, "DataOriginal_FINAL.csv"), show_col_types = FALSE) %>%
  filter(is_edge_case == "no") %>%
  mutate(
    gold_upper = recode(gold_triage,
                        A = 1L, B = 2L, C = 3L, D = 4L,
                        `A/B` = 2L, `B/C` = 3L, `C/D` = 4L),
    shift = llm_triage_ord - gold_upper
  )

label_levels <- c("Home (A)", "Routine (B)", "Urgent (C)", "ED now (D)")

df_clear <- df_clear %>%
  mutate(
    gold_label = factor(
      recode(gold_triage, A = "Home (A)", B = "Routine (B)",
             C = "Urgent (C)", D = "ED now (D)"),
      levels = label_levels),
    llm_label = factor(
      recode(llm_triage, A = "Home (A)", B = "Routine (B)",
             C = "Urgent (C)", D = "ED now (D)"),
      levels = label_levels)
  )

gold_totals <- df_clear %>% count(gold_label, name = "gold_n")

df_err <- df_clear %>%
  filter(shift != 0) %>%
  mutate(
    shift_category = case_when(
      shift < 0  ~ "Under-triage (1 level)",
      shift == 1 ~ "Over-triage (1 level)",
      shift >= 2 ~ "Over-triage (2 levels)"
    ),
    shift_category = factor(shift_category,
      levels = c("Under-triage (1 level)",
                 "Over-triage (1 level)",
                 "Over-triage (2 levels)"))
  )

total_clear  <- nrow(df_clear)
total_errors <- nrow(df_err)

flow_df <- df_err %>%
  count(gold_label, llm_label, shift_category, name = "freq") %>%
  left_join(gold_totals, by = "gold_label") %>%
  mutate(
    row_pct = 100 * freq / gold_n,
    flow_label = ifelse(freq >= 8,
                        paste0(freq, "\n(", sprintf("%.1f", row_pct), "%)"),
                        NA_character_)
  )

shift_colors <- c(
  "Under-triage (1 level)" = colors$under,
  "Over-triage (1 level)"  = colors$over,
  "Over-triage (2 levels)" = colors$over2
)

p <- ggplot(flow_df,
            aes(y = freq,
                axis1 = gold_label,
                axis2 = llm_label)) +
  geom_flow(aes(fill = shift_category), width = 1/3, alpha = 0.90, colour = NA) +
  geom_stratum(fill = "#4A4A4A", width = 1/3, color = NA) +
  geom_text(stat = "stratum",
            aes(label = paste0(after_stat(stratum), "\n", round(after_stat(count)))),
            color = "white", fontface = "bold", size = 3.2,
            lineheight = 0.9) +
  geom_label(stat = "flow",
             aes(label = flow_label),
             fill = "white",
             color = colors$text,
             size = 2.7,
             label.size = 0,
             label.padding = grid::unit(0.10, "lines"),
             alpha = 0.90,
             na.rm = TRUE) +
  scale_fill_manual(values = shift_colors, name = "Mis-triage direction") +
  scale_x_discrete(limits = c("Gold standard\n(mis-triaged cases only)",
                             "LLM recommendation\n(for mis-triaged cases)"),
                   expand = c(0.12, 0.06)) +
  labs(
    y = NULL,
    title = "Where mis-triaged patients are routed",
    subtitle = paste0("Clear cases only. Mis-triaged: ", total_errors, "/", total_clear,
                      ". Intermediate acuity (B/C) receives 119/162 = 73.5% of mis-triages.")
  ) +
  theme_pub(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = grid::unit(0.55, "cm"),
    legend.text = element_text(size = 9),
    panel.grid.major = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(10, 12, 10, 12)
  ) +
  guides(fill = guide_legend(nrow = 1))

suppressWarnings(
  ggsave(paste0(OUT_DIR, "Figure2_Sankey.png"), p,
         width = 9.2, height = 6.0, dpi = 400, bg = "white")
)
suppressWarnings(
  ggsave(paste0(OUT_DIR, "Figure2_Sankey.pdf"), p,
         width = 9.2, height = 6.0, device = cairo_pdf)
)
