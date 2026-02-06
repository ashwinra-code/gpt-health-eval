library(readr)
library(dplyr)
library(ggplot2)
library(ggalluvial)

df <- read_csv("/data/DataOriginal_FINAL.csv", show_col_types = FALSE) %>%
  filter(is_edge_case == "no") %>%
  mutate(
    gold_upper = recode(gold_triage,
                        A = 1L, B = 2L, C = 3L, D = 4L,
                        `A/B` = 2L, `B/C` = 3L, `C/D` = 4L),
    shift = llm_triage_ord - gold_upper
  )

df_errors <- df %>%
  filter(shift != 0) %>%
  mutate(
    shift_category = case_when(
      shift <= -2 ~ "Under-triage (\u22652 levels)",
      shift == -1 ~ "Under-triage (1 level)",
      shift == 1  ~ "Over-triage (1 level)",
      shift >= 2  ~ "Over-triage (\u22652 levels)"
    ),
    shift_category = factor(shift_category,
      levels = c("Under-triage (\u22652 levels)",
                 "Under-triage (1 level)",
                 "Over-triage (1 level)",
                 "Over-triage (\u22652 levels)"))
  )

gold_error_n <- df_errors %>% count(gold_triage, name = "n_err")
gold_total_n <- df %>% count(gold_triage, name = "n_tot")
gold_labels <- gold_total_n %>%
  left_join(gold_error_n, by = "gold_triage") %>%
  mutate(n_err = ifelse(is.na(n_err), 0L, n_err),
         label = paste0(recode(gold_triage,
           A = "Home (A)", B = "Routine (B)",
           C = "Urgent (C)", D = "ED now (D)"),
           "\n", n_err, "/", n_tot, " wrong"))

label_map <- setNames(gold_labels$label, gold_labels$gold_triage)
label_levels_gold <- label_map[c("A", "B", "C", "D")]

llm_error_n <- df_errors %>% count(llm_triage, name = "n_land")
llm_labels <- llm_error_n %>%
  mutate(label = paste0(recode(llm_triage,
    A = "Home (A)", B = "Routine (B)",
    C = "Urgent (C)", D = "ED now (D)"),
    "\nn=", n_land))

llm_label_map <- setNames(llm_labels$label, llm_labels$llm_triage)
all_llm <- c("A", "B", "C", "D")
for (lev in all_llm) {
  if (!(lev %in% names(llm_label_map))) {
    llm_label_map[lev] <- paste0(recode(lev,
      A = "Home (A)", B = "Routine (B)",
      C = "Urgent (C)", D = "ED now (D)"), "\nn=0")
  }
}
label_levels_llm <- llm_label_map[c("A", "B", "C", "D")]

df_errors <- df_errors %>%
  mutate(
    gold_label = factor(label_map[gold_triage], levels = label_levels_gold),
    llm_label  = factor(llm_label_map[llm_triage], levels = label_levels_llm)
  )

flow_df <- df_errors %>%
  count(gold_label, llm_label, shift_category) %>%
  rename(freq = n)

cat("Error flow counts:\n")
print(flow_df, n = Inf)

shift_colors <- c(
  "Under-triage (\u22652 levels)" = "#A30000",
  "Under-triage (1 level)"        = "#D55E00",
  "Over-triage (1 level)"         = "#0072B2",
  "Over-triage (\u22652 levels)"  = "#004080"
)

colors <- list(text = "#2C3E50")

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

p <- ggplot(flow_df,
       aes(y = freq,
           axis1 = gold_label,
           axis2 = llm_label)) +
  geom_alluvium(aes(fill = shift_category),
                width = 1/3, alpha = 0.85,
                curve_type = "sigmoid") +
  geom_stratum(fill = "#4A4A4A", width = 1/3, color = NA) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            color = "white", fontface = "bold", size = 3.2,
            lineheight = 0.9) +
  annotate("label", x = 1.5, y = 123, label = "70",
           size = 3.2, fontface = "bold", fill = "#0072B2",
           color = "white",
           label.padding = unit(0.2, "lines")) +
  annotate("label", x = 1.35, y = 82, label = "13",
           size = 2.8, fontface = "bold", fill = "#004080",
           color = "white",
           label.padding = unit(0.2, "lines")) +
  annotate("label", x = 1.45, y = 42, label = "35",
           size = 3.2, fontface = "bold", fill = "#0072B2",
           color = "white",
           label.padding = unit(0.2, "lines")) +
  annotate("label", x = 1.55, y = 26, label = "33",
           size = 3.2, fontface = "bold", fill = "#D55E00",
           color = "white",
           label.padding = unit(0.2, "lines")) +
  annotate("label", x = 1.5, y = 159, label = "8",
           size = 2.5, fontface = "bold", fill = "#D55E00",
           color = "white",
           label.padding = unit(0.15, "lines")) +
  scale_fill_manual(values = shift_colors, name = "Error direction",
                    drop = TRUE) +
  scale_x_discrete(limits = c("Gold standard\n(true urgency)",
                               "LLM recommendation\n(assigned triage)"),
                   expand = c(0.15, 0.05)) +
  labs(y = NULL) +
  theme_pub(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 9),
    panel.grid.major = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = "bold", size = 11),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(fill = guide_legend(nrow = 1))

suppressWarnings(
  ggsave("/results/Figure2_Sankey.png", p,
         width = 8, height = 6.5, dpi = 400, bg = "white")
)
suppressWarnings(
  ggsave("/results/Figure2_Sankey.pdf", p,
         width = 8, height = 6.5, device = cairo_pdf)
)