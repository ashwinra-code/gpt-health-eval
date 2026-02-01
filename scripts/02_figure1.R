library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)

DATA_DIR <- "../data/"
OUT_DIR  <- "output/"

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

df <- read_csv(paste0(DATA_DIR, "DataOriginal_FINAL.csv"), show_col_types = FALSE) %>%
  mutate(
    gold_upper = recode(gold_triage, A=1L, B=2L, C=3L, D=4L, `A/B`=2L, `B/C`=3L, `C/D`=4L),
    under_triage = llm_triage_ord < gold_upper,
    over_triage = llm_triage_ord > gold_upper,
    correct = llm_triage_ord == gold_upper
  )

colors <- list(over = "#0072B2", under = "#D55E00", correct = "#999999", text = "#2C3E50")

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

error_data <- df %>%
  filter(is_edge_case == "no") %>%
  group_by(gold_triage) %>%
  summarise(
    n = n(),
    over_n = sum(over_triage),
    under_n = sum(under_triage),
    correct_n = sum(correct),
    over_rate = mean(over_triage) * 100,
    under_rate = mean(under_triage) * 100,
    correct_rate = mean(correct) * 100,
    total_error = (1 - mean(correct)) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    gold_label = factor(gold_triage, levels = c("A", "B", "C", "D"),
                        labels = c("Home\n(A)", "Routine\n(B)", "Urgent\n(C)", "ED now\n(D)")),
    gold_label_h = factor(gold_triage, levels = c("D", "C", "B", "A"),
                          labels = c("ED now (D)\nn=64", "Urgent (C)\nn=160",
                                     "Routine (B)\nn=128", "Home (A)\nn=128"))
  )

panel_a <- ggplot(error_data, aes(x = gold_label, y = total_error)) +
  geom_hline(yintercept = 50, color = "#ECEFF1", linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(group = 1), color = colors$text, linewidth = 1.3) +
  geom_point(size = 7, color = colors$text, fill = "white", shape = 21, stroke = 1.8) +
  geom_text(aes(label = sprintf("%.1f", total_error)), size = 2.5, fontface = "bold") +
  geom_text(data = error_data %>% filter(gold_triage == "A"),
            aes(y = total_error - 12, label = paste0(over_n, "/", n)),
            size = 2.8, hjust = 0.3, color = "#7F8C8D") +
  geom_text(data = error_data %>% filter(gold_triage == "D"),
            aes(y = total_error - 10, label = paste0(under_n, "/", n)),
            size = 2.8, color = "#7F8C8D") +
  annotate("text", x = 1.4, y = 56, label = "All errors\nover-triage",
           color = colors$over, fontface = "bold.italic", size = 3, hjust = 0, lineheight = 0.85) +
  annotate("curve", x = 1.35, xend = 1.08, y = 54, yend = 60, curvature = 0.2,
           color = colors$over, linewidth = 0.4, arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +
  annotate("text", x = 3.6, y = 42, label = "All errors\nunder-triage",
           color = colors$under, fontface = "bold.italic", size = 3, hjust = 1, lineheight = 0.85) +
  annotate("curve", x = 3.65, xend = 3.92, y = 44, yend = 49, curvature = -0.2,
           color = colors$under, linewidth = 0.4, arrow = arrow(length = unit(0.08, "inches"), type = "closed")) +
  scale_y_continuous(limits = c(0, 72), breaks = c(0, 25, 50), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = NULL, y = "Mis-triage rate") +
  theme_pub() +
  theme(panel.grid.major = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank()) +
  coord_cartesian(clip = "off")

plot_data <- error_data %>%
  mutate(y_pos = as.numeric(gold_label_h), under_end = -under_rate,
         correct_end = correct_rate, over_end = correct_rate + over_rate)

panel_b <- ggplot(plot_data) +
  geom_rect(aes(ymin = y_pos - 0.38, ymax = y_pos + 0.38, xmin = under_end, xmax = 0), fill = colors$under) +
  geom_rect(aes(ymin = y_pos - 0.38, ymax = y_pos + 0.38, xmin = 0, xmax = correct_end), fill = colors$correct) +
  geom_rect(aes(ymin = y_pos - 0.38, ymax = y_pos + 0.38, xmin = correct_end, xmax = over_end), fill = colors$over) +
  geom_vline(xintercept = 0, color = colors$text, linewidth = 0.6) +
  geom_text(data = plot_data %>% filter(under_rate > 15),
            aes(y = y_pos, x = under_end / 2, label = paste0(round(under_rate, 1), "%\n(", under_n, ")")),
            color = "white", fontface = "bold", size = 3, lineheight = 0.9) +
  geom_text(data = plot_data %>% filter(gold_triage %in% c("B", "C")),
            aes(y = y_pos, x = correct_end / 2, label = paste0(round(correct_rate, 1), "%")),
            color = "white", fontface = "bold", size = 3.2) +
  geom_text(data = plot_data %>% filter(gold_triage %in% c("A", "D")),
            aes(y = y_pos, x = correct_end / 2, label = paste0(round(correct_rate, 1), "%\n(", correct_n, ")")),
            color = "white", fontface = "bold", size = 2.8, lineheight = 0.9) +
  geom_text(data = plot_data %>% filter(over_rate > 15),
            aes(y = y_pos, x = correct_end + over_rate / 2, label = paste0(round(over_rate, 1), "%\n(", over_n, ")")),
            color = "white", fontface = "bold", size = 3, lineheight = 0.9) +
  annotate("segment", x = -45, xend = -35, y = 0.35, yend = 0.35, color = colors$under, linewidth = 3) +
  annotate("text", x = -33, y = 0.35, label = "Under-triage", color = colors$under, size = 2.8, hjust = 0, fontface = "bold") +
  annotate("segment", x = 20, xend = 30, y = 0.35, yend = 0.35, color = colors$correct, linewidth = 3) +
  annotate("text", x = 32, y = 0.35, label = "Correct", color = colors$correct, size = 2.8, hjust = 0, fontface = "bold") +
  annotate("segment", x = 60, xend = 70, y = 0.35, yend = 0.35, color = colors$over, linewidth = 3) +
  annotate("text", x = 72, y = 0.35, label = "Over-triage", color = colors$over, size = 2.8, hjust = 0, fontface = "bold") +
  scale_x_continuous(breaks = seq(-50, 100, 25), labels = function(x) paste0(abs(x), "%"), limits = c(-58, 105), expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:4, labels = levels(plot_data$gold_label_h), limits = c(0.2, 4.6), expand = c(0, 0)) +
  labs(x = "% of cases: Under-triage <- 0 -> Correct + Over-triage", y = NULL) +
  theme_pub() +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(face = "bold", lineheight = 1.0))

fig <- (panel_a | panel_b) +
  plot_layout(widths = c(0.42, 0.58)) +
  plot_annotation(tag_levels = "A", theme = theme(plot.tag = element_text(size = 14, face = "bold")))

ggsave(paste0(OUT_DIR, "Figure1.png"), fig, width = 11, height = 4.5, dpi = 400, bg = "white")
ggsave(paste0(OUT_DIR, "Figure1.pdf"), fig, width = 11, height = 4.5, device = cairo_pdf)
