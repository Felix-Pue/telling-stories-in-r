library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)
library(RColorBrewer)
# Modify Data
lines <- readLines("teleworkable_job_2025.csv")
lines <- str_remove(lines, '^"')
lines <- str_remove(lines, '"$')
mat <- str_split_fixed(lines, "\t", n = 6)
colnames(mat) <- c("Group", mat[1, 2:6])
df2 <- as.data.frame(mat[-1, ], stringsAsFactors = FALSE)
# Mutate
df2 <- df2 %>%
  mutate(across(-Group, ~ as.numeric(.x)))
group_order <- c("Non-teleworkable job", "Constrained", "Reluctant", "Aligned teleworker")
# Make graph long
df_long2 <- df2 %>%
  pivot_longer(
    cols = -Group,
    names_to = "Measure",
    values_to = "Percent"
  ) %>%
  mutate(
    Group = factor(Group, levels = group_order),
    Measure = factor(Measure, levels = c(
      "Worried about work",
      "Tired after work",
      "Job affects family time",
      "Family affects concentration at work",
      "Family affects work time"
    )),
    Measure_wrap = str_wrap(as.character(Measure), width = 24)
  ) %>%
  filter(!is.na(Group))
# Create Plot
ggplot(df_long2, aes(x = Group, y = Percent, fill = Measure_wrap)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7, color = "white", linewidth = 0.25
  ) +
  scale_fill_manual(values = brewer.pal(5, "RdYlBu")) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, 10),
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "Well-Being by Onsite & Hybrid Group",
    x = NULL,
    y = "Share of respondents",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )
