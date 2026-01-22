#1. Set-Up
library(tidyverse)
library(scales)
#2. Load Data
df <- read_csv("TUM WS 25 26/Dataset_Telling_Stories_in R/post_pandemic_remote_work_health_impact_2025.csv")

#3. Clean Data
df_clean <- df %>%
  #Fix column name
  rename(Salary_Range_raw = `Salary_Range;;;;`) %>%
  # Drop NA Rows
  filter(!is.na(Work_Arrangement),
         !is.na(Burnout_Level),
         !is.na(Social_Isolation_Score))
df_clean <- df_clean %>%
  mutate(
    Work_Arrangement = factor(Work_Arrangement, levels = c("Onsite", "Hybrid", "Remote"))
  )

#4. Manipulate Dataset
#4.1 Change Burnout Level to numerical
df2 <- df_clean %>%
  mutate(
    Burnout_Score = case_when(
      Burnout_Level == "Low" ~ 1,
      Burnout_Level == "Medium" ~ 2,
      Burnout_Level == "High" ~ 3
    )
  )

#4.2 Create Percentage Scores
counts <- df2 %>%
  count(Work_Arrangement, Social_Isolation_Score, Burnout_Score, name = "n")
totals <- counts %>%
  group_by(Work_Arrangement) %>%
  summarise(total = sum(n), .groups = "drop")
heat <- counts %>%
  left_join(totals, by = "Work_Arrangement") %>%
  mutate(pct = n / total)


#5.Create Plot
#5.1 Label Tiles above 10% share
heat <- heat %>%
  mutate(label = if_else(pct >= 0.10, percent(pct, accuracy = 1), ""))

ggplot(heat, aes(x = Social_Isolation_Score, y = Burnout_Score, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label), size = 3.6) +
  facet_grid(~ Work_Arrangement) +
  scale_x_continuous(breaks = 1:5, labels = c("Low (1)", "2", "3", "4", "High (5)")) +
  scale_y_continuous(
    breaks = 1:3,
    labels = c("Low (1)", "Medium (2)", "High (3)")
  ) +
  scale_fill_distiller(
    palette = "RdBu",
    direction = -1,
    name = "% of group",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Work arrangement and its influence on Isolation vs. Bournout",
    subtitle = "Each tile shows the share of people within that work arrangement",
    x = "Social isolation score (1–5)",
    y = "Burnout level (1-3)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  )

