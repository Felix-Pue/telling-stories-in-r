library(readxl)
library(tidyverse)
library(scales)

# --- read in data and prepare it for vizualization ---

# Define the file paths
file_path <- "Documents/Uni Unterlagen Master/5. Semester/Data Viz/remote_work.xlsx"
file_path_hours <- "Documents/Uni Unterlagen Master/5. Semester/Data Viz/hours_worked.xlsx"

# Helper function to read, clean, and label a specific sheet
read_eurostat_sheet <- function(path, sheet_name, label) {
  
  data <- read_excel(path, sheet = sheet_name, skip = 11, na = ":")
  
  data_clean <- data %>%
    rename(Country = 1) %>%
    slice(-(1:3)) %>%
    filter(!is.na(Country)) %>%
    filter(!str_starts(Country, "Special value")) %>% 
    filter(Country != ":") %>%
    pivot_longer(
      cols = -Country, 
      names_to = "Year", 
      values_to = "Percentage"
    ) %>%
    mutate(
      Year = as.numeric(Year),
      # --- HIER IST DIE ÄNDERUNG ---
      Percentage = as.numeric(Percentage), # Erzwingt Umwandlung in Zahlen
      Frequency = label
    )
  
  return(data_clean)
}

# Load the 3 sheets of the raw data file and the gdp file
df_sometimes <- read_eurostat_sheet(file_path, "Sheet 1", "Sometimes")
df_usually <- read_eurostat_sheet(file_path, "Sheet 2", "Usually")
df_never <- read_eurostat_sheet(file_path, "Sheet 3", "Never")

# Combine all sheets to a single df
remote_work_full <- bind_rows(df_never, df_sometimes, df_usually) %>%
  mutate(Frequency = factor(Frequency, levels = c("Never", "Sometimes", "Usually")))

# read in the worked hours sheet
raw_data <- read_excel(file_path_hours, sheet = 1)

# clean df
hours_worked_quarterly <- raw_data %>%
  select(Country, matches("\\d{4}-Q\\d")) %>%
  mutate(across(everything(), as.character)) %>%
  # change from wide to long
  pivot_longer(
    cols = -Country,
    names_to = "Quarter_String",
    values_to = "Hours_Change"
  ) %>%
  mutate(
    Hours_Change = na_if(Hours_Change, ":"),
    Hours_Change = as.numeric(Hours_Change)
  ) %>%
  separate(Quarter_String, into = c("Year", "Quarter"), sep = "-") %>%
  mutate(
    Year = as.numeric(Year),
    Quarter = as.numeric(str_remove(Quarter, "Q")),
    Year_Quarter_Num = Year + (Quarter - 1) / 4
  ) %>%
  filter(Year >= 2015)

# --- first graph ---
average_trend <- remote_work_full %>%
  group_by(Year, Frequency) %>%
  summarise(Avg_Percentage = mean(Percentage, na.rm = TRUE), .groups = "drop")

hours_trend <- hours_worked_quarterly %>%
  group_by(Year, Quarter, Year_Quarter_Num) %>%
  summarise(Hours_Change = mean(Hours_Change, na.rm = TRUE), .groups = "drop") %>%
  filter(Year_Quarter_Num <= 2024.0)

# scaling for secondary axis
offset <- 50             
scale_factor <- 50 / 15 

quarterly_breaks <- seq(2015, 2024, by = 0.25)

label_data <- average_trend %>%
  filter(Year %in% c(2019, 2024)) %>%
  mutate(
    vjust_pos = case_when(
      Year == 2019 & Frequency %in% c("Never", "Sometimes") ~ -1.2,
      Year == 2019 & Frequency == "Usually" ~ 2.2,
      Year == 2024 & Frequency == "Sometimes" ~ -1.2,
      Year == 2024 & Frequency %in% c("Never", "Usually") ~ 2.2,
      TRUE ~ -1.5 
    )
  )

# viziualizing the data
ggplot() + 
    annotate("rect", 
           xmin = 2019.917, xmax = 2023.25, 
           ymin = 0, ymax = 100, 
           alpha = 0.1, fill = "firebrick") + 
  
  annotate("text", 
           x = 2021.5835, 
           y = 100,
           vjust = -1,
           label = "COVID-19 Pandemic", 
           color = "firebrick", fontface = "bold", size = 4) +
  
  geom_hline(yintercept = offset, linetype = "dotted", color = "grey40") +
  annotate("text", x = 2015.2, y = offset + 2, label = NULL, 
           color = "grey40", size = 3, fontface = "italic", hjust = 0) +
  
  # hours worked line
  geom_line(data = hours_trend,
            aes(x = Year_Quarter_Num, 
                y = Hours_Change * scale_factor + offset, 
                color = "Hours Worked Change (Quarterly)"),
            linewidth = 1) +
  
  # remote work lines and points
  geom_line(data = average_trend, 
            aes(x = Year, y = Avg_Percentage, color = Frequency), 
            linewidth = 1.2) +
  
  geom_point(data = average_trend, 
             aes(x = Year, y = Avg_Percentage, color = Frequency), 
             size = 2, shape = 21, fill = "white", stroke = 1) +
  
  # data point labeling
  geom_text(data = label_data,
            aes(x = Year, y = Avg_Percentage, color = Frequency, 
                label = label_number(accuracy = 0.1, suffix = "%")(Avg_Percentage),
                vjust = vjust_pos), 
            fontface = "bold", size = 3.5, show.legend = FALSE) +
  
  scale_x_continuous(
    breaks = seq(2015, 2024, by = 1),
    minor_breaks = quarterly_breaks
  ) +
  
  scale_y_continuous(
    name = "Share of employees working remotely",
    labels = label_number(suffix = "%"),
    expand = c(0, 0),
    limits = c(0, 100),
    sec.axis = sec_axis(~ (. - offset) / scale_factor, 
                        name = "Change in Hours Worked (vs. previous quarter)",
                        labels = label_number(suffix = "%", accuracy = 1)) 
  ) +
  
  coord_cartesian(ylim = c(0, 100), clip = "off") +
  
  scale_color_manual(values = c("Never" = "grey50", 
                                "Sometimes" = "#56B4E9", 
                                "Usually" = "#0072B2",
                                "Hours Worked Change (Quarterly)" = "firebrick"),
                     labels = c("Hours Worked Change (Quarterly)" = "Change in worked hours")) + 
  
  labs(
    title = "Remote Work Trend and Working Hours Volatility in Europe",
    subtitle = "Comparing european remote work trends with changes in average hours worked",
    x = NULL,
    color = NULL 
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_line(color = "grey60", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    
    legend.position = "bottom",
    legend.justification = "left",
    
    axis.title.y.right = element_text(color = "black", margin = margin(l = 10)),
    axis.text.y.right = element_text(color = "black"),
    
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 30), color = "grey30"),
  )

# --- second graph ---
# prepare data
comparison_data <- remote_work_full %>%
  filter(Frequency != "Never") %>%
  group_by(Country, Year) %>%
  summarise(Total_Remote = sum(Percentage, na.rm = TRUE), .groups = "drop") %>%
  filter(Year %in% c(2019, 2024)) %>%
  pivot_wider(names_from = Year, values_from = Total_Remote, names_prefix = "Y") %>%
  drop_na(Y2019, Y2024) %>%
  filter(!str_detect(Country, "Euro")) %>% 
  filter(Y2024 > 0) %>% 
  filter(Y2019 > 0)

# vizualize data
ggplot(comparison_data, aes(y = reorder(Country, Y2024))) + 
  
  geom_segment(aes(x = Y2019, xend = Y2024, yend = Country), 
               color = "grey80", size = 1) +
  
  geom_point(aes(x = Y2019, color = "2019"), size = 2) +
  
  geom_point(aes(x = Y2024, color = "2024"), size = 2) +
  
  scale_color_manual(
    name = NULL,
    values = c("2019" = "grey60", "2024" = "#0072B2"),
    breaks = c("2019", "2024")
  ) +
  
  scale_x_continuous(labels = label_number(suffix = "%"), expand = c(0, 1)) +
  
  labs(
    title = "Remote Work Adoption by Country (2019 vs. 2024)",
    subtitle = "Comparison of pre and post-pandemic share of employees working from home",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.justification = "left",

    panel.grid.major.y = element_blank(), 
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey30")
  )