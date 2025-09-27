# Create a nicely formatted summary table
library(dplyr)
library(knitr)
library(formattable)

setwd('/Users/mjp38/GitHub/FoodTradeNetwork/')  # Adjust this path as needed


format_table <- function(data) {
  # Process the data
  summary_table <- data %>%
    # Remove NA rows
    filter(!is.na(Water_Type)) %>%
    # Group and summarize
    group_by(WF_Crop, Period) %>%
    summarise(
      Blue_Water = sum(Total_Virtual_Water_m3[Water_Type == "Blue_Irrigated"]) / 1e9,
      Green_Water = sum(Total_Virtual_Water_m3[Water_Type %in% 
                                                 c("Green_Irrigated", "Green_Rainfed")]) / 1e9,
      Total_Trade = sum(Total_Trade_Volume_kg) / 1e9,
      .groups = "drop"
    ) %>%
    # Calculate percentages
    mutate(
      Total_Water = Blue_Water + Green_Water,
      Blue_Pct = Blue_Water / Total_Water * 100,
      Green_Pct = Green_Water / Total_Water * 100
    ) %>%
    # Arrange data
    arrange(WF_Crop, Period)
  
  # Create formatted table
  formatted_table <- summary_table %>%
    mutate(
      Period = factor(Period, levels = c("Early", "Late")),
      `Blue Water (km³)` = sprintf("%.1f", Blue_Water),
      `Green Water (km³)` = sprintf("%.1f", Green_Water),
      `Total Water (km³)` = sprintf("%.1f", Total_Water),
      `Blue %` = sprintf("%.1f%%", Blue_Pct),
      `Green %` = sprintf("%.1f%%", Green_Pct),
      `Trade Volume (Gt)` = sprintf("%.2f", Total_Trade)
    ) %>%
    select(WF_Crop, Period, `Blue Water (km³)`, `Green Water (km³)`, 
           `Total Water (km³)`, `Blue %`, `Green %`, `Trade Volume (Gt)`)
  
  return(formatted_table)
}

# Create table from the data
data <- read.csv("outputs/key_statistics.csv")
formatted_table <- format_table(data)

# Print table
kable(formatted_table,
      caption = "Table 1: Virtual water flows and trade volumes by crop category comparing early (2001-2005) and late (2016-2020) periods. Water volumes are shown in cubic kilometers (km³) and trade volumes in gigatonnes (Gt). Percentages indicate the relative contribution of blue (irrigated) and green (rainfed + irrigated green) water to total virtual water flows.",
      align = c('l', 'l', 'r', 'r', 'r', 'r', 'r', 'r'),
      format = "pipe")

# Save as CSV
write.csv(formatted_table,
          "outputs/formatted_water_footprint_statistics.csv",
          row.names = FALSE)