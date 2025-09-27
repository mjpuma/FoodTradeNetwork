# Network Metrics Analysis Script

# Set working directory to the script's location
if (rstudioapi::isAvailable()) {
  # If running in RStudio
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # If running from command line, assuming script is in analysis directory
  script_dir <- getSrcDirectory(function(x) {x})
  if (length(script_dir) > 0) setwd(script_dir)
}

# Create output directory if it doesn't exist
dir.create("outputs", showWarnings = FALSE)

# Load required libraries
library(tidyverse)
library(gridExtra)
library(viridis)
library(rstudioapi)

# Print current working directory for debugging
print(getwd())

# Read the network metrics data - reading from current directory
metrics <- read.csv("outputs/network_metrics_comparison.csv")

# Print data summary for verification
print(head(metrics))
print(dim(metrics))

# Function to create comparison plot for a single metric
plot_metric_comparison <- function(data, metric_name) {
  # Calculate mean values across crops for each water type and period
  summary_data <- data %>%
    group_by(Water_Type) %>%
    summarise(
      early_mean = mean(get(paste0(metric_name, "_early")), na.rm = TRUE),
      late_mean = mean(get(paste0(metric_name, "_late")), na.rm = TRUE),
      early_se = sd(get(paste0(metric_name, "_early")), na.rm = TRUE) / sqrt(n()),
      late_se = sd(get(paste0(metric_name, "_late")), na.rm = TRUE) / sqrt(n())
    ) %>%
    pivot_longer(
      cols = c(early_mean, late_mean),
      names_to = "period",
      values_to = "mean_value"
    ) %>%
    mutate(
      se = if_else(period == "early_mean", early_se, late_se),
      period = if_else(period == "early_mean", "1996-2000", "2016-2020"),
      Water_Type = factor(Water_Type, 
                          levels = c("Blue_Irrigated", "Green_Irrigated", "Green_Rainfed"),
                          labels = c("Blue\nIrrigated", "Green\nIrrigated", "Green\nRainfed"))
    )
  
  # Create the plot
  ggplot(summary_data, aes(x = Water_Type, y = mean_value, fill = period)) +
    geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.8) +
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se),
                  position = position_dodge(0.9), width = 0.25) +
    scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.7) +
    labs(
      title = str_to_title(str_replace_all(metric_name, "_", " ")),
      x = "Water Type",
      y = str_to_title(str_replace_all(metric_name, "_", " ")),
      fill = "Period"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom",
      panel.grid.major.x = element_blank()
    )
}

# Create plots for key metrics
density_plot <- plot_metric_comparison(metrics, "density")
reciprocity_plot <- plot_metric_comparison(metrics, "reciprocity")
mean_degree_plot <- plot_metric_comparison(metrics, "mean_degree")

# Combine plots
combined_plot <- grid.arrange(
  density_plot, reciprocity_plot, mean_degree_plot,
  ncol = 1,
  heights = c(1, 1, 1),
  top = textGrob(
    "Network Metrics Comparison Across Water Types and Time Periods",
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

# Save the combined plot
ggsave(
  "outputs/network_metrics_comparison_revised.png",
  combined_plot,
  width = 10,
  height = 12,
  dpi = 300
)

# Statistical analysis of changes
metric_changes <- metrics %>%
  group_by(Water_Type) %>%
  summarise(
    across(
      ends_with(c("_early", "_late")),
      ~mean(., na.rm = TRUE),
      .names = "{.col}_mean"
    )
  ) %>%
  mutate(
    density_change = density_late_mean - density_early_mean,
    reciprocity_change = reciprocity_late_mean - reciprocity_early_mean,
    mean_degree_change = mean_degree_late_mean - mean_degree_early_mean
  )

# Save statistical summary
write.csv(metric_changes, "outputs/network_metrics_changes.csv", row.names = FALSE)

# Print session info for reproducibility
writeLines(capture.output(sessionInfo()), "outputs/session_info.txt")