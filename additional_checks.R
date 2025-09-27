# Add this analysis block after loading data but before main calculations
# Save these in additional_checks.R
check_water_proportions <- function(water_footprint_data, period_name) {
  water_summary <- water_footprint_data %>%
    group_by(Water_Type, WF_Crop) %>%
    summarise(
      total_wf_km3 = sum(WaterFootprint_km3_per_year, na.rm = TRUE),
      n_countries = n_distinct(iso3),
      .groups = 'drop'
    ) %>%
    arrange(desc(total_wf_km3)) %>%
    # Add rank within each water type
    group_by(Water_Type) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  # Create formatted output
  sink(file.path("outputs", paste0("water_distribution_", period_name, ".txt")))
  cat("==============================================\n")
  cat("Water Distribution Analysis -", period_name, "\n")
  cat("==============================================\n\n")
  
  for(wt in unique(water_summary$Water_Type)) {
    cat("\n", wt, ":\n", sep="")
    cat("------------------------\n")
    water_summary %>%
      filter(Water_Type == wt) %>%
      head(5) %>%
      print(n = 5)
    cat("\n")
  }
  sink()
  
  # Save detailed data
  write.csv(water_summary, 
            file.path("outputs", paste0("water_distribution_", period_name, ".csv")),
            row.names = FALSE)
  
  return(water_summary)
}

analyze_china_flows <- function(flows, period_name) {
  china_analysis <- flows %>%
    filter(
      PartnerISO3 == "CHN" | ReporterISO3 == "CHN",
      !is.na(Water_Type)
    ) %>%
    group_by(year, Water_Type, WF_Crop) %>%
    summarise(
      imports_km3 = sum(virtual_water_m3[PartnerISO3 == "CHN"], na.rm = TRUE) / 1e9,
      exports_km3 = sum(virtual_water_m3[ReporterISO3 == "CHN"], na.rm = TRUE) / 1e9,
      net_trade_km3 = imports_km3 - exports_km3,
      .groups = 'drop'
    ) %>%
    arrange(desc(abs(net_trade_km3)))
  
  # Create formatted output
  sink(file.path("outputs", paste0("china_trade_", period_name, ".txt")))
  cat("==============================================\n")
  cat("China's Virtual Water Trade -", period_name, "\n")
  cat("==============================================\n\n")
  
  cat("Top 10 Net Trade Flows (km³/year):\n")
  cat("------------------------\n")
  china_analysis %>%
    arrange(desc(abs(net_trade_km3))) %>%
    head(10) %>%
    print(n = 10)
  
  cat("\nSummary by Water Type:\n")
  cat("------------------------\n")
  china_analysis %>%
    group_by(Water_Type) %>%
    summarise(
      total_imports = sum(imports_km3),
      total_exports = sum(exports_km3),
      net_trade = sum(net_trade_km3)
    ) %>%
    print()
  sink()
  
  # Save detailed data
  write.csv(china_analysis,
            file.path("outputs", paste0("china_trade_", period_name, ".csv")),
            row.names = FALSE)
  
  return(china_analysis)
}

analyze_trade_connections <- function(flows, period_name) {
  connection_summary <- flows %>%
    group_by(WF_Crop) %>%
    summarise(
      n_traders = n_distinct(c(ReporterISO3, PartnerISO3)),
      n_connections = n_distinct(paste(ReporterISO3, PartnerISO3)),
      total_volume_Mt = sum(value_primary, na.rm = TRUE) / 1e6,
      avg_trade_size_Mt = mean(value_primary, na.rm = TRUE) / 1e6,
      .groups = 'drop'
    ) %>%
    arrange(n_connections)
  
  # Create formatted output
  sink(file.path("outputs", paste0("trade_connections_", period_name, ".txt")))
  cat("==============================================\n")
  cat("Trade Connection Analysis -", period_name, "\n")
  cat("==============================================\n\n")
  
  cat("Ranked by Number of Connections:\n")
  cat("------------------------\n")
  print(connection_summary)
  
  cat("\nSummary Statistics:\n")
  cat("------------------------\n")
  cat("Average connections per crop:", mean(connection_summary$n_connections), "\n")
  cat("Most connected crop:", connection_summary$WF_Crop[which.max(connection_summary$n_connections)], "\n")
  cat("Least connected crop:", connection_summary$WF_Crop[which.min(connection_summary$n_connections)], "\n")
  sink()
  
  # Save detailed data
  write.csv(connection_summary,
            file.path("outputs", paste0("trade_connections_", period_name, ".csv")),
            row.names = FALSE)
  
  return(connection_summary)
}