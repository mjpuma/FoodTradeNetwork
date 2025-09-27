# =============================================================================
# Saudi Arabia Virtual Water Import Analysis for Climate Risk Assessment
# =============================================================================

# Load required libraries
library(tidyverse)
library(ggplot2)
library(igraph)
library(ggraph)
library(sf)
library(rnaturalearth)
library(gridExtra)
library(viridis)

# Function to analyze Saudi Arabia's virtual water imports
analyze_saudi_imports <- function(flows_data, period_name = "analysis") {
  
  cat("\nAnalyzing Saudi Arabia's Virtual Water Imports\n")
  cat("============================================\n")
  
  # Filter for Saudi Arabia as importer (PartnerISO3 == "SAU")
  saudi_imports <- flows_data %>%
    filter(PartnerISO3 == "SAU") %>%
    filter(!is.na(virtual_water_m3), virtual_water_m3 > 0)
  
  if(nrow(saudi_imports) == 0) {
    warning("No import data found for Saudi Arabia")
    return(NULL)
  }
  
  # Aggregate by source country and crop
  imports_by_country <- saudi_imports %>%
    group_by(ReporterISO3, WF_Crop, Water_Type) %>%
    summarise(
      total_import_m3 = sum(virtual_water_m3, na.rm = TRUE),
      trade_volume_kg = sum(value_primary, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      total_import_km3 = total_import_m3 / 1e9,
      trade_volume_mt = trade_volume_kg / 1e9
    )
  
  # Overall imports by country (all crops combined)
  imports_by_country_total <- imports_by_country %>%
    group_by(ReporterISO3) %>%
    summarise(
      total_virtual_water_km3 = sum(total_import_km3, na.rm = TRUE),
      total_trade_volume_mt = sum(trade_volume_mt, na.rm = TRUE),
      n_crops = n_distinct(WF_Crop),
      .groups = 'drop'
    ) %>%
    arrange(desc(total_virtual_water_km3))
  
  # Imports by crop (all countries combined)
  imports_by_crop <- imports_by_country %>%
    group_by(WF_Crop, Water_Type) %>%
    summarise(
      total_virtual_water_km3 = sum(total_import_km3, na.rm = TRUE),
      total_trade_volume_mt = sum(trade_volume_mt, na.rm = TRUE),
      n_suppliers = n_distinct(ReporterISO3),
      .groups = 'drop'
    ) %>%
    arrange(desc(total_virtual_water_km3))
  
  # Water type breakdown
  imports_by_water_type <- imports_by_country %>%
    group_by(Water_Type) %>%
    summarise(
      total_virtual_water_km3 = sum(total_import_km3, na.rm = TRUE),
      proportion = total_virtual_water_km3 / sum(imports_by_country$total_import_km3) * 100,
      .groups = 'drop'
    )
  
  # Print summary statistics
  cat(sprintf("\nTotal virtual water imports: %.2f km³/year\n", 
              sum(imports_by_country$total_import_km3)))
  cat(sprintf("Number of supplier countries: %d\n", 
              length(unique(imports_by_country$ReporterISO3))))
  cat(sprintf("Number of crop types: %d\n", 
              length(unique(imports_by_country$WF_Crop))))
  
  cat("\nTop 10 supplier countries by virtual water volume:\n")
  print(head(imports_by_country_total, 10))
  
  cat("\nTop crops by virtual water volume:\n")
  print(head(imports_by_crop, 10))
  
  return(list(
    by_country = imports_by_country,
    by_country_total = imports_by_country_total,
    by_crop = imports_by_crop,
    by_water_type = imports_by_water_type,
    raw_data = saudi_imports
  ))
}

# Function to create Saudi Arabia import network visualization
plot_saudi_import_network <- function(saudi_data, min_threshold_km3 = 0.5) {
  
  # Get top suppliers above threshold
  network_data <- saudi_data$by_country_total %>%
    filter(total_virtual_water_km3 >= min_threshold_km3) %>%
    select(from = ReporterISO3, weight = total_virtual_water_km3) %>%
    mutate(to = "SAU")
  
  if(nrow(network_data) == 0) {
    message("No flows above threshold for network plot")
    return(NULL)
  }
  
  # Create igraph object
  g <- graph_from_data_frame(network_data, directed = TRUE)
  
  # Create network plot
  p <- ggraph(g, layout = "star") +
    geom_edge_link(
      aes(width = weight, alpha = weight),
      arrow = arrow(length = unit(4, "mm")),
      end_cap = circle(3, "mm"),
      color = "#2171b5"
    ) +
    geom_node_point(
      aes(size = ifelse(name == "SAU", 12, 6),
          color = ifelse(name == "SAU", "Saudi Arabia", "Supplier")),
      alpha = 0.8
    ) +
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 4,
      max.overlaps = 30,
      fontface = ifelse(V(g)$name == "SAU", "bold", "plain")
    ) +
    scale_edge_width_continuous(
      name = "Virtual Water\n(km³/year)",
      range = c(1, 4)
    ) +
    scale_edge_alpha_continuous(range = c(0.6, 0.9), guide = "none") +
    scale_color_manual(
      values = c("Saudi Arabia" = "#d62728", "Supplier" = "#1f77b4"),
      name = "Country Type"
    ) +
    scale_size_identity() +
    labs(
      title = "Saudi Arabia's Virtual Water Import Network",
      subtitle = paste("Agricultural imports ≥", min_threshold_km3, "km³/year virtual water"),
      caption = "Source: FAO trade data and water footprint calculations"
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 10, color = "gray60")
    )
  
  return(p)
}

# Function to create Saudi Arabia's top suppliers chart
plot_saudi_top_suppliers <- function(saudi_data, n_top = 15) {
  
  # Get top suppliers
  top_suppliers <- saudi_data$by_country_total %>%
    head(n_top) %>%
    mutate(
      Country = case_when(
        ReporterISO3 == "USA" ~ "United States",
        ReporterISO3 == "BRA" ~ "Brazil", 
        ReporterISO3 == "ARG" ~ "Argentina",
        ReporterISO3 == "AUS" ~ "Australia",
        ReporterISO3 == "CAN" ~ "Canada",
        ReporterISO3 == "UKR" ~ "Ukraine",
        ReporterISO3 == "RUS" ~ "Russia",
        ReporterISO3 == "IND" ~ "India",
        ReporterISO3 == "THA" ~ "Thailand",
        ReporterISO3 == "PAK" ~ "Pakistan",
        TRUE ~ ReporterISO3
      )
    )
  
  p <- ggplot(top_suppliers, aes(x = reorder(Country, total_virtual_water_km3), 
                                 y = total_virtual_water_km3)) +
    geom_col(fill = "#2171b5", alpha = 0.8) +
    coord_flip() +
    labs(
      title = "Saudi Arabia's Top Virtual Water Import Sources",
      subtitle = paste("Top", n_top, "supplier countries by virtual water volume"),
      x = NULL,
      y = "Virtual Water Imports (km³/year)",
      caption = "Virtual water = water used to produce imported agricultural goods"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 10, color = "gray60"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# Function to create crop breakdown visualization
plot_saudi_crop_imports <- function(saudi_data) {
  
  # Prepare data for plotting
  crop_data <- saudi_data$by_crop %>%
    mutate(
      crop_clean = case_when(
        WF_Crop == "Temperate Cereals" ~ "Temperate Cereals\n(Wheat, Barley, etc.)",
        WF_Crop == "Tropical Cereals" ~ "Tropical Cereals\n(Sorghum, Millet)",
        WF_Crop == "Temperate Roots" ~ "Temperate Roots\n(Potatoes, Sugar Beet)",
        WF_Crop == "Tropical Roots" ~ "Tropical Roots\n(Cassava, Sweet Potato)",
        TRUE ~ WF_Crop
      ),
      Water_Type_clean = case_when(
        Water_Type == "Blue_Irrigated" ~ "Blue Water\n(Irrigation)",
        Water_Type == "Green_Irrigated" ~ "Green Water\n(Irrigated)",
        Water_Type == "Green_Rainfed" ~ "Green Water\n(Rainfed)",
        TRUE ~ Water_Type
      )
    )
  
  p1 <- ggplot(crop_data, aes(x = reorder(crop_clean, total_virtual_water_km3), 
                              y = total_virtual_water_km3,
                              fill = Water_Type_clean)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(
      values = c("Blue Water\n(Irrigation)" = "#2171b5", 
                 "Green Water\n(Irrigated)" = "#74c476",
                 "Green Water\n(Rainfed)" = "#31a354")
    ) +
    labs(
      title = "Saudi Arabia's Virtual Water Imports by Crop Type",
      x = NULL,
      y = "Virtual Water Imports (km³/year)",
      fill = "Water Source"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(p1)
}

# Function to create water type breakdown pie chart
plot_saudi_water_types <- function(saudi_data) {
  
  water_type_data <- saudi_data$by_water_type %>%
    mutate(
      Water_Type_clean = case_when(
        Water_Type == "Blue_Irrigated" ~ "Blue Water (Irrigation)",
        Water_Type == "Green_Irrigated" ~ "Green Water (Irrigated)",
        Water_Type == "Green_Rainfed" ~ "Green Water (Rainfed)",
        TRUE ~ Water_Type
      )
    )
  
  p <- ggplot(water_type_data, aes(x = "", y = proportion, fill = Water_Type_clean)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(
      values = c("Blue Water (Irrigation)" = "#2171b5", 
                 "Green Water (Irrigated)" = "#74c476",
                 "Green Water (Rainfed)" = "#31a354")
    ) +
    labs(
      title = "Virtual Water Import Composition",
      subtitle = "By water source type",
      fill = "Water Source"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    ) +
    geom_text(aes(label = paste0(round(proportion, 1), "%")), 
              position = position_stack(vjust = 0.5))
  
  return(p)
}

# Function to create comprehensive Saudi Arabia dashboard
create_saudi_dashboard <- function(flows_data, period_name = "2016-2020") {
  
  # Analyze the data
  saudi_data <- analyze_saudi_imports(flows_data, period_name)
  
  if(is.null(saudi_data)) {
    stop("No data available for Saudi Arabia")
  }
  
  # Create individual plots
  network_plot <- plot_saudi_import_network(saudi_data, min_threshold_km3 = 0.5)
  suppliers_plot <- plot_saudi_top_suppliers(saudi_data, n_top = 12)
  crops_plot <- plot_saudi_crop_imports(saudi_data)
  water_types_plot <- plot_saudi_water_types(saudi_data)
  
  # Save individual plots
  ggsave("outputs/saudi_import_network.png", network_plot, 
         width = 12, height = 10, dpi = 300)
  ggsave("outputs/saudi_top_suppliers.png", suppliers_plot, 
         width = 10, height = 8, dpi = 300)
  ggsave("outputs/saudi_crop_imports.png", crops_plot, 
         width = 10, height = 8, dpi = 300)
  ggsave("outputs/saudi_water_types.png", water_types_plot, 
         width = 8, height = 6, dpi = 300)
  
  # Create combined dashboard
  dashboard <- grid.arrange(
    suppliers_plot,
    crops_plot,
    ncol = 1,
    top = textGrob(
      paste("Saudi Arabia Virtual Water Imports Dashboard -", period_name),
      gp = gpar(fontsize = 16, fontface = "bold")
    )
  )
  
  # Save dashboard
  ggsave("outputs/saudi_imports_dashboard.png", dashboard, 
         width = 12, height = 14, dpi = 300)
  
  # Save data tables
  write.csv(saudi_data$by_country_total, "outputs/saudi_suppliers_ranking.csv", row.names = FALSE)
  write.csv(saudi_data$by_crop, "outputs/saudi_crop_imports.csv", row.names = FALSE)
  write.csv(saudi_data$by_water_type, "outputs/saudi_water_types.csv", row.names = FALSE)
  
  # Create climate risk summary
  create_climate_risk_summary(saudi_data, period_name)
  
  return(list(
    data = saudi_data,
    plots = list(
      network = network_plot,
      suppliers = suppliers_plot,
      crops = crops_plot,
      water_types = water_types_plot,
      dashboard = dashboard
    )
  ))
}

# Function to create climate risk context summary
create_climate_risk_summary <- function(saudi_data, period_name) {
  
  # Calculate key metrics
  total_imports <- sum(saudi_data$by_country_total$total_virtual_water_km3)
  top_5_dependence <- sum(head(saudi_data$by_country_total, 5)$total_virtual_water_km3) / total_imports * 100
  
  # Water stress analysis
  high_stress_countries <- c("AUS", "USA", "ARG", "PAK", "IND", "UKR")  # Example water-stressed regions
  stress_dependence <- saudi_data$by_country_total %>%
    filter(ReporterISO3 %in% high_stress_countries) %>%
    summarise(total = sum(total_virtual_water_km3)) %>%
    pull(total) / total_imports * 100
  
  # Create summary text
  summary_text <- paste0(
    "CLIMATE RISK ASSESSMENT: Saudi Arabia's Virtual Water Imports (", period_name, ")\n",
    "=======================================================================\n\n",
    "KEY METRICS:\n",
    "• Total virtual water imports: ", round(total_imports, 2), " km³/year\n",
    "• Number of supplier countries: ", nrow(saudi_data$by_country_total), "\n",
    "• Top 5 supplier concentration: ", round(top_5_dependence, 1), "%\n",
    "• Imports from water-stressed regions: ", round(stress_dependence, 1), "%\n\n",
    "CLIMATE VULNERABILITIES:\n",
    "• High dependence on imports makes Saudi Arabia vulnerable to:\n",
    "  - Droughts in supplier countries\n",
    "  - Trade disruptions due to climate events\n",
    "  - Changing precipitation patterns affecting rainfed agriculture\n",
    "  - Competition for water resources in exporting regions\n\n",
    "TOP SUPPLIER COUNTRIES:\n"
  )
  
  # Add top suppliers
  for(i in 1:min(10, nrow(saudi_data$by_country_total))) {
    supplier <- saudi_data$by_country_total[i, ]
    summary_text <- paste0(summary_text,
                           sprintf("• %s: %.2f km³/year (%.1f%%)\n", 
                                   supplier$ReporterISO3, 
                                   supplier$total_virtual_water_km3,
                                   supplier$total_virtual_water_km3/total_imports*100))
  }
  
  # Write summary to file
  writeLines(summary_text, "outputs/saudi_climate_risk_summary.txt")
  cat(summary_text)
  
  return(summary_text)
}

# Example usage:
# After running the main water footprint analysis, use:
saudi_results <- create_saudi_dashboard(flows_late, "2016-2020")