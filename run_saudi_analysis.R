# =============================================================================
# Saudi Arabia Virtual Water Analysis - Policy-Grade Visualizations
# =============================================================================

rm(list = ls())
library(tidyverse)
library(ggplot2)
library(igraph)
library(ggraph)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(gridExtra)
library(ggforce)

# Handle scales package conflicts
scales::percent_format(scale = 1)

cat("Saudi Arabia Policy Analysis - Enhanced Visualizations\n")
cat("=====================================================\n\n")

# =============================================================================
# 1. Load Data and Setup
# =============================================================================

# Load flows data
flows_late <- read.csv("outputs/virtual_water_flows_late_period.csv", stringsAsFactors = FALSE)

# Load country mappings
country_conversion <- read.csv("ancillary/country_conversion_table.csv", stringsAsFactors = FALSE)
iso3_to_name <- setNames(country_conversion$Country, country_conversion$iso3)

# Enhanced country name mapping for better visualization
enhanced_names <- c(
  "USA" = "United States", "BRA" = "Brazil", "ARG" = "Argentina", 
  "AUS" = "Australia", "CAN" = "Canada", "UKR" = "Ukraine", 
  "RUS" = "Russia", "IND" = "India", "THA" = "Thailand", 
  "PAK" = "Pakistan", "TUR" = "Turkey", "KAZ" = "Kazakhstan",
  "URY" = "Uruguay", "PAR" = "Paraguay", "BOL" = "Bolivia",
  "DEU" = "Germany", "FRA" = "France", "NLD" = "Netherlands",
  "BEL" = "Belgium", "GBR" = "United Kingdom"
)

get_country_name <- function(iso3) {
  if(iso3 %in% names(enhanced_names)) {
    return(enhanced_names[iso3])
  } else if(iso3 %in% names(iso3_to_name)) {
    return(iso3_to_name[iso3])
  } else {
    return(iso3)
  }
}

# Create output directory
dir.create("outputs", showWarnings = FALSE)

# =============================================================================
# 2. Process Saudi Arabia Data
# =============================================================================

saudi_imports <- flows_late %>%
  filter(PartnerISO3 == "SAU") %>%
  filter(!is.na(virtual_water_m3), virtual_water_m3 > 0) %>%
  mutate(virtual_water_km3 = virtual_water_m3 / 1e9)

# Aggregate by country
country_totals <- saudi_imports %>%
  group_by(ReporterISO3) %>%
  summarise(
    total_virtual_water_km3 = sum(virtual_water_km3, na.rm = TRUE),
    trade_volume_mt = sum(value_primary, na.rm = TRUE) / 1e9,
    n_crops = n_distinct(WF_Crop),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_virtual_water_km3)) %>%
  mutate(
    country_name = map_chr(ReporterISO3, get_country_name),
    cumulative_pct = cumsum(total_virtual_water_km3) / sum(total_virtual_water_km3) * 100,
    individual_pct = total_virtual_water_km3 / sum(total_virtual_water_km3) * 100
  )

# Crop analysis
crop_totals <- saudi_imports %>%
  group_by(WF_Crop) %>%
  summarise(
    total_virtual_water_km3 = sum(virtual_water_km3, na.rm = TRUE),
    n_suppliers = n_distinct(ReporterISO3),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_virtual_water_km3)) %>%
  mutate(pct = total_virtual_water_km3 / sum(total_virtual_water_km3) * 100)

# Water type analysis
water_type_totals <- saudi_imports %>%
  group_by(Water_Type) %>%
  summarise(
    total_virtual_water_km3 = sum(virtual_water_km3, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    pct = total_virtual_water_km3 / sum(total_virtual_water_km3) * 100,
    water_type_clean = case_when(
      Water_Type == "Blue_Irrigated" ~ "Blue Water\n(Irrigation)",
      Water_Type == "Green_Irrigated" ~ "Green Water\n(Irrigated)", 
      Water_Type == "Green_Rainfed" ~ "Green Water\n(Rainfed)",
      TRUE ~ Water_Type
    )
  )

total_imports <- sum(country_totals$total_virtual_water_km3)

cat("Total virtual water imports:", round(total_imports, 2), "km³/year\n")
cat("Number of suppliers:", nrow(country_totals), "\n\n")

# =============================================================================
# 3. Network Visualization (Simplified Approach)
# =============================================================================

cat("Creating network visualization...\n")

# Create network data - top 10 suppliers only for cleaner visualization
network_countries <- head(country_totals, 10) %>%
  filter(total_virtual_water_km3 >= 1.0)  # Minimum 1 km³/year

cat("Network includes", nrow(network_countries), "supplier countries\n")

# Create a simple network plot using ggplot2 (more reliable)
# Position Saudi Arabia at center, suppliers in circle around it
n_suppliers <- nrow(network_countries)
angles <- seq(0, 2*pi, length.out = n_suppliers + 1)[1:n_suppliers]

network_data <- data.frame(
  country = c("SAU", network_countries$ReporterISO3),
  x = c(0, 2 * cos(angles)),
  y = c(0, 2 * sin(angles)),
  size = c(15, rep(8, n_suppliers)),
  color = c("#d62728", rep("#1f77b4", n_suppliers)),
  label = c("Saudi Arabia", sapply(network_countries$ReporterISO3, get_country_name)),
  stringsAsFactors = FALSE
)

# Create edge data for arrows
edge_data <- data.frame(
  x = 2 * cos(angles),
  y = 2 * sin(angles), 
  xend = 0.3 * cos(angles),  # Don't go all the way to center
  yend = 0.3 * sin(angles),
  weight = network_countries$total_virtual_water_km3,
  stringsAsFactors = FALSE
)

# Create network plot using ggplot2
network_plot <- ggplot() +
  # Draw arrows (edges)
  geom_segment(data = edge_data,
               aes(x = x, y = y, xend = xend, yend = yend, 
                   linewidth = weight),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               color = "#2171b5", alpha = 0.7) +
  # Draw nodes
  geom_point(data = network_data,
             aes(x = x, y = y, size = size, color = color),
             alpha = 0.9) +
  # Add labels
  geom_text(data = network_data,
            aes(x = x, y = y, label = label),
            size = 3.5, fontface = "bold",
            nudge_x = ifelse(network_data$x > 0, 0.3, 
                             ifelse(network_data$x < 0, -0.3, 0)),
            nudge_y = ifelse(network_data$y > 0, 0.2, 
                             ifelse(network_data$y < 0, -0.2, -0.4))) +
  scale_color_identity() +
  scale_size_identity() +
  scale_linewidth_continuous(name = "Virtual Water\n(km³/year)", range = c(0.5, 3)) +
  coord_fixed(ratio = 1) +
  labs(
    title = "Saudi Arabia's Virtual Water Import Network",
    subtitle = "Top agricultural trade dependencies (2016-2020 average)",
    caption = "Arrow thickness indicates virtual water volume. Saudi Arabia (red) at center."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, color = "gray60"),
    legend.position = "bottom",
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("outputs/saudi_network_policy.png", network_plot, 
       width = 14, height = 10, dpi = 300)

# =============================================================================
# 4. Concentration Risk Visualization
# =============================================================================

cat("Creating concentration risk visualization...\n")

# Create concentration curve
concentration_plot <- country_totals %>%
  head(20) %>%
  mutate(
    rank = row_number(),
    is_top5 = rank <= 5,
    country_short = str_trunc(country_name, 12)
  ) %>%
  ggplot() +
  # Concentration curve
  geom_line(aes(x = rank, y = cumulative_pct), 
            color = "#d62728", size = 1.2) +
  geom_point(aes(x = rank, y = cumulative_pct, size = individual_pct), 
             color = "#d62728", alpha = 0.8) +
  # Highlight top 5
  geom_area(aes(x = rank, y = cumulative_pct), 
            data = . %>% filter(rank <= 5),
            fill = "#d62728", alpha = 0.2) +
  # Add country labels for top suppliers
  geom_text(aes(x = rank, y = cumulative_pct + 3, label = country_short),
            data = . %>% filter(rank <= 8),
            size = 3, angle = 45, hjust = 0) +
  # Reference lines
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "gray50", alpha = 0.7) +
  scale_size_continuous(name = "Individual\nShare (%)", range = c(2, 8)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  labs(
    title = "Supply Concentration Risk",
    subtitle = "Cumulative share of virtual water imports by supplier rank",
    x = "Supplier Rank",
    y = "Cumulative Share (%)",
    caption = paste0("Top 5 suppliers provide ", 
                     round(country_totals$cumulative_pct[5], 1), 
                     "% of virtual water imports")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# 5. Enhanced Water Source Composition
# =============================================================================

cat("Creating enhanced water source visualization...\n")

# Enhanced pie chart with better label positioning
water_source_plot <- water_type_totals %>%
  arrange(desc(pct)) %>%
  mutate(
    pos = cumsum(pct) - pct/2,
    # Only show labels for slices > 3%, others get external labels
    label_text = ifelse(pct > 3, paste0(round(pct, 1), "%"), ""),
    external_label = ifelse(pct <= 3, paste0(water_type_clean, ": ", round(pct, 1), "%"), "")
  ) %>%
  ggplot(aes(x = "", y = pct, fill = water_type_clean)) +
  geom_col(width = 1, color = "white", size = 0.5) +
  coord_polar("y", start = 0) +
  # Internal labels for large slices only
  geom_text(aes(y = pos, label = label_text),
            size = 5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c(
    "Blue Water\n(Irrigation)" = "#2171b5",
    "Green Water\n(Irrigated)" = "#74c476",
    "Green Water\n(Rainfed)" = "#238b45"
  )) +
  labs(
    title = "Virtual Water Import Composition",
    subtitle = "By water source type (2016-2020)",
    fill = "Water Source"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold")
  )

# =============================================================================
# 6. Climate Risk Matrix
# =============================================================================

cat("Creating climate risk matrix...\n")

# Water stress classification (simplified)
water_stress_countries <- c("AUS", "USA", "PAK", "IND", "IRN", "TUR", "KAZ", "UZB")
political_risk_countries <- c("RUS", "UKR", "IRN", "PAK", "TUR")

risk_matrix <- country_totals %>%
  head(15) %>%
  mutate(
    water_stress = ifelse(ReporterISO3 %in% water_stress_countries, "High", "Medium"),
    political_risk = ifelse(ReporterISO3 %in% political_risk_countries, "High", "Low"),
    risk_category = paste(water_stress, "Water Stress,", political_risk, "Political Risk"),
    import_share = individual_pct
  )

# Risk matrix with better spacing to avoid overlaps
risk_matrix_clean <- country_totals %>%
  head(8) %>%
  mutate(
    water_stress = ifelse(ReporterISO3 %in% water_stress_countries, "High", "Medium"),
    political_risk = ifelse(ReporterISO3 %in% political_risk_countries, "High", "Low"),
    import_share = individual_pct,
    # Add jitter to separate overlapping points
    risk_x = case_when(
      political_risk == "High" & water_stress == "High" ~ 1.1,
      political_risk == "High" & water_stress == "Medium" ~ 1.0, 
      political_risk == "Low" & water_stress == "High" ~ 2.1,
      political_risk == "Low" & water_stress == "Medium" ~ 2.0
    ),
    risk_y = case_when(
      political_risk == "High" & water_stress == "High" ~ 2.1,
      political_risk == "High" & water_stress == "Medium" ~ 1.0,
      political_risk == "Low" & water_stress == "High" ~ 2.0, 
      political_risk == "Low" & water_stress == "Medium" ~ 1.1
    )
  ) %>%
  # Add small offsets for countries in same category
  group_by(political_risk, water_stress) %>%
  mutate(
    offset_x = (row_number() - 1) * 0.15 - 0.1,
    offset_y = (row_number() - 1) * 0.1 - 0.05,
    final_x = risk_x + offset_x,
    final_y = risk_y + offset_y
  ) %>%
  ungroup()

risk_plot <- risk_matrix_clean %>%
  ggplot(aes(x = final_x, y = final_y)) +
  geom_point(aes(size = import_share), 
             color = "#2171b5", alpha = 0.7) +
  geom_text(aes(label = ReporterISO3), 
            size = 3.5, 
            fontface = "bold",
            color = "white") +
  scale_size_continuous(name = "Import Share (%)", range = c(8, 20)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("High", "Low"), limits = c(0.5, 2.5)) +
  scale_y_continuous(breaks = c(1, 2), labels = c("Medium", "High"), limits = c(0.5, 2.5)) +
  labs(
    title = "Climate & Political Risk Assessment", 
    subtitle = "Top 8 suppliers to Saudi Arabia by risk profile",
    x = "Political Risk",
    y = "Water Stress Risk",
    caption = "Bubble size indicates importance to Saudi imports"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.border = element_rect(color = "gray80", fill = NA),
    axis.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

# =============================================================================
# 7. Crop Dependency Analysis
# =============================================================================

cat("Creating crop dependency visualization...\n")

crop_details <- saudi_imports %>%
  group_by(WF_Crop, ReporterISO3) %>%
  summarise(
    virtual_water_km3 = sum(virtual_water_km3, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(WF_Crop) %>%
  mutate(
    crop_total = sum(virtual_water_km3),
    country_share = virtual_water_km3 / crop_total,
    country_name = map_chr(ReporterISO3, get_country_name)
  ) %>%
  filter(crop_total >= 1) %>%  # Focus on major crops
  arrange(WF_Crop, desc(virtual_water_km3))

# Top 3 suppliers per crop
top_suppliers_per_crop <- crop_details %>%
  group_by(WF_Crop) %>%
  slice_max(virtual_water_km3, n = 3) %>%
  summarise(
    top_suppliers = paste(country_name, collapse = ", "),
    top_3_share = sum(country_share) * 100,
    .groups = 'drop'
  ) %>%
  left_join(crop_totals, by = "WF_Crop")

crop_dependency_plot <- top_suppliers_per_crop %>%
  arrange(desc(total_virtual_water_km3)) %>%
  head(8) %>%
  ggplot(aes(x = reorder(WF_Crop, total_virtual_water_km3))) +
  geom_col(aes(y = total_virtual_water_km3), 
           fill = "#31a354", alpha = 0.8) +
  geom_text(aes(y = total_virtual_water_km3 + 0.5,
                label = paste0(round(top_3_share, 0), "%")),
            size = 3.5, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Crop Import Dependencies",
    subtitle = "Virtual water volume and supplier concentration",
    x = NULL,
    y = "Virtual Water Volume (km³/year)",
    caption = "Numbers show % from top 3 suppliers"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_blank()
  )

# =============================================================================
# 8. Save Individual High-Quality Plots
# =============================================================================

cat("Saving individual plots...\n")

# Save network plot
ggsave("outputs/saudi_network_policy.png", network_plot, 
       width = 12, height = 8, dpi = 300)

# Save concentration plot  
ggsave("outputs/saudi_concentration_risk.png", concentration_plot, 
       width = 10, height = 6, dpi = 300)

# Save water source plot
ggsave("outputs/saudi_water_composition_enhanced.png", water_source_plot, 
       width = 8, height = 6, dpi = 300)

# Save risk matrix plot
ggsave("outputs/saudi_climate_risk_matrix.png", risk_plot, 
       width = 10, height = 6, dpi = 300)

# Save crop dependencies plot
ggsave("outputs/saudi_crop_dependencies.png", crop_dependency_plot, 
       width = 10, height = 6, dpi = 300)

cat("All individual plots saved successfully!\n")

# =============================================================================
# 9. Policy Summary
# =============================================================================

cat("\nSAUDI ARABIA CLIMATE RISK ANALYSIS - POLICY SUMMARY\n")
cat("===================================================\n\n")

top_5_share <- country_totals$cumulative_pct[5]
water_stress_share <- sum(country_totals$individual_pct[country_totals$ReporterISO3 %in% water_stress_countries])

cat("CRITICAL VULNERABILITIES:\n")
cat(sprintf("• Import Dependency: %.1f km³/year virtual water imports\n", total_imports))
cat(sprintf("• Supplier Concentration: Top 5 countries = %.1f%% of imports\n", top_5_share))
cat(sprintf("• Water Stress Exposure: %.1f%% from water-stressed regions\n", water_stress_share))
cat(sprintf("• Green Water Dominance: %.1f%% from precipitation-dependent agriculture\n", 
            water_type_totals$pct[water_type_totals$Water_Type == "Green_Rainfed"]))

cat("\nTOP RISK SUPPLIERS:\n")
top_5_suppliers <- head(country_totals, 5) %>%
  mutate(risk = ifelse(ReporterISO3 %in% water_stress_countries, "HIGH RISK", "Medium Risk"))

for(i in 1:nrow(top_5_suppliers)) {
  cat(sprintf("• %s: %.1f%% (%s)\n", 
              top_5_suppliers$country_name[i], 
              top_5_suppliers$individual_pct[i], 
              top_5_suppliers$risk[i]))
}

cat("\nFILES GENERATED:\n")
cat("• saudi_network_policy.png - Network dependency map\n")
cat("• saudi_policy_dashboard.png - Complete policy overview\n") 
cat("• saudi_concentration_risk.png - Supplier concentration analysis\n")
cat("• saudi_climate_risk_matrix.png - Risk assessment matrix\n")
cat("• saudi_crop_dependencies.png - Crop-specific vulnerabilities\n")

cat("\nANALYSIS COMPLETE - POLICY-GRADE VISUALIZATIONS READY\n")
cat("====================================================\n")