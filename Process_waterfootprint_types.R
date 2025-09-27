# =============================================================================
# Water Footprint Analysis Script with Water Type Tracking
# Units Overview:
# - Water Footprint Input: km³/year (cubic kilometers per year)
# - Production Input: metric tons
# - Converted Units:
#   * Water: liters/year (1 km³ = 1e12 liters)
#   * Production: kg (1 metric ton = 1000 kg)
#   * Final Water Footprint: liters/kg/year
# Water Types:
# - Green_Rainfed: Rainwater consumed
# - Green_Irrigated: Rainwater consumed in irrigated areas
# - Blue_Irrigated: Irrigation water consumed
# =============================================================================

# -----------------------------
# 0. Set Working Directory
# -----------------------------
setwd('/Users/mjp38/GitHub/FoodTradeNetwork/')  # Adjust this path as needed

# -----------------------------
# 1. Load Required Packages
# -----------------------------
required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "ggplot2",
  "rnaturalearth", "rnaturalearthdata", "sf",
  "gridExtra", "grid", "FAOSTAT", "RColorBrewer", "ggrepel"
)

installed_packages <- rownames(installed.packages())
for(p in required_packages){
  if(!(p %in% installed_packages)){
    install.packages(p, dependencies = TRUE)
  }
}

# Load the packages with suppressed startup messages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  library(gridExtra)
  library(grid)
  library(FAOSTAT)
  library(ggrepel) 
})

# -----------------------------
# 2. Function Definitions
# -----------------------------
# Function to standardize crop names
standardize_crop_name <- function(crop_name) {
  crop_name %>%
    str_trim() %>%
    str_to_title() %>%
    str_replace_all("\\s+", " ")
}

# Enhanced function to read and process water footprint data
read_water_footprint <- function(file_path, water_type) {
  tryCatch({
    sheet_names <- excel_sheets(file_path)
    data_list <- list()
    
    for (sheet in sheet_names) {
      df <- read_excel(file_path, sheet = sheet)
      colnames(df)[1] <- "Country"
      
      df$WF_Crop <- sheet
      df$Water_Type <- water_type
      
      # Convert to long format while maintaining units as km³/year
      df_long <- df %>%
        pivot_longer(
          cols = -c(Country, WF_Crop, Water_Type),
          names_to = "Year",
          values_to = "WaterFootprint_km3_per_year"
        )
      
      data_list[[sheet]] <- df_long
    }
    
    return(bind_rows(data_list))
  }, error = function(e) {
    stop("Error processing water footprint file ", file_path, ": ", e$message)
  })
}

# Function to filter specific water types
get_water_footprint_by_type <- function(data, water_type = NULL) {
  if (is.null(water_type)) {
    # Return sum of all types if no specific type is requested
    return(data %>%
             group_by(Country, WF_Crop, Year) %>%
             summarise(
               WaterFootprint_km3_per_year = sum(WaterFootprint_km3_per_year, na.rm = TRUE),
               Water_Type = "Total",
               .groups = 'drop'
             ))
  } else {
    # Return only the specified water type
    return(data %>%
             filter(Water_Type == water_type))
  }
}

# -----------------------------
# 3. Read and Process Initial Data
# -----------------------------
water_types <- c("Green Rainfed", "Green Irrigated", "Blue Irrigated", "Total")

# Read water footprint data (all in km³/year)
gw_rainfed_df <- read_water_footprint("ancillary/GW_rainfed_lpjml.xlsx", "Green_Rainfed")
gw_irrigated_df <- read_water_footprint("ancillary/GW_irrigated_lpjml.xlsx", "Green_Irrigated")
bw_irrigated_df <- read_water_footprint("ancillary/BW_irrigated_lpjml.xlsx", "Blue_Irrigated")

# Combine all water footprint data while preserving water type information
water_footprint_long <- bind_rows(gw_rainfed_df, gw_irrigated_df, bw_irrigated_df) %>%
  mutate(
    Country = str_replace_all(Country, "^['\"]+|['\"]+$", "") %>% str_trim(),
    WF_Crop = standardize_crop_name(WF_Crop),
    Year = as.numeric(Year)
  )

# Create different versions of the water footprint data
water_footprint_rainfed <- get_water_footprint_by_type(water_footprint_long, "Green_Rainfed")
water_footprint_irrigated_green <- get_water_footprint_by_type(water_footprint_long, "Green_Irrigated")
water_footprint_irrigated_blue <- get_water_footprint_by_type(water_footprint_long, "Blue_Irrigated")
water_footprint_total <- get_water_footprint_by_type(water_footprint_long)

# -----------------------------
# 4. Country and Crop Mapping Setup
# -----------------------------
# Read the conversion table for country names
tryCatch({
  conversion_table <- read.csv("ancillary/country_conversion_table.csv", 
                               stringsAsFactors = FALSE) %>%
    mutate(
      Country = str_trim(Country),
      iso3 = str_trim(ISO3.alpha)
    )
  
  country_list <- conversion_table %>%
    select(Country, iso3) %>%
    filter(!is.na(iso3) & iso3 != "")
}, error = function(e) {
  stop("Error reading conversion table: ", e$message)
})

# Define crop mapping with FAO codes
crop_mapping <- data.frame(
  FAO_Code = c(
    # Temperate Cereals
    15, 44, 71, 75,
    # Tropical Cereals
    56, 83, 79,
    # Temperate Roots
    116, 157,
    # Tropical Roots
    125, 122, 137,
    # Pulses
    176, 187, 191,
    # Individual Crops
    236, 242, 270, 267, 156
  ),
  FAO_Crop_Name = c(
    # Temperate Cereals
    "Wheat", "Barley", "Rye", "Oats",
    # Tropical Cereals
    "Maize", "Sorghum", "Millet",
    # Temperate Roots
    "Potatoes", "Beets",
    # Tropical Roots
    "Cassava", "Sweet potatoes", "Yams",
    # Pulses
    "Beans, dry", "Peas, dry", "Chickpeas",
    # Individual Crops
    "Soybeans", "Groundnuts, with shell", "Rapeseed", "Sunflower seed", "Sugar cane"
  ),
  WF_Crop = c(
    # Temperate Cereals
    rep("Temperate Cereals", 4),
    # Tropical Cereals
    rep("Tropical Cereals", 3),
    # Temperate Roots
    rep("Temperate Roots", 2),
    # Tropical Roots
    rep("Tropical Roots", 3),
    # Pulses
    rep("Pulses", 3),
    # Individual Crops
    "Soyabean", "Groundnut", "Rapeseed", "Sunflower", "Sugar"
  ),
  stringsAsFactors = FALSE
)

# Handle alternative country names
alternative_names <- data.frame(
  Country = c(
    "Democratic Republic of the Congo", "Congo", "Republic of the Congo",
    "Sudan", "Sudan (former)", "South Sudan",
    "Turkey", "Turkiye", "Türkiye",
    "Iran (Islamic Republic of)", "Russia", "Syrian Arab Republic", "Viet Nam"
  ),
  Standardized_Country = c(
    "Democratic Republic of Congo", "Republic of Congo", "Republic of Congo",
    "Sudan", "Sudan", "South Sudan",
    "Turkey", "Turkey", "Turkey",
    "Iran", "Russian Federation", "Syria", "Vietnam"
  ),
  stringsAsFactors = FALSE
)

# Process water footprint data with country standardization
process_water_footprint <- function(water_data) {
  water_data %>%
    left_join(alternative_names, by = "Country") %>%
    mutate(
      Country = if_else(!is.na(Standardized_Country), Standardized_Country, Country)
    ) %>%
    select(-Standardized_Country) %>%
    left_join(country_list, by = "Country") %>%
    filter(!is.na(iso3)) %>%
    rename(ISO3 = iso3)
}

# Apply processing to each water type dataset
water_footprint_rainfed <- process_water_footprint(water_footprint_rainfed)
water_footprint_irrigated_green <- process_water_footprint(water_footprint_irrigated_green)
water_footprint_irrigated_blue <- process_water_footprint(water_footprint_irrigated_blue)
water_footprint_total <- process_water_footprint(water_footprint_total)

# -----------------------------
# 5. Production Data Processing
# -----------------------------
tryCatch({
  production_data_file <- "data_raw/crop_production_data.rds"
  
  if (!file.exists(production_data_file)) {
    cat("Production data file not found. Downloading from FAOSTAT...\n")
    
    fao_item_codes <- crop_mapping$FAO_Code
    
    production_data_raw <- FAOSTAT::get_faostat_data(
      domain_code = "QC",  # Crops Production
      item_code = fao_item_codes,
      element_code = 5510,  # Production Quantity
      output_format = "RDS"
    )
    
    dir.create("data_raw", showWarnings = FALSE)
    saveRDS(production_data_raw, production_data_file)
    cat("Production data downloaded and saved to", production_data_file, "\n")
  } else {
    production_data_raw <- readRDS(production_data_file)
    cat("Production data loaded from", production_data_file, "\n")
  }
  
  # Process production data
  production_data <- production_data_raw %>%
    mutate(
      Country = str_trim(area),
      Year = as.numeric(year),
      Production_Quantity = as.numeric(value),  # Value in metric tons
      item = standardize_crop_name(item)
    ) %>%
    left_join(alternative_names, by = "Country") %>%
    mutate(
      Country = if_else(!is.na(Standardized_Country), Standardized_Country, Country)
    ) %>%
    select(-Standardized_Country) %>%
    left_join(country_list, by = "Country") %>%
    filter(!is.na(iso3)) %>%
    rename(ISO3 = iso3)
  
  # Prepare final production dataset
  production_data_prepared <- production_data %>%
    left_join(crop_mapping, by = c("item" = "FAO_Crop_Name")) %>%
    filter(!is.na(WF_Crop)) %>%
    mutate(Production_kg = Production_Quantity * 1000) %>%  # Convert metric tons to kg
    group_by(ISO3, Year, WF_Crop) %>%
    summarise(
      Production_kg = sum(Production_kg, na.rm = TRUE),
      .groups = 'drop'
    )
  
}, error = function(e) {
  stop("Error processing production data: ", e$message)
})

# -----------------------------
# 6. Merge and Calculate Water Footprint Metrics for Each Water Type
# -----------------------------
# Function to merge water footprint and production data
merge_water_production <- function(water_data, production_data) {
  water_data %>%
    inner_join(production_data, by = c("ISO3", "Year", "WF_Crop")) %>%
    mutate(
      # Convert water footprint from km³/year to L/year
      WaterFootprint_liters_per_year = WaterFootprint_km3_per_year * 1e12,
      # Calculate water footprint intensity in L/kg/year
      WF_L_per_kg = WaterFootprint_liters_per_year / Production_kg
    ) %>%
    # Handle extreme values
    mutate(
      WF_L_per_kg = case_when(
        is.infinite(WF_L_per_kg) | is.nan(WF_L_per_kg) ~ NA_real_,
        WF_L_per_kg < 0 ~ NA_real_,
        WF_L_per_kg > quantile(WF_L_per_kg, 0.99, na.rm = TRUE) ~ NA_real_,
        TRUE ~ WF_L_per_kg
      )
    )
}

# Create merged datasets for each water type
merged_data_rainfed <- merge_water_production(water_footprint_rainfed, production_data_prepared)
merged_data_irrigated_green <- merge_water_production(water_footprint_irrigated_green, production_data_prepared)
merged_data_irrigated_blue <- merge_water_production(water_footprint_irrigated_blue, production_data_prepared)
merged_data_total <- merge_water_production(water_footprint_total, production_data_prepared)

# -----------------------------
# 7. Temporal Analysis by Water Type
# -----------------------------
# Define time periods
early_years <- 2001:2005
late_years <- 2016:2020
periods <- list(
  early = early_years,
  late = late_years
)

# Enhanced function to get period averages with water type tracking
get_period_data <- function(data, period_years) {
  data %>%
    filter(Year %in% period_years) %>%
    group_by(ISO3, WF_Crop, Water_Type) %>%
    summarise(
      WF_L_per_kg = mean(WF_L_per_kg, na.rm = TRUE),            # L/kg/year
      Total_WF = mean(WaterFootprint_liters_per_year, na.rm = TRUE),  # L/year
      Production_kg = mean(Production_kg, na.rm = TRUE),         # kg/year
      n_years = n_distinct(Year),
      .groups = 'drop'
    ) %>%
    mutate(
      data_quality = if_else(n_years >= length(period_years) / 2, "good", "sparse")
    )
}

# Get period data for each water type
early_data_by_type <- list(
  rainfed = get_period_data(merged_data_rainfed, periods$early),
  irrigated_green = get_period_data(merged_data_irrigated_green, periods$early),
  irrigated_blue = get_period_data(merged_data_irrigated_blue, periods$early),
  total = get_period_data(merged_data_total, periods$early)
)

late_data_by_type <- list(
  rainfed = get_period_data(merged_data_rainfed, periods$late),
  irrigated_green = get_period_data(merged_data_irrigated_green, periods$late),
  irrigated_blue = get_period_data(merged_data_irrigated_blue, periods$late),
  total = get_period_data(merged_data_total, periods$late)
)

# Add RColorBrewer to required packages at the start of your script
required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "ggplot2",
  "rnaturalearth", "rnaturalearthdata", "sf",
  "gridExtra", "grid", "FAOSTAT", "RColorBrewer"
)

# In Section 8, update the plotting functions:

# -----------------------------
# 8. Create Enhanced Visualizations with Water Type Distinction
# -----------------------------
# Prepare the world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Function to create breaks for discrete categories
create_discrete_breaks <- function(x, n = 7) {
  if(all(is.na(x))) return(NULL)
  x_range <- range(x, na.rm = TRUE)
  # Create breaks that are meaningful numbers
  breaks <- pretty(x_range, n = n)
  # Ensure the range is covered
  breaks <- c(min(breaks), breaks, max(breaks)) %>% unique() %>% sort()
  return(breaks)
}

# Separate function for water footprint maps
create_water_maps <- function(period_data, period_name, water_type) {
  wf_map_list <- list()
  total_wf_map_list <- list()
  
  for (current_crop in unique(period_data$WF_Crop)) {
    crop_data <- period_data %>% 
      filter(WF_Crop == current_crop)
    
    world_crop <- world %>%
      left_join(crop_data, by = c("iso_a3" = "ISO3"))
    
    # Water footprint per kg map (units: L/kg/year)
    wf_caption <- paste("Water footprint intensity in L/kg/year\n",
                        switch(water_type,
                               "Green Rainfed" = "(Rainwater consumed in rainfed fields)",
                               "Green Irrigated" = "(Rainwater consumed in irrigated fields)",
                               "Blue Irrigated" = "(Irrigation water from surface/groundwater)",
                               "Total" = "(Total water consumption)"))
    
    wf_map <- ggplot(data = world_crop) +
      geom_sf(aes(fill = WF_L_per_kg), color = "gray") +
      scale_fill_viridis_c(
        option = "viridis",
        na.value = "lightgray",
        # Use limits to control the range
        limits = c(
          min(period_data$WF_L_per_kg, na.rm = TRUE),
          quantile(period_data$WF_L_per_kg, 0.95, na.rm = TRUE)
        ),
        # Add more breaks for better color resolution
        n.breaks = 10,
        name = sprintf("Water Footprint\n(%s)\n(L/kg/year)", water_type)
      ) +
      theme_minimal() +
      labs(
        title = paste(current_crop, "-", period_name, "period", "\nWater Type:", water_type),
        caption = wf_caption
      ) +
      theme(
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm")  # Made legend key wider
      )
    
    # Total water use map (units: L/year)
    total_wf_map <- ggplot(data = world_crop) +
      geom_sf(aes(fill = Total_WF), color = "gray") +
      scale_fill_viridis_c(
        option = "magma",
        na.value = "lightgray",
        trans = "log10",
        # Add more breaks for better color resolution
        n.breaks = 10,
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        name = sprintf("Total Water Use\n(%s)\n(L/year)", water_type)
      ) +
      theme_minimal() +
      labs(
        title = paste(current_crop, "Total Water Use -", period_name, "period", "\nWater Type:", water_type),
        caption = wf_caption
      ) +
      theme(
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm")  # Made legend key wider
      )
    
    wf_map_list[[current_crop]] <- wf_map
    total_wf_map_list[[current_crop]] <- total_wf_map
  }
  
  return(list(
    wf = wf_map_list,
    total_wf = total_wf_map_list
  ))
}

# Separate function for production maps (independent of water type)
create_production_maps <- function(period_data, period_name) {
  prod_map_list <- list()
  
  for (current_crop in unique(period_data$WF_Crop)) {
    crop_data <- period_data %>% 
      filter(WF_Crop == current_crop)
    
    world_crop <- world %>%
      left_join(crop_data, by = c("iso_a3" = "ISO3"))
    
    # Production map (units: kg/year)
    prod_map <- ggplot(data = world_crop) +
      geom_sf(aes(fill = Production_kg), color = "gray") +
      scale_fill_viridis_c(
        option = "cividis",
        na.value = "lightgray",
        trans = "log10",
        # Add more breaks for better color resolution
        n.breaks = 10,
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        name = "Production\n(kg/year)"
      ) +
      theme_minimal() +
      labs(
        title = paste(current_crop, "Production -", period_name, "period"),
        caption = "Annual production in kilograms (log scale)"
      ) +
      theme(
        plot.title = element_text(size = 10),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm")  # Made legend key wider
      )
    
    prod_map_list[[current_crop]] <- prod_map
  }
  
  return(prod_map_list)
}

# Add this after create_production_maps but before creating the maps:

# Function to create temporal comparison plots with water type distinction
create_temporal_comparison <- function(early_grobs, late_grobs, title_text, water_type) {
  crops <- names(early_grobs)
  n_crops <- length(crops)
  
  # Create layout matrix for side-by-side comparison
  layout_mat <- matrix(1:(2 * n_crops), ncol = 2, byrow = TRUE)
  
  # Create title
  title <- textGrob(
    sprintf("%s - %s\n(%s vs %s)",
            title_text,
            water_type,
            paste(range(periods$early), collapse = "-"),
            paste(range(periods$late), collapse = "-")),
    gp = gpar(fontsize = 14, font = 2)
  )
  
  # Combine grobs
  all_grobs <- vector("list", 2 * n_crops)
  for (i in seq_len(n_crops)) {
    all_grobs[[layout_mat[i, 1]]] <- early_grobs[[i]]
    all_grobs[[layout_mat[i, 2]]] <- late_grobs[[i]]
  }
  
  # Arrange plots
  arrangeGrob(
    grobs = all_grobs,
    layout_matrix = layout_mat,
    top = title
  )
}


# Create water footprint maps for each water type and period
early_maps <- list(
  rainfed = create_water_maps(early_data_by_type$rainfed, "Early", "Green Rainfed"),
  irrigated_green = create_water_maps(early_data_by_type$irrigated_green, "Early", "Green Irrigated"),
  irrigated_blue = create_water_maps(early_data_by_type$irrigated_blue, "Early", "Blue Irrigated"),
  total = create_water_maps(early_data_by_type$total, "Early", "Total")
)

late_maps <- list(
  rainfed = create_water_maps(late_data_by_type$rainfed, "Late", "Green Rainfed"),
  irrigated_green = create_water_maps(late_data_by_type$irrigated_green, "Late", "Green Irrigated"),
  irrigated_blue = create_water_maps(late_data_by_type$irrigated_blue, "Late", "Blue Irrigated"),
  total = create_water_maps(late_data_by_type$total, "Late", "Total")
)

# Create single set of production maps
early_prod_maps <- create_production_maps(early_data_by_type$total, "Early")
late_prod_maps <- create_production_maps(late_data_by_type$total, "Late")

# Convert maps to grobs for each water type
convert_water_maps_to_grobs <- function(maps) {
  list(
    wf = lapply(maps$wf, ggplotGrob),
    total_wf = lapply(maps$total_wf, ggplotGrob)
  )
}

early_grobs <- list(
  rainfed = convert_water_maps_to_grobs(early_maps$rainfed),
  irrigated_green = convert_water_maps_to_grobs(early_maps$irrigated_green),
  irrigated_blue = convert_water_maps_to_grobs(early_maps$irrigated_blue),
  total = convert_water_maps_to_grobs(early_maps$total)
)

late_grobs <- list(
  rainfed = convert_water_maps_to_grobs(late_maps$rainfed),
  irrigated_green = convert_water_maps_to_grobs(late_maps$irrigated_green),
  irrigated_blue = convert_water_maps_to_grobs(late_maps$irrigated_blue),
  total = convert_water_maps_to_grobs(late_maps$total)
)

# Convert production maps to grobs
early_prod_grobs <- lapply(early_prod_maps, ggplotGrob)
late_prod_grobs <- lapply(late_prod_maps, ggplotGrob)

# -----------------------------
# 8. Create Enhanced Visualizations and Save Results
# -----------------------------
# [First all the visualization code from my previous response]

# Then add this saving section at the end of Section 8:

# -----------------------------
# 8a. Save Visualizations
# -----------------------------
# Create directory for outputs if it doesn't exist
dir.create("outputs", showWarnings = FALSE)

# Save water footprint comparison plots
for (water_type in names(early_grobs)) {
  # Water footprint intensity maps
  comparison_plot <- create_temporal_comparison(
    early_grobs[[water_type]]$wf, 
    late_grobs[[water_type]]$wf, 
    "Water Footprint",
    water_types[match(water_type, names(early_grobs))]
  )
  
  ggsave(
    file.path("outputs", sprintf("water_footprint_%s_comparison.png",
                                 tolower(gsub(" ", "_", water_types[match(water_type, names(early_grobs))])))),
    comparison_plot,
    width = 20,
    height = 5 * length(unique(merged_data_total$WF_Crop)),
    dpi = 300
  )
  
  # Total water use maps
  comparison_plot <- create_temporal_comparison(
    early_grobs[[water_type]]$total_wf, 
    late_grobs[[water_type]]$total_wf, 
    "Total Water Use",
    water_types[match(water_type, names(early_grobs))]
  )
  
  ggsave(
    file.path("outputs", sprintf("total_water_%s_comparison.png",
                                 tolower(gsub(" ", "_", water_types[match(water_type, names(early_grobs))])))),
    comparison_plot,
    width = 20,
    height = 5 * length(unique(merged_data_total$WF_Crop)),
    dpi = 300
  )
}

# Save production comparison plot (only once)
production_comparison <- create_temporal_comparison(
  early_prod_grobs, 
  late_prod_grobs, 
  "Production",
  "All Water Types"
)

ggsave(
  file.path("outputs", "production_comparison.png"),
  production_comparison,
  width = 20,
  height = 5 * length(unique(merged_data_total$WF_Crop)),
  dpi = 300
)


# -----------------------------
# 9. Function to create temporal comparison plots with water type distinction
# -----------------------------

create_temporal_comparison <- function(early_grobs, late_grobs, title_text, water_type) {
  crops <- names(early_grobs)
  n_crops <- length(crops)
  
  # Create layout matrix for side-by-side comparison
  layout_mat <- matrix(1:(2 * n_crops), ncol = 2, byrow = TRUE)
  
  # Create title
  title <- textGrob(
    sprintf("%s - %s\n(%s vs %s)",
            title_text,
            water_type,
            paste(range(periods$early), collapse = "-"),
            paste(range(periods$late), collapse = "-")),
    gp = gpar(fontsize = 14, font = 2)
  )
  
  # Combine grobs
  all_grobs <- vector("list", 2 * n_crops)
  for (i in seq_len(n_crops)) {
    all_grobs[[layout_mat[i, 1]]] <- early_grobs[[i]]
    all_grobs[[layout_mat[i, 2]]] <- late_grobs[[i]]
  }
  
  # Arrange plots
  arrangeGrob(
    grobs = all_grobs,
    layout_matrix = layout_mat,
    top = title
  )
}



# -----------------------------
# 10. Save Data Results
# -----------------------------

# Save results by water type
for (water_type in names(early_data_by_type)) {
  write.csv(
    early_data_by_type[[water_type]] %>%
      rename(
        WF_L_per_kg_per_year = WF_L_per_kg,
        Total_WF_L_per_year = Total_WF,
        Production_kg_per_year = Production_kg
      ),
    file.path("outputs", sprintf("water_footprint_early_period_%s.csv", 
                                 tolower(gsub(" ", "_", water_types[match(water_type, names(early_data_by_type))])))),
    row.names = FALSE
  )
  
  write.csv(
    late_data_by_type[[water_type]] %>%
      rename(
        WF_L_per_kg_per_year = WF_L_per_kg,
        Total_WF_L_per_year = Total_WF,
        Production_kg_per_year = Production_kg
      ),
    file.path("outputs", sprintf("water_footprint_late_period_%s.csv", 
                                 tolower(gsub(" ", "_", water_types[match(water_type, names(late_data_by_type))])))),
    row.names = FALSE
  )
}

# Print comprehensive temporal statistics by water type
print("\nTemporal comparison summary by water type:")
print("==========================================")

for (water_type in names(early_data_by_type)) {
  cat(sprintf("\n=== Water Type: %s ===\n", 
              water_types[match(water_type, names(early_data_by_type))]))
  
  for (current_crop in unique(early_data_by_type[[water_type]]$WF_Crop)) {
    cat(sprintf("\n--- Crop: %s ---\n", current_crop))
    
    # Water Footprint stats (L/kg/year)
    early_wf <- mean(early_data_by_type[[water_type]]$WF_L_per_kg[
      early_data_by_type[[water_type]]$WF_Crop == current_crop], na.rm = TRUE)
    late_wf <- mean(late_data_by_type[[water_type]]$WF_L_per_kg[
      late_data_by_type[[water_type]]$WF_Crop == current_crop], na.rm = TRUE)
    wf_change_pct <- (late_wf - early_wf) / early_wf * 100
    
    cat("\nWater Footprint (L/kg/year):")
    cat("\n  Early period mean:", round(early_wf, 1))
    cat("\n  Late period mean:", round(late_wf, 1))
    cat("\n  Percent change:", round(wf_change_pct, 1), "%")
    
    # Production stats (kg/year)
    early_prod <- mean(early_data_by_type[[water_type]]$Production_kg[
      early_data_by_type[[water_type]]$WF_Crop == current_crop], na.rm = TRUE)
    late_prod <- mean(late_data_by_type[[water_type]]$Production_kg[
      late_data_by_type[[water_type]]$WF_Crop == current_crop], na.rm = TRUE)
    prod_change_pct <- (late_prod - early_prod) / early_prod * 100
    
    cat("\nProduction (kg/year):")
    cat("\n  Early period mean:", format(round(early_prod), big.mark = ","))
    cat("\n  Late period mean:", format(round(late_prod), big.mark = ","))
    cat("\n  Percent change:", round(prod_change_pct, 1), "%\n")
  }
}


# ---------
# 11. Function to create boxplots plots 
# ---------
create_wf_boxplots <- function(early_data, late_data, water_type) {
  # Combine data from both periods
  plot_data <- bind_rows(
    early_data %>% 
      filter(WF_L_per_kg > 0) %>%  # Remove zeros
      mutate(Period = "Early"),
    late_data %>% 
      filter(WF_L_per_kg > 0) %>%  # Remove zeros
      mutate(Period = "Late")
  )
  
  # Create boxplot with multiple crops
  # Calculate the 95th percentile of the y-axis variable
  upper_limit <- quantile(plot_data$WF_L_per_kg, 0.95, na.rm = TRUE)
  
  # Create the boxplot with adjusted y-axis limits
  p <- ggplot(plot_data, aes(x = WF_Crop, y = WF_L_per_kg, fill = Period)) +
    geom_boxplot(
      position = position_dodge(width = 0.8),
      width = 0.7,
      outlier.shape = NA  # Hides outliers
    ) +
    scale_fill_manual(values = c("Early" = "skyblue", "Late" = "darkblue")) +
    labs(
      title = paste("Water Footprint Distribution by Crop Group\nWater Type:", water_type),
      x = "Crop Group",
      y = "Water Footprint (L/kg/year)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "bottom"
    ) +
    coord_cartesian(ylim = c(0, upper_limit))  # Adjust the lower limit as needed
  
  
  
  # Calculate summary statistics
  stats_df <- plot_data %>%
    group_by(Period, WF_Crop) %>%
    summarise(
      Mean = mean(WF_L_per_kg, na.rm = TRUE),
      Median = median(WF_L_per_kg, na.rm = TRUE),
      Q1 = quantile(WF_L_per_kg, 0.25, na.rm = TRUE),
      Q3 = quantile(WF_L_per_kg, 0.75, na.rm = TRUE),
      Min = min(WF_L_per_kg, na.rm = TRUE),
      Max = max(WF_L_per_kg, na.rm = TRUE),
      N_Countries = n(),
      .groups = 'drop'
    ) %>%
    mutate(Water_Type = water_type)
  
  return(list(
    plot = p,
    stats = stats_df
  ))
}

# Create plots for each water type
comparison_plots <- list(
  rainfed = create_wf_boxplots(
    early_data_by_type$rainfed, 
    late_data_by_type$rainfed, 
    "Green Rainfed"
  ),
  irrigated_green = create_wf_boxplots(
    early_data_by_type$irrigated_green, 
    late_data_by_type$irrigated_green, 
    "Green Irrigated"
  ),
  irrigated_blue = create_wf_boxplots(
    early_data_by_type$irrigated_blue, 
    late_data_by_type$irrigated_blue, 
    "Blue Irrigated"
  ),
  total = create_wf_boxplots(
    early_data_by_type$total, 
    late_data_by_type$total, 
    "Total"
  )
)

# Save plots
dir.create("outputs/boxplots", recursive = TRUE, showWarnings = FALSE)

# Save each plot
for (water_type in names(comparison_plots)) {
  ggsave(
    filename = file.path("outputs/boxplots", 
                         sprintf("%s_boxplot.png", 
                                 tolower(gsub(" ", "_", water_type)))),
    plot = comparison_plots[[water_type]]$plot,
    width = 12,
    height = 8
  )
}

# Combine and save statistics
all_stats <- bind_rows(
  lapply(comparison_plots, function(x) x$stats)
)

write.csv(all_stats, 
          file.path("outputs", "water_footprint_boxplot_statistics.csv"), 
          row.names = FALSE)