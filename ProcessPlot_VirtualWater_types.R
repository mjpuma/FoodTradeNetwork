# =============================================================================
# Complete Water Footprint and Trade Flow Analysis Script
# Supports both period and single year analysis
# Part 1: Setup, Data Loading, and Core Functions
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
  "rnaturalearth", "rnaturalearthdata", "sf", "viridis",
  "igraph", "rworldmap", "RColorBrewer", "FAOSTAT", "ggpubr", 
  "networkD3", "tibble"
)


installed_packages <- rownames(installed.packages())
for(p in required_packages){
  if(!(p %in% installed_packages)){
    install.packages(p, dependencies = TRUE)
  }
}

suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})

# -----------------------------
# 2. Setup and Configuration
# -----------------------------
# Define analysis options
analysis_type <- "period"  # Options: "period" or "single_year"
target_year <- 2005            # Only used if analysis_type is "single_year"

# Define periods (used if analysis_type is "period")
periods <- list(
  early = 2001:2005,
  late = 2016:2020
)

# -----------------------------
# 3. Helper Functions
# -----------------------------
# Function to get analysis years based on configuration
get_analysis_years <- function() {
  if(analysis_type == "single_year") {
    return(target_year)
  } else {
    return(periods)
  }
}

# Function to get year label for outputs
get_year_label <- function(years) {
  if(length(years) == 1) {
    return(as.character(years))
  } else {
    return(paste(min(years), max(years), sep = "-"))
  }
}

# Function to standardize crop names
standardize_crop_name <- function(crop_name) {
  crop_name %>%
    str_trim() %>%
    str_to_title() %>%
    str_replace_all("\\s+", " ")
}

# -----------------------------
# 4. Load Base Data
# -----------------------------
# Load country conversion table
country_conversion <- read.csv("ancillary/country_conversion_table.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Country = str_trim(Country),
    FAOST_CODE = as.character(FAOSTAT),
    iso3 = str_trim(ISO3.alpha)
  ) %>%
  filter(!is.na(iso3) & iso3 != "")

# Create country mappings
faostat_to_iso3 <- setNames(country_conversion$iso3, country_conversion$FAOST_CODE)
iso3_to_name <- setNames(country_conversion$Country, country_conversion$iso3)

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

# Define crop mapping
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
    rep("Temperate Cereals", 4),
    rep("Tropical Cereals", 3),
    rep("Temperate Roots", 2),
    rep("Tropical Roots", 3),
    rep("Pulses", 3),
    "Soyabean", "Groundnut", "Rapeseed", "Sunflower", "Sugar"
  ),
  stringsAsFactors = FALSE
) %>%
  mutate(FAO_Code = as.character(FAO_Code))

# Load extraction rates
extraction_rates <- read_excel("ancillary/FoodCommodity_ForCarole_v5.xlsx", sheet = "withData") %>%
  rename(
    FAO_Code = `FAO Code`,
    Extraction_Rate = `Primary Product Extraction Rate`
  ) %>%
  mutate(
    FAO_Code = as.character(FAO_Code),
    Extraction_Rate = ifelse(is.na(Extraction_Rate) | Extraction_Rate == 0, 1, Extraction_Rate)
  )

# -----------------------------
# 5. Water Footprint Functions
# -----------------------------
# Function to read and process raw water footprint data
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

# Function to get water footprint by type
get_water_footprint_by_type <- function(data, water_type = NULL) {
  if (is.null(water_type)) {
    return(data %>%
             group_by(Country, WF_Crop, Year) %>%
             summarise(
               WaterFootprint_km3_per_year = sum(WaterFootprint_km3_per_year, na.rm = TRUE),
               Water_Type = "Total",
               .groups = 'drop'
             ))
  } else {
    return(data %>% filter(Water_Type == water_type))
  }
}

# Function to process production data
process_production_data <- function(years) {
  production_data_file <- "data_raw/crop_production_data.rds"
  
  if (!file.exists(production_data_file)) {
    cat("Downloading production data from FAOSTAT...\n")
    fao_item_codes <- crop_mapping$FAO_Code
    production_data_raw <- FAOSTAT::get_faostat_data(
      domain_code = "QC",
      item_code = fao_item_codes,
      element_code = 5510,
      output_format = "RDS"
    )
    dir.create("data_raw", showWarnings = FALSE)
    saveRDS(production_data_raw, production_data_file)
  } else {
    production_data_raw <- readRDS(production_data_file)
  }
  
  production_data_raw %>%
    mutate(
      Country = str_trim(area),
      Year = as.numeric(year),
      Production_Quantity = as.numeric(value),
      item = standardize_crop_name(item)
    ) %>%
    left_join(alternative_names, by = "Country") %>%
    mutate(Country = ifelse(!is.na(Standardized_Country), Standardized_Country, Country)) %>%
    select(-Standardized_Country) %>%
    left_join(country_conversion, by = "Country") %>%
    filter(!is.na(iso3)) %>%
    left_join(crop_mapping, by = c("item" = "FAO_Crop_Name")) %>%
    filter(!is.na(WF_Crop)) %>%
    mutate(Production_kg = Production_Quantity * 1000) %>%
    filter(Year %in% years) %>%
    group_by(iso3, Year, WF_Crop) %>%
    summarise(
      Production_kg = sum(Production_kg, na.rm = TRUE),
      .groups = 'drop'
    )
}

# -----------------------------
# 6. Main Processing Functions
# -----------------------------
# Function to calculate water footprints
# Function to read and process raw water footprint data
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

# Function to get water footprint by type
get_water_footprint_by_type <- function(data, water_type = NULL) {
  if (is.null(water_type)) {
    return(data %>%
             group_by(Country, WF_Crop, Year, iso3) %>%  # Added iso3 to grouping
             summarise(
               WaterFootprint_km3_per_year = sum(WaterFootprint_km3_per_year, na.rm = TRUE),
               Water_Type = "Total",
               .groups = 'drop'
             ))
  } else {
    return(data %>% filter(Water_Type == water_type))
  }
}

# Function to calculate water footprints
# Function to calculate water footprints
calculate_water_footprints <- function(analysis_years) {
  # Read raw water footprint data
  gw_rainfed_df <- read_water_footprint("ancillary/GW_rainfed_lpjml.xlsx", "Green_Rainfed")
  gw_irrigated_df <- read_water_footprint("ancillary/GW_irrigated_lpjml.xlsx", "Green_Irrigated")
  bw_irrigated_df <- read_water_footprint("ancillary/BW_irrigated_lpjml.xlsx", "Blue_Irrigated")
  
  # Create a mapping dataframe for consistent country handling
  country_mapping <- country_conversion %>%
    select(Country, iso3) %>%
    # Add alternative names
    bind_rows(
      alternative_names %>%
        select(Country) %>%
        left_join(alternative_names, by = "Country") %>%
        left_join(country_conversion %>% select(Country, iso3), 
                  by = c("Standardized_Country" = "Country")) %>%
        select(Country, iso3)
    ) %>%
    distinct() %>%
    group_by(Country) %>%
    slice(1) %>%  # Take first match if multiple exist
    ungroup()
  
  # Combine all water footprint data
  water_footprint_long <- bind_rows(gw_rainfed_df, gw_irrigated_df, bw_irrigated_df) %>%
    mutate(
      Country = str_replace_all(Country, "^['\"]+|['\"]+$", "") %>% str_trim(),
      WF_Crop = standardize_crop_name(WF_Crop),
      Year = as.numeric(Year)
    ) %>%
    # Join with country mapping to get consistent ISO3 codes
    left_join(country_mapping, by = "Country", relationship = "many-to-many") %>%
    filter(!is.na(iso3))  # Remove any countries we couldn't match
  
  # Get different versions of water footprint data
  water_footprint_rainfed <- get_water_footprint_by_type(water_footprint_long, "Green_Rainfed")
  water_footprint_irrigated_green <- get_water_footprint_by_type(water_footprint_long, "Green_Irrigated")
  water_footprint_irrigated_blue <- get_water_footprint_by_type(water_footprint_long, "Blue_Irrigated")
  water_footprint_total <- get_water_footprint_by_type(water_footprint_long)
  
  # Get production data
  production_data <- process_production_data(analysis_years)
  
  # Function to merge and calculate water footprint intensities
  merge_and_calculate <- function(water_data, production_data) {
    water_data %>%
      inner_join(production_data, by = c("iso3", "Year", "WF_Crop"), relationship = "many-to-many") %>%
      mutate(
        # Convert water footprint from km³/year to L/year
        WaterFootprint_liters_per_year = WaterFootprint_km3_per_year * 1e12,
        # Calculate water footprint intensity in L/kg/year
        WF_L_per_kg = WaterFootprint_liters_per_year / Production_kg
      ) %>%
      group_by(iso3, WF_Crop, Water_Type) %>%
      summarise(
        WF_L_per_kg = mean(WF_L_per_kg, na.rm = TRUE),
        Total_WF_L_per_year = mean(WaterFootprint_liters_per_year, na.rm = TRUE),
        Production_kg = mean(Production_kg, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      # Handle any invalid values
      mutate(
        WF_L_per_kg = ifelse(is.infinite(WF_L_per_kg) | is.nan(WF_L_per_kg), NA, WF_L_per_kg),
        WF_L_per_kg = pmax(WF_L_per_kg, 0, na.rm = TRUE)  # Ensure non-negative values
      )
  }
  
  # Calculate final water footprint datasets
  results <- list(
    rainfed = merge_and_calculate(water_footprint_rainfed, production_data),
    irrigated_green = merge_and_calculate(water_footprint_irrigated_green, production_data),
    irrigated_blue = merge_and_calculate(water_footprint_irrigated_blue, production_data),
    total = merge_and_calculate(water_footprint_total, production_data)
  )
  
  # Add some error checking and reporting
  for (type in names(results)) {
    n_rows <- nrow(results[[type]])
    if (n_rows == 0) {
      warning(sprintf("No results for %s water footprint type", type))
    } else {
      cat(sprintf("Processed %d entries for %s water footprint type\n", n_rows, type))
    }
  }
  
  return(results)
}


# Function to process trade data
process_trade_data <- function(years) {
  trade_data <- read.csv("inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv") %>%
    select(
      reporter = Reporter.Country.Code,
      partner = Partner.Country.Code,
      cropid = Item.Code,
      element = Element,
      year = Year,
      value = Value
    ) %>%
    filter(
      year %in% years,
      cropid %in% crop_mapping$FAO_Code,
      element == "Export Quantity",
      reporter != partner
    ) %>%
    mutate(
      reporter = as.character(reporter),
      partner = as.character(partner),
      cropid = as.character(cropid),
      value = as.numeric(value)
    ) %>%
    filter(reporter %in% country_conversion$FAOST_CODE & 
             partner %in% country_conversion$FAOST_CODE)
  
  # Add extraction rates and convert to primary equivalents
  trade_data %>%
    left_join(extraction_rates, by = c("cropid" = "FAO_Code")) %>%
    mutate(
      value_primary = value / Extraction_Rate * 1000  # Convert to kg
    ) %>%
    left_join(crop_mapping, by = c("cropid" = "FAO_Code")) %>%
    mutate(
      ReporterISO3 = faostat_to_iso3[reporter],
      PartnerISO3 = faostat_to_iso3[partner]
    )
}

# Function to calculate virtual water flows
calculate_virtual_water_flows <- function(trade_data, water_footprint_data) {
  trade_data %>%
    left_join(
      water_footprint_data,
      by = c("ReporterISO3" = "iso3", "WF_Crop" = "WF_Crop"),
      relationship = "many-to-many"
    ) %>%
    mutate(
      # Calculate virtual water flow (m³)
      virtual_water_m3 = value_primary * WF_L_per_kg / 1000,
      # Replace NA/NaN/negative with 0
      virtual_water_m3 = pmax(replace(virtual_water_m3, is.na(virtual_water_m3), 0), 0)
    ) %>%
    # Aggregate by reporter, partner, crop, and water type
    group_by(ReporterISO3, PartnerISO3, WF_Crop, Water_Type, year) %>%
    summarise(
      virtual_water_m3 = sum(virtual_water_m3, na.rm = TRUE),
      trade_volume_kg = sum(value_primary, na.rm = TRUE),
      .groups = "drop"
    )
}

# Function to create visualizations
plot_water_flows <- function(flows, current_period, years) {
  # Get world map and centroids
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world_map <- rworldmap::getMap(resolution = "low")
  country_centroids <- data.frame(
    ISO3 = world_map$ISO_A3,
    Longitude = coordinates(world_map)[,1],
    Latitude = coordinates(world_map)[,2]
  ) %>%
    filter(ISO3 != "-99")
  
  # Calculate average flows across years and ensure positive values
  flows_avg <- flows %>%
    group_by(ReporterISO3, PartnerISO3, WF_Crop, Water_Type) %>%
    summarise(
      virtual_water_m3 = mean(virtual_water_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(virtual_water_m3 > 0)  # Remove any negative or zero flows
  
  # Add coordinates
  flows_avg <- flows_avg %>%
    left_join(country_centroids, by = c("ReporterISO3" = "ISO3")) %>%
    rename(Exporter_Longitude = Longitude, Exporter_Latitude = Latitude) %>%
    left_join(country_centroids, by = c("PartnerISO3" = "ISO3")) %>%
    rename(Importer_Longitude = Longitude, Importer_Latitude = Latitude) %>%
    filter(!is.na(Exporter_Longitude) & !is.na(Importer_Longitude))
  
  # Create plots for each crop and water type combination
  for (crop in unique(flows_avg$WF_Crop)) {
    for (water_type in unique(flows_avg$Water_Type)) {
      # Filter data
      plot_data <- flows_avg %>%
        filter(
          WF_Crop == crop,
          Water_Type == water_type
        )
      
      # Only plot if we have data
      if(nrow(plot_data) == 0) {
        warning(sprintf("No valid data for %s - %s, skipping plot", crop, water_type))
        next
      }
      
      # Only plot significant flows (top 5%)
      threshold <- quantile(plot_data$virtual_water_m3, 0.95)
      plot_data <- plot_data %>% 
        filter(virtual_water_m3 >= threshold) %>%
        # Ensure line widths will be valid
        mutate(
          scaled_flow = (virtual_water_m3 - min(virtual_water_m3)) / 
            (max(virtual_water_m3) - min(virtual_water_m3)) * 0.9 + 0.1
        )
      
      # Get year label
      year_label <- get_year_label(years)
      
      # Create plot
      p <- ggplot() +
        geom_sf(data = world, fill = "antiquewhite") +
        geom_curve(
          data = plot_data,
          aes(
            x = Exporter_Longitude,
            y = Exporter_Latitude,
            xend = Importer_Longitude,
            yend = Importer_Latitude,
            linewidth = scaled_flow
          ),
          color = "steelblue",
          curvature = 0.33,
          alpha = 0.7
        ) +
        scale_linewidth_continuous(
          range = c(0.1, 1),
          labels = scales::label_number(scale_cut = scales::cut_short_scale()),
          guide = guide_legend(title = "Virtual Water Flow (m³)")
        ) +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "aliceblue"),
          panel.grid = element_blank(),
          legend.position = "bottom"
        ) +
        labs(
          title = sprintf("Virtual Water Flows - %s - %s\n(%s)",
                          crop, water_type, year_label),
          caption = "Data: FAO & Water Footprint Analysis",
          x = "", y = ""
        )
      
      # Save plot
      filename <- file.path(
        "outputs/flow_maps",
        sprintf("VirtualWaterFlows_%s_%s_%s.png",
                gsub(" ", "_", crop),
                gsub(" ", "_", water_type),
                if(analysis_type == "single_year") target_year else current_period)
      )
      dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
      ggsave(filename, p, width = 12, height = 8, dpi = 300)
    }
  }
}


# -----------------------------
# Function to perform network analysis
# -----------------------------
analyze_network <- function(flows, current_period) {
  # Calculate average flows across years
  flows_avg <- flows %>%
    group_by(ReporterISO3, PartnerISO3, WF_Crop, Water_Type) %>%
    summarise(
      virtual_water_m3 = mean(virtual_water_m3, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Perform network analysis for each crop and water type
  for (crop in unique(flows_avg$WF_Crop)) {
    for (water_type in unique(flows_avg$Water_Type)) {
      # Filter data
      network_data <- flows_avg %>%
        filter(
          WF_Crop == crop,
          Water_Type == water_type,
          virtual_water_m3 > 0
        )
      
      # Skip if there's no data
      if(nrow(network_data) == 0) {
        warning(sprintf("No data for %s - %s, skipping network analysis.", crop, water_type))
        next
      }
      
      # Create graph
      g <- igraph::graph_from_data_frame(
        network_data[, c("ReporterISO3", "PartnerISO3", "virtual_water_m3")],
        directed = TRUE
      )
      
      # Calculate centrality measures using igraph functions
      centrality <- data.frame(
        Country = igraph::V(g)$name,
        InDegree = igraph::degree(g, mode = "in"),
        OutDegree = igraph::degree(g, mode = "out"),
        Betweenness = igraph::betweenness(g, directed = TRUE),
        Closeness = igraph::closeness(g, mode = "all")
      ) %>%
        mutate(
          Country_Name = iso3_to_name[Country]
        )
      
      # Save results
      filename <- file.path(
        "outputs/network_stats",
        sprintf("NetworkMetrics_%s_%s_%s.csv",
                gsub(" ", "_", crop),
                gsub(" ", "_", water_type),
                if(analysis_type == "single_year") target_year else current_period)
      )
      dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
      write.csv(centrality, filename, row.names = FALSE)
    }
  }
}



create_comparative_flow_maps <- function(flows_early, flows_late) {
  # Get world map and centroids
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world_map <- rworldmap::getMap(resolution = "low")
  country_centroids <- data.frame(
    ISO3 = world_map$ISO_A3,
    Longitude = coordinates(world_map)[,1],
    Latitude = coordinates(world_map)[,2]
  ) %>%
    filter(ISO3 != "-99")
  
  # Process flows for both periods
  process_flows <- function(flows, period) {
    flows %>%
      group_by(ReporterISO3, PartnerISO3, WF_Crop, Water_Type) %>%
      summarise(
        virtual_water_m3 = mean(virtual_water_m3, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(virtual_water_m3 > 0) %>%
      left_join(country_centroids, by = c("ReporterISO3" = "ISO3")) %>%
      rename(Exporter_Longitude = Longitude, Exporter_Latitude = Latitude) %>%
      left_join(country_centroids, by = c("PartnerISO3" = "ISO3")) %>%
      rename(Importer_Longitude = Longitude, Importer_Latitude = Latitude) %>%
      filter(!is.na(Exporter_Longitude) & !is.na(Importer_Longitude))
  }
  
  flows_early_processed <- process_flows(flows_early, "early")
  flows_late_processed <- process_flows(flows_late, "late")
  
  # Create plots for each crop and water type combination
  for (crop in unique(c(flows_early_processed$WF_Crop, flows_late_processed$WF_Crop))) {
    for (water_type in unique(c(flows_early_processed$Water_Type, flows_late_processed$Water_Type))) {
      # Create base plot function
      create_flow_map <- function(data, period) {
        plot_data <- data %>%
          filter(
            WF_Crop == crop,
            Water_Type == water_type
          )
        
        if(nrow(plot_data) == 0) return(NULL)
        
        threshold <- quantile(plot_data$virtual_water_m3, 0.95)
        min_linewidth <- 1
        max_linewidth <- 5
        
        plot_data <- plot_data %>% 
          filter(virtual_water_m3 >= threshold) %>%
          mutate(
            scaled_flow = (virtual_water_m3 - min(virtual_water_m3)) / 
              (max(virtual_water_m3) - min(virtual_water_m3)) * (max_linewidth - min_linewidth) + min_linewidth
          )
        
        ggplot() +
          geom_sf(data = world, fill = "antiquewhite") +
          geom_curve(
            data = plot_data,
            aes(
              x = Exporter_Longitude,
              y = Exporter_Latitude,
              xend = Importer_Longitude,
              yend = Importer_Latitude,
              linewidth = scaled_flow
            ),
            color = "steelblue",
            curvature = 0.33,
            alpha = 0.7
          ) +
          scale_linewidth_continuous(
            range = c(min_linewidth, max_linewidth),
            labels = scales::label_number(scale_cut = scales::cut_short_scale())
          ) +
          theme_minimal(base_size = 14) +
          theme(
            panel.background = element_rect(fill = "aliceblue"),
            panel.grid = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12)
          ) +
          labs(
            title = sprintf("%s Period", period),
            linewidth = "Flow (m³)"
          )
      }
      
      # Create both plots
      p1 <- create_flow_map(flows_early_processed, "Early (2001-2005)")
      p2 <- create_flow_map(flows_late_processed, "Late (2016-2020)")
      
      if(!is.null(p1) && !is.null(p2)) {
        # Combine plots
        combined_plot <- ggpubr::ggarrange(p1, p2,
                                           ncol = 2,
                                           common.legend = TRUE,
                                           legend = "bottom",
                                           font.label = list(size = 14),  # Adjusted label font size
                                           labels = c("A", "B"),          # Added panel labels
                                           label.x = 0.1, label.y = 0.95  # Adjusted label positions
        )
        
        combined_plot <- ggpubr::annotate_figure(combined_plot,
                                                 top = ggpubr::text_grob(
                                                   sprintf("%s %s Flows", crop, water_type),
                                                   face = "bold", size = 18)  # Increased title size
        )
        
        # Save combined plot
        ggsave(
          file.path("outputs/flow_maps_comparative",
                    sprintf("VirtualWaterFlows_Comparative_%s_%s.png",
                            gsub(" ", "_", crop),
                            gsub(" ", "_", water_type))),
          combined_plot,
          width = 20,
          height = 8,
          dpi = 300
        )
      }
    }
  }
}




# Function to create bar plots of top flows
create_top_flows_plot <- function(flows, n_top = 10) {
  # Calculate average flows
  top_flows <- flows %>%
    group_by(ReporterISO3, PartnerISO3, WF_Crop, Water_Type) %>%
    summarise(
      virtual_water_m3 = mean(virtual_water_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Add country names
    mutate(
      Exporter = iso3_to_name[ReporterISO3],
      Importer = iso3_to_name[PartnerISO3]
    ) %>%
    # Get top flows
    group_by(WF_Crop, Water_Type) %>%
    slice_max(order_by = virtual_water_m3, n = n_top) %>%
    mutate(
      Flow_Label = paste(Exporter, "→", Importer),
      virtual_water_km3 = virtual_water_m3 / 1e9  # Convert to km³ for better readability
    )
  
  # Create plots for each crop and water type
  for (crop in unique(top_flows$WF_Crop)) {
    for (water_type in unique(top_flows$Water_Type)) {
      plot_data <- top_flows %>%
        filter(WF_Crop == crop, Water_Type == water_type)
      
      if(nrow(plot_data) > 0) {
        p <- ggplot(plot_data, aes(x = reorder(Flow_Label, virtual_water_km3), y = virtual_water_km3)) +
          geom_col(fill = "steelblue") +
          coord_flip() +
          theme_minimal() +
          labs(
            title = sprintf("Top %d Virtual Water Flows - %s - %s", n_top, crop, water_type),
            x = "Trade Flow",
            y = "Virtual Water Flow (km³/year)",
            caption = "Average annual flows"
          ) +
          theme(
            plot.title = element_text(face = "bold"),
            axis.text.y = element_text(size = 8)
          )
        
        ggsave(
          file.path("outputs/top_flows",
                    sprintf("TopFlows_%s_%s.png",
                            gsub(" ", "_", crop),
                            gsub(" ", "_", water_type))),
          p,
          width = 12,
          height = 8,
          dpi = 300
        )
      }
    }
  }
}


# -----------------------------
# Function to create Circos plots
# -----------------------------
create_circos_plot <- function(flows, period_name) {
  # Ensure necessary packages are loaded
  if (!requireNamespace("circlize", quietly = TRUE)) {
    install.packages("circlize")
  }
  library(circlize)
  
  if (!requireNamespace("tibble", quietly = TRUE)) {
    install.packages("tibble")
  }
  library(tibble)
  
  # Aggregate flows for the Circos plot
  circos_data <- flows %>%
    group_by(ReporterISO3, PartnerISO3, WF_Crop, Water_Type) %>%
    summarise(
      virtual_water_m3 = sum(virtual_water_m3, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # For each combination of crop and water type
  for (crop in unique(circos_data$WF_Crop)) {
    for (water_type in unique(circos_data$Water_Type)) {
      plot_data <- circos_data %>%
        filter(WF_Crop == crop, Water_Type == water_type) %>%
        filter(virtual_water_m3 > 0)
      
      if(nrow(plot_data) == 0) next
      
      # Limit to top countries to reduce clutter
      top_countries <- plot_data %>%
        group_by(ReporterISO3) %>%
        summarise(total_flow = sum(virtual_water_m3, na.rm = TRUE)) %>%
        top_n(15, total_flow) %>%
        pull(ReporterISO3)
      
      plot_data_filtered <- plot_data %>%
        filter(ReporterISO3 %in% top_countries & PartnerISO3 %in% top_countries)
      
      if(nrow(plot_data_filtered) == 0) next
      
      # Create a matrix of flows
      flow_matrix <- plot_data_filtered %>%
        select(ReporterISO3, PartnerISO3, virtual_water_m3) %>%
        tidyr::pivot_wider(names_from = PartnerISO3, values_from = virtual_water_m3, values_fill = 0)
      
      # Convert to matrix and set row names
      flow_matrix_matrix <- as.matrix(flow_matrix[,-1])  # Exclude the ReporterISO3 column
      rownames(flow_matrix_matrix) <- flow_matrix$ReporterISO3
      
      # Generate Circos plot
      output_file <- file.path("outputs/circos_plots",
                               sprintf("Circos_%s_%s_%s.png",
                                       crop, water_type, period_name))
      png(filename = output_file, width = 1000, height = 1000)
      
      circos.clear()
      circos.par(start.degree = 90, gap.degree = 5)
      chordDiagram(flow_matrix_matrix, 
                   transparency = 0.5,
                   annotationTrack = c("name", "grid"),
                   preAllocateTracks = list(track.height = 0.1))
      
      title(main = sprintf("%s %s Flows (%s)", crop, water_type, period_name), cex.main = 1.5)
      dev.off()
    }
  }
}



# -----------------------------
# Function to create Sankey plots with limited nodes
# -----------------------------
create_sankey_plot <- function(flows, period_name, top_n_countries = 10) {
  if (!requireNamespace("networkD3", quietly = TRUE)) {
    install.packages("networkD3")
  }
  library(networkD3)
  
  # Aggregate flows for the Sankey diagram
  sankey_data <- flows %>%
    group_by(ReporterISO3, PartnerISO3, WF_Crop, Water_Type) %>%
    summarise(
      virtual_water_m3 = sum(virtual_water_m3, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # For each combination of crop and water type
  for (crop in unique(sankey_data$WF_Crop)) {
    for (water_type in unique(sankey_data$Water_Type)) {
      plot_data <- sankey_data %>%
        filter(WF_Crop == crop, Water_Type == water_type) %>%
        filter(virtual_water_m3 > 0)
      
      if(nrow(plot_data) == 0) next
      
      # Calculate total flow per country (as exporters)
      total_export_flows <- plot_data %>%
        group_by(ReporterISO3) %>%
        summarise(total_flow = sum(virtual_water_m3)) %>%
        arrange(desc(total_flow))
      
      # Get top N countries by total export flow
      top_countries <- total_export_flows %>%
        slice(1:top_n_countries) %>%
        pull(ReporterISO3)
      
      # # Set a flow threshold (e.g., flows greater than 1e9 m³)
      # flow_threshold <- 1e9
      # 
      # # Filter data to include only significant flows
      # plot_data_filtered <- plot_data %>%
      #   filter(virtual_water_m3 >= flow_threshold)
      
      
      # Replace countries not in top_countries with 'Other'
      plot_data_filtered <- plot_data %>%
        mutate(
          ReporterISO3 = ifelse(ReporterISO3 %in% top_countries, ReporterISO3, "Other Exporters"),
          PartnerISO3 = ifelse(PartnerISO3 %in% top_countries, PartnerISO3, "Other Importers")
        ) %>%
        group_by(ReporterISO3, PartnerISO3) %>%
        summarise(virtual_water_m3 = sum(virtual_water_m3), .groups = 'drop')
      
      # Prepare nodes and links
      nodes <- data.frame(
        name = unique(c(plot_data_filtered$ReporterISO3, plot_data_filtered$PartnerISO3)),
        stringsAsFactors = FALSE
      )
      
      links <- plot_data_filtered %>%
        mutate(
          source = match(ReporterISO3, nodes$name) - 1,
          target = match(PartnerISO3, nodes$name) - 1,
          value = virtual_water_m3
        ) %>%
        select(source, target, value)
      
      # Convert to plain data frames to avoid warnings
      nodes <- as.data.frame(nodes)
      links <- as.data.frame(links)
      
      # Create Sankey diagram
      sankey <- sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "source",
        Target = "target",
        Value = "value",
        NodeID = "name",
        fontSize = 14,
        nodeWidth = 30,
        sinksRight = FALSE,
        iterations = 0  # Faster rendering
      )
      
      # Save the plot
      output_file <- file.path("outputs/sankey_diagrams",
                               sprintf("Sankey_%s_%s_%s.html",
                                       gsub(" ", "_", crop), gsub(" ", "_", water_type), period_name))
      htmlwidgets::saveWidget(sankey, file = output_file)
    }
  }
}


# -----------------------------
# Function to plot network metrics
# -----------------------------
create_network_metrics_plot <- function(flows) {
  # Calculate network metrics for each country
  network_metrics <- flows %>%
    group_by(WF_Crop, Water_Type) %>%
    group_modify(~{
      # Create graph
      g <- igraph::graph_from_data_frame(
        .x %>% select(ReporterISO3, PartnerISO3, virtual_water_m3),
        directed = TRUE
      )
      
      # Calculate metrics
      data.frame(
        Country = igraph::V(g)$name,
        Betweenness = igraph::betweenness(g, directed = TRUE),
        InDegree = igraph::degree(g, mode = "in"),
        OutDegree = igraph::degree(g, mode = "out"),
        Strength = igraph::strength(g),
        PageRank = igraph::page_rank(g)$vector
      )
    }) %>%
    ungroup() %>%
    mutate(Country_Name = iso3_to_name[Country])
  
  # Create plots for each crop and water type
  for (crop in unique(network_metrics$WF_Crop)) {
    for (water_type in unique(network_metrics$Water_Type)) {
      plot_data <- network_metrics %>%
        filter(WF_Crop == crop, Water_Type == water_type)
      
      if(nrow(plot_data) > 0) {
        # Get top 20 countries by PageRank
        plot_data <- plot_data %>%
          slice_max(order_by = PageRank, n = 20)
        
        p <- ggplot(plot_data, aes(x = reorder(Country_Name, PageRank), y = PageRank)) +
          geom_col(fill = "darkred") +
          coord_flip() +
          theme_minimal() +
          labs(
            title = sprintf("Top 20 Countries by Network Centrality - %s - %s", crop, water_type),
            x = "Country",
            y = "PageRank Score",
            caption = "Higher PageRank indicates greater importance in the network"
          ) +
          theme(
            plot.title = element_text(face = "bold"),
            axis.text.y = element_text(size = 8)
          )
        
        ggsave(
          file.path("outputs/network_metrics",
                    sprintf("NetworkCentrality_%s_%s.png",
                            gsub(" ", "_", crop),
                            gsub(" ", "_", water_type))),
          p,
          width = 12,
          height = 8,
          dpi = 300
        )
      }
    }
  }
}



# -----------------------------
# 7. Main Execution
# -----------------------------
# -----------------------------
# 7. Main Execution
# -----------------------------
main <- function() {
  # Create all necessary output directories
  dir.create("outputs", showWarnings = FALSE)
  dir.create("outputs/flow_maps", showWarnings = FALSE)
  dir.create("outputs/flow_maps_comparative", showWarnings = FALSE)
  dir.create("outputs/top_flows", showWarnings = FALSE)
  dir.create("outputs/network_metrics", showWarnings = FALSE)
  dir.create("outputs/network_stats", showWarnings = FALSE)
  dir.create("outputs/sankey_diagrams", showWarnings = FALSE)   # New directory
  dir.create("outputs/circos_plots", showWarnings = FALSE)      # New directory
  
  if(analysis_type == "single_year") {
    cat(sprintf("\nProcessing year %d...\n", target_year))
    years <- target_year
    period_name <- as.character(target_year)
    
    # Calculate water footprints
    water_footprints <- calculate_water_footprints(years)
    
    # Save water footprint results
    for (type in names(water_footprints)) {
      write.csv(
        water_footprints[[type]],
        file.path("outputs", sprintf("water_footprint_%d_%s.csv", 
                                     target_year, type)),
        row.names = FALSE
      )
    }
    
    # Process trade data and calculate virtual water flows
    trade_data <- process_trade_data(years)
    flows <- calculate_virtual_water_flows(trade_data, bind_rows(water_footprints))
    
    # Create original visualizations
    plot_water_flows(flows, period_name, years)
    analyze_network(flows, period_name)
    
    # Create additional visualizations
    create_top_flows_plot(flows)
    create_network_metrics_plot(flows)
    
    # New plotting functions
    create_sankey_plot(flows, period_name)
    create_circos_plot(flows, period_name)
    
  } else {
    # Process early period
    cat("\nProcessing early period...\n")
    years_early <- periods$early
    water_footprints_early <- calculate_water_footprints(years_early)
    
    # Save early period results
    for (type in names(water_footprints_early)) {
      write.csv(
        water_footprints_early[[type]],
        file.path("outputs", sprintf("water_footprint_early_period_%s.csv", type)),
        row.names = FALSE
      )
    }
    
    # Process early period trade data
    trade_data_early <- process_trade_data(years_early)
    flows_early <- calculate_virtual_water_flows(trade_data_early, bind_rows(water_footprints_early))
    
    # Create original visualizations for early period
    plot_water_flows(flows_early, "early", years_early)
    analyze_network(flows_early, "early")
    
    # New plotting functions for early period
    create_sankey_plot(flows_early, "early")
    create_circos_plot(flows_early, "early")
    
    # Process late period
    cat("\nProcessing late period...\n")
    years_late <- periods$late
    water_footprints_late <- calculate_water_footprints(years_late)
    
    # Save late period results
    for (type in names(water_footprints_late)) {
      write.csv(
        water_footprints_late[[type]],
        file.path("outputs", sprintf("water_footprint_late_period_%s.csv", type)),
        row.names = FALSE
      )
    }
    
    # Process late period trade data
    trade_data_late <- process_trade_data(years_late)
    flows_late <- calculate_virtual_water_flows(trade_data_late, bind_rows(water_footprints_late))
    
    # Create original visualizations for late period
    plot_water_flows(flows_late, "late", years_late)
    analyze_network(flows_late, "late")
    
    # New plotting functions for late period
    create_sankey_plot(flows_late, "late")
    create_circos_plot(flows_late, "late")
    
    # Create additional comparative visualizations
    cat("\nCreating comparative visualizations...\n")
    try({
      create_comparative_flow_maps(flows_early, flows_late)
      create_top_flows_plot(flows_early)
      create_top_flows_plot(flows_late)
      create_network_metrics_plot(flows_early)
      create_network_metrics_plot(flows_late)
    })
    
    cat("\nProcessing completed successfully!\n")
  }
}



# Run the analysis
main()
      