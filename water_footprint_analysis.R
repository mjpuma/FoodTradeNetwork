# =============================================================================
# Updated Water Footprint and Trade Flow Analysis Script - Section 1
# Core Setup and Helper Functions
# =============================================================================

# -----------------------------
# 1. Load Required Packages
# -----------------------------
required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "ggplot2",
  "rnaturalearth", "rnaturalearthdata", "sf", "viridis",
  "igraph", "rworldmap", "RColorBrewer", "FAOSTAT", "ggpubr",
  "networkD3", "tibble", "circlize", "htmlwidgets", "grid", "rlang"
)

installed_packages <- rownames(installed.packages())
for (p in required_packages) {
  if (!(p %in% installed_packages)) {
    install.packages(p, dependencies = TRUE)
  }
}

suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})

# -----------------------------
# 2. Configuration Settings
# -----------------------------
# Define analysis options
analysis_type <- "period"  # Options: "period" or "single_year"
target_year <- 2005        # Only used if analysis_type is "single_year"

# Define periods
periods <- list(
  early = 2001:2005,
  late = 2016:2020
)

# -----------------------------
# 3. Unit Conversion Functions
# -----------------------------
# Global unit conversion factors
UNIT_CONVERSIONS <- list(
  L_to_m3 = 1/1000,
  m3_to_km3 = 1/1e9,
  L_to_km3 = 1/1e12,
  km3_to_L = 1e12
)

convert_water_units <- function(value, from_unit, to_unit) {
  if(from_unit == "km3" && to_unit == "L") {
    return(value * UNIT_CONVERSIONS$km3_to_L)
  } else if(from_unit == "L" && to_unit == "km3") {
    return(value * UNIT_CONVERSIONS$L_to_km3)
  } else if(from_unit == "L" && to_unit == "m3") {
    return(value * UNIT_CONVERSIONS$L_to_m3)
  } else if(from_unit == "m3" && to_unit == "km3") {
    return(value * UNIT_CONVERSIONS$m3_to_km3)
  }
  stop(paste("Unsupported unit conversion from", from_unit, "to", to_unit))
}

# Function to validate water volume values
validate_water_volume <- function(value, unit, context = "") {
  # Define reasonable limits for different units
  limits <- list(
    km3 = c(0, 1000),  # Maximum reasonable virtual water flow per crop
    m3 = c(0, 1e12),   # Corresponding value in m3
    L = c(0, 1e15)     # Corresponding value in L
  )
  
  if(!unit %in% names(limits)) {
    warning(paste("Unknown unit for validation:", unit))
    return(TRUE)
  }
  
  valid <- value >= limits[[unit]][1] & value <= limits[[unit]][2]
  if(any(!valid, na.rm = TRUE)) {
    warning(paste(
      "Suspicious water volume values detected in", context, "\n",
      "Unit:", unit, "\n",
      "Range:", paste(range(value, na.rm = TRUE), collapse = " to "), "\n",
      "Number of suspicious values:", sum(!valid, na.rm = TRUE)
    ))
  }
  return(valid)
}

# -----------------------------
# 4. Helper Functions
# -----------------------------
# Function to get analysis years based on configuration
get_analysis_years <- function() {
  if (analysis_type == "single_year") {
    return(target_year)
  } else {
    return(periods)
  }
}

# Function to get year label for outputs
get_year_label <- function(years) {
  if (length(years) == 1) {
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

# Function to create standardized logging
create_logger <- function(context) {
  log_file <- file.path("outputs", "logs", paste0(
    context, "_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".log"
  ))
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
  
  function(msg, level = "INFO") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message <- paste0(timestamp, " - [", level, "] - ", msg)
    cat(message, "\n")
    cat(message, "\n", file = log_file, append = TRUE)
  }
}

# =============================================================================
# Updated Water Footprint and Trade Flow Analysis Script - Section 2
# Data Loading and Mapping Functions
# =============================================================================

# -----------------------------
# 5. Load Base Data
# -----------------------------
# Load country conversion table
load_country_mappings <- function() {
  # Create logger
  log_msg <- create_logger("country_mapping")
  
  country_conversion <- read.csv("ancillary/country_conversion_table.csv", 
                                 stringsAsFactors = FALSE) %>%
    mutate(
      Country = str_trim(Country),
      FAOST_CODE = as.character(FAOSTAT),
      iso3 = str_trim(ISO3.alpha)
    ) %>%
    filter(!is.na(iso3) & iso3 != "") %>%
    distinct(iso3, .keep_all = TRUE)
  
  # Modify country_conversion to combine China regions
  china_codes <- c("41", "96", "128", "351")  # China, Hong Kong SAR, Macao SAR, China
  country_conversion <- country_conversion %>%
    filter(!FAOST_CODE %in% china_codes)
  
  china_entries <- data.frame(
    Country = "China",
    FAOST_CODE = china_codes,
    iso3 = "CHN",
    stringsAsFactors = FALSE
  )
  
  country_conversion <- bind_rows(country_conversion, china_entries) %>%
    distinct(iso3, .keep_all = TRUE)
  
  log_msg(paste("Processed", nrow(country_conversion), "country mappings"))
  
  return(country_conversion)
}

# Create mapping tables
create_mappings <- function(country_conversion) {
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
  
  return(list(
    faostat_to_iso3 = faostat_to_iso3,
    iso3_to_name = iso3_to_name,
    alternative_names = alternative_names
  ))
}

# Create crop mappings
create_crop_mappings <- function() {
  # Production data mapping
  production_mapping <- data.frame(
    FAO_Code = c(
      # Individual crops - production codes
      "27",   # Rice, paddy
      "156",  # Sugar cane
      "56",   # Maize
      "242",  # Groundnuts (with shell)
      "270",  # Rapeseed
      "236",  # Soybeans
      "267",  # Sunflower seed
      
      # Pulses
      "176",  # Beans, dry
      "187",  # Peas, dry
      "191",  # Chick peas
      
      # Cereals
      "15",   # Wheat
      "44",   # Barley
      "71",   # Rye
      "75",   # Oats
      "83",   # Sorghum
      "79",   # Millet
      
      # Roots
      "116",  # Potatoes
      "157",  # Sugar beet
      "125",  # Cassava
      "122",  # Sweet potatoes
      "137"   # Yams
    ),
    WF_Crop = c(
      "Rice",
      "Sugar",
      "Maize",
      "Groundnut",
      "Rapeseed",
      "Soyabean",
      "Sunflower",
      rep("Pulses", 3),
      rep("Temperate Cereals", 4),
      rep("Tropical Cereals", 2),
      rep("Temperate Roots", 2),
      rep("Tropical Roots", 3)
    ),
    stringsAsFactors = FALSE
  )
  
  # Trade data mapping (using same structure but with trade-specific codes)
  trade_mapping <- data.frame(
    FAO_Code = c(
      # Individual crops - trade codes (same as production except for groundnuts)
      "27",   # Rice, paddy
      "156",  # Sugar cane
      "56",   # Maize
      "243",  # Groundnuts, shelled
      "270",  # Rapeseed
      "236",  # Soybeans
      "267",  # Sunflower seed
      
      # Rest same as production
      "176", "187", "191",  # Pulses
      "15", "44", "71", "75",  # Temperate Cereals
      "83", "79",  # Tropical Cereals
      "116", "157",  # Temperate Roots
      "125", "122", "137"  # Tropical Roots
    ),
    WF_Crop = production_mapping$WF_Crop,
    stringsAsFactors = FALSE
  )
  
  return(list(
    production = production_mapping,
    trade = trade_mapping
  ))
}

# Load extraction rates
load_extraction_rates <- function() {
  log_msg <- create_logger("extraction_rates")
  
  extraction_rates <- read_excel("ancillary/FoodCommodity_ForCarole_v5.xlsx", 
                                 sheet = "withData") %>%
    rename(
      FAO_Code = `FAO Code`,
      Extraction_Rate = `Primary Product Extraction Rate`
    ) %>%
    mutate(
      FAO_Code = as.character(FAO_Code),
      Extraction_Rate = ifelse(is.na(Extraction_Rate) | Extraction_Rate == 0, 1, Extraction_Rate)
    )
  
  log_msg(paste("Loaded", nrow(extraction_rates), "extraction rates"))
  return(extraction_rates)
}

# Validate mappings
validate_mappings <- function(production_mapping, trade_mapping) {
  log_msg <- create_logger("mapping_validation")
  
  # Check for duplicate codes
  prod_dupes <- production_mapping$FAO_Code[duplicated(production_mapping$FAO_Code)]
  trade_dupes <- trade_mapping$FAO_Code[duplicated(trade_mapping$FAO_Code)]
  
  if(length(prod_dupes) > 0) {
    log_msg(paste("Duplicate production codes found:", 
                  paste(prod_dupes, collapse=", ")), "WARNING")
  }
  if(length(trade_dupes) > 0) {
    log_msg(paste("Duplicate trade codes found:", 
                  paste(trade_dupes, collapse=", ")), "WARNING")
  }
  
  # Check for consistency in WF_Crop categories
  prod_cats <- sort(unique(production_mapping$WF_Crop))
  trade_cats <- sort(unique(trade_mapping$WF_Crop))
  
  if(!identical(prod_cats, trade_cats)) {
    log_msg("WF_Crop categories differ between mappings", "WARNING")
    log_msg(paste("Production:", paste(prod_cats, collapse=", ")))
    log_msg(paste("Trade:", paste(trade_cats, collapse=", ")))
  }
  
  return(list(
    production_codes = sort(production_mapping$FAO_Code),
    trade_codes = sort(trade_mapping$FAO_Code),
    production_crops = prod_cats,
    trade_crops = trade_cats,
    has_errors = length(prod_dupes) > 0 || length(trade_dupes) > 0 || 
      !identical(prod_cats, trade_cats)
  ))
}

# =============================================================================
# Updated Water Footprint and Trade Flow Analysis Script - Section 3
# Water Footprint Data Processing Functions
# =============================================================================

# -----------------------------
# 6. Water Footprint Functions
# -----------------------------

# Simple read_water_footprint that preserves the original data
read_water_footprint <- function(file_path, water_type) {
  log_msg <- create_logger("water_footprint_reading")
  
  tryCatch({
    log_msg(paste("Starting to read:", file_path))
    
    # Read sheet names
    sheet_names <- excel_sheets(file_path)
    log_msg(paste("Found sheets:", paste(sheet_names, collapse = ", ")))
    
    # Initialize data list
    data_list <- list()
    
    # Define sheets to skip
    skip_sheets <- c("C3per", "C4per", "C3annual", "Global")
    
    for (sheet in sheet_names) {
      if (sheet %in% skip_sheets) {
        log_msg(paste("Skipping non-crop sheet:", sheet))
        next
      }
      
      log_msg(paste("Processing sheet:", sheet))
      
      # Read the sheet with error handling
      df <- tryCatch({
        suppressMessages(read_excel(file_path, sheet = sheet))
      }, error = function(e) {
        log_msg(paste("Error reading sheet:", sheet, "- Error:", e$message), "ERROR")
        return(NULL)
      })
      
      if (is.null(df)) next
      
      # Process the sheet
      colnames(df)[1] <- "Country"
      
      # Add metadata
      df$WF_Crop <- sheet
      df$Water_Type <- water_type
      
      # Convert to long format - keep original values
      df_long <- df %>%
        pivot_longer(
          cols = -c(Country, WF_Crop, Water_Type),
          names_to = "Year",
          values_to = "WaterFootprint_km3_per_year"  # Data is already in km3/year
        )
      
      # Sample check for this sheet
      sample_data <- head(df_long)
      log_msg(paste("\nSample data from sheet", sheet, ":"))
      print(sample_data)
      
      data_list[[sheet]] <- df_long
      log_msg(paste("Successfully processed sheet:", sheet,
                    "- Rows:", nrow(df_long)))
    }
    
    # Combine all data
    combined_data <- bind_rows(data_list)
    log_msg(paste("Total combined rows:", nrow(combined_data)))
    
    # Validate the range of values
    value_range <- range(combined_data$WaterFootprint_km3_per_year, na.rm = TRUE)
    log_msg(paste("Water footprint range (km³/year):", 
                  paste(format(value_range, scientific = TRUE), collapse = " to ")))
    
    return(combined_data)
    
  }, error = function(e) {
    log_msg(paste("Critical error:", e$message), "ERROR")
    stop("Error processing water footprint file ", file_path, ": ", e$message)
  })
}

# Add a diagnostic summary after water footprint calculation
summarize_water_footprints <- function(water_footprint_data) {
  log_msg <- create_logger("water_footprint_summary")
  
  # Overall summary
  log_msg("\nWater Footprint Summary:")
  log_msg(paste("Total records:", nrow(water_footprint_data)))
  log_msg(paste("Unique countries:", length(unique(water_footprint_data$Country))))
  log_msg(paste("Unique crops:", paste(unique(water_footprint_data$WF_Crop), collapse = ", ")))
  
  # Value ranges by water type
  water_footprint_data %>%
    group_by(Water_Type) %>%
    summarise(
      min_value = min(WaterFootprint_km3_per_year, na.rm = TRUE),
      max_value = max(WaterFootprint_km3_per_year, na.rm = TRUE),
      mean_value = mean(WaterFootprint_km3_per_year, na.rm = TRUE),
      n = n()
    ) %>%
    print()
  
  # Flag any potentially problematic values
  suspicious <- water_footprint_data %>%
    filter(WaterFootprint_km3_per_year > 100)  # Adjust threshold as needed
  
  if(nrow(suspicious) > 0) {
    log_msg("\nPotentially high values detected:")
    print(head(suspicious))
  }
}
# Function to get water footprint by type with unit validation
get_water_footprint_by_type <- function(data, water_type = NULL) {
  if (is.null(water_type)) {
    return(data %>%
             group_by(Country, WF_Crop, Year, iso3) %>%
             summarise(
               WaterFootprint_m3_per_year = sum(WaterFootprint_m3_per_year, na.rm = TRUE),
               Water_Type = "Total",
               .groups = 'drop'
             ))
  } else {
    return(data %>% filter(Water_Type == water_type))
  }
}

# Function to process production data with improved unit handling
process_production_data <- function(years) {
  log_msg <- create_logger("production_data")
  
  production_data_file <- "data_raw/crop_production_data.rds"
  
  if (!file.exists(production_data_file)) {
    log_msg("Downloading production data from FAOSTAT...")
    fao_item_codes <- production_mapping$FAO_Code
    
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
  
  # Process using production mapping
  production_data <- production_data_raw %>%
    mutate(
      item_code = as.character(item_code),
      year = as.numeric(year),
      area_code = as.character(area_code)
    ) %>%
    filter(year %in% years) %>%
    left_join(production_mapping, by = c("item_code" = "FAO_Code")) %>%
    filter(!is.na(WF_Crop)) %>%
    mutate(
      iso3 = as.character(area_code),
      # Convert to kg (FAOSTAT data is in tonnes)
      Production_kg = value * 1000
    ) %>%
    group_by(iso3, Year = year, WF_Crop) %>%
    summarise(
      Production_kg = sum(Production_kg, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Validate production data
  if (any(production_data$Production_kg < 0, na.rm = TRUE)) {
    log_msg("Negative production values detected!", "WARNING")
  }
  
  log_msg(paste("Processed production data:", nrow(production_data), "rows"))
  return(production_data)
}

# Calculate water footprints with consistent unit handling
calculate_water_footprints <- function(years) {
  log_msg <- create_logger("water_footprints")
  
  tryCatch({
    # Read water footprint data (keeping in km3)
    gw_rainfed_df <- read_water_footprint("ancillary/GW_rainfed_lpjml.xlsx", "Green_Rainfed")
    gw_irrigated_df <- read_water_footprint("ancillary/GW_irrigated_lpjml.xlsx", "Green_Irrigated")
    bw_irrigated_df <- read_water_footprint("ancillary/BW_irrigated_lpjml.xlsx", "Blue_Irrigated")
    
    # Get production data (keep in kg)
    production_data <- process_production_data(years)
    
    # Create country mapping from country_conversion data
    country_mapping <- country_conversion %>%
      select(Country, iso3) %>%
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
      slice(1) %>%
      ungroup()
    
    # Combine water footprint data
    water_footprint_long <- bind_rows(gw_rainfed_df, gw_irrigated_df, bw_irrigated_df) %>%
      mutate(
        Country = str_replace_all(Country, "^['\"]+|['\"]+$", "") %>% str_trim(),
        WF_Crop = standardize_crop_name(WF_Crop),
        Year = as.numeric(Year)
      ) %>%
      left_join(country_mapping, by = "Country", relationship = "many-to-many") %>%
      filter(!is.na(iso3))
    
    # Calculate water footprint intensities
    water_footprint_intensities <- water_footprint_long %>%
      mutate(iso3 = as.character(iso3)) %>%
      left_join(production_data, by = c("iso3", "Year", "WF_Crop"), 
                relationship = "many-to-many") %>%
      group_by(iso3, WF_Crop, Water_Type) %>%
      summarise(
        WaterFootprint_km3_per_year = mean(WaterFootprint_km3_per_year, na.rm = TRUE),
        Production_kg = mean(Production_kg, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      # Handle missing production data
      mutate(
        Production_kg = ifelse(is.na(Production_kg) | Production_kg == 0, NA, Production_kg)
      )
    
    # Add validation logging
    log_msg(paste("Processed water footprint intensities:", nrow(water_footprint_intensities), "rows"))
    log_msg(paste("Number of unique countries:", length(unique(water_footprint_intensities$iso3))))
    log_msg(paste("Number of unique crops:", length(unique(water_footprint_intensities$WF_Crop))))
    
    return(water_footprint_intensities)
    
  }, error = function(e) {
    log_msg(paste("Error in calculate_water_footprints:", e$message), "ERROR")
    stop(e)
  })
}

# =============================================================================
# Updated Water Footprint and Trade Flow Analysis Script - Section 4
# Trade Data and Virtual Water Flow Processing
# =============================================================================

# -----------------------------
# 7A. Unit Conversion Functions
# -----------------------------
convert_water_units <- function(value, from_unit, to_unit, context = "") {
  # Define reasonable limits for different units
  unit_limits <- list(
    km3 = c(0, 1000),         # Max reasonable virtual water flow ~1000 km³/year
    m3 = c(0, 1e12),          # Corresponding value in m³
    L = c(0, 1e15)            # Corresponding value in L
  )
  
  # Conversion factors
  conversions <- list(
    L_to_m3 = 1/1000,
    m3_to_km3 = 1/1e9,
    L_to_km3 = 1/1e12,
    km3_to_L = 1e12
  )
  
  # Perform conversion
  converted <- switch(paste(from_unit, to_unit, sep="_to_"),
                      "L_to_m3" = value * conversions$L_to_m3,
                      "m3_to_km3" = value * conversions$m3_to_km3,
                      "L_to_km3" = value * conversions$L_to_km3,
                      "km3_to_L" = value * conversions$km3_to_L,
                      stop("Unsupported conversion")
  )
  
  # Validate result
  if(!is.null(unit_limits[[to_unit]])) {
    valid_range <- unit_limits[[to_unit]]
    if(any(converted > valid_range[2], na.rm = TRUE)) {
      warning(sprintf(
        "Conversion produced suspiciously large values in %s:\n  From %s to %s\n  Max value: %g %s",
        context, from_unit, to_unit, max(converted, na.rm = TRUE), to_unit
      ))
    }
  }
  
  return(converted)
}

# -----------------------------
# 7B. Trade Data Processing
# -----------------------------
process_trade_data <- function(years) {
  log_msg <- create_logger("trade_data")
  
  log_msg("Starting trade data processing")
  
  # Read and process trade data
  trade_data <- read.csv("inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv") %>%
    select(
      reporter = Reporter.Country.Code,
      partner = Partner.Country.Code,
      cropid = Item.Code,
      element = Element,
      year = Year,
      value = Value
    ) %>%
    mutate(
      cropid = as.character(cropid)
    ) %>%
    # Use trade mapping here
    inner_join(trade_mapping, by = c("cropid" = "FAO_Code")) %>%
    filter(
      year %in% years,
      element == "Export Quantity",
      reporter != partner
    ) %>%
    mutate(
      reporter = as.character(reporter),
      partner = as.character(partner),
      value = as.numeric(value)
    )
  
  log_msg(paste("Filtered trade data:", nrow(trade_data), "rows"))
  
  # Join with country mappings
  trade_data <- trade_data %>%
    mutate(
      ReporterISO3 = faostat_to_iso3[reporter],
      PartnerISO3 = faostat_to_iso3[partner]
    ) %>%
    filter(!is.na(ReporterISO3) & !is.na(PartnerISO3))
  
  log_msg(paste("After ISO3 mapping:", nrow(trade_data), "rows"))
  
  # Add extraction rates and convert to primary equivalents
  trade_data <- trade_data %>%
    left_join(extraction_rates, by = c("cropid" = "FAO_Code")) %>%
    mutate(
      # Convert tonnes to kg and adjust for extraction rate
      value_primary = value * 1000 / Extraction_Rate  # tonnes to kg
    )
  
  # Validate trade volumes
  if (any(trade_data$value_primary < 0, na.rm = TRUE)) {
    log_msg("WARNING: Negative trade volumes detected!", "WARNING")
  }
  
  # Add validation for reasonable trade volumes
  max_expected_trade <- 1e9  # 1 billion kg
  suspicious_trades <- trade_data %>%
    filter(value_primary > max_expected_trade)
  
  if(nrow(suspicious_trades) > 0) {
    log_msg(sprintf(
      "WARNING: Found %d trades exceeding %d kg",
      nrow(suspicious_trades),
      max_expected_trade
    ), "WARNING")
  }
  
  log_msg(paste("Final processed rows:", nrow(trade_data), "rows"))
  
  return(trade_data)
}

# -----------------------------
# 7C. Virtual Water Flow Calculation
# -----------------------------
# Add this diagnostic function
diagnose_water_footprint_calculation <- function(flows, context = "") {
  log_msg <- create_logger("water_footprint_diagnostics")
  
  # Check each calculation step
  suspicious_flows <- flows %>%
    filter(virtual_water_km3 > 1000) %>%  # Flows above 1000 km³ are unrealistic
    select(
      ReporterISO3, PartnerISO3, WF_Crop, Water_Type,
      value_primary,  # trade volume in kg
      WF_m3_per_kg,  # water footprint intensity
      virtual_water_m3,
      virtual_water_km3
    ) %>%
    arrange(desc(virtual_water_km3))
  
  if(nrow(suspicious_flows) > 0) {
    log_msg(paste("\nDiagnostic Report for", context))
    log_msg("==================================")
    log_msg(sprintf("Found %d suspicious flows", nrow(suspicious_flows)))
    log_msg("\nTop 5 largest flows:")
    print(head(suspicious_flows, 5))
    
    # Analyze components
    log_msg("\nComponent Analysis:")
    log_msg("Trade volumes (kg) range:", 
            paste(range(suspicious_flows$value_primary), collapse = " to "))
    log_msg("Water footprint intensities (m³/kg) range:", 
            paste(range(suspicious_flows$WF_m3_per_kg), collapse = " to "))
    
    # Group by crop to see patterns
    crop_summary <- suspicious_flows %>%
      group_by(WF_Crop) %>%
      summarise(
        n = n(),
        mean_volume = mean(value_primary),
        mean_intensity = mean(WF_m3_per_kg),
        mean_flow = mean(virtual_water_km3)
      )
    
    log_msg("\nCrop-wise Summary of Suspicious Flows:")
    print(crop_summary)
  }
  
  return(suspicious_flows)
}


# Modified calculate_virtual_water_flows function with proper flow calculation
calculate_virtual_water_flows <- function(trade_data, water_footprint_data) {
  log_msg <- create_logger("virtual_water_flows")
  
  log_msg("Starting virtual water flow calculation")
  
  # Join trade and water footprint data
  flows <- trade_data %>%
    left_join(
      water_footprint_data,
      by = c("ReporterISO3" = "iso3", "WF_Crop" = "WF_Crop"),
      relationship = "many-to-many"
    )
  
  # Debug info for NA/zero checks
  log_msg("\nPre-calculation data summary:")
  log_msg(paste("Total rows:", nrow(flows)))
  log_msg(paste("Rows with NA water footprint:", 
                sum(is.na(flows$WaterFootprint_km3_per_year))))
  log_msg(paste("Rows with NA trade value:", 
                sum(is.na(flows$value_primary))))
  log_msg(paste("Rows with zero trade value:", 
                sum(flows$value_primary == 0, na.rm = TRUE)))
  
  # Calculate virtual water flows
  flows <- flows %>%
    mutate(
      # Calculate virtual water flow directly:
      # If country exports X kg of a crop that requires Y km3/year per kg production,
      # then the virtual water flow is X * Y km3/year
      virtual_water_km3 = (value_primary * WaterFootprint_km3_per_year) / Production_kg,
      
      # Handle NA and Inf values more carefully
      virtual_water_km3 = case_when(
        is.na(virtual_water_km3) ~ 0,
        is.infinite(virtual_water_km3) ~ 0,
        virtual_water_km3 < 0 ~ 0,
        TRUE ~ virtual_water_km3
      )
    )
  
  # Add validation checks for troubleshooting
  suspicious_flows <- flows %>%
    filter(virtual_water_km3 > 1000)  # Flows above 1000 km³ are suspicious
  
  if(nrow(suspicious_flows) > 0) {
    log_msg("\nDiagnostic Report for Pre-aggregation")
    log_msg("==================================")
    log_msg(paste("Found", nrow(suspicious_flows), "suspicious flows"))
    
    log_msg("\nTop 5 largest flows:")
    print(head(suspicious_flows %>% 
                 select(ReporterISO3, PartnerISO3, WF_Crop, 
                        Water_Type, value_primary, WaterFootprint_km3_per_year,
                        Production_kg, virtual_water_km3), 5))
    
    log_msg("\nComponent Analysis:")
    log_msg(paste("Trade volumes (kg) range:", 
                  paste(range(flows$value_primary, na.rm = TRUE), collapse = " to ")))
    log_msg(paste("Water footprint (km3/year) range:",
                  paste(range(flows$WaterFootprint_km3_per_year, na.rm = TRUE), 
                        collapse = " to ")))
    log_msg(paste("Production (kg) range:",
                  paste(range(flows$Production_kg, na.rm = TRUE), collapse = " to ")))
    
    # Calculate non-zero statistics
    non_zero_flows <- flows %>%
      filter(virtual_water_km3 > 0)
    
    if(nrow(non_zero_flows) > 0) {
      log_msg("\nNon-zero flow statistics:")
      flow_quantiles <- quantile(non_zero_flows$virtual_water_km3, 
                                 probs = c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99),
                                 na.rm = TRUE)
      print(flow_quantiles)
    }
  }
  
  # Final check
  log_msg("\nFinal flow statistics:")
  log_msg(paste("Total virtual water flow (km³):", 
                sum(flows$virtual_water_km3, na.rm = TRUE)))
  log_msg(paste("Number of non-zero flows:", 
                sum(flows$virtual_water_km3 > 0, na.rm = TRUE)))
  
  return(flows)
}



# -----------------------------
# 7D. Data Loss Investigation
# -----------------------------
investigate_data_loss <- function(trade_data, water_footprint_data, flows) {
  log_msg <- create_logger("data_loss")
  
  log_msg("Starting data loss investigation")
  
  # Check crops at each stage
  trade_crops <- unique(trade_data$WF_Crop)
  wf_crops <- unique(water_footprint_data$WF_Crop)
  flow_crops <- unique(flows$WF_Crop)
  
  log_msg(paste("Crops in trade data:", 
                paste(sort(trade_crops), collapse = ", ")))
  log_msg(paste("Crops in water footprint data:", 
                paste(sort(wf_crops), collapse = ", ")))
  log_msg(paste("Crops in final flows:", 
                paste(sort(flow_crops), collapse = ", ")))
  
  # Check for missing crops
  missing_after_join <- setdiff(trade_crops, flow_crops)
  
  if(length(missing_after_join) > 0) {
    log_msg(paste("Crops lost during join operations:", 
                  paste(missing_after_join, collapse = ", ")), "WARNING")
    
    # Investigate why these crops were lost
    for(crop in missing_after_join) {
      log_msg(paste("\nInvestigating missing crop:", crop))
      
      trade_records <- trade_data %>% 
        filter(WF_Crop == crop) %>%
        summarise(
          n_records = n(),
          total_volume = sum(value_primary, na.rm = TRUE)
        )
      
      wf_records <- water_footprint_data %>%
        filter(WF_Crop == crop) %>%
        summarise(
          n_records = n(),
          mean_intensity = mean(WF_m3_per_kg, na.rm = TRUE)
        )
      
      log_msg(sprintf("Trade data: %d records, %.2f kg total volume",
                      trade_records$n_records, trade_records$total_volume))
      log_msg(sprintf("Water footprint data: %d records, %.2f m³/kg mean intensity",
                      wf_records$n_records, wf_records$mean_intensity))
      
      # Check the join keys
      trade_countries <- unique(trade_data$ReporterISO3[trade_data$WF_Crop == crop])
      wf_countries <- unique(water_footprint_data$iso3[water_footprint_data$WF_Crop == crop])
      missing_countries <- setdiff(trade_countries, wf_countries)
      
      if(length(missing_countries) > 0) {
        log_msg(paste("Countries with trade data but no water footprint data:",
                      paste(missing_countries, collapse = ", ")), "WARNING")
      }
    }
  }
  
  return(list(
    trade_crops = trade_crops,
    wf_crops = wf_crops,
    flow_crops = flow_crops,
    missing_crops = missing_after_join
  ))
}

# =============================================================================
# Updated Water Footprint and Trade Flow Analysis Script - Section 5
# Visualization Functions
# =============================================================================

# -----------------------------
# 10. Data Preparation for Plotting
# -----------------------------
# Function to prepare data for visualization
prepare_visualization_data <- function(flows_early, flows_late) {
  log_msg <- create_logger("visualization_prep")
  
  # First, track what crops we have
  track_crops_through_pipeline(flows_early, flows_late)
  
  # Process early period data
  early_data <- flows_early %>%
    mutate(
      Water_Category = case_when(
        Water_Type == "Blue_Irrigated" ~ "Blue",
        Water_Type %in% c("Green_Rainfed", "Green_Irrigated") ~ "Green",
        TRUE ~ as.character(Water_Type)
      )
    ) %>%
    group_by(WF_Crop, Water_Category) %>%
    summarise(
      # Data is already in km3
      Total_Virtual_Water_km3 = sum(virtual_water_km3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Period = "Early")
  
  # Process late period data
  late_data <- flows_late %>%
    mutate(
      Water_Category = case_when(
        Water_Type == "Blue_Irrigated" ~ "Blue",
        Water_Type %in% c("Green_Rainfed", "Green_Irrigated") ~ "Green",
        TRUE ~ as.character(Water_Type)
      )
    ) %>%
    group_by(WF_Crop, Water_Category) %>%
    summarise(
      Total_Virtual_Water_km3 = sum(virtual_water_km3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Period = "Late")
  
  # Combine the data
  plot_data <- bind_rows(early_data, late_data) %>%
    filter(!is.na(Water_Category))
  
  # Validate combined data
  log_msg(paste("Total unique crops:", length(unique(plot_data$WF_Crop))))
  log_msg(paste("Value range (km³):", 
                paste(range(plot_data$Total_Virtual_Water_km3, na.rm = TRUE), 
                      collapse = " to ")))
  
  # Additional validation
  if(any(plot_data$Total_Virtual_Water_km3 > 1000, na.rm = TRUE)) {
    log_msg("WARNING: Some flows exceed 1000 km³/year", "WARNING")
    # Print suspicious values
    suspicious <- plot_data %>%
      filter(Total_Virtual_Water_km3 > 1000) %>%
      arrange(desc(Total_Virtual_Water_km3))
    print(suspicious)
  }
  
  return(plot_data)
}

# -----------------------------
# 11. Enhanced Visualization Functions
# -----------------------------
create_dot_plot <- function(plot_data) {
  # Prepare data with consistent crop ordering
  plot_data <- plot_data %>%
    mutate(
      WF_Crop = factor(WF_Crop, levels = c(
        "Rice", "Maize", "Temperate Cereals", "Tropical Cereals",
        "Groundnut", "Rapeseed", "Soyabean", "Sunflower",
        "Sugar", "Pulses",
        "Temperate Roots", "Tropical Roots"
      ))
    )
  
  # Create the dot plot
  p <- ggplot(plot_data, 
              aes(x = Total_Virtual_Water_km3, 
                  y = WF_Crop, 
                  color = Water_Category,
                  shape = Period)) +
    geom_point(size = 5) +  # Increased point size
    scale_x_log10(
      labels = scales::comma,
      breaks = scales::breaks_log(n = 6)
    ) +
    scale_color_manual(values = c("Blue" = "#2171b5", "Green" = "#31a354")) +
    scale_shape_manual(values = c("Early" = 16, "Late" = 17)) +
    labs(
      x = expression(paste("Virtual Water Volume (km"^3, "/year), log scale")),
      y = NULL,
      color = "Water Source",
      shape = "Period",
      title = "Virtual Water Flows by Crop Type"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.minor.x = element_line(color = "gray95"),
      legend.position = "bottom"
    )
  
  return(p)
}

create_small_multiples <- function(plot_data) {
  # Prepare data with consistent crop ordering
  plot_data <- plot_data %>%
    mutate(
      WF_Crop = factor(WF_Crop, levels = c(
        "Rice", "Maize", "Temperate Cereals", "Tropical Cereals",
        "Groundnut", "Rapeseed", "Soyabean", "Sunflower",
        "Sugar", "Pulses",
        "Temperate Roots", "Tropical Roots"
      ))
    )
  
  # Create the small multiples plot
  p <- ggplot(plot_data, 
              aes(x = Period, 
                  y = Total_Virtual_Water_km3,
                  fill = Water_Category)) +
    geom_col(position = "dodge", width = 0.7) +
    facet_wrap(~WF_Crop, scales = "free_y", ncol = 3) +
    scale_fill_manual(values = c("Blue" = "#2171b5", "Green" = "#31a354")) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = NULL,
      y = expression(paste("Virtual Water Volume (km"^3, "/year)")),
      fill = "Water Source",
      title = "Virtual Water Flows by Crop Type"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

create_top_flows_viz <- function(flows_late) {
  # Calculate net flows for top exporters and importers
  top_flows <- flows_late %>%
    group_by(ReporterISO3, Water_Type) %>%
    summarise(
      net_flow_km3 = sum(virtual_water_km3, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Get top 15 for each water type
    group_by(Water_Type) %>%
    arrange(desc(abs(net_flow_km3))) %>%
    slice_head(n = 15) %>%
    ungroup() %>%
    # Add country names
    mutate(
      Country = iso3_to_name[ReporterISO3],
      Water_Type = factor(Water_Type, 
                          levels = c("Blue_Irrigated", 
                                     "Green_Rainfed", 
                                     "Green_Irrigated"))
    )
  
  # Create visualization
  p <- ggplot(top_flows, 
              aes(x = reorder(Country, net_flow_km3), 
                  y = net_flow_km3,
                  fill = Water_Type)) +
    geom_col() +
    facet_wrap(~Water_Type, scales = "free_y", ncol = 1) +
    coord_flip() +
    scale_fill_manual(
      values = c(
        "Blue_Irrigated" = "#2171b5",
        "Green_Rainfed" = "#31a354",
        "Green_Irrigated" = "#74c476"
      )
    ) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Top 15 Virtual Water Traders by Water Type (2016-2020)",
      x = NULL,
      y = expression(paste("Net Virtual Water Flow (km"^3, "/year)")),
      fill = "Water Source"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      strip.text = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 11),
      legend.position = "bottom"
    )
  
  return(p)
}

# -----------------------------
# 12. Main Plotting Function
# -----------------------------
create_all_visualizations <- function(flows_early, flows_late) {
  log_msg <- create_logger("visualization")
  
  # Prepare data
  plot_data <- prepare_visualization_data(flows_early, flows_late)
  
  if (!is.null(plot_data) && nrow(plot_data) > 0) {
    log_msg("Creating visualizations")
    
    # Create plots
    dot_plot <- create_dot_plot(plot_data)
    small_multiples <- create_small_multiples(plot_data)
    top_flows <- create_top_flows_viz(flows_late)
    
    # Save plots
    ggsave(
      filename = file.path("outputs", "VW_DotPlot.png"),
      plot = dot_plot,
      width = 12,
      height = 10,
      dpi = 300
    )
    
    ggsave(
      filename = file.path("outputs", "VW_SmallMultiples.png"),
      plot = small_multiples,
      width = 15,
      height = 12,
      dpi = 300
    )
    
    ggsave(
      filename = file.path("outputs", "VW_TopFlows.png"),
      plot = top_flows,
      width = 12,
      height = 15,
      dpi = 300
    )
    
    log_msg("All visualizations created successfully")
    
    return(list(
      dot_plot = dot_plot,
      small_multiples = small_multiples,
      top_flows = top_flows
    ))
  } else {
    log_msg("Error: No data available for plotting", "ERROR")
    return(NULL)
  }
}

# =============================================================================
# Updated Water Footprint and Trade Flow Analysis Script - Section 6
# Main Execution and Helper Functions
# =============================================================================

# -----------------------------
# 13. Final Helper Functions
# -----------------------------
# Function to track crops through pipeline
track_crops_through_pipeline <- function(flows_early, flows_late) {
  log_msg <- create_logger("crop_tracking")
  
  # Get unique crops at each stage
  early_crops <- unique(flows_early$WF_Crop)
  late_crops <- unique(flows_late$WF_Crop)
  
  # Check water types for each crop and period
  water_types_summary <- bind_rows(
    flows_early %>%
      group_by(WF_Crop, Water_Type) %>%
      summarise(
        total_flow_km3 = sum(virtual_water_km3, na.rm = TRUE),
        n = n(),
        period = "Early",
        .groups = 'drop'
      ),
    flows_late %>%
      group_by(WF_Crop, Water_Type) %>%
      summarise(
        total_flow_km3 = sum(virtual_water_km3, na.rm = TRUE),
        n = n(),
        period = "Late",
        .groups = 'drop'
      )
  )
  
  # Log summaries
  log_msg("\nCrop and Water Type Summary:")
  log_msg(paste("Early period crops:", paste(sort(early_crops), collapse = ", ")))
  log_msg(paste("Late period crops:", paste(sort(late_crops), collapse = ", ")))
  
  # Return tracking data
  return(list(
    early_crops = early_crops,
    late_crops = late_crops,
    water_types_summary = water_types_summary
  ))
}

# Function to validate outputs
validate_outputs <- function(flows_early, flows_late) {
  log_msg <- create_logger("output_validation")
  
  # Check for reasonable ranges in both periods
  validate_period <- function(flows, period_name) {
    log_msg(paste("\nValidating", period_name, "period:"))
    
    # Check flow ranges
    flow_range <- range(flows$virtual_water_km3, na.rm = TRUE)
    log_msg(paste("Flow range (km³):", 
                  paste(format(flow_range, scientific = TRUE), collapse = " to ")))
    
    # Check for extreme values
    extreme_threshold <- 1000  # km³, adjust based on expected maximum
    extreme_flows <- flows %>%
      filter(virtual_water_km3 > extreme_threshold)
    
    if(nrow(extreme_flows) > 0) {
      log_msg(paste("WARNING: Found", nrow(extreme_flows), 
                    "flows exceeding", extreme_threshold, "km³"))
      print(extreme_flows %>% 
              select(ReporterISO3, PartnerISO3, WF_Crop, 
                     Water_Type, virtual_water_km3))
    }
    
    return(list(
      flow_range = flow_range,
      extreme_flows = extreme_flows
    ))
  }
  
  early_validation <- validate_period(flows_early, "early")
  late_validation <- validate_period(flows_late, "late")
  
  # Compare periods
  log_msg("\nPeriod Comparison:")
  log_msg(paste("Total flow change:",
                sum(late_validation$flow_range) - sum(early_validation$flow_range),
                "km³"))
  
  return(list(
    early = early_validation,
    late = late_validation
  ))
}

# =============================================================================
# 14. Water Footprint Validation Functions
# =============================================================================

# Function to validate water footprint data
validate_water_footprint_data <- function(data) {
  log_msg <- create_logger("water_footprint_validation")
  
  validation_report <- list(
    total_rows = nrow(data),
    unique_crops = unique(data$WF_Crop),
    unique_countries = unique(data$Country),
    year_range = range(as.numeric(data$Year), na.rm = TRUE),
    missing_values = colSums(is.na(data)),
    water_types = unique(data$Water_Type),
    value_range = range(data$WaterFootprint_m3_per_year, na.rm = TRUE)
  )
  
  # Log validation results
  log_msg("Data Validation Report:")
  log_msg(paste("Total rows:", validation_report$total_rows))
  log_msg(paste("Unique crops:", 
                paste(validation_report$unique_crops, collapse = ", ")))
  log_msg(paste("Year range:", 
                paste(validation_report$year_range, collapse = " - ")))
  log_msg(paste("Water types:", 
                paste(validation_report$water_types, collapse = ", ")))
  log_msg(paste("Value range (m³):", 
                paste(format(validation_report$value_range, scientific = TRUE), 
                      collapse = " to ")))
  
  # Check for anomalies
  if (any(validation_report$missing_values > 0)) {
    log_msg(paste("WARNING: Missing values detected in columns:", 
                  paste(names(validation_report$missing_values[validation_report$missing_values > 0]),
                        collapse = ", ")), "WARNING")
  }
  
  # Check for negative values
  if (any(data$WaterFootprint_m3_per_year < 0, na.rm = TRUE)) {
    log_msg("WARNING: Negative water footprint values detected!", "WARNING")
  }
  
  # Check for unreasonable values (adjust thresholds as needed)
  extreme_threshold <- 1e12  # 1000 km³ in m³
  extreme_values <- data$WaterFootprint_m3_per_year > extreme_threshold
  if (any(extreme_values, na.rm = TRUE)) {
    log_msg(paste("WARNING:", sum(extreme_values, na.rm = TRUE), 
                  "values exceed", format(extreme_threshold, scientific = TRUE), 
                  "m³"), "WARNING")
  }
  
  return(validation_report)
}

# Function to validate trade data
validate_trade_data <- function(data) {
  log_msg <- create_logger("trade_data_validation")
  
  log_msg("Trade Data Validation:")
  log_msg(paste("Total rows:", nrow(data)))
  log_msg(paste("Unique crops:", 
                paste(unique(data$WF_Crop), collapse = ", ")))
  log_msg(paste("Year range:", 
                paste(range(data$year), collapse = " - ")))
  log_msg(paste("Number of reporting countries:", 
                length(unique(data$ReporterISO3))))
  log_msg(paste("Number of partner countries:", 
                length(unique(data$PartnerISO3))))
  
  # Check for negative values
  if (any(data$value_primary < 0, na.rm = TRUE)) {
    log_msg("WARNING: Negative trade volumes detected!", "WARNING")
  }
  
  # Check for unreasonable values
  extreme_threshold <- 1e9  # 1 billion kg
  extreme_values <- data$value_primary > extreme_threshold
  if (any(extreme_values, na.rm = TRUE)) {
    log_msg(paste("WARNING:", sum(extreme_values, na.rm = TRUE), 
                  "trade volumes exceed", format(extreme_threshold, scientific = TRUE), 
                  "kg"), "WARNING")
  }
  
  return(list(
    total_rows = nrow(data),
    unique_crops = unique(data$WF_Crop),
    year_range = range(data$year),
    reporters = unique(data$ReporterISO3),
    partners = unique(data$PartnerISO3),
    value_range = range(data$value_primary, na.rm = TRUE)
  ))
}

# Function to validate flows
validate_flows <- function(flows) {
  log_msg <- create_logger("flow_validation")
  
  log_msg("Virtual Water Flows Validation:")
  log_msg(paste("Total flow records:", nrow(flows)))
  log_msg(paste("Unique crops:", 
                paste(unique(flows$WF_Crop), collapse = ", ")))
  log_msg(paste("Water types:", 
                paste(unique(flows$Water_Type), collapse = ", ")))
  
  # Convert to km³ for logging
  total_flow_km3 = sum(flows$virtual_water_m3, na.rm = TRUE) / 1e9
  log_msg(paste("Total virtual water flow:", 
                format(total_flow_km3, scientific = TRUE), "km³"))
  
  # Check flow distributions by water type
  flows %>%
    group_by(Water_Type) %>%
    summarise(
      total_flow_km3 = sum(virtual_water_m3, na.rm = TRUE) / 1e9,
      .groups = 'drop'
    ) %>%
    arrange(desc(total_flow_km3)) %>%
    print()
  
  return(list(
    total_flows = total_flow_km3,
    n_records = nrow(flows),
    unique_crops = unique(flows$WF_Crop),
    water_types = unique(flows$Water_Type)
  ))
}

# -----------------------------
# 15. Main Execution Function
# -----------------------------
main <- function() {
  # Create output directories
  dir.create("outputs/logs", recursive = TRUE, showWarnings = FALSE)
  
  # Start logging
  log_msg <- create_logger("main_execution")
  
  log_msg("Starting virtual water flow analysis")
  
  tryCatch({
    # Load all base data first
    log_msg("Loading base data...")
    load_base_data()
    
    # Load and validate mappings
    log_msg("Loading and validating mappings...")
    mapping_validation <- validate_mappings(
      production_mapping, 
      trade_mapping
    )
    
    if(mapping_validation$has_errors) {
      log_msg("WARNING: Issues found in mappings. Check validation results.", "WARNING")
    }
    
    # Process data for each period
    if (analysis_type == "period") {
      # Process early period
      log_msg("Processing early period")
      years_early <- periods$early
      
      water_footprints_early <- tryCatch({
        calculate_water_footprints(years_early)
      }, error = function(e) {
        log_msg(paste("Error calculating early water footprints:", e$message), "ERROR")
        stop(e)
      })
      
      log_msg("Processing early period trade data")
      trade_data_early <- process_trade_data(years_early)
      
      log_msg("Calculating early period virtual water flows")
      flows_early <- calculate_virtual_water_flows(trade_data_early, water_footprints_early)
      
      # Investigate early period data
      log_msg("Investigating early period data loss")
      investigation_early <- investigate_data_loss(
        trade_data_early, 
        water_footprints_early, 
        flows_early
      )
      
      # Process late period
      log_msg("Processing late period")
      years_late <- periods$late
      
      water_footprints_late <- tryCatch({
        calculate_water_footprints(years_late)
      }, error = function(e) {
        log_msg(paste("Error calculating late water footprints:", e$message), "ERROR")
        stop(e)
      })
      
      log_msg("Processing late period trade data")
      trade_data_late <- process_trade_data(years_late)
      
      log_msg("Calculating late period virtual water flows")
      flows_late <- calculate_virtual_water_flows(trade_data_late, water_footprints_late)
      
      # Investigate late period data
      log_msg("Investigating late period data loss")
      investigation_late <- investigate_data_loss(
        trade_data_late, 
        water_footprints_late, 
        flows_late
      )
      
      # Validate outputs
      log_msg("Validating outputs")
      validation_results <- validate_outputs(flows_early, flows_late)
      
      # Create visualizations
      log_msg("Creating visualizations")
      plots <- create_all_visualizations(flows_early, flows_late)
      
      # In the main() function, modify the results saving section:
      
      # Save results
      log_msg("Saving results")
      
      # Create results directory if it doesn't exist
      dir.create("outputs/results", recursive = TRUE, showWarnings = FALSE)
      
      # Save flow data - notice flows already have virtual_water_km3
      write.csv(
        flows_early,  # No conversion needed, already has virtual_water_km3
        file.path("outputs", "results", "virtual_water_flows_early_period.csv"),
        row.names = FALSE
      )
      
      write.csv(
        flows_late,  # No conversion needed, already has virtual_water_km3
        file.path("outputs", "results", "virtual_water_flows_late_period.csv"),
        row.names = FALSE
      )
      
      # Calculate and save summary statistics using the correct column name
      summary_stats <- data.frame(
        Period = c("Early", "Late"),
        Total_Flow_km3 = c(
          sum(flows_early$virtual_water_km3, na.rm = TRUE),
          sum(flows_late$virtual_water_km3, na.rm = TRUE)
        ),
        Blue_Water_km3 = c(
          sum(flows_early$virtual_water_km3[flows_early$Water_Type == "Blue_Irrigated"], 
              na.rm = TRUE),
          sum(flows_late$virtual_water_km3[flows_late$Water_Type == "Blue_Irrigated"], 
              na.rm = TRUE)
        ),
        Green_Water_km3 = c(
          sum(flows_early$virtual_water_km3[grepl("Green", flows_early$Water_Type)], 
              na.rm = TRUE),
          sum(flows_late$virtual_water_km3[grepl("Green", flows_late$Water_Type)], 
              na.rm = TRUE)
        )
      )
      
      # Add percentage changes
      summary_stats <- summary_stats %>%
        mutate(
          Total_Change_Percent = c(NA, (Total_Flow_km3[2] / Total_Flow_km3[1] - 1) * 100),
          Blue_Change_Percent = c(NA, (Blue_Water_km3[2] / Blue_Water_km3[1] - 1) * 100),
          Green_Change_Percent = c(NA, (Green_Water_km3[2] / Green_Water_km3[1] - 1) * 100)
        )
      
      write.csv(
        summary_stats,
        file.path("outputs", "results", "virtual_water_flow_summary.csv"),
        row.names = FALSE
      )
      
      # Save detailed crop-wise summary
      crop_summary <- bind_rows(
        flows_early %>%
          group_by(WF_Crop, Water_Type) %>%
          summarise(
            Total_Flow_km3 = sum(virtual_water_km3, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(Period = "Early"),
        flows_late %>%
          group_by(WF_Crop, Water_Type) %>%
          summarise(
            Total_Flow_km3 = sum(virtual_water_km3, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(Period = "Late")
      )
      
      write.csv(
        crop_summary,
        file.path("outputs", "results", "crop_wise_summary.csv"),
        row.names = FALSE
      )
      
      # Log completion and summary statistics
      log_msg("Analysis completed successfully")
      log_msg("\nSummary Statistics:")
      log_msg(paste("Early period total flow:", 
                    format(summary_stats$Total_Flow_km3[1], digits = 2), "km³"))
      log_msg(paste("Late period total flow:", 
                    format(summary_stats$Total_Flow_km3[2], digits = 2), "km³"))
      log_msg(paste("Total flow change:", 
                    format(summary_stats$Total_Change_Percent[2], digits = 2), "%"))
      
      return(list(
        flows_early = flows_early,
        flows_late = flows_late,
        plots = plots,
        validation = validation_results,
        summary = summary_stats,
        crop_summary = crop_summary,
        investigations = list(
          early = investigation_early,
          late = investigation_late
        )
      ))
      
    } else {
      log_msg("Single year analysis not implemented", "ERROR")
      stop("Single year analysis not implemented")
    }
    
  }, error = function(e) {
    log_msg(paste("Critical error in main execution:", e$message), "ERROR")
    stop(e)
  })
}

# -----------------------------
# Load Base Data Function
# -----------------------------
load_base_data <- function() {
  log_msg <- create_logger("base_data")
  
  # Load country conversion table
  log_msg("Loading country conversion table")
  country_conversion <<- read.csv("ancillary/country_conversion_table.csv", 
                                  stringsAsFactors = FALSE) %>%
    mutate(
      Country = str_trim(Country),
      FAOST_CODE = as.character(FAOSTAT),
      iso3 = str_trim(ISO3.alpha)
    ) %>%
    filter(!is.na(iso3) & iso3 != "") %>%
    distinct(iso3, .keep_all = TRUE)
  
  # Modify country_conversion to combine China regions
  log_msg("Processing China regions")
  china_codes <- c("41", "96", "128", "351")  # China, Hong Kong SAR, Macao SAR, China
  country_conversion <<- country_conversion %>%
    filter(!FAOST_CODE %in% china_codes)
  
  china_entries <- data.frame(
    Country = "China",
    FAOST_CODE = china_codes,
    iso3 = "CHN",
    stringsAsFactors = FALSE
  )
  
  country_conversion <<- bind_rows(country_conversion, china_entries) %>%
    distinct(iso3, .keep_all = TRUE)
  
  log_msg("Creating country mappings")
  # Create country mappings
  faostat_to_iso3 <<- setNames(country_conversion$iso3, country_conversion$FAOST_CODE)
  iso3_to_name <<- setNames(country_conversion$Country, country_conversion$iso3)
  
  # Handle alternative country names
  log_msg("Setting up alternative country names")
  alternative_names <<- data.frame(
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
  
  # Load extraction rates
  log_msg("Loading extraction rates")
  extraction_rates <<- read_excel("ancillary/FoodCommodity_ForCarole_v5.xlsx", 
                                  sheet = "withData") %>%
    rename(
      FAO_Code = `FAO Code`,
      Extraction_Rate = `Primary Product Extraction Rate`
    ) %>%
    mutate(
      FAO_Code = as.character(FAO_Code),
      Extraction_Rate = ifelse(is.na(Extraction_Rate) | Extraction_Rate == 0, 1, Extraction_Rate)
    )
  
  # Create crop mappings
  log_msg("Creating crop mappings")
  production_mapping <<- data.frame(
    FAO_Code = c(
      # Individual crops - production codes
      "27",   # Rice, paddy
      "156",  # Sugar cane
      "56",   # Maize
      "242",  # Groundnuts (with shell)
      "270",  # Rapeseed
      "236",  # Soybeans
      "267",  # Sunflower seed
      
      # Pulses
      "176",  # Beans, dry
      "187",  # Peas, dry
      "191",  # Chick peas
      
      # Cereals
      "15",   # Wheat
      "44",   # Barley
      "71",   # Rye
      "75",   # Oats
      "83",   # Sorghum
      "79",   # Millet
      
      # Roots
      "116",  # Potatoes
      "157",  # Sugar beet
      "125",  # Cassava
      "122",  # Sweet potatoes
      "137"   # Yams
    ),
    WF_Crop = c(
      "Rice",
      "Sugar",
      "Maize",
      "Groundnut",
      "Rapeseed",
      "Soyabean",
      "Sunflower",
      rep("Pulses", 3),
      rep("Temperate Cereals", 4),
      rep("Tropical Cereals", 2),
      rep("Temperate Roots", 2),
      rep("Tropical Roots", 3)
    ),
    stringsAsFactors = FALSE
  )
  
  # Trade mapping (using same structure but with trade-specific codes)
  trade_mapping <<- data.frame(
    FAO_Code = c(
      # Individual crops - trade codes (same as production except for groundnuts)
      "27",   # Rice, paddy
      "156",  # Sugar cane
      "56",   # Maize
      "243",  # Groundnuts, shelled
      "270",  # Rapeseed
      "236",  # Soybeans
      "267",  # Sunflower seed
      
      # Rest same as production
      "176", "187", "191",  # Pulses
      "15", "44", "71", "75",  # Temperate Cereals
      "83", "79",  # Tropical Cereals
      "116", "157",  # Temperate Roots
      "125", "122", "137"  # Tropical Roots
    ),
    WF_Crop = production_mapping$WF_Crop,
    stringsAsFactors = FALSE
  )
  
  log_msg("Base data loading completed")
}

# -----------------------------
# 16. Execute Analysis
# -----------------------------
tryCatch({
  results <- main()
  cat("\nAnalysis completed successfully. Check the outputs directory for results.\n")
  cat("\nSummary statistics have been saved to outputs/results/\n")
}, error = function(e) {
  cat("\nError in main execution:", conditionMessage(e), "\n")
  cat("Check the log files in outputs/logs for details\n")
})