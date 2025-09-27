# Load Required Libraries
library(tidyr)
library(reshape2)
library(dplyr)
library(stringr)
library(readr)
library(fs)

# Set the working directory and necessary data paths
setwd('/Users/mjp38/GitHub/FoodTradeNetwork/')

# Load the comprehensive country list
country_list <- read.csv("ancillary/country_list195_2012to2016.csv")

# Create lookup tables
faostat_to_iso3 <- setNames(country_list$iso3, as.character(country_list$FAOST_CODE))
iso3_to_name <- setNames(country_list$Country, country_list$iso3)

# Improved function to replace codes with ISO3
replace_with_iso3 <- function(x) {
  x_char <- as.character(x)
  case_when(
    x_char %in% names(faostat_to_iso3) ~ faostat_to_iso3[x_char],
    x_char %in% country_list$iso3 ~ x_char,
    TRUE ~ NA_character_
  )
}

# Define commodities
commodities_list <- c('Banana', 'Coffee', 'Barley', 'Sugarcane', 'Wheat', 'Soybean',
                      'RapeseedOil', 'PalmOil', 'Groundnuts', 'Cocoa', 'Rice', 'Cassava')

# Define the range of year groups to process
year_groups <- list(c(2000, 2001), c(2002, 2003), c(2004, 2005),
                    c(2006, 2007), c(2008, 2009), c(2010, 2011),
                    c(2012, 2013), c(2014, 2015), c(2016, 2017),
                    c(2018, 2019), c(2020, 2021))

# Load trade data
trade_data_all_years <- read.csv("inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv")

# Function to process data for a single commodity
process_commodity <- function(baseCommodityName) {
  # Load static commodity lists using baseCommodityName
  commodities <- read.csv(paste0("ancillary/", baseCommodityName, "_cropcommodity_tradelist.csv"))
  
  # Initialize data frames to store results
  all_avg_data <- tibble()
  latest_data <- tibble()
  
  # Loop over each group of years
  for (years in year_groups) {
    year_label <- paste(years, collapse = "_")
    nameinput_avg <- paste(baseCommodityName, "Avg", year_label, sep = "_")
    nameinput_sum <- paste(baseCommodityName, "Sum", year_label, sep = "_")
    
    # Filter trade data for the specified years and commodities
    trade_dat <- trade_data_all_years %>%
      select(reporter = Reporter.Country.Code,
             ReporterName = Reporter.Countries,
             partner = Partner.Country.Code,
             PartnerName = Partner.Countries,
             cropid = Item.Code,
             element = Element,
             year = Year,
             value = Value) %>%
      dplyr::filter(year %in% years) %>%
      dplyr::filter(cropid %in% commodities$cropid)
    
    # Remove trade between a country and itself
    trade_dat <- dplyr::filter(trade_dat, reporter != partner)
    
    # Extract "export quantity" element
    exp_dat <- dplyr::filter(trade_dat, element == "Export Quantity") %>% select(-element)
    
    # Aggregate commodities after converting to kilocalories
    exp_agg <- inner_join(exp_dat, commodities) %>%
      mutate(value_kcal = value * as.numeric(kcal.ton)) %>%
      group_by(reporter, partner, year) %>%
      summarise(ekcal = sum(value_kcal, na.rm = TRUE), .groups = "drop")
    
    # Append to all_avg_data for the overall average calculation
    all_avg_data <- bind_rows(all_avg_data, exp_agg)
    
    # Calculate average flow matrix for the period
    exp_mat_avg <- acast(exp_agg, reporter ~ partner ~ year)
    exp_mat_avg <- exp_mat_avg[match(country_list$FAOST_CODE, dimnames(exp_mat_avg)[[1]]), , ]
    exp_mat_avg <- exp_mat_avg[, match(country_list$FAOST_CODE, dimnames(exp_mat_avg)[[2]]), ]
    exp_mat_avg[is.na(exp_mat_avg)] <- 0
    dimnames(exp_mat_avg) <- list(country_list$FAOST_CODE, country_list$FAOST_CODE, years)
    E0_avg <- apply(exp_mat_avg[, , 1:length(years)], 1:2, mean)
    
    # Save the average flow matrix
    save(E0_avg, file=paste0("inputs_processed/", nameinput_avg, "E0.RData"))
    
    # Convert E0_avg matrix to a dataframe with ISO3 codes for row names
    Tkbyc_avg <- as.data.frame(E0_avg, row.names = country_list$iso3)
    
    # Map FAOST_CODE column names to ISO3 codes
    colnames(Tkbyc_avg) <- faostat_to_iso3[colnames(Tkbyc_avg)]
    write.csv(Tkbyc_avg, paste0("inputs_processed/", nameinput_avg, "E0.csv"))
    
    # Calculate total flow matrix for the period
    exp_agg_total <- exp_agg %>%
      group_by(reporter, partner) %>%
      summarise(ekcal_total = sum(ekcal, na.rm = TRUE), .groups = "drop")
    
    exp_mat_sum <- acast(exp_agg_total, reporter ~ partner)
    exp_mat_sum <- exp_mat_sum[match(country_list$FAOST_CODE, dimnames(exp_mat_sum)[[1]]), ]
    exp_mat_sum <- exp_mat_sum[, match(country_list$FAOST_CODE, dimnames(exp_mat_sum)[[2]])]
    exp_mat_sum[is.na(exp_mat_sum)] <- 0
    dimnames(exp_mat_sum) <- list(country_list$FAOST_CODE, country_list$FAOST_CODE)
    
    # Save the total flow matrix
    save(exp_mat_sum, file=paste0("inputs_processed/", nameinput_sum, "E0.RData"))
    
    # Convert exp_mat_sum matrix to a dataframe with ISO3 codes for row names
    Tkbyc_sum <- as.data.frame(exp_mat_sum, row.names = country_list$iso3)
    
    # Map FAOST_CODE column names to ISO3 codes
    colnames(Tkbyc_sum) <- faostat_to_iso3[colnames(Tkbyc_sum)]
    write.csv(Tkbyc_sum, paste0("inputs_processed/", nameinput_sum, "E0.csv"))
    
    # If processing the latest period, store data for percentage calculations
    if (year_label == "2020_2021") {
      latest_data <- bind_rows(latest_data, exp_agg)
    }
  }
  
  # Calculate average import volumes from 2000-2021
  all_avg_data <- all_avg_data %>%
    group_by(reporter, partner) %>%
    summarise(ekcal_avg = mean(ekcal, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      reporter = replace_with_iso3(reporter),
      partner = replace_with_iso3(partner)
    )
  
  # Calculate import percentages for the entire period
  all_avg_data <- all_avg_data %>%
    group_by(reporter) %>%
    mutate(total_imports = sum(ekcal_avg, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(percentage = ekcal_avg / total_imports * 100) %>%
    select(reporter, partner, percentage) %>%
    filter(!is.na(reporter) & !is.na(partner))
  
  # Calculate average import volumes for 2020-2021
  latest_data <- latest_data %>%
    group_by(reporter, partner) %>%
    summarise(ekcal_avg = mean(ekcal, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      reporter = replace_with_iso3(reporter),
      partner = replace_with_iso3(partner)
    )
  
  # Calculate import percentages for 2020-2021
  latest_data <- latest_data %>%
    group_by(reporter) %>%
    mutate(total_imports = sum(ekcal_avg, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(percentage = ekcal_avg / total_imports * 100) %>%
    select(reporter, partner, percentage) %>%
    filter(!is.na(reporter) & !is.na(partner))
  
  # Save the results
  write.csv(all_avg_data, paste0("inputs_processed/Average_Import_Percentages_2000_2021_", baseCommodityName, ".csv"), row.names = FALSE)
  write.csv(latest_data, paste0("inputs_processed/Latest_Import_Percentages_2020_2021_", baseCommodityName, ".csv"), row.names = FALSE)
  
  print(paste("Processed and saved trade data and import percentage data for", baseCommodityName))
  
  return(list(all_avg_data = all_avg_data, latest_data = latest_data))
}

# Process all commodities with error handling
tryCatch({
  all_commodities_data <- lapply(commodities_list, process_commodity)
}, error = function(e) {
  print(paste("An error occurred:", e$message))
  print("Traceback:")
  print(rlang::last_trace())
})

# Function to analyze data for a single commodity
analyze_commodity <- function(commodity_data, commodity_name) {
  # Analyze the data
  top_partners <- commodity_data$latest_data %>%
    group_by(reporter) %>%
    top_n(5, percentage) %>%
    arrange(reporter, desc(percentage))
  
  print(paste("Top 5 trading partners for each reporter -", commodity_name))
  print(top_partners)
  
  return(top_partners)
}

# Analyze all commodities
analyzed_commodities_data <- mapply(analyze_commodity, 
                                    all_commodities_data, 
                                    commodities_list, 
                                    SIMPLIFY = FALSE)

print("Completed processing and analysis for all commodities.")

# Check for any remaining unmatched codes
check_unmatched_codes <- function(data) {
  unmatched_reporters <- data %>%
    filter(!reporter %in% country_list$iso3) %>%
    distinct(reporter)
  
  unmatched_partners <- data %>%
    filter(!partner %in% country_list$iso3) %>%
    distinct(partner)
  
  list(reporters = unmatched_reporters, partners = unmatched_partners)
}

unmatched_codes <- lapply(all_commodities_data, function(x) check_unmatched_codes(x$latest_data))

print("Unmatched codes:")
print(unmatched_codes)