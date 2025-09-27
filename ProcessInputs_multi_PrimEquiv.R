process_trade_data <- function(commodity_name, commodity_codes, years, extraction_rates_file, country_list_file, trade_data_file) {
  # Load Required Libraries
  library(tidyr)
  library(reshape2)
  library(dplyr)
  library(stringr)
  library(readr)
  library(fs)
  library(readxl)
  
  # Load the comprehensive country list
  country_list <- read.csv(country_list_file)
  country_list$FAOST_CODE <- as.character(country_list$FAOST_CODE)
  
  # Create lookup tables
  faostat_to_iso3 <- setNames(country_list$iso3, country_list$FAOST_CODE)
  iso3_to_name <- setNames(country_list$Country, country_list$iso3)
  
  # Load trade data
  trade_data_all_years <- read.csv(trade_data_file)
  
  # Read the extraction rate data
  extraction_rates <- read_excel(extraction_rates_file, sheet = "withData") %>%
    rename(
      FAO_Code = `FAO Code`,
      Extraction_Rate = `Primary Product Extraction Rate`
    ) %>%
    mutate(FAO_Code = as.numeric(FAO_Code)) %>%
    select(FAO_Code, Extraction_Rate)
  
  # Filter extraction rates for the commodity and its products
  extraction_rates_commodity <- extraction_rates %>%
    filter(FAO_Code %in% commodity_codes)
  extraction_rates_commodity$FAO_Code <- as.character(extraction_rates_commodity$FAO_Code)
  
  # Load commodity list
  commodities <- data.frame(cropid = commodity_codes)
  commodities$cropid <- as.character(commodities$cropid)
  
  # Define the year label
  year_label <- paste(years, collapse = "_")
  
  # Filter trade data for the specified years and commodity codes
  trade_dat <- trade_data_all_years %>%
    select(
      reporter = Reporter.Country.Code,
      ReporterName = Reporter.Countries,
      partner = Partner.Country.Code,
      PartnerName = Partner.Countries,
      cropid = Item.Code,
      element = Element,
      year = Year,
      value = Value
    ) %>%
    filter(year %in% years) %>%
    filter(cropid %in% commodity_codes)
  
  # Remove trade between a country and itself
  trade_dat <- filter(trade_dat, reporter != partner)
  
  # Convert country codes and 'cropid' to character for consistency
  trade_dat$reporter <- as.character(trade_dat$reporter)
  trade_dat$partner <- as.character(trade_dat$partner)
  trade_dat$cropid <- as.character(trade_dat$cropid)
  
  # Filter trade_dat to include only countries present in country_list$FAOST_CODE
  trade_dat <- trade_dat %>%
    filter(reporter %in% country_list$FAOST_CODE & partner %in% country_list$FAOST_CODE)
  
  # Extract "Export Quantity" element
  exp_dat <- filter(trade_dat, element == "Export Quantity") %>% select(-element)
  
  # Ensure 'value' is numeric
  exp_dat$value <- as.numeric(as.character(exp_dat$value))
  
  # Merge with commodities to ensure consistent commodity information
  exp_dat <- exp_dat %>%
    inner_join(commodities, by = "cropid")
  
  # Merge with extraction rates
  exp_dat <- exp_dat %>%
    left_join(extraction_rates_commodity, by = c("cropid" = "FAO_Code"))
  
  # Handle missing extraction rates
  exp_dat$Extraction_Rate[is.na(exp_dat$Extraction_Rate) | exp_dat$Extraction_Rate == 0] <- 1.0
  
  # Calculate primary crop equivalents
  exp_dat <- exp_dat %>%
    mutate(value_primary = value / Extraction_Rate)
  
  # Aggregate commodities after converting to primary crop equivalents
  exp_agg <- exp_dat %>%
    group_by(reporter, partner, year) %>%
    summarise(
      total_primary_equiv = sum(value_primary, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create the matrix including the year dimension
  exp_mat_avg <- acast(exp_agg, reporter ~ partner ~ year, value.var = "total_primary_equiv")
  
  # Replace NA with 0
  exp_mat_avg[is.na(exp_mat_avg)] <- 0
  
  # Ensure dimensions match
  exp_mat_avg <- exp_mat_avg[match(country_list$FAOST_CODE, dimnames(exp_mat_avg)[[1]]), , ]
  exp_mat_avg <- exp_mat_avg[, match(country_list$FAOST_CODE, dimnames(exp_mat_avg)[[2]]), ]
  
  # Assign dimension names
  dimnames(exp_mat_avg) <- list(country_list$FAOST_CODE, country_list$FAOST_CODE, years)
  
  # Average over years
  E0_avg <- apply(exp_mat_avg, c(1, 2), mean)
  
  # Replace NA with 0 in the final matrix
  E0_avg[is.na(E0_avg)] <- 0
  
  # Map FAOST_CODE to ISO3 codes for dimension names
  dimnames(E0_avg) <- list(faostat_to_iso3[country_list$FAOST_CODE], faostat_to_iso3[country_list$FAOST_CODE])
  
  # Save the average flow matrix
  dir.create("inputs_processed", showWarnings = FALSE)
  save(E0_avg, file = paste0("inputs_processed/", commodity_name, "_Avg_", year_label, "_PrimaryEquivE0.RData"))
  
  # Convert matrix to dataframe and save as CSV
  Tkbyc_avg <- as.data.frame(E0_avg, row.names = faostat_to_iso3[country_list$FAOST_CODE])
  colnames(Tkbyc_avg) <- faostat_to_iso3[country_list$FAOST_CODE]
  
  write.csv(Tkbyc_avg, paste0("inputs_processed/", commodity_name, "_Avg_", year_label, "_PrimaryEquivE0.csv"))
  
  # Return the processed data
  return(list(E0_avg = E0_avg, exp_dat = exp_dat, exp_agg = exp_agg))
}


# Define commodities and their FAO codes
commodities_info <- list(
#  Banana = c(486, 487),
#  Coffee = c(656, 657),
#  Barley = c(44),
#  Sugarcane = c(156),
  Wheat = c(15,16)
#  Soybean = c(236),
#  RapeseedOil = c(257, 258),
#  PalmOil = c(254, 255),
#  Groundnuts = c(242, 243),
#  Cocoa = c(661, 662),
#  Rice = c(27, 28),
#  Cassava = c(125)
)


# Define the range of year groups to process
year_groups <- list(
  # c(2000, 2001),
  # c(2002, 2003),
  # c(2004, 2005),
  # c(2006, 2007),
  # c(2008, 2009),
  # c(2010, 2011),
  # c(2012, 2013),
  # c(2014, 2015),
  # c(2016, 2017),
  # c(2018, 2019),
  c(2020, 2021)
)

# Set the working directory
setwd('/Users/mjp38/GitHub/FoodTradeNetwork/')  # Adjust this path as needed

# Set file paths
extraction_rates_file <- "ancillary/FoodCommodity_ForCarole_v5.xlsx"
country_list_file <- "ancillary/country_list195_2012to2016.csv"
trade_data_file <- "inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"

# Loop over commodities
for (commodity_name in names(commodities_info)) {
  commodity_codes <- commodities_info[[commodity_name]]
  
  # Loop over year groups
  for (years in year_groups) {
    # Call the processing function
    result <- process_trade_data(
      commodity_name,
      commodity_codes,
      years,
      extraction_rates_file,
      country_list_file,
      trade_data_file
    )
    
    # You can access the results if needed
    E0_avg <- result$E0_avg
    exp_dat <- result$exp_dat
    exp_agg <- result$exp_agg
    
    # Optional: Add code to perform additional analysis or save results
  }
}
