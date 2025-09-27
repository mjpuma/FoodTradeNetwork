# Load Required Libraries
library(tidyr)
library(reshape2)
library(dplyr)
library(stringr)
library(readr)
library(fs)
library(readxl)

# Set the working directory
setwd('/Users/mjp38/GitHub/FoodTradeNetwork/')  # Adjust this path as needed

# Load the comprehensive country list
country_list <- read.csv("ancillary/country_list195_2012to2016.csv")
country_list$FAOST_CODE <- as.character(country_list$FAOST_CODE)

# Create lookup tables
faostat_to_iso3 <- setNames(country_list$iso3, country_list$FAOST_CODE)
iso3_to_name <- setNames(country_list$Country, country_list$iso3)

# Load trade data
trade_data_all_years <- read.csv("inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv")

# Read the extraction rate data
extraction_rates <- read_excel("ancillary/FoodCommodity_ForCarole_v5.xlsx", sheet = "withData") %>%
  rename(
    FAO_Code = `FAO Code`,
    Extraction_Rate = `Primary Product Extraction Rate`
  ) %>%
  mutate(FAO_Code = as.numeric(FAO_Code)) %>%
  select(FAO_Code, Extraction_Rate)

# Filter extraction rates for Maize and its products
maize_codes <- c(56, 57, 58, 59, 60, 61, 63, 64, 66, 68, 636)  # FAO Codes for Maize and related products
extraction_rates_maize <- extraction_rates %>%
  filter(FAO_Code %in% maize_codes)

# Load Maize commodity list
commodities <- data.frame(cropid = maize_codes)
commodities$cropid <- as.character(commodities$cropid)

# Define the year group and commodity
years <- c(2000, 2001)
year_label <- paste(years, collapse = "_")
baseCommodityName <- "Maize"

# Filter trade data for the specified years and Maize commodities
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
  filter(cropid %in% maize_codes)

# Remove trade between a country and itself
trade_dat <- filter(trade_dat, reporter != partner)

# Convert country codes and 'cropid' to character for consistency
trade_dat$reporter <- as.character(trade_dat$reporter)
trade_dat$partner <- as.character(trade_dat$partner)
trade_dat$cropid <- as.character(trade_dat$cropid)

# **Filter trade_dat to include only countries present in country_list$FAOST_CODE**
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
extraction_rates_maize$FAO_Code <- as.character(extraction_rates_maize$FAO_Code)
exp_dat <- exp_dat %>%
  left_join(extraction_rates_maize, by = c("cropid" = "FAO_Code"))

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

# Convert country codes to character
exp_agg$reporter <- as.character(exp_agg$reporter)
exp_agg$partner <- as.character(exp_agg$partner)

# Create the matrix including the year dimension
exp_mat_avg <- acast(exp_agg, reporter ~ partner ~ year, value.var = "total_primary_equiv")

# Replace NA with 0
exp_mat_avg[is.na(exp_mat_avg)] <- 0

# Ensure dimensions match
# Match exporters
exp_mat_avg <- exp_mat_avg[match(country_list$FAOST_CODE, dimnames(exp_mat_avg)[[1]]), , ]
# Match importers
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
save(E0_avg, file = paste0("inputs_processed/", baseCommodityName, "_Avg_", year_label, "_PrimaryEquivE0.RData"))

# Convert matrix to dataframe and save as CSV
Tkbyc_avg <- as.data.frame(E0_avg, row.names = faostat_to_iso3[country_list$FAOST_CODE])
colnames(Tkbyc_avg) <- faostat_to_iso3[country_list$FAOST_CODE]

write.csv(Tkbyc_avg, paste0("inputs_processed/", baseCommodityName, "_Avg_", year_label, "_PrimaryEquivE0.csv"))



# Perform Checks and Print Outputs

# 1. Print dimensions of E0_avg
print("Dimensions of E0_avg:")
print(dim(E0_avg))  # Should be (number of countries) x (number of countries)

# 2. Print the first 5 rows and columns of E0_avg
print("First 5 rows and columns of E0_avg:")
print(E0_avg[1:5, 1:5])

# 3. Define countries of interest (adjust as needed)
exporters <- c("USA", "CHN", "IND", "BRA", "ARG")  # Exporting countries (ISO3 codes)
importers <- c("MEX", "JPN", "NLD", "EGY", "ESP")  # Importing countries (ISO3 codes)

# Extract the submatrix for these countries
submatrix <- E0_avg[exporters, importers]

# Print the submatrix
print("Submatrix of E0_avg for selected countries:")
print(submatrix)

# 4. Print the first few rows of exp_dat
print("First few rows of exp_dat:")
print(head(exp_dat))

# 5. Print the first few rows of exp_agg
print("First few rows of exp_agg:")
print(head(exp_agg))

# 6. Create inverse mapping from ISO3 to FAOST_CODE
iso3_to_faostat <- setNames(country_list$FAOST_CODE, country_list$iso3)

# Get FAOST_CODE for USA, Mexico, and Japan
usa_code <- iso3_to_faostat["USA"]
mex_code <- iso3_to_faostat["MEX"]
jpn_code <- iso3_to_faostat["JPN"]

# Filter exp_agg for USA exports to Mexico and Japan
usa_exports <- exp_agg %>%
  filter(reporter == usa_code & partner %in% c(mex_code, jpn_code))

# Print the filtered data
print("USA exports to Mexico and Japan in exp_agg:")
print(usa_exports)

# 7. Check the trade_dat dataset

# a. Print dimensions of trade_dat
print("Dimensions of trade_dat:")
print(dim(trade_dat))

# b. Print the first few rows of trade_dat
print("First few rows of trade_dat:")
print(head(trade_dat))

# c. Check data types of columns
print("Structure of trade_dat:")
str(trade_dat)

# d. Verify filtering steps

# Years present in trade_dat
print("Years present in trade_dat:")
print(unique(trade_dat$year))

# Commodity codes present in trade_dat
print("Commodity codes present in trade_dat:")
print(unique(trade_dat$cropid))

# Any records where reporter equals partner
print("Any records where reporter equals partner:")
print(any(trade_dat$reporter == trade_dat$partner))

# e. Inspect the 'value' column
print("Summary of 'value' column in trade_dat:")
print(summary(trade_dat$value))

# f. Cross-check with exp_dat

# Sample record from trade_dat (USA exporting maize to Mexico in 2000)
trade_dat_sample <- trade_dat %>%
  filter(reporter == usa_code, partner == mex_code, cropid == "56", year == 2000)

print("Sample record from trade_dat:")
print(trade_dat_sample)

# Corresponding record in exp_dat
exp_dat_sample <- exp_dat %>%
  filter(reporter == usa_code, partner == mex_code, cropid == "56", year == 2000)

print("Corresponding record in exp_dat:")
print(exp_dat_sample)

# g. Check for missing values in 'value' column
print("Number of missing values in 'value' column:")
print(sum(is.na(trade_dat$value)))

# h. Ensure consistency of country codes
# Verify that there are no unmatched country codes
# Unmatched reporter codes
unmatched_reporter_codes <- setdiff(unique(trade_dat$reporter), country_list$FAOST_CODE)
print("Unmatched reporter codes:")
print(unmatched_reporter_codes)  # Should be empty

# Unmatched partner codes
unmatched_partner_codes <- setdiff(unique(trade_dat$partner), country_list$FAOST_CODE)
print("Unmatched partner codes:")
print(unmatched_partner_codes)  # Should be empty
