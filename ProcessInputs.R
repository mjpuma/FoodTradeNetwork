## Process *structured* input datasets for Food Shock Cascade (FSC) Model-------------------
## Creates trade, production, and reserves matrices for use in cascade model

# Remove all objects from the workspace
rm(list = ls())

## Requires existing files in working directory: 
## Commodity List (with kcal conversions), 
## Production data from FAOSTAT, production quantity in tonnes 
## Trade data from FAOSTAT, detailed trail matrix, normalized, all data 
## Reserves data from USDA, downloadable dataset - psd grains pulses 

library(tidyr)
library(dplyr)
library(reshape2)
library(stringr)

# Command Line:
# rscript main/ProcessInputs.R "/Users/mjp38/GitHub/FSC-WorldModelers/" "Wheat_Avg20192020"
# args <- commandArgs(trailingOnly = TRUE)
# working_directory <- c(args[1])                 # Directory
# nameinput <- c(args[2])
# print(nameinput)

#RStudio
setwd('/Users/mjp38/GitHub/FSC-WorldModelers/')
working_directory <- getwd()
setwd(working_directory)


# Define the index of the commodity to select
num_commodity <- 12

commodities <- c('Banana_Avg20192021', 'Coffee_Avg20192021',
                 'Barley_Avg20192021', 'Sugarcane_Avg20192021',
                 'Maize_Avg20192021', 'Soybean_Avg20192021',
                 'RapeseedOil_Avg20192021', 'PalmOil_Avg20192021',
                 'Groundnuts_Avg20192021', 'Cocoa_Avg20192021', 
                 'Rice_Avg20192021', 'Cassava_Avg20192021')

# Use the index to select a commodity from the list
nameinput <- commodities[num_commodity]


# Set year range for production, trade, and reserves data ---------------------------------
yr_range <- 2019:2021

# Load ancillary data  ---------------------------------
# 1) Load country list valid for simulation years
country_list <- read.csv("ancillary/country_list195_2012to2016.csv")
# 2)  Commodity list for bilateral trade 
#~ commodities<-read.csv("ancillary/cropcommodity_tradelist.csv")
commodities<-read.csv(paste0("ancillary/", nameinput, "cropcommodity_tradelist.csv"))
# 3)  Commodity list for production
#~ commodities_prod<-read.csv("ancillary/cropcommodity_prodlist.csv")
commodities_prod<-read.csv(paste0("ancillary/", nameinput, "cropcommodity_prodlist.csv"))
# 4)  Commodity list for reserves
#~ commodities_reserves<-read.csv("ancillary/cropcommodity_reserveslist.csv")
commodities_reserves<-read.csv(paste0("ancillary/", nameinput, "cropcommodity_reserveslist.csv"))

# PART 1: Trade (Detailed trade matrix pre-downloaded trade from FAOSTAT)  --------------

# Step 1: Download detailed trade matrix from FAO  ====1
trade_dat <- read.csv("inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv", stringsAsFactors = FALSE)
# Killian
#trade_dat <- read.csv("inputs/trade_matrix_normalized.csv",
#                      stringsAsFactors = FALSE)

# Step 2: Filter using "dplyr" filter function: 1) by year and 2) by "cropcommodity_list"  ====
trade_dat <- select(trade_dat, reporter = Reporter.Country.Code, 
                    ReporterName = Reporter.Countries,
                    partner = Partner.Country.Code, 
                    PartnerName = Partner.Countries, 
                    cropid = Item.Code, 
                    element = Element,
                    year = Year, 
                    value = Value) %>%
  dplyr::filter(year %in% yr_range) %>%
  dplyr::filter(cropid %in% commodities$cropid)

# Step 3: Remove trade between a country and itself in trade_dat dataframe  ====
trade_dat <- dplyr::filter(trade_dat, reporter != partner)

# Step 4: Extract element "export quantity" from bilateral trade data  ====
exp_dat <- dplyr::filter(trade_dat, element == "Export Quantity") %>% select(-element)

# Step 5: Aggregate commodities after converting to kilocalories ====
#  caloric conversions are in dataframe "commoditites" 
exp_agg <- inner_join(exp_dat, commodities) %>%
  mutate(value_kcal = value * as.numeric(kcal.ton)) %>%  ##caloric conversion##
  group_by(reporter, partner, year) %>%
  summarise(ekcal = sum(value_kcal, na.rm = TRUE))       ##sum commodities##

#~ print(as.numeric(kcal.ton))

# Step 6: Convert export dataframe to a country x country x year matrix ====
#         Countries are ordered by FAOSTAT country code (increasing)
exp_mat <- acast(exp_agg, reporter ~ partner ~ year)
exp_mat <- exp_mat[match(country_list$FAOST_CODE, dimnames(exp_mat)[[1]]), , ]
exp_mat <- exp_mat[, match(country_list$FAOST_CODE, dimnames(exp_mat)[[2]]), ]
exp_mat[is.na(exp_mat)] <- 0
dimnames(exp_mat) <- list(country_list$FAOST_CODE, country_list$FAOST_CODE, yr_range)
E0 <- apply(exp_mat[, , 1:length(yr_range)], 1:2, mean) ###Remove before commit (assumes fixed length 1:5)
print(E0)

# Step 7: Save bilateral export matrix ordered by FAOSTAT country code ====
#         FAOSTAT country code are in increasing order
#~ save(E0, file="inputs_processed/E0.RData")
save(E0, file=paste0("inputs_processed/", nameinput, "E0.RData"))

# Export matrices with country names
Tkbyc<-as.data.frame(E0, row.names = country_list$Country, col.names = dimnames(E0)[[2]] )
#~ write.csv(Tkbyc, "inputs_processed/E0.csv")
write.csv(Tkbyc, paste0("inputs_processed/", nameinput, "E0.csv"))


## PART 2 : Get production data from FAOSTAT  ----
## QC: production-crops domain, 5510: production in tonnes

# Step 1: Download and import production data from FAOSTAT ==== 
prod_dat<-read.csv("inputs/Production_Crops_Livestock_E_All_Data_(Normalized).csv")
#prod_dat<-read.csv("inputs/production_normalized.csv")

# Step 2: Format imported data ====
#yr_range = 2015:2017
# Rename to match functions code
prod_dat$FAO<-prod_dat$Area.Code
prod_dat$itemCode<-prod_dat$Item.Code
prod_dat$FAOST_CODE<-prod_dat$Area.Code
prod_dat$name<-prod_dat$Item

# Keep only relevant columns
prod_dat <- prod_dat[, c("FAOST_CODE", "Year", "Value", "itemCode","Element", "name")]

# Step 3: Filter data by year and commodity ====
prod_dat<-prod_dat  %>%
  dplyr::filter(Year %in% yr_range) %>%
  dplyr::filter(itemCode %in% commodities_prod$cropid) %>%
  dplyr::filter(Element %in% commodities_prod$element)

# Check if 'prod_dat' has no rows after filtering
if(nrow(prod_dat) == 0) {
  # Create placeholders for Pkbyc, P0, and E0
  Pkbyc <- data.frame(iso3 = country_list$iso3, P0 = numeric(length(country_list$iso3)))
  P0 <- rep(0, length(country_list$iso3)) # Replace 0 with the appropriate placeholder if needed
  E0 <- rep(0, length(country_list$iso3)) # Replace 0 with the appropriate placeholder if needed
  
  # Specify file names based on 'nameinput'
  csv_filename <- paste0("inputs_processed/", nameinput, "Production.csv")
  rdata_filename_Pkbyc <- paste0("inputs_processed/", nameinput, "P0.Rdata")
  rdata_filename_P0E0 <- paste0("inputs_processed/", nameinput, "P0E0.RData")
  
  # Write out CSV and save Rdata files
  write.csv(Pkbyc, csv_filename, row.names = FALSE)
  save(Pkbyc, file = rdata_filename_Pkbyc)
  save(P0, E0, file = rdata_filename_P0E0)
} else {
  # Step 4: Convert units and sum across commodities ====
  #         Add kcal conversion factor,
  #         then sum calories by country and year across different cereals
  prod_agg <- inner_join(prod_dat, commodities, by = c("itemCode" = "cropid")) %>%
    mutate(value_kcal = Value * as.numeric(kcal.ton)) %>%
    #~   mutate(value_kcal = Value ) %>%
    #~   mutate(value_kcal =  as.numeric(kcal.ton)) %>%
    group_by(FAOST_CODE, Year) %>%
    summarise(pkcal = sum(value_kcal, na.rm = TRUE))
  #~ print(prod_agg)
  
  # Step 5: Reshape data into country x year matrix ----
  prod_mat <- spread(prod_agg, key = Year, value = pkcal, fill = 0)
  prod_mat <- prod_mat[match(country_list$FAOST_CODE, prod_mat$FAOST_CODE), ] #expand to include all countries
  prod_mat <- as.matrix(prod_mat[, -1]) #remove country code column
  prod_mat[is.na(prod_mat)] <- 0 #replace NAs with 0
  
  # Step 6: Average for time period and put in  dataframe ----
  print(prod_mat[, 1:length(yr_range)])
  P0 <- rowMeans(prod_mat[, 1:length(yr_range)])
  #~ print(length(yr_range))
  colnames(prod_mat) <- NULL
  Pkbyc <- prod_mat
  Pkbyc<-data_frame(country_list$iso3, P0=P0)
  colnames(Pkbyc)[1] <- "iso3"
  
  # Step 7: Save files ----
  write.csv(Pkbyc,paste0("inputs_processed/", nameinput, "Production.csv"), row.names=FALSE)
  save(Pkbyc, file= paste0("inputs_processed/", nameinput, "P0.Rdata"))
  save(P0, E0, file=paste0("inputs_processed/", nameinput, "P0E0.RData")) 
  
  # Clear unneeded files
  rm(prod_agg)
}

# Clear unneeded files
rm(prod_dat)


# PART 3: Stocks of cereals in countries x years matrix ----
# Special handling for commodities without reserve data like Bananas

# Define if the commodity has reserve data
has_reserves_data <- !nameinput %in% c('Banana_Avg20192021', 'Coffee_Avg20192021',
                                       'Sugarcane_Avg20192021','Soybean_Avg20192021',
                                       'RapeseedOil_Avg20192021', 'PalmOil_Avg20192021',
                                       'Groundnuts_Avg20192021', 'Cocoa_Avg20192021',
                                       'Cassava_Avg20192021', 
                                       'Avocados_Avg20192021','Wine_Avg20192021') 

if (has_reserves_data) {
  # Load and process reserves data for commodities with available data
  psd <- read.csv("inputs/psd_grains_pulses.csv")
  
  # The following steps process the psd data, similar to the previously outlined steps:
  # 1. Extract year-end stocks
  # 2. Filter by commodities
  # 3. Adjust names to match the PSD database
  # 4. Calculate Reserves in kcal
  # 5. Perform necessary name adjustments and aggregations
  # Step 1: Load data ----
  psd <- read.csv("inputs/psd_grains_pulses.csv")
  
  # Step 2: Extract year-end stocks for specified year range ----
  #         Units: 1000 metric tons
  psd_yrsubset <-
    select(
      psd,
      Country = Country_Name,
      Year = Market_Year,
      Product = Commodity_Description,
      Variable = Attribute_Description,
      Value
    ) %>%
    dplyr::filter(Year %in% yr_range) %>%
    spread(key = Variable, value = Value) %>%
    select(Country, Year, Product, R = 'Ending Stocks')
  
  # Step 3: Read table of commodities and kcal/tonnes conversion factors ----
  commodities_usda_names <- commodities_reserves
  
  # Step 4: Adjust names to match PSD database ----
  #         The second name is the one used in PSD.
  # The commodities included for 'Ending Stocks' are: Barley, Corn, Millet, Mixed Grain,
  # Oats, "Rice, Milled", Rye, Sorghum, Wheat
  subs_list <- c("Rice Milled" = "Rice, Milled",
                 "Maize" = "Corn",
                 "Mixed grain" = "Mixed Grain")
  commodities_usda_names$cropname <-
    str_replace_all(commodities_usda_names$cropname, subs_list)
  
  # Step 5: Filter by commodities ----
  psd_yrsubset <- psd_yrsubset  %>%
    dplyr::filter(Product %in% commodities_usda_names$cropname)
  
  # Step 6: Calculate Reserves in kcal, reshape to Country x Year ----
  Rkbyc <-
    inner_join(psd_yrsubset,
               commodities_usda_names,
               by = c("Product" = "cropname")) %>%
    select(Country, Year, Product, R, convf = kcal.ton) %>%
    mutate(R = R * 1000 * as.numeric(convf)) %>%
    group_by(Country, Year) %>%
    summarise(R = sum(R, na.rm = TRUE)) %>%
    spread(key = Year, value = R, fill = 0)
  
  # Step 7: Multiple fixes to ensure countries match between FAOSTAT and PSD data ----
  
  # Fix non-matching country names ====
  countries_to_match <-
    unique(Rkbyc$Country[!(Rkbyc$Country %in% country_list$Country)])
  countries_to_match
  country_repl <-
    c(
      "Bolivia" = "Bolivia (Plurinational State of)",
      "Brunei" =  "Brunei Darussalam",
      "Burkina" =  "Burkina Faso",
      "Burma" =   "Myanmar",
      "China" = "China (mainland)",
      "Former Czechoslovakia" = "Czechoslovakia",
      "Former Yugoslavia" = "Yugoslav SFR",
      "Gambia, The" = "Gambia",
      "Iran" = "Iran (Islamic Republic of)",
      "Korea, North" = "Democratic People's Republic of Korea",
      "Korea, South" =  "Republic of Korea",
      "Laos" = "Lao People's Democratic Republic",
      "Macedonia" = "The former Yugoslav Republic of Macedonia",
      "Moldova" = "Republic of Moldova",
      "Russia" =  "Russian Federation",
      "Syria" = "Syrian Arab Republic",
      "Tanzania" = "United Republic of Tanzania",
      "Union of Soviet Socialist Repu" = "USSR",
      "United States" = "United States of America",
      "Venezuela" =  "Venezuela (Bolivarian Republic of)",
      "Vietnam" = "Viet Nam"
    )
  Rkbyc$Country <- str_replace_all(Rkbyc$Country, country_repl)
  Rkbyc$Country <-
    str_replace(Rkbyc$Country, fixed("Congo (Brazzaville)"), "Congo") %>%
    str_replace(fixed("Congo (Kinshasa)"), "Democratic Republic of the Congo") %>%
    str_replace(fixed("Yemen (Aden)"), "Yemen Dem") %>%
    str_replace(fixed("Yemen (Sanaa)"), "Yemen Ar Rp")
  
  ## Add Taiwan and HK to China, South Sudan to Sudan, and two Yemens
  #Rkbyc["China", ] <- Rkbyc["China", ] + Rkbyc["Hong Kong", ] + Rkbyc["Taiwan", ]
  #Rkbyc <- Rkbyc[!(rownames(Rkbyc) %in% c("Hong Kong", "Taiwan", "South Sudan",
  #                                        "Yemen Ar Rp", "Yemen Dem")), ]
  
  cnames <- Rkbyc$Country
  Rkbyc <- as.matrix(Rkbyc[,-1, ])
  rownames(Rkbyc) <- cnames
  
  ## Apportion European Union reserves proportionally to production ====
  eu_list <- c(
    "Austria",
    "Belgium",
    "Denmark",
    "Finland",
    "France",
    "Germany",
    "Greece",
    "Ireland",
    "Italy",
    "Luxembourg",
    "Netherlands",
    "Portugal",
    "Spain",
    "Sweden",
    "United Kingdom",
    "Bulgaria",
    "Croatia",
    "Cyprus",
    "Czech Republic",
    "Estonia",
    "Hungary",
    "Latvia",
    "Lithuania",
    "Poland",
    "Romania",
    "Slovakia",
    "Slovenia"
  )
  eu_yrs <- which(as.numeric(colnames(Rkbyc)) >= 1998)
  
  
  # Label rows in production matrix from from country_list ====
  Pkbyc_v2 <- prod_mat
  rownames(Pkbyc_v2) <- country_list$Country
  prop_eu <- scale(Pkbyc_v2[eu_list, eu_yrs], center = FALSE,
                   scale = colSums(Pkbyc_v2[eu_list, eu_yrs]))
  
  # Add individual EU countries to matrix ====
  Rkbyc <-
    rbind(Rkbyc, matrix(
      0,
      nrow = length(eu_list),
      ncol = ncol(Rkbyc),
      dimnames = list(eu_list)
    ))
  Rkbyc[eu_list, eu_yrs] <-
    sweep(prop_eu, 2, Rkbyc["European Union", eu_yrs], "*")
  Rkbyc <-
    Rkbyc[-which(rownames(Rkbyc) %in% c("European Union")),] #delete EU row
  
  # Step 8: Create new matrix with all countries in Pkbyc (full country list) ----
  
  # Fill reserves by matching names, leave the rest at zero ====
  Rkbyc_all <- Rkbyc
  Rkbyc_all <-
    Rkbyc_all[match(country_list$Country, dimnames(Rkbyc_all)[[1]]), ]
  Rkbyc_all[is.na(Rkbyc_all)] <- 0
  
  rownames(Rkbyc_all) <- country_list$Country
  dimnames(Rkbyc_all) <- NULL
  #~ saveRDS(Rkbyc_all, file = "inputs_processed/cereals_stocks.RData")
  saveRDS(Rkbyc_all,
          file = paste0("inputs_processed/", nameinput, "cereals_stocks.RData"))
  
  # Take average years and save as R0 ====
  dimnames(Rkbyc_all) <- list(country_list$iso3, yr_range)
  R0 <- rowMeans(Rkbyc_all[, eu_yrs])
  R0 <- R0[!is.na(names(R0))]
  
} else {
  # For commodities without reserve data, create a zero-filled matrix
  Rkbyc_all <- matrix(0, nrow = length(country_list$Country), ncol = length(yr_range))
  rownames(Rkbyc_all) <- country_list$Country
  colnames(Rkbyc_all) <- as.character(yr_range)  # Set column names to years
}

# Proceed with calculating the average reserves and preparing the data for CSV export
# Ensure dimension names are set for Rkbyc_all to match country_list$iso3 and yr_range
dimnames(Rkbyc_all) <- list(country_list$iso3, as.character(yr_range))

# Calculate the average reserves
R0 <- rowMeans(Rkbyc_all, na.rm = TRUE)

# Create a dataframe for saving
Rkbyc <- data.frame(iso3 = country_list$iso3, R0 = R0)

# Save the results
saveRDS(Rkbyc_all, file = paste0("inputs_processed/", nameinput, "cereals_stocks.RData"))
save(R0, file = paste0("inputs_processed/", nameinput, "R0.RData"))
write.csv(Rkbyc, paste0("inputs_processed/", nameinput, "Reserves.csv"), row.names = FALSE)

# Optionally, clear unneeded objects from the environment
# rm(list=ls())







# # PART 3: Stocks of cereals in countries x years matrix  ----
# #         USDA Foreign Agricultural Service 
# #         Production, Supply, and Distribution (PSD) data
# #         Download the file USDA-PSD data pulses grains
# #yr_range = 2017:2019 # NOTE: length must match number of years for production
# 
# # Add a flag to identify if the commodity has reserve data
# has_reserves_data <- !nameinput %in% c('Banana_Avg20192021') #, 'OtherFruitOrVeg')
# 
# if (has_reserves_data) {
#   # Step 1: Load data ----
#   psd <- read.csv("inputs/psd_grains_pulses.csv")
#   
#   # Step 2: Extract year-end stocks for specified year range ---- 
#   #         Units: 1000 metric tons
#   psd_yrsubset <- select(psd, Country = Country_Name, Year = Market_Year,
#                          Product = Commodity_Description,
#                          Variable = Attribute_Description, Value) %>%
#     dplyr::filter(Year %in% yr_range) %>%
#     spread(key = Variable, value = Value) %>%
#     select(Country, Year, Product, R = 'Ending Stocks')
#   
#   # Step 3: Read table of commodities and kcal/tonnes conversion factors ----
#   commodities_usda_names <- commodities_reserves
#   
#   # Step 4: Adjust names to match PSD database ----
#   #         The second name is the one used in PSD.
#   # The commodities included for 'Ending Stocks' are: Barley, Corn, Millet, Mixed Grain,
#   # Oats, "Rice, Milled", Rye, Sorghum, Wheat
#   subs_list <- c("Rice Milled" = "Rice, Milled", 
#                  "Maize" = "Corn", 
#                  "Mixed grain" = "Mixed Grain")
#   commodities_usda_names$cropname <- str_replace_all(commodities_usda_names$cropname, subs_list)
#   
#   # Step 5: Filter by commodities ----
#   psd_yrsubset<-psd_yrsubset  %>%
#     dplyr::filter(Product %in% commodities_usda_names$cropname)
#   
#   # Step 6: Calculate Reserves in kcal, reshape to Country x Year ----
#   Rkbyc <- inner_join(psd_yrsubset, commodities_usda_names, by = c("Product" = "cropname")) %>%
#     select(Country, Year, Product, R, convf = kcal.ton) %>%
#     mutate(R = R * 1000 * as.numeric(convf)) %>%
#     group_by(Country, Year) %>%
#     summarise(R = sum(R, na.rm = TRUE)) %>%
#     spread(key = Year, value = R, fill = 0)
#   
#   # Step 7: Multiple fixes to ensure countries match between FAOSTAT and PSD data ----
#   
#   # Fix non-matching country names ====
#   countries_to_match <- unique(Rkbyc$Country[!(Rkbyc$Country %in% country_list$Country)])
#   countries_to_match
#   country_repl <- c("Bolivia" = "Bolivia (Plurinational State of)", 
#                     "Brunei" =  "Brunei Darussalam", 
#                     "Burkina" =  "Burkina Faso",
#                     "Burma" =   "Myanmar",
#                     "China" = "China (mainland)",
#                     "Former Czechoslovakia" = "Czechoslovakia", 
#                     "Former Yugoslavia" = "Yugoslav SFR", 
#                     "Gambia, The" = "Gambia",
#                     "Iran" = "Iran (Islamic Republic of)", 
#                     "Korea, North" = "Democratic People's Republic of Korea", 
#                     "Korea, South" =  "Republic of Korea", 
#                     "Laos" = "Lao People's Democratic Republic",
#                     "Macedonia" = "The former Yugoslav Republic of Macedonia",
#                     "Moldova" = "Republic of Moldova", 
#                     "Russia" =  "Russian Federation", 
#                     "Syria" = "Syrian Arab Republic",
#                     "Tanzania" = "United Republic of Tanzania", 
#                     "Union of Soviet Socialist Repu" = "USSR", 
#                     "United States" = "United States of America", 
#                     "Venezuela" =  "Venezuela (Bolivarian Republic of)", 
#                     "Vietnam" = "Viet Nam")
#   Rkbyc$Country <- str_replace_all(Rkbyc$Country, country_repl)
#   Rkbyc$Country <- str_replace(Rkbyc$Country, fixed("Congo (Brazzaville)"), "Congo") %>%
#     str_replace(fixed("Congo (Kinshasa)"), "Democratic Republic of the Congo") %>%
#     str_replace(fixed("Yemen (Aden)"), "Yemen Dem") %>%
#     str_replace(fixed("Yemen (Sanaa)"), "Yemen Ar Rp")
#   
#   ## Add Taiwan and HK to China, South Sudan to Sudan, and two Yemens
#   #Rkbyc["China", ] <- Rkbyc["China", ] + Rkbyc["Hong Kong", ] + Rkbyc["Taiwan", ]
#   #Rkbyc <- Rkbyc[!(rownames(Rkbyc) %in% c("Hong Kong", "Taiwan", "South Sudan", 
#   #                                        "Yemen Ar Rp", "Yemen Dem")), ]
#   
#   cnames <- Rkbyc$Country
#   Rkbyc <- as.matrix(Rkbyc[, -1,])
#   rownames(Rkbyc) <- cnames
#   
#   ## Apportion European Union reserves proportionally to production ====
#   eu_list <- c("Austria", 
#                "Belgium", 
#                "Denmark", 
#                "Finland", 
#                "France", 
#                "Germany",
#                "Greece", 
#                "Ireland", 
#                "Italy", 
#                "Luxembourg", 
#                "Netherlands",
#                "Portugal", 
#                "Spain", 
#                "Sweden", 
#                "United Kingdom",
#                "Bulgaria", 
#                "Croatia", 
#                "Cyprus", 
#                "Czech Republic",
#                "Estonia", 
#                "Hungary", 
#                "Latvia", 
#                "Lithuania", 
#                "Poland",
#                "Romania", 
#                "Slovakia", 
#                "Slovenia")
#   eu_yrs <- which(as.numeric(colnames(Rkbyc)) >= 1998)
#   
#   
#   # Label rows in production matrix from from country_list ====
#   Pkbyc_v2<-prod_mat
#   rownames(Pkbyc_v2) <- country_list$Country
#   prop_eu <- scale(Pkbyc_v2[eu_list, eu_yrs], center = FALSE,
#                    scale = colSums(Pkbyc_v2[eu_list, eu_yrs]))
#   
#   # Add individual EU countries to matrix ====
#   Rkbyc <- rbind(Rkbyc, matrix(0, nrow = length(eu_list), ncol = ncol(Rkbyc), 
#                                dimnames = list(eu_list)))
#   Rkbyc[eu_list, eu_yrs] <- sweep(prop_eu, 2, Rkbyc["European Union", eu_yrs], "*") 
#   Rkbyc <- Rkbyc[-which(rownames(Rkbyc) %in% c("European Union")), ] #delete EU row
#   
#   # Step 8: Create new matrix with all countries in Pkbyc (full country list) ----
#   
#   # Fill reserves by matching names, leave the rest at zero ====
#   Rkbyc_all<-Rkbyc
#   Rkbyc_all <- Rkbyc_all[match(country_list$Country, dimnames(Rkbyc_all)[[1]]),]
#   Rkbyc_all[is.na(Rkbyc_all)] <- 0
#   
#   rownames(Rkbyc_all)<-country_list$Country
#   dimnames(Rkbyc_all)<-NULL
#   #~ saveRDS(Rkbyc_all, file = "inputs_processed/cereals_stocks.RData")
#   saveRDS(Rkbyc_all, file = paste0("inputs_processed/", nameinput, "cereals_stocks.RData"))
#   
#   # Take average years and save as R0 ====
#   dimnames(Rkbyc_all) <- list(country_list$iso3, yr_range)
#   R0 <- rowMeans(Rkbyc_all[, eu_yrs])
#   R0<-R0[!is.na(names(R0))]
#   #~ save(R0, file = "inputs_processed/R0.RData")
#   save(R0, file = paste0("inputs_processed/", nameinput, "R0.RData"))
#   
#   Rkbyc<-data_frame(iso3=names(R0), R0=R0)
#   #~ write.csv(Rkbyc,"inputs_processed/Reserves.csv", row.names=FALSE)
#   write.csv(Rkbyc,paste0("inputs_processed/", nameinput, "Reserves.csv"), row.names=FALSE)
# } else {
#   # Create a dataframe of zeros if there's no reserve data for the commodity
#   R0 <- rep(0, length(country_list$iso3))
#   Rkbyc <- data.frame(iso3 = country_list$iso3, R0 = R0)
# }
# 
# # Save the R0 dataframe in the same format, even if it's all zeros
# save(R0, file = paste0("inputs_processed/", nameinput, "R0.RData"))
# write.csv(Rkbyc, paste0("inputs_processed/", nameinput, "Reserves.csv"), row.names = FALSE)
# 
# ### clear environment
# #rm(list=ls()) 
