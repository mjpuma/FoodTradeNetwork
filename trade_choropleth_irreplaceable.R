# Load Required Libraries
library(igraph)
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# Load and Prepare Data
load_trade_data <- function(file_path) {
  load(file_path)
  graph_from_adjacency_matrix(E0, mode = "directed", weighted = TRUE)
}

# Function to calculate consistency score for each commodity
calculate_consistency_score <- function(trade_data, year_range) {
  trade_summary <- trade_data %>%
    group_by(to, Commodity) %>%
    summarise(total_trade = sum(weight, na.rm = TRUE), num_years = n_distinct(Year)) %>%
    mutate(consistency_score = num_years / length(unique(year_range)))
  
  return(trade_summary)
}

# Load Data for Specific Years and Commodities
load_all_trade_data <- function(data_directory, commodities, year_groups) {
  all_trade_data <- list()
  
  for (years in year_groups) {
    year_label <- paste(years, collapse = "_")
    
    for (commodity in commodities) {
      file_path <- paste0(data_directory, commodity, "_Avg_", year_label, "E0.RData")
      
      # Load and process network
      network <- load_trade_data(file_path)
      
      # Extract trade data
      trade_data <- as.data.frame(get.data.frame(network, what = "edges"))
      trade_data$Commodity <- commodity
      trade_data$Year <- mean(years)
      
      # Ensure ISO codes are character
      trade_data$from <- as.character(trade_data$from)
      trade_data$to <- as.character(trade_data$to)
      
      all_trade_data[[paste0(commodity, "_", year_label)]] <- trade_data
    }
  }
  
  return(bind_rows(all_trade_data))
}

# Directories and Parameters
data_directory <- "/Users/mjp38/GitHub/FoodTradeNetwork/inputs_processed/"
commodities <- c('Banana', 'Coffee', 'Barley', 'Sugarcane', 'Wheat', 'Soybean',
                 'RapeseedOil', 'PalmOil', 'Groundnuts', 'Cocoa', 'Rice', 'Cassava')
year_groups <- list(c(2000, 2001), c(2002, 2003), c(2004, 2005), 
                    c(2006, 2007), c(2008, 2009), c(2010, 2011), 
                    c(2012, 2013), c(2014, 2015), c(2016, 2017), 
                    c(2018, 2019), c(2020, 2021))

# Load All Trade Data
all_trade_data <- load_all_trade_data(data_directory, commodities, year_groups)

# Calculate Consistency Scores
consistency_scores <- calculate_consistency_score(all_trade_data, unlist(year_groups))

# Verify consistency_scores dataframe
print(head(consistency_scores))

# Load World Map Data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create a mapping between the numeric codes and the ISO codes
iso_mapping <- data.frame(
  iso_n3 = world$iso_n3,
  iso_a3 = world$iso_a3
)

# Convert the consistency_scores `to` column from numeric to character
consistency_scores$to <- as.character(consistency_scores$to)

# Merge the consistency_scores with the iso_mapping to get ISO codes
consistency_scores <- consistency_scores %>%
  left_join(iso_mapping, by = c("to" = "iso_n3")) %>%
  select(iso_a3, total_trade, num_years, consistency_score, Commodity) %>%
  rename(to = iso_a3)

# Verify the mapping
print(head(consistency_scores))


