# This script analyzes food trade networks focusing on Japan's imports:
# 1. Loads trade data for multiple commodities and year groups
# 2. Constructs network graphs and calculates network metrics
# 3. Extracts and summarizes # 4. Outputs:
#    - Network metrics for each commodity and year group
#    - Detailed Japan import data across all years and commodities
#    - Summary of non-zero imports into Japan by source country and commodity
# Input: RData files with trade matrices, country list CSV
# Output: CSV files with network metrics and Japan's import data

# Load Required Libraries
library(igraph)
library(dplyr)
library(readr)

# Load and Prepare Data
load_trade_data <- function(file_path, iso3names) {
  print(paste("Loading data from:", file_path))
  e <- new.env()
  load(file_path, envir = e)
  
  if ("E0_avg" %in% names(e)) {
    E0 <- e$E0_avg
    print("Loaded E0_avg from file")
  } else if ("exp_mat_sum" %in% names(e)) {
    E0 <- e$exp_mat_sum
    print("Loaded exp_mat_sum from file")
  } else {
    stop(paste("Neither E0_avg nor exp_mat_sum found in:", file_path))
  }
  
  print(paste("Dimensions of loaded matrix:", paste(dim(E0), collapse = "x")))
  
  colnames(E0) <- iso3names
  rownames(E0) <- iso3names
  
  graph_from_adjacency_matrix(E0, mode = "directed", weighted = TRUE)
}

# Enhanced Network Metrics Calculation Function
calculate_network_metrics <- function(network, year) {
  num_countries <- vcount(network)
  
  # Convert to undirected graph for community detection
  undirected_network <- as.undirected(network, mode = "mutual")
  
  # Perform Louvain community detection on the undirected graph
  communities <- cluster_louvain(undirected_network)
  
  metrics <- tibble(
    iso3 = V(network)$name,
    Year = year,
    degree_total = degree(network, mode = "all"),
    degree_in = degree(network, mode = "in"),
    degree_out = degree(network, mode = "out"),
    strength_total = strength(network, weights = E(network)$weight),
    strength_in = strength(network, mode = "in", weights = E(network)$weight),
    strength_out = strength(network, mode = "out", weights = E(network)$weight),
    betweenness = betweenness(network, directed = TRUE) / ((num_countries - 1) * (num_countries - 2) / 2),
    eigencentrality = eigen_centrality(network)$vector,
    closeness = closeness(network, normalized = TRUE),
    clustering_coefficient_local = transitivity(network, type = "local"),
    clustering_coefficient_localavg = transitivity(network, type = "localaverage"),
    pagerank = page_rank(network)$vector,
    community_modularity = modularity(communities),
    community_count = length(unique(membership(communities)))
  )
  
  return(metrics)
}

# Function to extract non-zero imports into Japan
extract_imports_to_japan <- function(network, year, country_code) {
  import_edges <- E(network)[.to(country_code)]
  import_data <- tibble(
    from_iso3 = V(network)[ends(network, import_edges)[, 1]]$name,
    to_iso3 = country_code,
    Year = year,
    weight = E(network)[import_edges]$weight
  ) %>%
    filter(weight > 0)
  
  return(import_data)
}

# Setting Directories and Parameters
data_directory <- "/Users/mjp38/GitHub/FoodTradeNetwork/inputs_processed/"
output_directory <- "/Users/mjp38/GitHub/FoodTradeNetwork/outputs/"

# Define the range of year groups to process
year_groups <- list(c(2000, 2001), c(2002, 2003), c(2004, 2005),
                    c(2006, 2007), c(2008, 2009), c(2010, 2011),
                    c(2012, 2013), c(2014, 2015), c(2016, 2017),
                    c(2018, 2019), c(2020, 2021))

# Define commodities
commodities <- c('Banana', 'Coffee', 'Barley', 'Sugarcane', 'Wheat', 'Soybean',
                 'RapeseedOil', 'PalmOil', 'Groundnuts', 'Cocoa', 'Rice', 'Cassava')

# Load ancillary data
country_list <- read_csv("/Users/mjp38/GitHub/FSC-WorldModelers/ancillary/country_list195_2012to2016.csv")
iso3names <- country_list$iso3

# Specify the country code for Japan
japan_code <- "JPN"

# Main Processing Loop
all_japan_imports <- tibble()

for (years in year_groups) {
  year_label <- paste(years, collapse = "_")
  midpoint_year <- mean(years)
  
  for (commodity in commodities) {
    file_path_avg <- paste0(data_directory, commodity, "_Avg_", year_label, "E0.RData")
    file_path_sum <- paste0(data_directory, commodity, "_Sum_", year_label, "E0.RData")
    
    tryCatch({
      # Load and process network for average flows
      network_avg <- load_trade_data(file_path_avg, iso3names)
      metrics_avg <- calculate_network_metrics(network_avg, midpoint_year)
      output_path_metrics_avg <- paste0(output_directory, commodity, "_", year_label, "_metrics_avg.csv")
      write_csv(metrics_avg, output_path_metrics_avg)
      
      # Load and process network for total flows
      network_sum <- load_trade_data(file_path_sum, iso3names)
      metrics_sum <- calculate_network_metrics(network_sum, midpoint_year)
      output_path_metrics_sum <- paste0(output_directory, commodity, "_", year_label, "_metrics_sum.csv")
      write_csv(metrics_sum, output_path_metrics_sum)
      
      # Extract non-zero imports into Japan from average flow network
      japan_imports_avg <- extract_imports_to_japan(network_avg, midpoint_year, japan_code)
      japan_imports_avg <- japan_imports_avg %>%
        mutate(Commodity = commodity, Type = "Average")
      
      # Extract non-zero imports into Japan from total flow network
      japan_imports_sum <- extract_imports_to_japan(network_sum, midpoint_year, japan_code)
      japan_imports_sum <- japan_imports_sum %>%
        mutate(Commodity = commodity, Type = "Total")
      
      # Accumulate Japan's import data
      all_japan_imports <- bind_rows(all_japan_imports, japan_imports_avg, japan_imports_sum)
      
      print(paste("Processed", commodity, "for years", year_label))
    }, error = function(e) {
      print(paste("Error processing", commodity, "for years", year_label, ":", e$message))
    })
  }
}

# Output Japan import data to a single file
output_path_imports <- paste0(output_directory, "Japan_Imports_All_Years.csv")
write_csv(all_japan_imports, output_path_imports)

# Create a summary of non-zero imports across all years
japan_imports_summary <- all_japan_imports %>%
  filter(Type == "Total") %>%  # Use only the total flows, not averages
  group_by(from_iso3, Commodity) %>%
  summarise(
    total_weight = sum(weight, na.rm = TRUE),
    years_present = n_distinct(Year)
  ) %>%
  filter(total_weight > 0) %>%
  arrange(Commodity, desc(total_weight))

# Output the summary to a CSV file
output_path_summary <- paste0(output_directory, "Japan_Imports_Summary_All_Years.csv")
write_csv(japan_imports_summary, output_path_summary)

print("Network metrics calculation and import data extraction for Japan completed for all specified commodities and year groups.")
print(paste("Detailed import data saved to:", output_path_imports))
print(paste("Summary of non-zero imports across all years saved to:", output_path_summary))