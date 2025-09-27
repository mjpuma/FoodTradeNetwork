## Install required packages if not already installed
##install.packages(c("sf", "ggplot2", "dplyr", "rworldmap", "viridis", "rnaturalearth", "rnaturalearthdata", "tidyr"))

# Load libraries
library(sf)
library(ggplot2)
library(dplyr)
library(rworldmap)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)  # Load tidyr for replace_na function

# Set the working directory and necessary data paths
setwd('/Users/mjp38/GitHub/FoodTradeNetwork/')

# Load the averaged trade flow matrix
load("inputs_processed/Wheat_Avg_2020_2021_PrimaryEquivE0.RData")  # Adjust the file path as needed

# Convert the matrix to a data frame
trade_df <- as.data.frame(as.table(E0_avg))
colnames(trade_df) <- c("Exporter", "Importer", "TradeVolume")
trade_df <- trade_df %>% filter(TradeVolume > 0)

# Get country centroids
world_map <- rworldmap::getMap(resolution = "low")
country_centroids <- data.frame(
  ISO3 = world_map$ISO_A3,
  Longitude = coordinates(world_map)[,1],
  Latitude = coordinates(world_map)[,2]
)
country_centroids <- country_centroids %>% filter(ISO3 != "-99")

# Merge coordinates with trade data
trade_df <- trade_df %>%
  left_join(country_centroids, by = c("Exporter" = "ISO3")) %>%
  rename(Exporter_Longitude = Longitude, Exporter_Latitude = Latitude) %>%
  left_join(country_centroids, by = c("Importer" = "ISO3")) %>%
  rename(Importer_Longitude = Longitude, Importer_Latitude = Latitude)

# Remove any rows with missing coordinates
trade_df <- trade_df %>% filter(!is.na(Exporter_Longitude) & !is.na(Importer_Longitude))

# Define a threshold for trade volume (e.g., top 5%)
threshold <- quantile(trade_df$TradeVolume, 0.95)

# Filter the trade data
trade_df_filtered <- trade_df %>% filter(TradeVolume >= threshold)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Plot the static flow map
ggplot() +
  geom_sf(data = world, fill = "antiquewhite") +
  geom_curve(
    data = trade_df_filtered,
    aes(
      x = Exporter_Longitude,
      y = Exporter_Latitude,
      xend = Importer_Longitude,
      yend = Importer_Latitude,
      linewidth = TradeVolume
    ),
    color = "steelblue",
    curvature = 0.33,
    alpha = 0.7
  ) +
  scale_linewidth_continuous(range = c(0.1, 1), guide = "none") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Global Maize Trade Flows (2000-2001 Average)",
    caption = "Data Source: FAO"
  )

# Calculate net trade balance
exports_df <- trade_df %>%
  group_by(Exporter) %>%
  summarise(TotalExports = sum(TradeVolume))

imports_df <- trade_df %>%
  group_by(Importer) %>%
  summarise(TotalImports = sum(TradeVolume))

net_trade_df <- exports_df %>%
  full_join(imports_df, by = c("Exporter" = "Importer")) %>%
  replace_na(list(TotalExports = 0, TotalImports = 0)) %>%
  mutate(NetTrade = TotalExports - TotalImports) %>%
  rename(Country = Exporter)

# Merge net trade data with world map
world_trade <- world %>%
  left_join(net_trade_df, by = c("iso_a3" = "Country"))

# Plot the choropleth map
ggplot(data = world_trade) +
  geom_sf(aes(fill = NetTrade), color = "gray") +
  scale_fill_viridis(option = "plasma", na.value = "lightgray") +
  theme_minimal() +
  labs(
    title = "Net Maize Trade Balance by Country (2000-2001 Average)",
    fill = "Net Trade (tons)",
    caption = "Data Source: FAO"
  )

# Top 10 exporters
top_exporters <- exports_df %>%
  arrange(desc(TotalExports)) %>%
  slice_head(n = 10)

ggplot(top_exporters, aes(x = reorder(Exporter, TotalExports), y = TotalExports)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 10 Maize Exporting Countries (2000-2001 Average)",
    x = "Country",
    y = "Total Exports (tons)"
  )


# Check for missing exporter coordinates
missing_exporter_coords <- trade_df %>% filter(is.na(Exporter_Longitude))
print("Missing Exporter Coordinates:")
print(unique(missing_exporter_coords$Exporter))

# Check for missing importer coordinates
missing_importer_coords <- trade_df %>% filter(is.na(Importer_Longitude))
print("Missing Importer Coordinates:")
print(unique(missing_importer_coords$Importer))
