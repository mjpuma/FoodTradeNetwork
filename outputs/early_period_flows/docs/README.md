# Virtual Water Flow Excel Files Documentation

## File Naming Convention
Files are named according to the pattern: {year}_{water_type}_flows.xlsx

For example:
- 1996_blue_irrigated_flows.xlsx
- 1996_green_irrigated_flows.xlsx
- 1996_green_rainfed_flows.xlsx

## File Contents
Each Excel file contains multiple sheets, one for each commodity. The commodities include:
- Rice
- Maize
- Temperate Cereals
- Tropical Cereals
- Groundnut
- Rapeseed
- Soyabean
- Sunflower
- Sugar
- Pulses
- Temperate Roots
- Tropical Roots

### Sheet Structure
Each sheet contains three columns:
1. Exporter: Country code (ISO3) of the exporting country
2. Importer: Country code (ISO3) of the importing country
3. Virtual_Water_km3: Volume of virtual water traded (in cubic kilometers)

Data in each sheet is sorted by Virtual_Water_km3 in descending order.

## Data Description
- Virtual water flows represent the amount of water embedded in traded agricultural commodities
- Values are in cubic kilometers (km³)
- Three types of water flows are tracked:
  1. Blue Irrigated: Water from irrigation systems
  2. Green Irrigated: Rainwater used by irrigated crops
  3. Green Rainfed: Rainwater used by rainfed crops
