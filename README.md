# Virtual Water Footprint Analysis

A comprehensive R-based framework for analyzing global virtual water trade flows in agricultural commodities, with applications for climate risk assessment and food security analysis.

## Overview

This project calculates and analyzes virtual water flows embedded in international agricultural trade. Virtual water represents the water used to produce traded agricultural goods - when a country imports wheat, it effectively imports the water that was used to grow that wheat. This analysis is crucial for understanding:

- Water security dependencies between countries
- Climate risk exposure through agricultural imports
- Evolution of global water trade networks
- Food security vulnerabilities

## Key Features

- **Comprehensive Coverage**: 12+ major crop categories across 25 years (1996-2020)
- **Multi-dimensional Analysis**: Blue water (irrigation) vs. green water (rainfall) trade flows
- **Network Analysis**: Trade relationship patterns and centrality metrics
- **Country-specific Studies**: Detailed import/export profiles (e.g., Saudi Arabia, Japan)
- **Climate Risk Assessment**: Dependency analysis and vulnerability metrics
- **Validation Framework**: Data quality checks and benchmark comparisons

## Directory Structure

```
├── Main Analysis Scripts
│   ├── water_footprint_analysis_v1.R    # Primary analysis engine
│   ├── water_footprint_analysis_v2.R    # Enhanced version
│   ├── run_water_footprint_analysis.R   # Simplified wrapper
│   └── saudi_arabia_analysis.R          # Country-specific analysis
│
├── Data Processing
│   ├── ProcessInputs*.R                 # Trade data processing
│   ├── Process_waterfootprint_types.R   # Water footprint calculations
│   └── ProcessPlot_VirtualWater_types.R # Visualization generation
│
├── Data Sources
│   ├── ancillary/                       # Water footprint data & mappings
│   │   ├── *W_*_lpjml.xlsx             # Water footprint by crop/country
│   │   ├── country_conversion_table.csv # ISO3 country mappings
│   │   └── FoodCommodity_ForCarole_v5.xlsx # Extraction rates
│   ├── inputs/                          # Raw trade & political data
│   │   └── Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv
│   ├── inputs_processed/                # Processed commodity data
│   └── data_raw/                        # FAO production data
│
├── Analysis Results
│   ├── outputs/                         # Results, plots, validation
│   │   ├── *.png                       # Network maps, trade rankings
│   │   ├── *.csv                       # Processed results
│   │   └── logs/                       # Execution logs
│   └── media/                          # Additional visualizations
│
├── Validation & Testing
│   ├── validation_checks.R             # Data quality validation
│   ├── additional_checks.R             # Extended validation
│   └── test*.R                         # Unit tests
│
└── Utilities
    ├── *.py                            # Python analysis scripts
    └── old/                            # Archived versions
```

## Installation

### Prerequisites

**R Dependencies:**
```r
required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "ggplot2",
  "rnaturalearth", "rnaturalearthdata", "sf", "viridis",
  "igraph", "rworldmap", "RColorBrewer", "FAOSTAT", "ggpubr",
  "networkD3", "tibble", "circlize", "htmlwidgets", "grid",
  "ggraph", "tidygraph", "gridExtra"
)

# Install missing packages
install.packages(setdiff(required_packages, rownames(installed.packages())))
```

**Data Requirements:**
- FAO trade data (included: 7.6GB Trade_DetailedTradeMatrix file)
- Water footprint data (included: Excel files in ancillary/)
- Country mapping tables (included)

### Setup

1. Clone or download the project directory
2. Ensure all required R packages are installed
3. Verify data files are present in `inputs/` and `ancillary/`
4. Run initial validation: `source("validation_checks.R")`

## Recent Updates (December 2024)

### Saudi Arabia Climate Risk Analysis
A new specialized analysis focusing on Saudi Arabia's agricultural import dependencies and climate vulnerabilities has been added:

- **Policy-grade visualizations** showing network dependencies, risk assessment, and concentration analysis
- **Climate risk metrics** quantifying water stress exposure and supplier dependencies  
- **Publication-ready outputs** suitable for policy presentations and academic papers

**Key Results:**
- 68.4 km³/year virtual water imports (massive climate dependency)
- 67.1% concentration in top 5 suppliers (systemic risk)
- 95.7% green water dependence (precipitation-vulnerable)

### Quick Start - Saudi Arabia Analysis

**Option 1: Use Existing Results (Recommended)**
```r
# Run Saudi Arabia analysis using processed data
source("saudi_policy_analysis.R")

# Generates 5 policy-grade visualizations:
# - Network dependency map
# - Supplier concentration analysis  
# - Climate & political risk matrix
# - Crop vulnerability assessment
# - Water source composition
```

### Option 2: Full Analysis from Scratch
```r
# Load the main analysis framework
source("water_footprint_analysis_v1.R")

# Run complete analysis for both periods (30-60 minutes)
main()  # Analyzes 1996-2000 vs 2016-2020

# Then run country-specific analysis
source("run_saudi_analysis.R")
```

## Quick Start

### Option 1: Use Pre-processed Data (Recommended)
If you have existing results in `outputs/`, use the streamlined approach:

```r
# Run complete Saudi Arabia analysis (loads existing data)
source("run_saudi_analysis.R")

# Generates all visualizations and reports automatically
# Results saved to outputs/ directory
```

### Option 2: Full Analysis from Scratch
```r
# Load the main analysis framework
source("water_footprint_analysis_v1.R")

# Run complete analysis for both periods (30-60 minutes)
main()  # Analyzes 1996-2000 vs 2016-2020

# Then run country-specific analysis
source("run_saudi_analysis.R")
```

### Country-Specific Analysis Template
```r
# Example: Saudi Arabia climate risk assessment
source("saudi_arabia_analysis.R")

# Load existing flow data
flows_late <- read.csv("outputs/virtual_water_flows_late_period.csv")

# Run analysis
saudi_results <- create_saudi_dashboard(flows_late, "2016-2020")

# Generates:
# - Import network visualization
# - Top supplier rankings  
# - Crop dependency analysis
# - Climate risk summary
```

## File Management

### Essential Files (Keep)
```
├── water_footprint_analysis_v1.R    # Main analysis engine
├── saudi_arabia_analysis.R          # Country-specific analysis  
├── run_saudi_analysis.R             # Complete Saudi workflow
├── ancillary/                       # Core water footprint data
├── inputs/                          # Raw trade data
├── outputs/virtual_water_flows_*.csv # Processed results
└── outputs/*.png                    # Key visualizations
```

### Safe to Archive/Delete
```
├── water_footprint_analysis_v0*.R   # Older versions
├── WF_original.R                    # Superseded by v1
├── test*.R                          # Development files
├── old/                             # Archived versions
└── outputs/logs/ (older entries)    # Keep recent logs only
```

## Key Outputs

### Visualizations
- **Network Maps**: Global trade flow visualizations with country nodes and flow arrows
- **Trade Rankings**: Top importing/exporting countries by commodity and water type
- **Temporal Analysis**: Evolution of trade patterns over time
- **Country Profiles**: Detailed import dependency analysis

### Data Products
- **Virtual Water Flows**: Bilateral trade flows in km³/year by crop and water type
- **Network Metrics**: Centrality measures, clustering coefficients, modularity
- **Validation Reports**: Data quality checks and benchmark comparisons
- **Country Rankings**: Supplier dependencies and trade concentrations

### Climate Risk Outputs
- Import dependency percentages
- Supplier concentration metrics
- Water stress exposure analysis
- Vulnerability assessments

## Core Concepts

### Water Types
- **Blue Water**: Irrigation water from rivers, lakes, aquifers
- **Green Water (Irrigated)**: Soil moisture from irrigation
- **Green Water (Rainfed)**: Soil moisture from precipitation

### Crop Categories
- **Cereals**: Wheat, barley, rice, maize, sorghum, millet
- **Oilseeds**: Soybeans, rapeseed, sunflower, groundnuts
- **Roots**: Potatoes, cassava, sweet potatoes, sugar beet
- **Other**: Sugar cane, pulses (beans, peas, chickpeas)

### Analysis Methods
- **Direct Proportion Approach**: Virtual water = (Trade Volume / Production) × Water Footprint
- **Network Analysis**: Graph theory metrics applied to trade networks
- **Temporal Comparison**: Early period (1996-2000) vs. late period (2016-2020)

## Data Sources

### Primary Data
- **FAO Trade Data**: Detailed Trade Matrix, production statistics
- **Water Footprint Data**: LPJmL model outputs (Rost et al.)
- **Country Mappings**: ISO3 codes and regional classifications

### Processing Pipeline
1. **Raw Data Ingestion**: FAO codes → standardized crop categories
2. **Water Footprint Calculation**: Country-crop-year water volumes
3. **Trade Flow Processing**: Bilateral trade in primary equivalents
4. **Virtual Water Calculation**: Trade volumes × water intensities
5. **Network Construction**: Countries as nodes, flows as weighted edges

## Applications

### Climate Risk Assessment
- Identify countries with high import dependencies
- Assess exposure to water-stressed supplier regions
- Quantify concentration risks in supply chains

### Food Security Analysis
- Map virtual water trade networks
- Analyze supplier diversity and resilience
- Track temporal evolution of dependencies

### Policy Applications
- Trade policy impact assessment
- Water diplomacy and cooperation
- Agricultural investment planning

## Examples

### Saudi Arabia Case Study
Saudi Arabia exemplifies climate vulnerability through agricultural imports:

```r
# Generate comprehensive Saudi analysis
saudi_results <- create_saudi_dashboard(flows_late, "2016-2020")

# Key findings:
# - 80%+ virtual water from imports
# - High concentration: top 5 suppliers provide ~60%
# - Major sources: USA, Brazil, Argentina (water-stressed regions)
# - Cereals dominate: wheat, barley for food and livestock
```

### Network Evolution Analysis
```r
# Compare network structure over time
early_metrics <- analyze_network_metrics(flows_early, "1996-2000")
late_metrics <- analyze_network_metrics(flows_late, "2016-2020")

# Findings:
# - Increased trade network density
# - Growing role of emerging exporters
# - Shift from regional to global trade patterns
```

## Validation

The framework includes comprehensive validation:
- **Mass Balance**: Exports = Imports globally
- **Benchmark Comparison**: Results vs. published studies (Konar et al.)
- **Unit Consistency**: Water volumes, trade quantities, intensities
- **Data Completeness**: Missing value analysis and imputation

## Troubleshooting

### Common Issues
1. **Memory Errors**: Large datasets require 8GB+ RAM
2. **Missing Dependencies**: Install all required R packages
3. **Data File Paths**: Ensure correct working directory
4. **Long Execution Times**: Full analysis takes 30-60 minutes

### Performance Optimization
- Use `analysis_type = "single_year"` for testing
- Process subsets of countries/crops for development
- Enable parallel processing where available

## Contributing

### Code Structure
- Main functions in `water_footprint_analysis_v1.R`
- Country-specific analyses as separate scripts
- Validation functions in dedicated files
- Plotting functions modularized

### Adding New Countries
```r
# Template for new country analysis
analyze_COUNTRY_imports <- function(flows_data, period_name) {
  # Filter for target country
  country_flows <- flows_data %>% filter(PartnerISO3 == "COUNTRY_CODE")
  # Process and analyze...
}
```

### Data Updates
- Update FAO trade data in `inputs/`
- Refresh water footprint data in `ancillary/`
- Modify crop mappings as needed
- Run validation suite after updates

## Citation

When using this framework, please cite:
- Original methodology papers (Hoekstra & Mekonnen, Konar et al.)
- Data sources (FAO, LPJmL water footprint model)
- This analytical framework (if appropriate)

## License

This project is developed for academic and policy research purposes. Data sources maintain their original licensing terms.

## Contact

For questions about methodology, data sources, or applications, please refer to the documentation or create an issue in the project repository.

---

**Last Updated**: December 2024  
**Version**: 2.0  
**Compatibility**: R 4.0+, requires ~10GB disk space