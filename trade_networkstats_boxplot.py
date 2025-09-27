import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os

# Function to load and combine metrics data
def load_metrics_data(directory, commodities, year_groups):
    frames = []
    for years in year_groups:
        year_label = "_".join(map(str, years))
        for commodity in commodities:
            file_path = os.path.join(directory, f'{commodity}_{year_label}_metrics.csv')
            if os.path.exists(file_path):
                df = pd.read_csv(file_path)
                df['Commodity'] = commodity
                df['Year Group'] = year_label
                frames.append(df)
    return pd.concat(frames, ignore_index=True)

# Function to plot boxplots for network metrics, pooled by commodity
def plot_metric_distributions(metrics_df, iso3, metric, output_directory):
    # Filter data for the selected country
    country_data = metrics_df[metrics_df['iso3'] == iso3]

    # Calculate median values for ordering
    order = country_data.groupby('Commodity')[metric].median().sort_values(ascending=False).index

    # First plot: original order
    plt.figure(figsize=(12, 8))
    sns.boxplot(x='Commodity', y=metric, data=country_data)
    plt.title(f'Pooled Distribution of {metric} for {iso3} Across All Years')
    plt.xlabel('Commodity')
    plt.ylabel(metric)
    plt.xticks(rotation=45)
    plt.tight_layout()
    save_path = os.path.join(output_directory, f'{iso3}_{metric}_pooled_distribution.png')
    plt.savefig(save_path)
    plt.show()

    # Second plot: ordered by median
    plt.figure(figsize=(12, 8))
    sns.boxplot(x='Commodity', y=metric, data=country_data, order=order)
    plt.title(f'Pooled Distribution of {metric} for {iso3} Sorted by Median')
    plt.xlabel('Commodity')
    plt.ylabel(metric)
    plt.xticks(rotation=45)
    plt.tight_layout()
    save_path_ordered = os.path.join(output_directory, f'{iso3}_{metric}_pooled_distribution_ordered.png')
    plt.savefig(save_path_ordered)
    plt.show()

# Directories and Parameters
data_directory = '/Users/mjp38/GitHub/FoodTradeNetwork/outputs/'
output_directory = '/Users/mjp38/GitHub/FoodTradeNetwork/media/'
year_groups = [[2000, 2001], [2002, 2003], [2004, 2005], [2006, 2007], [2008, 2009], [2010, 2011], [2012, 2013], [2014, 2015], [2016, 2017], [2018, 2019], [2020, 2021]]
commodities = ['Banana', 'Coffee', 'Barley', 'Sugarcane', 'Wheat', 'Soybean', 'RapeseedOil', 'PalmOil', 'Groundnuts', 'Cocoa', 'Rice', 'Cassava']

# Load Data
metrics_df = load_metrics_data(data_directory, commodities, year_groups)

# ISO3 code and metric for the plot
selected_country_iso3 = 'JPN'
selected_metric = 'pagerank'
# degree_total	degree_in	degree_out	strength_total	strength_in strength_out betweenness	eigencentrality	closeness	clustering_coefficient_localavg clustering_coefficient_local 	pagerank community_modularity community_count

# Plot and save distribution boxplots
plot_metric_distributions(metrics_df, selected_country_iso3, selected_metric, output_directory)
