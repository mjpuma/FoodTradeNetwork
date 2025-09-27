import pandas as pd
import os
import geopandas as gpd
import matplotlib.pyplot as plt
import seaborn as sns

# Function to load and combine trade data
def load_trade_data(directory, commodities, year_groups, iso_mapping):
    all_trade_data = []
    for years in year_groups:
        year_label = "_".join(map(str, years))
        for commodity in commodities:
            file_path = os.path.join(directory, f'{commodity}_Avg_{year_label}E0.csv')
            if os.path.exists(file_path):
                print(f"Loading file: {file_path}")  # Debug statement
                df = pd.read_csv(file_path)
                df = df.rename(columns={"Unnamed: 0": "from"})
                df_melted = df.melt(id_vars=['from'], var_name='to', value_name='weight')
                df_melted['Commodity'] = commodity
                df_melted['Year'] = year_label
                
                # Merge with iso_mapping to ensure proper ISO3 codes
                df_melted = df_melted.merge(iso_mapping, left_on='from', right_on='name', how='left').drop('from', axis=1).rename(columns={'iso_a3': 'from'})
                df_melted = df_melted.merge(iso_mapping, left_on='to', right_on='name', how='left').drop('to', axis=1).rename(columns={'iso_a3': 'to'})
                all_trade_data.append(df_melted)
            else:
                print(f"File not found: {file_path}")  # Debug statement
    if not all_trade_data:
        raise ValueError("No data files found. Please check the file paths and directory.")
    return pd.concat(all_trade_data, ignore_index=True)

# Function to calculate consistency score
def calculate_consistency_score(trade_data, year_groups):
    trade_data['to'] = trade_data['to'].astype(str)
    total_years = len(year_groups)
    consistency_scores = trade_data.groupby(['to', 'Commodity']).agg(
        total_trade=('weight', 'sum'),
        num_years=('Year', 'nunique')
    ).reset_index()
    consistency_scores['consistency_score'] = consistency_scores['num_years'] / total_years
    return consistency_scores

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

# Function to plot time series and boxplots for network metrics, pooled by commodity
def plot_metric_distributions(metrics_df, iso3, metric, output_directory):
    country_data = metrics_df[metrics_df['iso3'] == iso3]
    order = country_data.groupby('Commodity')[metric].median().sort_values(ascending=False).index
    palette = sns.color_palette("husl", len(order))

    fig, axes = plt.subplots(nrows=2, ncols=1, figsize=(14, 16))

    time_series_data = country_data.groupby(['Year Group', 'Commodity'])[metric].mean().unstack()
    colors = {commodity: palette[idx] for idx, commodity in enumerate(order)}
    
    for commodity in order:
        axes[0].plot(time_series_data.index, time_series_data[commodity], marker='o', label=commodity, color=colors[commodity])

    axes[0].set_title(f'Time Series of {metric} for {iso3} (2000-2021)', fontsize=16)
    axes[0].set_xlabel('Year Group', fontsize=14)
    axes[0].set_ylabel(metric, fontsize=14)
    axes[0].tick_params(axis='x', rotation=45, labelsize=12)
    axes[0].tick_params(axis='y', labelsize=12)
    axes[0].legend(title='Commodity', fontsize=12, title_fontsize=14)

    sns.boxplot(ax=axes[1], x='Commodity', y=metric, data=country_data, order=order, palette=colors)
    axes[1].set_title(f'Pooled Distribution of {metric} for {iso3} Sorted by Median', fontsize=16)
    axes[1].set_xlabel('Commodity', fontsize=14)
    axes[1].set_ylabel(metric, fontsize=14)
    axes[1].tick_params(axis='x', rotation=45, labelsize=12)
    axes[1].tick_params(axis='y', labelsize=12)

    plt.tight_layout()
    save_path = os.path.join(output_directory, f'{iso3}_{metric}_time_series_and_boxplot.png')
    plt.savefig(save_path)
    plt.show()

# Function to plot a choropleth map for a selected network metric
def plot_choropleth(metrics_df, metric, world, output_directory, connected_countries):
    # Group by country and calculate the mean of the selected metric
    metric_data = metrics_df.groupby('iso3')[metric].mean().reset_index()
    world_metric = world.merge(metric_data, left_on='iso_a3', right_on='iso3', how='left')
    
    # Filter to keep only countries connected to Japan
    world_metric = world_metric[world_metric['iso_a3'].isin(connected_countries)]

    # Plotting the choropleth map
    fig, ax = plt.subplots(1, 1, figsize=(15, 10))
    world_metric.boundary.plot(ax=ax, linewidth=1)
    world_metric.plot(column=metric, ax=ax, legend=True,
                      legend_kwds={'label': f"Average {metric} by Country",
                                   'orientation': "horizontal"})
    ax.set_title(f"Choropleth Map of {metric}", fontsize=16)
    ax.set_axis_off()

    save_path = os.path.join(output_directory, f'choropleth_{metric}.png')
    plt.savefig(save_path)
    plt.show()

# Directories and Parameters
data_directory = '/Users/mjp38/GitHub/FoodTradeNetwork/inputs_processed/'
output_directory = '/Users/mjp38/GitHub/FoodTradeNetwork/outputs/'
year_groups = [[2000, 2001], [2002, 2003], [2004, 2005], [2006, 2007], [2008, 2009], [2010, 2011], [2012, 2013], [2014, 2015], [2016, 2017], [2018, 2019], [2020, 2021]]
commodities = ['Banana', 'Coffee', 'Barley', 'Sugarcane', 'Wheat', 'Soybean', 'RapeseedOil', 'PalmOil', 'Groundnuts', 'Cocoa', 'Rice', 'Cassava']

# Load World Map Data
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
world = world[world.name != "Antarctica"]  # Remove Antarctica
iso_mapping = world[['iso_a3', 'name']].dropna()

# Load All Trade Data
all_trade_data = load_trade_data(data_directory, commodities, year_groups, iso_mapping)
print(f"Loaded trade data: {all_trade_data.shape}")  # Debug statement

# Calculate Consistency Scores
consistency_scores = calculate_consistency_score(all_trade_data, year_groups)
print(f"Calculated consistency scores: {consistency_scores.shape}")  # Debug statement

# Load Network Metrics Data
metrics_df = load_metrics_data(output_directory, commodities, year_groups)
print(f"Loaded network metrics data: {metrics_df.shape}")  # Debug statement

# Plot and Save Network Metrics
selected_country_iso3 = 'JPN'
selected_metric = 'community_count'
plot_metric_distributions(metrics_df, selected_country_iso3, selected_metric, output_directory)
print(f"Plotted network metrics for {selected_country_iso3}")  # Debug statement

# Define selected year and commodity
selected_year = '2020_2021'
selected_commodity = 'Wheat'
selected_choropleth_metric = 'degree_in'

# Filter trade data to find countries connected to Japan in the specific year and commodity
trade_data_year = all_trade_data[(all_trade_data['Year'] == selected_year) & (all_trade_data['Commodity'] == selected_commodity)]
connected_countries = trade_data_year[trade_data_year['to'] == 'JPN']['from'].unique().tolist()
print(f"Connected countries to {selected_country_iso3}: {connected_countries}")

# Filter metrics data for the selected year and commodity
metrics_df_year = metrics_df[(metrics_df['Year Group'] == selected_year) & (metrics_df['Commodity'] == selected_commodity)]
metric_data = metrics_df_year[metrics_df_year['iso3'].isin(connected_countries)][['iso3', selected_choropleth_metric]]
print(f"Metric data for plotting:\n{metric_data.head()}")

# Plot Choropleth Map for a Selected Network Metric
plot_choropleth(metrics_df_year, selected_choropleth_metric, world, output_directory, connected_countries)
print(f"Plotted choropleth map for {selected_choropleth_metric} in {selected_year} for {selected_commodity} (Filtered by Connections to {selected_country_iso3})")












