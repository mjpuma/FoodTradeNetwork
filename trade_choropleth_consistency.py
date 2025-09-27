"""
Trade Network Analysis and Visualization Script

This script analyzes food trade networks, focusing on imports to a selected country (default: Japan).
It calculates various trade metrics and generates visualizations based on processed trade data.

Key Features:
1. Loads and processes trade data for multiple commodities and year groups
2. Calculates three main trade metrics:
   - Trade Volume Stability Index (TVSI)
   - Trade Partnership Persistence (TPP)
   - Relative Trade Intensity Index (RTII)
3. Generates choropleth maps for each metric and commodity
4. Produces CSV files with calculated metrics
5. Creates a prose summary of the metrics

Input:
- CSV files named '{commodity}_Avg_{year_range}E0.csv' in the specified input directory
- Each CSV should contain a trade matrix with countries as both rows and columns

Output:
- Choropleth maps saved as PNG files in the specified output directory
- CSV files with calculated metrics for each commodity
- Console output with a prose summary of the metrics

Usage:
1. Ensure all required libraries are installed (pandas, geopandas, matplotlib, scipy)
2. Set the correct paths for data_directory and output_directory
3. Adjust the commodities list and year_groups as needed
4. Run the script

Note: This script is designed to work with output from a corresponding R script that processes
raw trade data into the required CSV format. Ensure compatibility between the two scripts.
"""

import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import os
import matplotlib.colors as mcolors
import matplotlib.cm as cm
from scipy.stats import variation

# Function to load and process trade data
def load_and_process_trade_data(directory, commodities, year_groups):
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
                all_trade_data.append(df_melted)
            else:
                print(f"File not found: {file_path}")  # Debug statement
    if all_trade_data:
        all_trade_data_combined = pd.concat(all_trade_data, ignore_index=True)
        return all_trade_data_combined
    else:
        print("No trade data files found.")
        return None

# Function to calculate Trade Volume Stability Index (TVSI)
def calculate_tvsi(trade_data, country_iso3, commodity, year_groups):
    filtered_data = trade_data[(trade_data['to'] == country_iso3) & 
                               (trade_data['Commodity'] == commodity) &
                               (trade_data['weight'] > 0)]
    tvsi = filtered_data.groupby('from')['weight'].apply(lambda x: 1 - variation(x)).reset_index()
    tvsi.columns = ['iso3', 'TVSI']
    return tvsi

# Function to calculate Trade Partnership Persistence (TPP)
def calculate_tpp(trade_data, country_iso3, commodity, year_groups):
    filtered_data = trade_data[(trade_data['to'] == country_iso3) & 
                               (trade_data['Commodity'] == commodity) &
                               (trade_data['weight'] > 0)]
    presence = filtered_data.groupby(['from', 'Year']).size().unstack(fill_value=0)
    tpp = presence.sum(axis=1) / len(year_groups)
    tpp = tpp.reset_index()
    tpp.columns = ['iso3', 'TPP']
    return tpp

# Function to calculate Relative Trade Intensity Index (RTII)
def calculate_rtii(trade_data, country_iso3, commodity, year_groups):
    filtered_data = trade_data[(trade_data['to'] == country_iso3) & 
                               (trade_data['Commodity'] == commodity) &
                               (trade_data['weight'] > 0)]
    total_trade = filtered_data.groupby('Year')['weight'].sum().mean()
    rtii = filtered_data.groupby('from')['weight'].mean() / total_trade
    rtii = rtii.reset_index()
    rtii.columns = ['iso3', 'RTII']
    return rtii

# Function to plot combined choropleth maps for metrics
def plot_choropleths(metrics_dfs, metrics, world, output_directory, connected_countries, filter_connections, commodity):
    if len(metrics_dfs) == 3:
        fig = plt.figure(figsize=(30, 20))
        gs = fig.add_gridspec(2, 2, height_ratios=[1, 1], width_ratios=[1, 1])
        axes = [fig.add_subplot(gs[0, 0]), fig.add_subplot(gs[0, 1]), fig.add_subplot(gs[1, :])]
    else:
        fig, axes = plt.subplots(nrows=2, ncols=2, figsize=(30, 20))
        axes = axes.flatten()

    fig.subplots_adjust(hspace=0.1, wspace=0.1)
    
    for ax, (metric_df, metric) in zip(axes, metrics_dfs):
        metric_data = metric_df.groupby('iso3')[metric].mean().reset_index()
        if filter_connections:
            metric_data = metric_data[metric_data['iso3'].isin(connected_countries)]
        
        world_metric = world.merge(metric_data, left_on='iso_a3', right_on='iso3', how='left')
        vmin, vmax = 0, 1  # Set range from 0 to 1
        cmap = plt.get_cmap('YlOrRd', 10)  # Use a discrete colormap
        norm = mcolors.BoundaryNorm(boundaries=[i/10 for i in range(11)], ncolors=10)
        
        world_metric.boundary.plot(ax=ax, linewidth=0.5, color='black')
        world_metric.plot(column=metric, ax=ax, legend=False, cmap=cmap, norm=norm, edgecolor='face')
        ax.set_title(f"{metric} for {commodity}", fontsize=24)
        ax.set_axis_off()
        
        sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
        sm.set_array([])
        cbar = fig.colorbar(sm, ax=ax, orientation='horizontal', fraction=0.046, pad=0.04, aspect=30)
        cbar.ax.tick_params(labelsize=16)
        cbar.set_label(f"Average {metric}", fontsize=20)
    
    plt.tight_layout()
    save_path = os.path.join(output_directory, f'choropleth_{commodity}_combined.jpg')
    plt.savefig(save_path, bbox_inches='tight', dpi=300)
    plt.show()
    
# Function to create DataFrames for all metrics
def get_all_metrics_dataframe(trade_data, selected_country_iso3, commodities, year_groups):
    metrics_dfs = {
        'TVSI': [],
        'TPP': [],
        'RTII': []
    }
    for commodity in commodities:
        tvsi_df = calculate_tvsi(trade_data, selected_country_iso3, commodity, year_groups)
        tvsi_df['Commodity'] = commodity
        metrics_dfs['TVSI'].append(tvsi_df)

        tpp_df = calculate_tpp(trade_data, selected_country_iso3, commodity, year_groups)
        tpp_df['Commodity'] = commodity
        metrics_dfs['TPP'].append(tpp_df)

        rtii_df = calculate_rtii(trade_data, selected_country_iso3, commodity, year_groups)
        rtii_df['Commodity'] = commodity
        metrics_dfs['RTII'].append(rtii_df)

    combined_dfs = {
        metric: pd.concat(dfs, ignore_index=True) for metric, dfs in metrics_dfs.items()
    }
    return combined_dfs

# Function to convert all metrics DataFrames to prose
def convert_metrics_to_prose(metrics_dataframes, selected_country_iso3):
    prose = f"The trade metrics for {selected_country_iso3} across various commodities and countries are as follows:\n\n"
    for metric_name, metric_df in metrics_dataframes.items():
        prose += f"{metric_name}:\n"
        for commodity in metric_df['Commodity'].unique():
            prose += f"For {commodity}:\n"
            subset = metric_df[metric_df['Commodity'] == commodity]
            for index, row in subset.iterrows():
                prose += f"- {row['iso3']}: {metric_name} = {row[metric_name]:.2e}\n"
            prose += "\n"
    return prose

# Main Code
data_directory = '/Users/mjp38/GitHub/FoodTradeNetwork/inputs_processed/'
output_directory = '/Users/mjp38/GitHub/FoodTradeNetwork/outputs/'
year_groups = [[2000, 2001], [2002, 2003], [2004, 2005], [2006, 2007], [2008, 2009], [2010, 2011], [2012, 2013], [2014, 2015], [2016, 2017], [2018, 2019], [2020, 2021]]
commodities = ['Banana', 'Coffee', 'Barley', 'Sugarcane', 'Wheat', 'Soybean', 'RapeseedOil', 'PalmOil', 'Groundnuts', 'Cocoa', 'Rice', 'Cassava']

# Load World Map Data
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
world = world[(world.pop_est > 0) & (world.name != "Antarctica")]  # Remove Antarctica and tiny population countries

# Load All Trade Data
all_trade_data = load_and_process_trade_data(data_directory, commodities, year_groups)
if all_trade_data is not None:
    print(f"Loaded trade data: {all_trade_data.shape}")  # Debug statement

    # Define selected parameters
    selected_country_iso3 = 'JPN'
    filter_connections = True  # Toggle for filtering based on connections to Japan

    # Iterate over each commodity
    for commodity in commodities:
        tvsi_df = calculate_tvsi(all_trade_data, selected_country_iso3, commodity, year_groups)
        tpp_df = calculate_tpp(all_trade_data, selected_country_iso3, commodity, year_groups)
        rtii_df = calculate_rtii(all_trade_data, selected_country_iso3, commodity, year_groups)

        plot_choropleths([(tvsi_df, 'TVSI'), 
                          (tpp_df, 'TPP'), 
                          (rtii_df, 'RTII')], 
                          ['TVSI', 'TPP', 'RTII'], 
                          world, output_directory, tvsi_df['iso3'].tolist(), filter_connections, commodity)
    
    # Generate DataFrames for all metrics
    all_metrics_dataframes = get_all_metrics_dataframe(all_trade_data, 'JPN', commodities, year_groups)

    # Save the DataFrames to CSV files
    for metric_name, metric_df in all_metrics_dataframes.items():
        metric_df.to_csv(os.path.join(output_directory, f"{metric_name}_Values_by_Country_and_Commodity.csv"), index=False)

    # Generate prose from all metrics DataFrames
    metrics_prose = convert_metrics_to_prose(all_metrics_dataframes, 'Japan')
    print(metrics_prose)
else:
    print("No trade data to process.")
