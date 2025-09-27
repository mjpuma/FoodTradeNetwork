import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import os
import matplotlib.colors as mcolors
from datetime import datetime

# Load World Map Data
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
world = world[(world['pop_est'] > 0) & (world['name'] != "Antarctica")]  # Remove Antarctica and tiny population countries

# Function to load and process geopolitical risk data
def load_geopolitical_risk_data(file_path):
    gpr_data = pd.read_csv(file_path)
    gpr_columns = [col for col in gpr_data.columns if 'GPRHC_' in col or 'GPRC_' in col]
    # Extract relevant columns
    gpr_data_filtered = gpr_data[['month'] + gpr_columns].copy()
    # Correctly parse dates with four-digit year format
    gpr_data_filtered['month'] = pd.to_datetime(gpr_data_filtered['month'], format='%m/%d/%Y')
    # Print date range for verification
    print(f"Date range in the dataset: {gpr_data_filtered['month'].min()} to {gpr_data_filtered['month'].max()}")
    # Filter the last 5 years of data
    last_5_years_start = gpr_data_filtered['month'].max() - pd.DateOffset(years=5)
    gpr_data_filtered_last_5_years = gpr_data_filtered[gpr_data_filtered['month'] >= last_5_years_start]
    print(f"Filtered date range: {gpr_data_filtered_last_5_years['month'].min()} to {gpr_data_filtered_last_5_years['month'].max()}")
    # Calculate the average GPR for each country for recent and historical data
    gprc_columns = [col for col in gpr_data_filtered.columns if col.startswith('GPRC_')]
    gprhc_columns = [col for col in gpr_data_filtered.columns if col.startswith('GPRHC_')]
    
    gprc_data_avg = gpr_data_filtered_last_5_years[gprc_columns].mean().reset_index()
    gprc_data_avg.columns = ['country_code', 'avg_gprc']
    gprc_data_avg['country_code'] = gprc_data_avg['country_code'].str.replace('GPRC_', '')

    gprhc_data_avg = gpr_data_filtered_last_5_years[gprhc_columns].mean().reset_index()
    gprhc_data_avg.columns = ['country_code', 'avg_gprhc']
    gprhc_data_avg['country_code'] = gprhc_data_avg['country_code'].str.replace('GPRHC_', '')

    return gprc_data_avg, gprhc_data_avg, gpr_data_filtered, gpr_data_filtered_last_5_years

# Function to merge geopolitical risk data with world map data
def merge_risk_data_with_world(world, geo_risk_df, column_name):
    world = world.rename(columns={'iso_a3': 'country_code'})
    merged_world = world.merge(geo_risk_df, on='country_code', how='left')
    return merged_world

# Function to plot choropleth map
def plot_choropleth(world, output_directory, title, column_name, filename):
    fig, ax = plt.subplots(1, 1, figsize=(15, 10))
    world.boundary.plot(ax=ax, linewidth=1)
    
    # Define dynamic intervals for the color bar based on data range
    vmin, vmax = world[column_name].min(), world[column_name].max()  # Adjust range according to your data
    cmap = plt.get_cmap('YlOrRd', 10)  # Use a discrete colormap
    norm = mcolors.BoundaryNorm(boundaries=[vmin + i*(vmax-vmin)/10 for i in range(11)], ncolors=10)

    # Plotting the choropleth map with discrete color bar
    world.plot(column=column_name, ax=ax, legend=False, cmap=cmap, norm=norm, missing_kwds={"color": "lightgrey"})
    ax.set_title(title, fontsize=24)
    ax.set_axis_off()

    # Adjust the size of colorbar labels and ticks
    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    cbar = fig.colorbar(sm, ax=ax, orientation='horizontal', fraction=0.046, pad=0.04)
    cbar.ax.tick_params(labelsize=16)
    cbar.set_label('Geopolitical Risk Score', fontsize=20)

    save_path = os.path.join(output_directory, filename)
    plt.savefig(save_path)
    plt.show()
    plt.close()

# Function to plot time series for multiple countries using GPRHC
def plot_time_series_countries(data, countries, output_directory):
    data['month'] = pd.to_datetime(data['month'])
    fig, axes = plt.subplots(nrows=4, ncols=2, figsize=(18, 24), sharex=True)
    axes = axes.flatten()

    for idx, country in enumerate(countries):
        ax = axes[idx]
        col_name = f'GPRHC_{country}'
        if col_name in data.columns:
            data.plot(x='month', y=col_name, ax=ax, label=f'Historical GPRHC {country}', color='blue')
            overall_avg = data[col_name].mean()
            last_5_years_avg = data.loc[data['month'] >= data['month'].max() - pd.DateOffset(years=5), col_name].mean()
            ax.axhline(overall_avg, color='blue', linestyle='--', label='Overall Average (GPRHC)')
            ax.axhline(last_5_years_avg, color='cyan', linestyle='--', label='Last 5 Years Average (GPRHC)')
            ax.set_xlim([datetime(1900, 1, 1), data['month'].max()])
            ax.set_title(f'Time Series of Historical Geopolitical Risk for {country}', fontsize=16)
            ax.set_xlabel('Date', fontsize=12)
            ax.set_ylabel('Geopolitical Risk Score', fontsize=12)
            ax.legend()
        else:
            ax.set_title(f'No data for {country}', fontsize=16)
            ax.set_xlabel('Date', fontsize=12)
            ax.set_ylabel('Geopolitical Risk Score', fontsize=12)

    plt.tight_layout()
    save_path = os.path.join(output_directory, 'time_series_GPRHC_countries.jpg')
    plt.savefig(save_path)
    plt.close()

# Main Code
output_directory = '/Users/mjp38/GitHub/FoodTradeNetwork/outputs'  # Define your output directory
if not os.path.exists(output_directory):
    os.makedirs(output_directory)

# Load Geopolitical Risk Data
gprc_data_avg, gprhc_data_avg, gpr_data_filtered, gpr_data_filtered_last_5_years = load_geopolitical_risk_data('/Users/mjp38/GitHub/FoodTradeNetwork/inputs/data_gpr_export.csv')
print(f"Loaded geopolitical risk data: GPRC {gprc_data_avg.shape}, GPRHC {gprhc_data_avg.shape}")  # Debug statement

# Save filtered data for verification
gpr_data_filtered_last_5_years.to_csv(os.path.join(output_directory, 'filtered_gpr_data_last_5_years.csv'), index=False)

# Merge Risk Data with World Map Data
merged_world_gprc = merge_risk_data_with_world(world, gprc_data_avg, 'avg_gprc')
merged_world_gprhc = merge_risk_data_with_world(world, gprhc_data_avg, 'avg_gprhc')
print(f"Merged world data: GPRC {merged_world_gprc.shape}, GPRHC {merged_world_gprhc.shape}")  # Debug statement

# Plot Choropleth Maps
recent_date_range = f"{gpr_data_filtered_last_5_years['month'].min().date()} to {gpr_data_filtered_last_5_years['month'].max().date()}"
historical_date_range = f"{gpr_data_filtered['month'].min().date()} to {gpr_data_filtered['month'].max().date()}"

plot_choropleth(merged_world_gprc, output_directory, f'Choropleth Map of Recent Geopolitical Risk (GPRC)\n({recent_date_range})', 'avg_gprc', 'choropleth_geopolitical_risk_gprc.jpg')

plot_choropleth(merged_world_gprhc, output_directory, f'Choropleth Map of Historical Geopolitical Risk (GPRHC)\n({historical_date_range})', 'avg_gprhc', 'choropleth_geopolitical_risk_gprhc.jpg')

# Plot time series for multiple countries
countries = ['USA', 'GBR', 'JPN', 'RUS', 'DEU', 'KOR', 'MEX', 'CHN']
plot_time_series_countries(gpr_data_filtered, countries, output_directory)

print("All plots have been generated and saved to the output directory.")












