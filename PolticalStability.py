import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import os
import matplotlib.colors as mcolors

# Load the dataset
file_path = '/Users/mjp38/GitHub/FoodTradeNetwork/inputs/PolticalStability.csv'
data = pd.read_csv(file_path)

# Convert year columns to numeric, coercing errors to NaN
for col in data.columns[4:]:
    data[col] = pd.to_numeric(data[col], errors='coerce')

# Calculate average for specified periods
data['2014-2016'] = data[['2014 [YR2014]', '2015 [YR2015]', '2016 [YR2016]']].mean(axis=1)
data['2020-2022'] = data[['2020 [YR2020]', '2021 [YR2021]', '2022 [YR2022]']].mean(axis=1)

# Load World Map Data
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
world = world[(world['pop_est'] > 0) & (world['name'] != "Antarctica")]  # Remove Antarctica and tiny population countries

# Function to create a choropleth map for a specific period
def plot_choropleth(ax, world, data, period):
    data_period = data[['Country Code', period]].rename(columns={period: 'Stability'})
    
    world = world.rename(columns={'iso_a3': 'Country Code'})
    merged_world = world.merge(data_period, on='Country Code', how='left')
    
    vmin, vmax = -2.5, 2.5
    boundaries = [vmin + i * 0.5 for i in range(int((vmax - vmin) / 0.5) + 1)]
    cmap = plt.get_cmap('coolwarm', len(boundaries) - 1)
    norm = mcolors.BoundaryNorm(boundaries, cmap.N, clip=True)
    
    merged_world.plot(column='Stability', ax=ax, legend=False, cmap=cmap, norm=norm, missing_kwds={"color": "lightgrey"})
    ax.set_title(f'Average {period}', fontsize=24)
    ax.set_axis_off()
    
    return boundaries, cmap, norm

# Main Code
output_directory = '/Users/mjp38/GitHub/FoodTradeNetwork/outputs'
if not os.path.exists(output_directory):
    os.makedirs(output_directory)

# Create a two-panel plot
fig, axes = plt.subplots(1, 2, figsize=(20, 10), constrained_layout=True)
periods = ['2014-2016', '2020-2022']

boundaries, cmap, norm = None, None, None
for ax, period in zip(axes.flatten(), periods):
    b, c, n = plot_choropleth(ax, world, data, period)
    if b is not None and c is not None and n is not None:
        boundaries, cmap, norm = b, c, n

# Adjust colorbar to match all subplots
if boundaries is not None and cmap is not None and norm is not None:
    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])

    # Add colorbar to the figure
    cbar = fig.colorbar(sm, ax=axes.ravel().tolist(), orientation='horizontal', fraction=0.05, pad=0.02, boundaries=boundaries, ticks=boundaries)
    cbar.ax.tick_params(labelsize=16)
    cbar.set_label('Political Stability Score', fontsize=20)

# Save the figure
save_path = os.path.join(output_directory, 'two_panel_choropleth_political_stability.jpg')
plt.savefig(save_path)
plt.show()
plt.close()

print("Two-panel choropleth plot has been generated and saved to the output directory.")




