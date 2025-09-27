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
                frames.append(df)
    return pd.concat(frames, ignore_index=True)

# Plotting and saving function
def plot_country_comparison(metrics_df, iso3, metric, output_directory, commodity):
    fig, ax = plt.subplots(figsize=(10, 6))
    data = metrics_df[(metrics_df['iso3'] == iso3) & (metrics_df['Commodity'] == commodity)]
    sns.lineplot(x='Year', y=metric, data=data, marker='o', label=f'{iso3} - {metric}', ax=ax)
    
    # Compare with the mean of all other countries
    mean_data = metrics_df[metrics_df['iso3'] != iso3].groupby('Year').agg({metric: 'mean'}).reset_index()
    sns.lineplot(x='Year', y=metric, data=mean_data, marker='o', linestyle='--', label=f'Average of Others - {metric}', ax=ax)
    
    plt.title(f'Trend of {metric} for {iso3} ({commodity}) Compared to Others')
    plt.legend()
    plt.grid(True)
    
    # Save the plot
    filename = f"{iso3}_{commodity}_{metric}.png"
    save_path = os.path.join(output_directory, filename)
    plt.savefig(save_path)
    #plt.close()

# Directories and Parameters
data_directory = '/Users/mjp38/GitHub/FSC-WorldModelers/outputs/'
output_directory = '/Users/mjp38/GitHub/FSC-WorldModelers/media/'  # Ensure this directory exists or add code to create it
year_groups = [[2000, 2001], [2002, 2003], [2004, 2005], [2006, 2007], [2008, 2009], [2010, 2011], [2012, 2013], [2014, 2015], [2016, 2017], [2018, 2019], [2020, 2021]]
commodities = ['Banana', 'Coffee', 'Barley', 'Sugarcane', 'Wheat', 'Soybean', 'RapeseedOil', 'PalmOil', 'Groundnuts', 'Cocoa', 'Rice', 'Cassava']

# Load Data
metrics_df = load_metrics_data(data_directory, commodities, year_groups)

# ISO3 code and commodity for the plot
selected_country_iso3 = 'JPN'
selected_commodity = 'Banana'  # Example commodity
selected_metric = 'degree_in'  # Example metric

# Plot and save
plot_country_comparison(metrics_df, selected_country_iso3, selected_metric, output_directory, selected_commodity)
