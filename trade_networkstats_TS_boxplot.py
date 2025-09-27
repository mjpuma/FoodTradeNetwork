import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os
import networkx as nx
from tqdm import tqdm

def load_trade_data(directory, commodities, year_groups):
    frames = []
    for commodity in tqdm(commodities, desc="Loading commodities"):
        for years in year_groups:
            year_label = f"Avg_{years[0]}_{years[1]}"
            file_path = os.path.join(directory, f'{commodity}_{year_label}E0.csv')
            if os.path.exists(file_path):
                df = pd.read_csv(file_path, index_col=0)
                df = df.reset_index().melt(id_vars='index', var_name='partner', value_name='trade_volume')
                df = df.rename(columns={'index': 'reporter'})
                df['Commodity'] = commodity
                df['Year Group'] = f"{years[0]}-{years[1]}"
                frames.append(df)
            else:
                print(f"File not found: {file_path}")
    
    if not frames:
        print("No files were found matching the expected pattern.")
        return pd.DataFrame()
    
    return pd.concat(frames, ignore_index=True)

def calculate_network_metrics(trade_data):
    metrics = []
    total_groups = trade_data.groupby(['Commodity', 'Year Group']).ngroups
    
    for (commodity, year_group), group in tqdm(trade_data.groupby(['Commodity', 'Year Group']), total=total_groups, desc="Calculating metrics"):
        print(f"Processing {commodity} for {year_group}")
        
        # Filter out zero trade volumes
        group = group[group['trade_volume'] > 0]
        
        if group.empty:
            print(f"No trade data for {commodity} in {year_group}")
            continue
        
        G = nx.from_pandas_edgelist(group, 'reporter', 'partner', 'trade_volume', create_using=nx.DiGraph())
        
        print(f"  Network created with {G.number_of_nodes()} nodes and {G.number_of_edges()} edges")
        
        in_degree = dict(G.in_degree())
        out_degree = dict(G.out_degree())
        in_strength = dict(G.in_degree(weight='trade_volume'))
        out_strength = dict(G.out_degree(weight='trade_volume'))
        
        print("  Calculating community structure...")
        communities = nx.community.greedy_modularity_communities(G.to_undirected())
        community_count = len(communities)
        community_modularity = nx.community.modularity(G.to_undirected(), communities)
        
        print("  Calculating global clustering coefficient...")
        global_clustering_coefficient = nx.average_clustering(G, weight='trade_volume')
        
        print("  Calculating eigenvector centrality...")
        try:
            eigenvector_centrality = nx.eigenvector_centrality(G, weight='trade_volume', max_iter=1000)
        except nx.PowerIterationFailedConvergence:
            print("  Eigenvector centrality calculation failed to converge")
            eigenvector_centrality = {node: 0 for node in G.nodes()}
        
        print("  Calculating closeness centrality...")
        closeness_centrality = nx.closeness_centrality(G)
        
        print("  Calculating betweenness centrality...")
        betweenness_centrality = nx.betweenness_centrality(G, weight='trade_volume')
        
        print("  Calculating PageRank...")
        pagerank = nx.pagerank(G, weight='trade_volume')
        
        print("  Calculating clustering coefficient...")
        clustering_coefficient = nx.clustering(G, weight='trade_volume')
        
        for node in G.nodes():
            metrics.append({
                'Commodity': commodity,
                'Year Group': year_group,
                'Country': node,
                'In Degree': in_degree.get(node, 0),
                'Out Degree': out_degree.get(node, 0),
                'In Strength': in_strength.get(node, 0),
                'Out Strength': out_strength.get(node, 0),
                'Total Degree': in_degree.get(node, 0) + out_degree.get(node, 0),
                'Eigenvector Centrality': eigenvector_centrality.get(node, 0),
                'Closeness Centrality': closeness_centrality.get(node, 0),
                'Betweenness Centrality': betweenness_centrality.get(node, 0),
                'PageRank': pagerank.get(node, 0),
                'Clustering Coefficient': clustering_coefficient.get(node, 0),
                'community_count': community_count,
                'community_modularity': community_modularity,
                'global_clustering_coefficient': global_clustering_coefficient
            })
        
        print(f"Completed processing {commodity} for {year_group}")
    
    return pd.DataFrame(metrics)

def plot_metric_distributions(metrics_df, iso3, metric, output_directory, figure_number):
    country_data = metrics_df[metrics_df['Country'] == iso3]
    
    if country_data.empty:
        print(f"No data available for country {iso3}")
        return
    
    order = country_data.groupby('Commodity')[metric].median().sort_values(ascending=False).index
    palette = sns.color_palette("husl", len(order))
    
    fig, axes = plt.subplots(nrows=2, ncols=1, figsize=(14, 16))
    
    time_series_data = country_data.groupby(['Year Group', 'Commodity'])[metric].mean().unstack()
    colors = {commodity: palette[idx] for idx, commodity in enumerate(order)}
    
    for commodity in order:
        if commodity in time_series_data.columns:
            axes[0].plot(time_series_data.index, time_series_data[commodity], marker='o', label=commodity, color=colors[commodity])
    
    axes[0].set_title(f'Figure {figure_number}: Time Series of {metric} for {iso3} (2000-2021)', fontsize=16)
    axes[0].set_xlabel('Year Group', fontsize=14)
    axes[0].set_ylabel(metric, fontsize=14)
    axes[0].tick_params(axis='x', rotation=45, labelsize=12)
    axes[0].tick_params(axis='y', labelsize=12)
    axes[0].legend(title='Commodity', fontsize=12, title_fontsize=14)
    
    sns.boxplot(ax=axes[1], x='Commodity', y=metric, data=country_data, order=order, palette=colors)
    axes[1].set_title(f'Figure {figure_number}: Pooled Distribution of {metric} for {iso3} Sorted by Median', fontsize=16)
    axes[1].set_xlabel('Commodity', fontsize=14)
    axes[1].set_ylabel(metric, fontsize=14)
    axes[1].tick_params(axis='x', rotation=45, labelsize=12)
    axes[1].tick_params(axis='y', labelsize=12)
    
    plt.tight_layout()
    
    save_path = os.path.join(output_directory, f'Figure_{figure_number}_{iso3}_{metric.replace(" ", "_")}_time_series_and_boxplot.jpg')
    plt.savefig(save_path)
    print(f"Plot saved to: {save_path}")
    plt.show()
    plt.close(fig)

def generate_commodity_summary(metrics_df):
    summaries = []
    for commodity in metrics_df['Commodity'].unique():
        commodity_data = metrics_df[metrics_df['Commodity'] == commodity]
        summary = {
            'Commodity': commodity,
            'Years': f"{commodity_data['Year Group'].min()} to {commodity_data['Year Group'].max()}",
            'Number of Countries': commodity_data['Country'].nunique()
        }
        
        for metric in ['In Degree', 'Out Degree', 'In Strength', 'Out Strength', 'Total Degree', 'Eigenvector Centrality', 
                       'Closeness Centrality', 'Betweenness Centrality', 'PageRank', 'Clustering Coefficient',
                       'community_count', 'community_modularity', 'global_clustering_coefficient']:
            summary[f'{metric} (Mean)'] = commodity_data[metric].mean()
            summary[f'{metric} (Median)'] = commodity_data[metric].median()
            summary[f'{metric} (Max)'] = commodity_data[metric].max()
            
        summaries.append(summary)
    
    return pd.DataFrame(summaries)

# Directories and Parameters
data_directory = '/Users/mjp38/GitHub/FoodTradeNetwork/inputs_processed/'
output_directory = '/Users/mjp38/GitHub/FoodTradeNetwork/media/'
year_groups = [[2000, 2001], [2002, 2003], [2004, 2005], [2006, 2007], [2008, 2009], 
               [2010, 2011], [2012, 2013], [2014, 2015], [2016, 2017], [2018, 2019], [2020, 2021]]
commodities = ['Banana', 'Coffee', 'Barley', 'Sugarcane', 'Wheat', 'Soybean', 
               'RapeseedOil', 'PalmOil', 'Groundnuts', 'Cocoa', 'Rice', 'Cassava']

# Load Data
print("Loading trade data...")
try:
    trade_data = load_trade_data(data_directory, commodities, year_groups)
    print(f"Loaded trade data shape: {trade_data.shape}")
    print(f"Columns in loaded data: {trade_data.columns}")
    print(f"Unique commodities in loaded data: {trade_data['Commodity'].unique()}")
    print(f"Unique year groups in loaded data: {trade_data['Year Group'].unique()}")
except Exception as e:
    print(f"Error loading trade data: {str(e)}")
    raise

# Check if trade_data is empty
if trade_data.empty:
    print("No trade data was loaded. Please check your data files and paths.")
    exit()

# Calculate network metrics
print("Calculating network metrics...")
try:
    metrics_df = calculate_network_metrics(trade_data)
    print(f"Calculated metrics data shape: {metrics_df.shape}")
    print(f"Columns in metrics data: {metrics_df.columns}")
except Exception as e:
    print(f"Error calculating network metrics: {str(e)}")
    raise

# Generate commodity summary
print("Generating commodity summary...")
commodity_summary = generate_commodity_summary(metrics_df)
print(commodity_summary)

# Save commodity summary to CSV
summary_path = os.path.join(output_directory, 'commodity_network_metrics_summary.csv')
commodity_summary.to_csv(summary_path, index=False)
print(f"Commodity summary saved to: {summary_path}")

# Analyze Japan's metrics
print("\nAnalyzing Japan's metrics:")
japan_metrics = metrics_df[metrics_df['Country'] == 'JPN']

for commodity in japan_metrics['Commodity'].unique():
    japan_commodity_data = japan_metrics[japan_metrics['Commodity'] == commodity]
    print(f"\nAnalysis for {commodity}:")
    for metric in ['In Degree', 'Out Degree', 'In Strength', 'Out Strength', 'Eigenvector Centrality', 'Betweenness Centrality', 'PageRank']:
        japan_value = japan_commodity_data[metric].mean()
        overall_mean = commodity_summary[commodity_summary['Commodity'] == commodity][f'{metric} (Mean)'].values[0]
        print(f"  {metric}: Japan's average ({japan_value:.4f}) vs Overall mean ({overall_mean:.4f})")

# ISO3 code for the plot
selected_country_iso3 = 'JPN'

# Define the metrics in the specified order
network_metrics = [
    'community_count',  # Figure A1
    'community_modularity',  # Figure A2
    'global_clustering_coefficient',  # Figure A3
    'In Degree',  # Figure A4
    'In Strength',  # Figure A5
    'Clustering Coefficient',  # Figure A6 (local clustering coefficient)
    'Eigenvector Centrality',  # Figure A7
    'PageRank'  # Figure A8
]

# Plot and save distribution boxplots for each metric
print("Generating plots...")
for i, metric in enumerate(tqdm(network_metrics, desc="Plotting metrics")):
    try:
        figure_number = f"A{i+1}"
        plot_metric_distributions(metrics_df, selected_country_iso3, metric, output_directory, figure_number)
    except Exception as e:
        print(f"Error plotting {metric}: {str(e)}")

print("All operations completed.")