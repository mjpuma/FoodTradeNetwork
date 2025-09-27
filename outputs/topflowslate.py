import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Set style for publication-quality figure
plt.style.use('seaborn-v0_8-whitegrid')
plt.rcParams['font.family'] = 'Arial'
plt.rcParams['font.size'] = 12

# Read data
total_flows_late = pd.read_csv('TopFlows_late_total.csv')
total_flows_early = pd.read_csv('TopFlows_early_total.csv')

# Create figure
fig, ax = plt.subplots(figsize=(12, 8))

# Prepare data
total_flows_late['Period'] = '2016-2020'
total_flows_early['Period'] = '1996-2000'
total_flows_late['Flow'] = total_flows_late.apply(lambda x: f"{x['From']} → {x['To']}", axis=1)
total_flows_early['Flow'] = total_flows_early.apply(lambda x: f"{x['From']} → {x['To']}", axis=1)

# Combine all unique flows
all_flows = pd.concat([
    total_flows_late[['Flow', 'virtual_water_km3', 'Period']],
    total_flows_early[['Flow', 'virtual_water_km3', 'Period']]
])

# Find max value for each flow for sorting
max_by_flow = all_flows.groupby('Flow')['virtual_water_km3'].max()
sorted_flows = max_by_flow.sort_values(ascending=True).index

# Prepare data for plotting
y_pos = np.arange(len(sorted_flows))
width = 0.35

# Create dictionaries for easy value lookup
early_dict = total_flows_early.set_index('Flow')['virtual_water_km3'].to_dict()
late_dict = total_flows_late.set_index('Flow')['virtual_water_km3'].to_dict()

# Get values maintaining order
early_values = [early_dict.get(flow, 0) for flow in sorted_flows]
late_values = [late_dict.get(flow, 0) for flow in sorted_flows]

# Plot bars
bars_early = ax.barh(y_pos - width/2, early_values, width, 
                    label='1996-2000', color='#2171b5', alpha=0.5)
bars_late = ax.barh(y_pos + width/2, late_values, width, 
                   label='2016-2020', color='#2171b5', alpha=0.9)

# Add labels and styling
ax.set_yticks(y_pos)
ax.set_yticklabels(sorted_flows, fontsize=11)
ax.set_title('Major Virtual Water Trade Flows', pad=20, fontsize=14, fontweight='bold')
ax.set_xlabel('Virtual Water Flow (km³/year)', fontsize=12)

# Add value labels
for i, (early_val, late_val) in enumerate(zip(early_values, late_values)):
    if early_val > 0:
        ax.text(early_val + max(max_by_flow) * 0.02, i - width/2, 
               f'{early_val:.1f}', va='center', fontsize=10)
    if late_val > 0:
        ax.text(late_val + max(max_by_flow) * 0.02, i + width/2, 
               f'{late_val:.1f}', va='center', fontsize=10)

# Customize appearance
ax.legend(loc='lower right', fontsize=11)
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.grid(True, axis='x', linestyle='--', alpha=0.7)
ax.set_axisbelow(True)

# Adjust layout and save
plt.tight_layout()
plt.savefig('top_flows_evolution_simplified.png', dpi=300, bbox_inches='tight', 
            facecolor='white', edgecolor='none')