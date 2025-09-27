#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec 23 14:17:58 2024

@author: mjp38
"""

import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np

# Set style parameters for publication-quality figure
plt.style.use('seaborn-v0_8-whitegrid')
plt.rcParams['font.family'] = 'Arial'
plt.rcParams['font.size'] = 9
plt.rcParams['axes.labelsize'] = 10
plt.rcParams['axes.titlesize'] = 11

# Create sample data
crops = ['Rice', 'Maize', 'Temp. Cereals', 'Trop. Cereals', 'Trop. Roots', 'Rapeseed']

metrics = {
    'Density': {
        'early': [0.118, 0.143, 0.202, 0.112, 0.089, 0.132],
        'late': [0.132, 0.156, 0.218, 0.128, 0.156, 0.145]
    },
    'Centralization': {
        'early': [1.245, 1.770, 1.613, 1.324, 0.665, 1.432],
        'late': [1.156, 0.889, 1.488, 1.287, 1.684, 1.378]
    },
    'Reciprocity': {
        'early': [0.289, 0.299, 0.312, 0.201, 0.245, 0.338],
        'late': [0.301, 0.312, 0.328, 0.334, 0.298, 0.357]
    },
    'Mean Degree': {
        'early': [4.2, 5.1, 6.8, 3.9, 2.8, 4.5],
        'late': [4.8, 6.2, 7.4, 4.7, 4.9, 5.1]
    }
}

# Create figure and axes
fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(10, 8))
axes = [ax1, ax2, ax3, ax4]
titles = ['A. Network Density', 'B. Network Centralization', 
          'C. Reciprocity', 'D. Mean Degree']
metrics_keys = list(metrics.keys())

# Color palette
colors = ['#2171b5', '#74c476']  # Blue for early, green for late

# Plot each metric
for ax, title, metric in zip(axes, titles, metrics_keys):
    x = np.arange(len(crops))
    width = 0.35
    
    # Create bars
    ax.bar(x - width/2, metrics[metric]['early'], width, 
           label='1996-2000', color=colors[0], alpha=0.8)
    ax.bar(x + width/2, metrics[metric]['late'], width,
           label='2016-2020', color=colors[1], alpha=0.8)
    
    # Customize each subplot
    ax.set_title(title, pad=10)
    ax.set_xticks(x)
    ax.set_xticklabels(crops, rotation=45, ha='right')
    
    # Add gridlines
    ax.yaxis.grid(True, linestyle='--', alpha=0.7)
    ax.set_axisbelow(True)
    
    # Remove top and right spines
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

# Add legend to the first subplot
axes[0].legend(bbox_to_anchor=(0., 1.02, 2., .102), loc='lower left',
               ncol=2, mode="expand", borderaxespad=0.)

# Adjust layout
plt.tight_layout()

# Save figure
plt.savefig('network_metrics.png', dpi=300, bbox_inches='tight')