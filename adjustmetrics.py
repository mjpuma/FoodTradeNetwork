#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun 14 02:59:30 2024

@author: mjp38
"""

import pandas as pd

# Paths to the input files
tvsi_input_path = '/Users/mjp38/GitHub/FoodTradeNetwork/outputs/TVSI_Values_by_Country_and_Commodity.csv'
tpp_input_path = '/Users/mjp38/GitHub/FoodTradeNetwork/outputs/TPP_Values_by_Country_and_Commodity.csv'
rtii_input_path = '/Users/mjp38/GitHub/FoodTradeNetwork/outputs/RTII_Values_by_Country_and_Commodity.csv'
output_directory = '/Users/mjp38/GitHub/FoodTradeNetwork/outputs/'

# Function to process the file and save the top N values for each commodity
def process_and_save_top_n(input_path, output_path, metric, n):
    df = pd.read_csv(input_path)
    top_df = pd.DataFrame()
    commodities = df['Commodity'].unique()
    for commodity in commodities:
        commodity_df = df[df['Commodity'] == commodity]
        sorted_commodity_df = commodity_df.sort_values(by=metric, ascending=False).head(n)
        top_df = pd.concat([top_df, sorted_commodity_df])
    top_df.to_csv(output_path, index=False)

# Number of top values to select
n = 20  # You can change this to any number you prefer

# Process TVSI file
tvsi_output_path = f'{output_directory}Top_{n}_TVSI_Values_by_Commodity.csv'
process_and_save_top_n(tvsi_input_path, tvsi_output_path, 'TVSI', n)

# Process TPP file
tpp_output_path = f'{output_directory}Top_{n}_TPP_Values_by_Commodity.csv'
process_and_save_top_n(tpp_input_path, tpp_output_path, 'TPP', n)

# Process RTII file
rtii_output_path = f'{output_directory}Top_{n}_RTII_Values_by_Commodity.csv'
process_and_save_top_n(rtii_input_path, rtii_output_path, 'RTII', n)

