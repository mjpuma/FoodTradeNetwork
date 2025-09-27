#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jun 25 12:03:50 2024

@author: mjp38
"""

import pandas as pd

# Load the filtered data
filtered_data_path = '/Users/mjp38/GitHub/FoodTradeNetwork/outputs/filtered_gpr_data_last_5_years.csv'
filtered_gpr_data_last_5_years = pd.read_csv(filtered_data_path)

# Display the first few rows of the dataframe
print(filtered_gpr_data_last_5_years.head())

# Check the number of unique country codes in the filtered data
unique_countries = filtered_gpr_data_last_5_years.columns[1:]  # Skip the 'month' column
num_countries = len(unique_countries)

print(f"Number of unique countries in the filtered data: {num_countries}")
print(f"Unique countries: {unique_countries}")
