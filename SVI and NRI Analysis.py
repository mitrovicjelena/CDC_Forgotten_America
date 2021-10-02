# -*- coding: utf-8 -*-
"""
Created on Mon Sep 27 16:19:54 2021

@author: Sam
"""

import os
import pandas as pd
import numpy as np
from pandasql import sqldf

################################
# LOAD DATA                    #
################################
os.chdir(r'C:/Users/Sam/OneDrive/Documents/Personal/Carolina Data Challenge/01. Data Sets')
svi_orig = pd.read_csv(r'SVI_2014.csv')
nri_orig = pd.read_csv(r'NRI Data/NRI_Table_CensusTracts.csv')

# create copies for analysis
svi = svi_orig
nri = nri_orig


################################
# DATA CLEANING                #
################################
# add population density and area designation
svi['pop_density'] = svi['E_TOTPOP']/svi['AREA_SQMI']
svi['area_type'] = np.where((svi['pop_density'] > 1000) & (svi['E_GROUPQ']/svi['E_TOTPOP'] < 0.4), 'Urbanized Area', 'Rural')

# count of urban and rural
svi.groupby('area_type').size()

# remove columns with E_TOTPOP = 0 and SPL_THEMES = -999
svi = svi[(svi['E_TOTPOP'] != 0) & (svi['SPL_THEMES']!=-999)].reset_index()


################################
# MERGE DATA SETS              #
################################
combined = pd.merge(svi.iloc[:, np.r_[1,4:10,11:78,82:84,88:90,92:94,99:132,-1]], nri.iloc[:, np.r_[10:366,-1]], how = 'inner', left_on = 'FIPS', right_on = 'TRACTFIPS')

# send to csv
combined.to_csv('C:/Users/Sam/OneDrive/Documents/Personal/Carolina Data Challenge/01. Data Sets/combined.csv', index = False)

################################
# BASIC DATA EXPLORATION       #
################################
# E_TOTPOP vs POPULATION
((combined['E_TOTPOP'] - combined['POPULATION'])/combined['E_TOTPOP']).std()

combined['pop_difference'] = combined['E_TOTPOP'] - combined['POPULATION']

combined['pop_difference_percent'] = combined['pop_difference']/combined['E_TOTPOP']

# view E_TOTPOP vs. POPULATION
pd.set_option('max_columns', None)
print(combined[['FIPS','COUNTY','STATE','E_TOTPOP','POPULATION','pop_difference', 'pop_difference_percent']].sort_values('pop_difference_percent').head())
print(combined[['FIPS','COUNTY','STATE','E_TOTPOP','POPULATION','pop_difference', 'pop_difference_percent']].sort_values('pop_difference_percent', ascending = False).head())
pd.reset_option('max_columns')
    # use E_TOTPOP values

