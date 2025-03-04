##### PRELIMINARY #####

# Use the magic command to reset the namespace
from IPython import get_ipython
get_ipython().run_line_magic('reset', '-sf')

# import modules
import os
import pyreadstat as rstat
import pandas as pd
import numpy as np
import econtools.metrics as mt
from econtools import outreg
import matplotlib.pyplot as plt

# Dynamically get the username and construct the path
username = os.getlogin()  # Get the current username

# Construct the base path
base_path = os.path.join("Shared drives", 
                         "Projects", 
                         "2025", 
                         "Orwell", 
                         "Breadcrumbs", 
                         "10 Quantitative Narrative Testing", 
                         "9 Main Survey")

# Fiky's directory
if username in ["elgha"]:
    working_folder = os.path.join(r"G:\\", base_path)
    os.chdir(working_folder)
elif username in ["user"]:
    working_folder = os.path.join(r"H:\\", base_path)
    os.chdir(working_folder)    
    
# Setup folder path
raw = os.path.join(working_folder, "2a input")
temp = os.path.join(working_folder, "2b temp")
output = os.path.join(working_folder, "2c output")    
log = os.path.join(working_folder, "3 log") 
gph = os.path.join(working_folder, "4 figures") 
tbl = os.path.join(working_folder, "5 tables") 

##### DATA SETUP #####

# Set date of data
date = "20250304"

# Set data
import_item = raw + "\\raw_" + date + ".sav"

# Load data
rdf, meta = rstat.read_sav(import_item)

# Create sex variable
sexlab = {1: 'Male', 2: 'Female', 3: 'Refuse to disclose', 4: 'Others'}
rdf['sex'] = rdf['ID03'].map(sexlab)

##### LENGTH OF SURVEY #####
# Create minutes and hours column
rdf['minutes'] = rdf['LOI'] / 60
rdf['hours'] = rdf['LOI'] / (60**2)

# Summarize 'minutes' column
print("######################")
print('Summary statistics of length of survey (in minutes)')
print(rdf['minutes'].describe(percentiles=[.25, .5, .75]))
print("######################")

## Checking outliers
# Calculate the first (Q1) and third (Q3) quartiles
Q1 = rdf['minutes'].quantile(0.25)
Q3 = rdf['minutes'].quantile(0.75)

# Compute the Interquartile Range (IQR)
IQR = Q3 - Q1

# Define the threshold for high outliers
threshold = 1.5  # Commonly used value; adjust as needed

# Calculate the upper bound for outliers
upper_bound = Q3 + threshold * IQR

# Create a new column 'High_Outlier' to flag high outliers
rdf['High_Outlier'] = rdf['minutes'] > upper_bound

# Summarize high outlier
print("######################")
print('% and tabulation of high outliers in survey time')
print(rdf['High_Outlier'].mean())
print(rdf['High_Outlier'].value_counts())
print("######################")

print("######################")
print('Survey time of high outliers (in minutes)')
print(rdf[rdf['High_Outlier'] == True]['minutes'].describe(percentiles=[.25, .5, .75]))
print('Survey time of high outliers (in hours)')
print(rdf[rdf['High_Outlier'] == True]['hours'].describe(percentiles=[.25, .5, .75]))
print("######################")

##### QUOTA #####

# Define (supposed) sample size
sampsize = 4000

## SEX AND AGE ##
# Drop third sex
qrdf = rdf[~(rdf['ID03']==3)].copy()

# Adjust bins to ensure 18-25 includes 18, and the rest are as intended
bins = [17, 25, 30, 40, 64]  # The first bin starts at 17 to include 18
labels = ['18-25', '26-30', '31-40', '41-64']  # Labels for the age groups

# Create the age group variable
qrdf.loc[:, 'age_group'] = pd.cut(qrdf['ID04'], bins=bins, labels=labels, right=True)

# Group by 'sex' and 'age_group' and count the number of occurrences
sexage_counts = qrdf.groupby(['sex', 'age_group'], observed=False).size().reset_index(name='current_counts')

# Calculate percentage of each group
sexage_counts['share_actual']=(sexage_counts['current_counts']/sampsize)*100

# Import agreed quota
import_item = raw + "\\sex_age_group_share.xlsx"
saqdf = pd.read_excel(import_item)
saqdf = saqdf.rename(columns={'Share': 'share_plan'})

# Join agreed vs actual
qdf1 = pd.merge(sexage_counts,saqdf,on=['sex','age_group'])

# Calculate difference in quota
qdf1['share_difference'] = qdf1['share_actual'] - qdf1['share_plan']

# Export result
export_item = output + "\\quota_sex_age_" + date + ".xlsx"
qdf1.to_excel(export_item, index=False)

## PROVINCE ##
# Create a dictionary to map province codes to names
province_dict = {
    1: 'Aceh',
    2: 'Sumatera Utara',
    3: 'Sumatera Barat',
    4: 'Riau',
    5: 'Jambi',
    6: 'Sumatera Selatan',
    7: 'Bengkulu',
    8: 'Lampung',
    9: 'Kepulauan Bangka Belitung',
    10: 'Kepulauan Riau',
    11: 'DKI Jakarta',
    12: 'Jawa Barat',
    13: 'Jawa Tengah',
    14: 'DI Yogyakarta',
    15: 'Jawa Timur',
    16: 'Banten',
    17: 'Bali',
    18: 'Nusa Tenggara Barat',
    19: 'Nusa Tenggara Timur',
    20: 'Kalimantan Barat',
    21: 'Kalimantan Tengah',
    22: 'Kalimantan Selatan',
    23: 'Kalimantan Timur',
    24: 'Kalimantan Utara',
    25: 'Sulawesi Utara',
    26: 'Sulawesi Tengah',
    27: 'Sulawesi Selatan',
    28: 'Sulawesi Tenggara',
    29: 'Gorontalo',
    30: 'Sulawesi Barat',
    31: 'Maluku',
    32: 'Maluku Utara',
    33: 'Papua Barat',
    34: 'Papua'
}

# Recode province
# Papua Barat: 33-34 to 33
# Papua: 35-38 to 34

# Recode 34 to 33
rdf.loc[rdf['ID01'] == 34, 'ID01'] = 33

# Recode values from 35 to 38 to 34
rdf.loc[rdf['ID01'].between(35, 38), 'ID01'] = 34

# Create province variable with label
rdf['province'] = rdf['ID01'].map(province_dict)

# Group by province and count the number of occurrences
prov_counts = rdf.groupby(['province','ID01'], observed=False).size().reset_index(name='current_counts')
prov_counts = prov_counts.sort_values(by='ID01')

# Calculate percentage of each group
prov_counts['share_actual']=(prov_counts['current_counts']/sampsize)*100

# Import agreed quota
import_item = raw + "\\province_weighted_percentage.xlsx"
provqdf = pd.read_excel(import_item)
provqdf = provqdf.rename(columns={'Percentage': 'share_plan', 'Province': 'province'})
provqdf['plan_counts']=(provqdf['share_plan']/100)*sampsize

# Join agreed vs actual
qdf2 = pd.merge(prov_counts,provqdf,on=['ID01','province'],how='outer')

# Replace NaN values in 'current_counts' and 'share_actual' with 0
qdf2[['current_counts', 'share_actual']] = qdf2[['current_counts', 'share_actual']].fillna(0)

# Calculate difference in quota
qdf2['share_difference'] = qdf2['share_actual'] - qdf2['share_plan']

# Calculate difference in counts
qdf2['count_difference'] = qdf2['current_counts'] - qdf2['plan_counts']

# Export result
export_item = output + "\\quota_province_" + date + ".xlsx"
qdf2.to_excel(export_item, index=False)

##### CONTROL AND TREATMENT DISTRIBUTION #####
# Group by experimental group and count the number of occurrences
treat_counts = rdf.groupby('lfCB', observed=False).size().reset_index(name='current_counts')
treat_counts = treat_counts.rename(columns={'lfCB': 'experiment_group'})

# Calculate distance against target
conditions = [
    treat_counts['experiment_group'].between(1, 5),
    treat_counts['experiment_group'] == 6
]
values = [630, 850]
treat_counts['target_counts'] = np.select(conditions, values, default=np.nan)
treat_counts['counts_difference'] = treat_counts['current_counts'] - treat_counts['target_counts']

# Export result
export_item = output + "\\treat_group_dist_" + date + ".xlsx"
treat_counts.to_excel(export_item, index=False)

##### BALANCE TEST #####

## Create dummies of treatment groups
for i in range(1, 7):
    rdf[f'treat{i}'] = rdf['lfCB'] == i

## REGION
# Define conditions
conditions = [
    rdf['ID01'].between(1, 16) | rdf['ID01'].isin([20, 21]),
    rdf['ID01'].between(17, 19) | rdf['ID01'].between(22, 30)
]

# Define corresponding region values
region_values = [1, 2]

# Apply conditions to create the 'region' column
rdf['region'] = np.select(conditions, region_values, default=3)

# Loop through region values and create binary columns dynamically
for i in range(1, 4):
    rdf[f'region{i}'] = rdf['region'] == i

## URBAN/RURAL
rdf['urban'] = rdf['ID02'] == 1

## SEX 
rdf['male'] = rdf['ID03'] == 1

## MARITAL STATUS
rdf['unmarried'] = rdf['ID05'] == 1

## EDUCATION
for i in range(1, 6):
    rdf[f'edu{i}'] = rdf['ID06'] == i

## GENDER HOUSEHOLD HEAD
rdf['hhhead_female'] = rdf['RT01'] == 2

## SOCIAL ASSISTANCE
rdf['nosocast'] = rdf['RT02'] == 3

## HOUSEHOLD SIZE
rdf['hhsize'] = rdf[['RT03r1', 'RT03r2', 'RT03r3']].sum(axis=1)    

## REGRESSIONS
# Dictionary to store results dynamically
results_dict = {}
for char in ['region1',
             'region2',
             'region3',
             'urban',
             'male',
             'ID04',
             'unmarried',
             'edu1',
             'edu2',
             'edu3',
             'edu4',
             'edu5',
             'hhhead_female',
             'nosocast',
             'hhsize']:
    results_dict[char] = mt.reg(rdf, char, ['treat1','treat2','treat3','treat4','treat5'], 
                                vce_type='robust', addcons=True)
    print("######################")
    print("Baseline balance test: " + char)
    print(results_dict[char])
    print("######################")

# Store result in a document
# Define the groups of variables and their corresponding names
groups = [
    (['region1', 'region2', 'region3', 'urban', 'male', 'ID04', 'unmarried'], 'group1'),
    (['edu1', 'edu2', 'edu3', 'edu4', 'edu5', 'hhhead_female', 'nosocast', 'hhsize'], 'group2')
]

# Function to process each group
def process_group(variables, group_name):
    # Retrieve regression results
    regs = tuple(results_dict[var] for var in variables)
    # Generate the balance test
    balance_test = outreg(regs)
    # Define the export path
    export_path = f"{log}\\balance_test_{group_name}_{date}.tex"
    # Save the balance test to a file
    with open(export_path, "w") as f:
        f.write(balance_test)

# Process each group
for variables, group_name in groups:
    process_group(variables, group_name)

##### CORRECT INTERPRETATION OF MESSAGE #####

# Setup variables
for i in ['A', 'B', 'C', 'E']:
    rdf[f'correct_CB01{i}'] = rdf[f'CB01{i}'] == 1
    rdf[f'correct_CB01{i}'] = rdf[f'correct_CB01{i}'].astype('boolean')
rdf['correct_CB01D'] = rdf['CB01D'].isin((2, 3))
rdf['correct_CB01D'] = rdf['correct_CB01D'].astype('boolean')

rdf.loc[rdf['lfCB'] != 1, 'correct_CB01A'] = np.nan
rdf.loc[rdf['lfCB'] != 2, 'correct_CB01B'] = np.nan
rdf.loc[rdf['lfCB'] != 3, 'correct_CB01C'] = np.nan
rdf.loc[rdf['lfCB'] != 4, 'correct_CB01D'] = np.nan
rdf.loc[rdf['lfCB'] != 5, 'correct_CB01E'] = np.nan

# Summarize results  
print("######################") 
print('% of respondents correctly interpreting the message') 
for i in ['A', 'B', 'C', 'D', 'E']:
     pct = (rdf[f'correct_CB01{i}'].mean())*100
     statement = f"{pct:.2f}% respondents in treatment {i} correctly interpreted the message"
     print(statement)
print("######################")   

# Bar chart  
# Step 1: Calculate the means of the specified columns
columns_of_interest = [f'correct_CB01{chr(i)}' for i in range(ord('A'), ord('F'))]
means = rdf[columns_of_interest].mean()

# Step 1: Plot the means using a bar chart
fig, ax = plt.subplots()
bars = ax.bar(means.index, means, color='skyblue', edgecolor='black')

# Step 2: Format the title with thousand separator
ax.set_title('Fraction of correct message interpretation')
plt.xlabel('Treatment groups')

# Step 3: Set x-axis labels to 1, 2, 3, etc.
ax.set_xticklabels(range(1, len(means) + 1))

# Step 4: Display data labels above each bar and remove the y-axis
ax.bar_label(bars, fmt='%.2f', padding=3)
ax.yaxis.set_visible(False)

# Step 5: Set y-axis limits to range from 0 to 1
ax.set_ylim(0, 1)

# Step 6: Remove the grid
ax.grid(False)

# Step 7: Remove the box around the plot
for spine in ax.spines.values():
    spine.set_visible(False)

# Step 8: Save the plot to a file
export_item = f"{gph}\\correct_interpretation_{date}.png"
plt.savefig(export_item, format='png', dpi=300, bbox_inches='tight')

# Show the plot
plt.show()