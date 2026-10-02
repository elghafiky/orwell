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
#    working_folder = os.path.join(r"G:\\", base_path) # laptop
    working_folder = os.path.join(r"H:\\", base_path) # computer
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

##### STRAIGHTLINING CHECK #####
# Straightlining = giving the same answer to every item in a grid (a battery
# of items that share one response scale). This section measures how common
# it is, what it relates to, whether it clusters, and how much the published
# treatment effects depend on respondents who straightlined.
# Definitions and thresholds were fixed in writing before computing; see
# "Straightlining check - inventory and definitions v0.2.md" (Cowork folder).
# Only pandas, numpy and scipy.special are needed here (scipy.stats is avoided
# because one of its libraries is blocked on some managed machines). The section uses raw survey
# variables from 'rdf' and does not rely on the recodes made above.
# Figures in the paper are unweighted, so every rate here is unweighted too.

from scipy import special

## SETTINGS
sl_seed = 859687378      # same seed as the main analysis
sl_nperm = 2000          # number of random sets in the clustering test
speed_main = 0.30        # speeding: page time below 30% of the grid's median
speed_alt = 0.50         # sensitivity: below 50% of the grid's median
min_unit_n = 30          # clustering units smaller than this are flagged

# Grids with 5+ items on one response scale (data question numbers)
# 'rating' grids feed (or could feed) substantive figures; 'binary' grids are
# used as inconsistency markers because their items are keyed in both
# directions, so a uniform answer contradicts itself.
grids = {
    'CB07': {'items': [f'CB07r{i}' for i in range(1, 7)], 'page': 'pagetimeCB07',
             'type': 'rating', 'points': 6, 'reverse': 'None',
             'published': 'Yes (WP Fig. 2; OA Tables OA.2-OA.3)'},
    'CB08': {'items': [f'CB08r{i}' for i in range(1, 6)], 'page': 'pagetimeCB08',
             'type': 'rating', 'points': 6, 'reverse': 'None',
             'published': 'No'},
    'QDK':  {'items': [f'QDKr{i}' for i in range(1, 14)], 'page': 'pagetimeQDK',
             'type': 'rating', 'points': 5, 'reverse': 'None (content runs both ways)',
             'published': 'Yes (WP Figs. 3-4; OA Tables OA.2-OA.3)'},
    'NP':   {'items': [f'NPr{i}' for i in range(1, 9)], 'page': 'pagetimeNP',
             'type': 'binary', 'points': 2, 'reverse': 'Yes (autonomy/conformity poles switch sides)',
             'published': 'No'},
    'SB':   {'items': [f'SBr{i}' for i in range(1, 14)], 'page': 'pagetimeSB',
             'type': 'binary', 'points': 2, 'reverse': 'Yes (items 5, 7, 9, 10, 13)',
             'published': 'Covariate (sdbi) in all regressions'},
}
rating_grids = [g for g in grids if grids[g]['type'] == 'rating']

# Social desirability (Marlowe-Crowne short form) keying: items 5, 7, 9, 10, 13
# are keyed opposite to the other eight
sb_pos = [5, 7, 9, 10, 13]
sb_neg = [i for i in range(1, 14) if i not in sb_pos]

# Labels for the answer straightliners gave
value_labels = {
    'CB07': {1: 'Strongly disagree', 2: 'Disagree', 3: 'Slightly disagree',
             4: 'Slightly agree', 5: 'Agree', 6: 'Strongly agree'},
    'QDK':  {1: 'Strongly oppose', 2: 'Oppose', 3: 'Neither (midpoint)',
             4: 'Support', 5: 'Strongly support'},
    'NP':   {1: 'Always left option', 2: 'Always right option'},
    'SB':   {1: 'Always "true"', 2: 'Always "false"'},
}
value_labels['CB08'] = value_labels['CB07']

# Log file: every summary printed below is also written here
sl_logfile = os.path.join(log, f"straightlining_check_{date}.txt")
with open(sl_logfile, 'w', encoding='utf-8') as f:
    f.write(f"STRAIGHTLINING CHECK - data {date} - run {pd.Timestamp.now():%Y-%m-%d %H:%M}\n")

def sl_log(text):
    """Print a line and append it to the log file."""
    print(text)
    with open(sl_logfile, 'a', encoding='utf-8') as f:
        f.write(str(text) + "\n")

def chi2_p(tab):
    """Pearson chi-square test of independence (no continuity correction)."""
    obs = np.asarray(tab, dtype=float)
    if obs.shape[0] < 2 or obs.shape[1] < 2:
        return np.nan
    expected = obs.sum(axis=1, keepdims=True) * obs.sum(axis=0, keepdims=True) / obs.sum()
    chi2 = ((obs - expected) ** 2 / expected).sum()
    return special.chdtrc((obs.shape[0] - 1) * (obs.shape[1] - 1), chi2)

def welch_p(a, b):
    """Two-sided p-value of Welch's t-test for a difference in means."""
    va, vb = a.var(ddof=1) / len(a), b.var(ddof=1) / len(b)
    t = (a.mean() - b.mean()) / np.sqrt(va + vb)
    df = (va + vb) ** 2 / (va ** 2 / (len(a) - 1) + vb ** 2 / (len(b) - 1))
    return 2 * special.stdtr(df, -abs(t))

def wilson(k, n, z=1.96):
    """Wilson 95% confidence interval for a proportion k/n."""
    if n == 0:
        return (np.nan, np.nan)
    p = k / n
    centre = (p + z**2 / (2 * n)) / (1 + z**2 / n)
    half = z * np.sqrt(p * (1 - p) / n + z**2 / (4 * n**2)) / (1 + z**2 / n)
    return (centre - half, centre + half)

## RESPONDENT-LEVEL MEASURES
sl = pd.DataFrame({'uuid': rdf['uuid']})

for g, spec in grids.items():
    X = rdf[spec['items']].to_numpy(dtype=float)
    k = X.shape[1]
    codes = np.unique(X[~np.isnan(X)])
    # count how many items carry each answer code, per respondent
    counts = np.column_stack([(X == c).sum(axis=1) for c in codes])
    modal_count = counts.max(axis=1)
    n_valid = (~np.isnan(X)).sum(axis=1)
    sl[f'{g}_nvalid'] = n_valid
    # strict: same answer on all k items (no grid has a "don't know" code and
    # no answers are missing, so the "don't know included" definition is the
    # same as strict in this survey)
    sl[f'{g}_strict'] = (modal_count == k) & (n_valid == k)
    # all but one: the most common answer covers at least k-1 items
    sl[f'{g}_abo'] = modal_count >= k - 1
    sl[f'{g}_abo_exact'] = modal_count == k - 1
    sl[f'{g}_modalshare'] = modal_count / k
    sl[f'{g}_ms80'] = sl[f'{g}_modalshare'] >= 0.8
    # the answer given, recorded for strict straightliners only
    sl[f'{g}_slvalue'] = np.where(sl[f'{g}_strict'], X[:, 0], np.nan)
    if spec['type'] == 'rating':
        sl[f'{g}_sd'] = np.nanstd(X, axis=1, ddof=1)
        sl[f'{g}_sd05'] = sl[f'{g}_sd'] <= 0.5
    # speeding on this grid's page, relative to the grid's median page time
    page = rdf[spec['page']].astype(float)
    sl[f'{g}_speed30'] = page < speed_main * page.median()
    sl[f'{g}_speed50'] = page < speed_alt * page.median()

# Social desirability: share of opposite-keyed item pairs answered the same
# way (1 = every pair answered alike, i.e. no attention to keying)
n_true_neg = (rdf[[f'SBr{i}' for i in sb_neg]] == 1).sum(axis=1)
n_true_pos = (rdf[[f'SBr{i}' for i in sb_pos]] == 1).sum(axis=1)
sl['SB_sameside'] = (n_true_neg * n_true_pos
                     + (len(sb_neg) - n_true_neg) * (len(sb_pos) - n_true_pos)) / (len(sb_neg) * len(sb_pos))

# Inconsistency markers (uniform answers on the two bidirectional grids)
sl['SB_incons'] = sl['SB_strict']
sl['NP_incons'] = sl['NP_strict']
sl['any_incons'] = sl['SB_incons'] | sl['NP_incons']

# Number of grids straightlined per respondent
sl['n_rating_strict'] = sl[[f'{g}_strict' for g in rating_grids]].sum(axis=1)
sl['n_rating_abo'] = sl[[f'{g}_abo' for g in rating_grids]].sum(axis=1)
sl['n_all_strict'] = sl[[f'{g}_strict' for g in grids]].sum(axis=1)
sl['any_rating_strict'] = sl['n_rating_strict'] > 0

# Whole-survey speeding (the vendor already removed completes under 12 minutes)
sl['loi_p10'] = rdf['LOI'] < rdf['LOI'].quantile(0.10)
sl['loi_p25'] = rdf['LOI'] < rdf['LOI'].quantile(0.25)

# Attention check: correct reading of the stimulus's main message (CB01),
# treatment arms 1-5 only; the control arm has no such question (left missing)
correct_code = {'A': [1], 'B': [1], 'C': [1], 'D': [2, 3], 'E': [1]}
sl['fail_cb01'] = np.nan
for arm, letter in enumerate(['A', 'B', 'C', 'D', 'E'], start=1):
    inarm = rdf['lfCB'] == arm
    sl.loc[inarm, 'fail_cb01'] = (~rdf.loc[inarm, f'CB01{letter}'].isin(correct_code[letter])).astype(float)

# Background characteristics (same age bands as the quota check above)
sl['age_group'] = pd.cut(rdf['ID04'], bins=[17, 25, 30, 40, 64],
                         labels=['18-25', '26-30', '31-40', '41-64']).astype(str)
sl['gender'] = rdf['ID03'].map({1: 'Male', 2: 'Female'})   # 7 'other/refused' left missing
sl['education'] = np.where(rdf['ID06'] == 5, 'Tertiary', 'Senior secondary or less')

# Clustering units (no device, panel source or recruitment channel in the data)
start = pd.to_datetime(rdf['start_date'])
sl['fw_day'] = start.dt.strftime('%Y-%m-%d')
sl['time_block'] = pd.cut(start.dt.hour, bins=[-1, 5, 11, 17, 23],
                          labels=['00-06', '06-12', '12-18', '18-24']).astype(str)
sl['day_block'] = sl['fw_day'] + ' ' + sl['time_block']
sl['arm'] = rdf['lfCB'].astype(int).astype(str)
sl['conjoint_version'] = rdf['Conjoint_Version'].astype(int).astype(str)   # randomised: placebo unit

N = len(sl)
sl_log("######################")
sl_log(f"Straightlining check: n = {N}")

## TABLE: RATES BY GRID AND DEFINITION (with chance baseline)
def chance_strict(g):
    """Expected strict-straightlining rate if each item were answered
    independently, drawing from that item's observed answer distribution."""
    X = rdf[grids[g]['items']]
    codes = np.unique(X.to_numpy()[~np.isnan(X.to_numpy())])
    shares = np.array([[(X[c] == v).mean() for c in X.columns] for v in codes])
    return shares.prod(axis=1).sum()

definitions = {'strict': 'Strict straightline (main)',
               'abo': 'All but one (modal answer on >= k-1 items)',
               'abo_exact': 'Exactly k-1 items on the modal answer',
               'ms80': 'Modal share >= 0.8',
               'sd05': 'Battery SD <= 0.5 (rating grids only)'}
rows = []
for g, spec in grids.items():
    for d, dlab in definitions.items():
        col = f'{g}_{d}'
        if col not in sl:
            continue
        kk = int(sl[col].sum())
        lo, hi = wilson(kk, N)
        rows.append({'grid': g, 'items': len(spec['items']), 'definition': d,
                     'definition_label': dlab, 'n': N, 'count': kk,
                     'rate': kk / N, 'ci95_low': lo, 'ci95_high': hi,
                     'chance_rate': chance_strict(g) if d == 'strict' else np.nan})
t_rates = pd.DataFrame(rows)
t_rates['ratio_to_chance'] = t_rates['rate'] / t_rates['chance_rate']

## TABLE: GRID FEATURES (length, scale, reverse items) next to the rates
t_grids = pd.DataFrame([{
    'grid': g, 'items': len(s['items']), 'scale_points': s['points'], 'type': s['type'],
    'reverse_worded_items': s['reverse'], 'published': s['published'],
    'strict_rate': sl[f'{g}_strict'].mean(), 'chance_strict_rate': chance_strict(g),
    'abo_rate': sl[f'{g}_abo'].mean(), 'mean_modal_share': sl[f'{g}_modalshare'].mean(),
    'median_page_seconds': rdf[s['page']].median(),
    'median_seconds_per_item': rdf[s['page']].median() / len(s['items'])}
    for g, s in grids.items()])

sl_log("Strict straightlining by grid (rate; chance rate):")
for _, r in t_grids.iterrows():
    sl_log(f"  {r['grid']:5s} k={r['items']:2d}  strict={r['strict_rate']:.3f}  "
           f"chance={r['chance_strict_rate']:.4f}  all-but-one={r['abo_rate']:.3f}")

## TABLE: WHICH ANSWER STRAIGHTLINERS GAVE
rows = []
for g in grids:
    v = sl.loc[sl[f'{g}_strict'], f'{g}_slvalue'].value_counts().sort_index()
    for val, cnt in v.items():
        rows.append({'grid': g, 'answer_code': int(val),
                     'answer_label': value_labels[g].get(int(val), ''),
                     'count': int(cnt), 'share_of_straightliners': cnt / v.sum(),
                     'share_of_sample': cnt / N})
t_values = pd.DataFrame(rows)

## TABLE: HOW MANY GRIDS EACH RESPONDENT STRAIGHTLINED
t_count = pd.concat([
    sl['n_rating_strict'].value_counts().sort_index().rename('count').to_frame()
      .assign(measure='Rating grids strict-straightlined (of 3)'),
    sl['n_rating_abo'].value_counts().sort_index().rename('count').to_frame()
      .assign(measure='Rating grids all-but-one (of 3)'),
    sl['n_all_strict'].value_counts().sort_index().rename('count').to_frame()
      .assign(measure='All grids strict-straightlined (of 5)'),
]).rename_axis('number_of_grids').reset_index()
t_count['share'] = t_count['count'] / N
t_count = t_count[['measure', 'number_of_grids', 'count', 'share']]

sl_log(f"Any rating grid strict-straightlined: {sl['any_rating_strict'].mean():.3f}; "
       f"SB or NP inconsistent: {sl['any_incons'].mean():.3f}")

## TABLE: CORRELATES
# For each grid and definition, the straightlining rate within each level of
# a correlate, with a chi-square test of independence.
def correlate_rows(flag, group, grid, dlab, cname):
    df = pd.DataFrame({'f': sl[flag], 'g': group}).dropna()
    out = []
    p = chi2_p(pd.crosstab(df['g'], df['f']))
    for lev, sub in df.groupby('g'):
        out.append({'grid': grid, 'definition': dlab, 'correlate': cname,
                    'level': str(lev), 'n': len(sub), 'rate': sub['f'].mean(),
                    'chi2_p': p})
    return out

rows = []
for g in grids:
    for d in ['strict', 'abo']:
        flag = f'{g}_{d}'
        corr = {
            'Speeding on this grid (<30% of median page time)': sl[f'{g}_speed30'],
            'Speeding on this grid (<50% of median) [sensitivity]': sl[f'{g}_speed50'],
            'Whole survey: LOI bottom decile': sl['loi_p10'],
            'Whole survey: LOI bottom quartile [sensitivity]': sl['loi_p25'],
            'Failed CB01 message check (arms 1-5)': sl['fail_cb01'],
            'Age group': sl['age_group'],
            'Gender': sl['gender'],
            'Education': sl['education'],
        }
        if grids[g]['type'] == 'rating':
            corr['SB or NP inconsistent'] = sl['any_incons']
        for cname, grp in corr.items():
            rows += correlate_rows(flag, grp, g, d, cname)
t_corr = pd.DataFrame(rows)

# Continuous check: social-desirability same-side share by straightlining
rows = []
for g in rating_grids:
    for d in ['strict', 'abo']:
        a = sl.loc[sl[f'{g}_{d}'], 'SB_sameside']
        b = sl.loc[~sl[f'{g}_{d}'], 'SB_sameside']
        rows.append({'grid': g, 'definition': d, 'mean_SB_sameside_straightliners': a.mean(),
                     'mean_SB_sameside_others': b.mean(), 'difference': a.mean() - b.mean(),
                     'welch_t_p': welch_p(a, b)})
t_sbcont = pd.DataFrame(rows)

## TABLE: CLUSTERING (random-set test)
# For each unit (e.g. a fieldwork day) we compare its straightlining rate with
# the rates of 2,000 random sets of respondents of the same size drawn from
# the whole sample. Drawing a random set of n people from N and counting the
# straightliners is a hypergeometric draw, so we simulate it directly.
# p = share of random sets at least as far from the overall rate as the unit.
# Holm correction within each grid x definition x unit type.
rng = np.random.default_rng(sl_seed)
unit_types = {'Fieldwork day': 'fw_day',
              'Start-time block (platform clock) [sensitivity]': 'time_block',
              'Day x time block [sensitivity]': 'day_block',
              'Treatment arm': 'arm',
              'Conjoint version (randomised placebo)': 'conjoint_version'}

def holm(pvals):
    """Holm step-down adjustment of a list of p-values."""
    p = np.asarray(pvals, dtype=float)
    order = np.argsort(p)
    adj = np.empty_like(p)
    running = 0
    for rank, idx in enumerate(order):
        running = max(running, min(1, (len(p) - rank) * p[idx]))
        adj[idx] = running
    return adj

rows = []
for g in grids:
    for d in ['strict', 'abo']:
        f = sl[f'{g}_{d}'].astype(int)
        K = int(f.sum())
        overall = K / N
        for utype, ucol in unit_types.items():
            block = []
            for unit, sub in f.groupby(sl[ucol]):
                n_u = len(sub)
                obs = sub.mean()
                sims = rng.hypergeometric(K, N - K, n_u, size=sl_nperm) / n_u
                extreme = (np.abs(sims - overall) >= abs(obs - overall) - 1e-12).sum()
                block.append({'grid': g, 'definition': d, 'unit_type': utype,
                              'unit': unit, 'n': n_u, 'rate': obs,
                              'overall_rate': overall,
                              'random_set_p05': np.quantile(sims, 0.025),
                              'random_set_p95': np.quantile(sims, 0.975),
                              'perm_p': (extreme + 1) / (sl_nperm + 1),
                              'small_unit': n_u < min_unit_n})
            adj = holm([b['perm_p'] for b in block])
            for b, a in zip(block, adj):
                b['perm_p_holm'] = a
            rows += block
t_clust = pd.DataFrame(rows)

## TABLE: STRAIGHTLINING BY TREATMENT ARM
# Rating grids CB07, CB08 and QDK come after the stimulus, so a difference
# between arms would mean the stimulus changed who straightlines. That matters
# for the sensitivity check below: dropping straightliners would then break
# the comparability that random assignment provides.
rows = []
for g in grids:
    for d in ['strict', 'abo']:
        p = chi2_p(pd.crosstab(sl['arm'], sl[f'{g}_{d}']))
        rates = sl.groupby('arm')[f'{g}_{d}'].mean()
        row = {'grid': g, 'definition': d, 'chi2_p_across_arms': p}
        for a, r in rates.items():
            row[f'rate_arm{a}'] = r
        rows.append(row)
t_arm = pd.DataFrame(rows)

sl_log("Straightlining by arm (chi-square p, strict):")
for _, r in t_arm[t_arm['definition'] == 'strict'].iterrows():
    sl_log(f"  {r['grid']:5s} p = {r['chi2_p_across_arms']:.3f}")

## SENSITIVITY OF PUBLISHED TREATMENT EFFECTS
# Re-estimates Model 1 of the paper (OLS, robust SE, no covariates; arm 4 is
# not analysed) for each outcome from a published grid, in the full sample and
# after excluding groups of respondents. Model 2 (lasso-selected covariates)
# and Westfall-Young p-values are not reproduced. The shifts show how much a
# figure depends on these respondents; they are not corrections.
def ols_robust(y, arms_in, treat_arms):
    """OLS of y on arm dummies with Stata-style robust (HC1) standard errors."""
    X = np.column_stack([np.ones(len(y))] + [(arms_in == a).astype(float) for a in treat_arms])
    XtX_inv = np.linalg.inv(X.T @ X)
    beta = XtX_inv @ X.T @ y
    e = y - X @ beta
    n, k = X.shape
    V = XtX_inv @ (X.T * e**2) @ X @ XtX_inv * n / (n - k)
    se = np.sqrt(np.diag(V))
    p = 2 * special.stdtr(n - k, -np.abs(beta / se))
    return beta[1:], se[1:], p[1:], n

# Outcome-to-arm pairings as pre-registered and coded in "4a main analysis.do"
pairings = {
    'CB07': {1: [1, 2, 5], 2: [2, 3], 3: [2], 4: [1, 5], 5: [1, 5], 6: [1, 5]},
    'QDK':  {1: [1, 2, 5], 2: [1, 3, 5], 3: [2, 3], 4: [1, 2, 3], 5: [1, 3],
             6: [1, 5], 7: [1, 5], 8: [1, 2], 9: [2, 3], 10: [1, 5],
             11: [1, 2], 12: [2, 3], 13: [2, 3]},
}
binary_rule = {'CB07': (5, 6), 'QDK': (4, 5)}    # "agree" / "support" as in the paper
binary_name = {'CB07': 'agree', 'QDK': 'support'}

samples = {
    'Full sample': lambda g: pd.Series(True, index=sl.index),
    'Excl. this grid strict straightliners': lambda g: ~sl[f'{g}_strict'],
    'Excl. this grid all-but-one [sensitivity]': lambda g: ~sl[f'{g}_abo'],
    'Excl. any rating-grid strict straightliner': lambda g: ~sl['any_rating_strict'],
    'Excl. SB- or NP-inconsistent': lambda g: ~sl['any_incons'],
    'Excl. this grid strict straightliners who also sped': lambda g: ~(sl[f'{g}_strict'] & sl[f'{g}_speed30']),
}

rows = []
ctrl_rows = []
arm_num = rdf['lfCB'].astype(int).to_numpy()
for g, pairs in pairings.items():
    lo_b, hi_b = binary_rule[g]
    for i, tarms in pairs.items():
        raw_y = rdf[f'{g}r{i}'].astype(float).to_numpy()
        for form in ['binary', 'likert']:
            y_all = ((raw_y >= lo_b) & (raw_y <= hi_b)).astype(float) if form == 'binary' else raw_y
            outcome = f'{binary_name[g]}{i}' if form == 'binary' else f'{g}r{i}'
            base = np.isin(arm_num, tarms + [6])     # control + paired arms only
            full_coef = {}
            for sname, sfun in samples.items():
                keep = base & sfun(g).to_numpy()
                b, se, p, n = ols_robust(y_all[keep], arm_num[keep], tarms)
                for a, bb, ss, pp in zip(tarms, b, se, p):
                    if sname == 'Full sample':
                        full_coef[a] = bb
                    rows.append({'grid': g, 'outcome': outcome, 'form': form,
                                 'arm': a, 'sample': sname, 'n': n, 'coef': bb,
                                 'se': ss, 'p': pp, 'shift_vs_full': bb - full_coef[a]})
                ctrl = keep & (arm_num == 6)
                ctrl_rows.append({'grid': g, 'outcome': outcome, 'form': form,
                                  'sample': sname, 'n_control': int(ctrl.sum()),
                                  'control_mean': y_all[ctrl].mean()})
t_eff = pd.DataFrame(rows)
t_eff['flag_shift'] = np.where(t_eff['form'] == 'binary',
                               t_eff['shift_vs_full'].abs() >= 0.02,
                               t_eff['shift_vs_full'].abs() >= 0.10)
t_eff['sig_full_p05'] = t_eff.groupby(['outcome', 'form', 'arm'])['p'].transform(lambda s: s.iloc[0] < 0.05)
t_eff['sig_here_p05'] = t_eff['p'] < 0.05

t_ctrl = pd.DataFrame(ctrl_rows)
t_ctrl['shift_vs_full'] = t_ctrl['control_mean'] - t_ctrl.groupby(['outcome', 'form'])['control_mean'].transform('first')

# Validation: full-sample binary estimates must match the paper's stored Model 1
t_valid = []
for g, fname in [('CB07', 'CB07_wyoung_linear_model1_uncond.dta'),
                 ('QDK', 'DK_wyoung_linear_model1_uncond.dta')]:
    path = os.path.join(temp, fname)
    if os.path.exists(path):
        paper = pd.read_stata(path)[['outcome', 'familyp', 'coef', 'stderr']]
        paper['arm'] = paper['familyp'].str.replace('treat', '').astype(int)
        mine = t_eff[(t_eff['grid'] == g) & (t_eff['form'] == 'binary') & (t_eff['sample'] == 'Full sample')]
        m = mine.merge(paper, on=['outcome', 'arm'], how='left')
        m['coef_diff'] = m['coef_x'] - m['coef_y']
        m['se_diff'] = m['se'] - m['stderr']
        t_valid.append(m[['grid', 'outcome', 'arm', 'coef_x', 'coef_y', 'coef_diff', 'se', 'stderr', 'se_diff']]
                       .rename(columns={'coef_x': 'coef_here', 'coef_y': 'coef_paper',
                                        'se': 'se_here', 'stderr': 'se_paper'}))
t_valid = pd.concat(t_valid) if t_valid else pd.DataFrame()
if len(t_valid):
    sl_log(f"Validation vs paper Model 1: max |coef diff| = {t_valid['coef_diff'].abs().max():.2e}, "
           f"max |SE diff| = {t_valid['se_diff'].abs().max():.2e}")

flagged = t_eff[(t_eff['sample'] != 'Full sample') & t_eff['flag_shift']]
sl_log(f"Effect estimates shifting by >= 2 pp (binary) or >= 0.1 (Likert): {len(flagged)} "
       f"of {int((t_eff['sample'] != 'Full sample').sum())} re-estimates")
sig_change = t_eff[(t_eff['sample'] != 'Full sample') & (t_eff['sig_full_p05'] != t_eff['sig_here_p05'])]
sl_log(f"Re-estimates crossing p = 0.05 (unadjusted): {len(sig_change)}")

## README AND EXPORT
readme = pd.DataFrame({'item': [
    'Purpose', 'Data', 'Weights', 'Definitions', 'Grids',
    'rates', 'grids', 'answer_given', 'grid_count', 'correlates', 'sb_sameside',
    'clustering', 'by_arm', 'effects', 'control_means', 'validation',
    'Caveats', 'Not reproduced', 'Privacy'],
    'description': [
    'Straightlining check (same answer on every item of a grid) for the Orwell narrative-testing RCT.',
    f'raw_{date}.sav, n = {N}. The vendor had already removed completes under 12 minutes.',
    'None. Published figures are unweighted.',
    'Fixed before computing in "Straightlining check - inventory and definitions v0.2.md". '
    'Strict = same answer on all k items; all but one = modal answer on >= k-1 items; modal share = modal count / k; '
    'battery SD = sample SD of the k answers. No grid has a "don\'t know" code and nothing is missing, so the '
    '"don\'t know included" definition equals strict.',
    'Rating grids: CB07 (6 items, 1-6), CB08 (5, 1-6), QDK (13, 1-5); none has reverse-worded items. Binary grids used as '
    'inconsistency markers: NP (8 forced-choice pairs), SB (13 true/false, Marlowe-Crowne short form, items 5, 7, 9, 10, 13 reverse-keyed).',
    'Rate per grid and definition, Wilson 95% CI, and the chance rate if items were answered independently.',
    'Grid features (length, scale, reverse items, publication) next to the rates.',
    'Which answer strict straightliners gave.',
    'How many grids each respondent straightlined.',
    'Straightlining rate by speeding, attention check, age, gender, education and inconsistency, with Pearson chi-square p.',
    'Social-desirability same-side share (share of opposite-keyed pairs answered alike) for straightliners vs others.',
    f'Unit rate vs {sl_nperm} random sets of the same size; perm_p two-sided; Holm within grid x definition x unit type. '
    'Time blocks use the platform clock (time zone not documented).',
    'Straightlining rate by treatment arm with chi-square p. The rating grids are post-treatment.',
    'Model 1 re-estimated (OLS, HC1 robust SE, no covariates) for each published CB07 and QDK outcome in six samples; '
    'flag_shift = |shift| >= 0.02 (binary) or >= 0.10 (Likert).',
    'Control-arm mean of each outcome in each sample.',
    'Full-sample binary estimates compared with the paper\'s stored Model 1 estimates (2b temp).',
    'Straightlining is not proof of bad data: without reverse-worded items a uniform answer may be genuine. '
    'Excluding respondents on a post-treatment behaviour can bias treatment effects; the shifts are a check, not a correction.',
    'Model 2 (lasso-selected covariates), ordered logit and Westfall-Young p-values.',
    'No names, contact details, IP addresses, free text or timestamps in any output.']})

export_item = output + "\\straightlining_check_" + date + ".xlsx"
with pd.ExcelWriter(export_item, engine='openpyxl') as xw:
    for name, tab in [('README', readme), ('rates', t_rates), ('grids', t_grids),
                      ('answer_given', t_values), ('grid_count', t_count),
                      ('correlates', t_corr), ('sb_sameside', t_sbcont),
                      ('clustering', t_clust), ('by_arm', t_arm), ('effects', t_eff),
                      ('control_means', t_ctrl), ('validation', t_valid)]:
        tab.to_excel(xw, sheet_name=name, index=False)

# Respondent-level flags: respondent ID and flags/measures only
flag_cols = ['uuid'] + [c for c in sl.columns
                        if any(c.startswith(g + '_') for g in grids) and not c.endswith('_nvalid')] + \
            ['any_incons', 'n_rating_strict', 'n_rating_abo', 'n_all_strict',
             'any_rating_strict', 'loi_p10', 'loi_p25', 'fail_cb01']
export_item = temp + "\\straightlining_flags_" + date + ".csv"
sl[flag_cols].to_csv(export_item, index=False)

sl_log(f"Exported: straightlining_check_{date}.xlsx (2c output), straightlining_flags_{date}.csv (2b temp)")
sl_log("######################")


##### RESPONSE-STYLE DIRECTION CHECK #####
# Could the published treatment effects come from a response style (answering
# "agree" or "true" whatever the content, i.e. yea-saying) rather than from
# genuine opinion change? Three checks, written up in "Direction check -
# Reimagining Development - note v0.2.md" (Cowork folder):
# 1. Do any pairs of outcome items genuinely contradict each other? In the
#    control arm, a genuine pair correlates negatively on raw codes, and fewer
#    respondents agree with both items than chance predicts.
# 2. Signs of the published effects. More yea-saying raises agreement with
#    every item, so an effect that LOWERS agreement cannot come from it.
# 3. Does treatment shift acquiescence on the balanced social desirability
#    battery (SB)? Per arm, pooled across arms, and jointly.
# Uses t_eff, ols_robust and sb_pos/sb_neg from the straightlining section.

dc_logfile = os.path.join(log, f"direction_check_{date}.txt")
with open(dc_logfile, 'w', encoding='utf-8') as f:
    f.write(f"RESPONSE-STYLE DIRECTION CHECK - data {date} - run {pd.Timestamp.now():%Y-%m-%d %H:%M}\n")

def dc_log(text):
    """Print a line and append it to the direction-check log file."""
    print(text)
    with open(dc_logfile, 'a', encoding='utf-8') as f:
        f.write(str(text) + "\n")

def ols_hc1(y, X):
    """OLS with Stata-style robust (HC1) covariance; returns beta, V, residual df."""
    XtX_inv = np.linalg.inv(X.T @ X)
    beta = XtX_inv @ X.T @ y
    e = y - X @ beta
    n, k = X.shape
    V = XtX_inv @ (X.T * e**2) @ X @ XtX_inv * n / (n - k)
    return beta, V, n - k

def wald_f(beta, V, idx, df):
    """Robust Wald F-test that the coefficients in positions idx are all zero."""
    b = beta[idx]
    F = b @ np.linalg.inv(V[np.ix_(idx, idx)]) @ b / len(idx)
    return F, special.fdtrc(len(idx), df, F)

def dummies(arm_values, levels):
    """Constant plus one indicator per level in 'levels' (others are the reference)."""
    return np.column_stack([np.ones(len(arm_values))] + [(arm_values == a).astype(float) for a in levels])

arm_all = rdf['lfCB'].astype(int).to_numpy()
dc_log("######################")

## 1. CANDIDATE OPPOSITE PAIRS (control arm, raw codes)
# "agree" = 4 or higher: slightly agree or more on the 1-6 scales (CB07, CB08),
# big role / support on the 1-5 scales (CB11, QDK)
pairs = [
    ('CB07r4', 'CB07r6', 'wealth gaps natural vs govt choices decide who holds power'),
    ('CB07r4', 'CB07r5', 'wealth gaps natural vs laws decide chances of success'),
    ('CB07r2', 'CB07r3', "govt may relocate people vs citizens shouldn't just accept govt"),
    ('CB08r1', 'CB08r4', 'govt as ruler (penguasa) vs steward (pengurus)'),
    ('CB08r1', 'CB08r5', 'govt as ruler vs referee (wasit)'),
    ('QDKr13', 'QDKr3', 'forest clearing for infrastructure vs green tech budget'),
    ('QDKr12', 'QDKr2', 'forest clearing for settlement vs green transport budget'),
    ('QDKr11', 'QDKr9', 'forest clearing incl. biofuel vs industry green subsidy'),
    ('CB11r1', 'CB11r4', 'laws matter for economy vs fate matters'),
]
ctl = rdf[rdf['lfCB'] == 6]
rows = []
for a, b, label in pairs:
    xa, xb = ctl[a].astype(float), ctl[b].astype(float)
    agree_a, agree_b = xa >= 4, xb >= 4
    rows.append({'item_a': a, 'item_b': b, 'content': label, 'n_control': len(ctl),
                 'raw_r': np.corrcoef(xa, xb)[0, 1],
                 'agree_both': (agree_a & agree_b).mean(),
                 'agree_both_if_independent': agree_a.mean() * agree_b.mean()})
t_pairs = pd.DataFrame(rows)
t_pairs['genuine_opposites'] = t_pairs['raw_r'] < 0
dc_log(f"Opposite-pair check: {int(t_pairs['genuine_opposites'].sum())} of {len(t_pairs)} candidate pairs "
       f"correlate negatively (raw r from {t_pairs['raw_r'].min():+.2f} to {t_pairs['raw_r'].max():+.2f})")

## 2. SIGNS OF THE PUBLISHED EFFECTS (Model 1, Likert form, full sample)
eff = t_eff[(t_eff['sample'] == 'Full sample') & (t_eff['form'] == 'likert')].copy()
eff['significant'] = eff['p'] < 0.05
eff['direction'] = np.where(eff['coef'] < 0, 'lowers agreement', 'raises agreement')
t_signs = (eff[eff['significant']].groupby(['grid', 'direction']).size()
           .unstack(fill_value=0).reset_index())
t_signs_arm = (eff[eff['significant']].groupby(['arm', 'direction']).size()
               .unstack(fill_value=0).reset_index())
n_sig, n_low = int(eff['significant'].sum()), int((eff['significant'] & (eff['coef'] < 0)).sum())
dc_log(f"Published effects: {n_sig} of {len(eff)} significant (p < .05); {n_low} lower agreement, "
       f"{n_sig - n_low} raise it")

# Average of CB07d and CB07f, arms 1 and 5 vs control: genuine persuasion
# moves the two items in opposite directions and leaves the average flat
keep = np.isin(arm_all, [1, 5, 6])
y_avg = ((rdf['CB07r4'] + rdf['CB07r6']) / 2).astype(float).to_numpy()[keep]
b, se, p, n = ols_robust(y_avg, arm_all[keep], [1, 5])
t_pairavg = pd.DataFrame({'arm': [1, 5], 'effect_on_average': b, 'se': se, 'p': p, 'n': n})

## 3. ACQUIESCENCE ON THE BALANCED SOCIAL DESIRABILITY BATTERY
# ACQ = average of the share answering "true" (code 1) on the 5 true-keyed
# items and on the 8 false-keyed items, so it is balanced across keying
endorse = (rdf[[f'SBr{i}' for i in range(1, 14)]] == 1).astype(float)
acq = ((endorse[[f'SBr{i}' for i in sb_pos]].mean(axis=1)
        + endorse[[f'SBr{i}' for i in sb_neg]].mean(axis=1)) / 2).to_numpy()
acq_sd = acq.std(ddof=1)

# per arm (control = arm 6)
beta, V, df = ols_hc1(acq, dummies(arm_all, [1, 2, 3, 4, 5]))
se = np.sqrt(np.diag(V))
t_acq_arm = pd.DataFrame({
    'arm': [1, 2, 3, 4, 5], 'effect': beta[1:], 'se': se[1:],
    'p': 2 * special.stdtr(df, -np.abs(beta[1:] / se[1:])),
    'effect_sd': beta[1:] / acq_sd,
    'ci95_low_sd': (beta[1:] - 1.96 * se[1:]) / acq_sd,
    'ci95_high_sd': (beta[1:] + 1.96 * se[1:]) / acq_sd})

# pooled and joint, for the four arms analysed in the paper and for all five
# fielded arms. Per-arm and joint tests have little power against a shift
# common to every arm; pooling all treated arms targets that case.
rows = []
for label, arms in [('Arms 1, 2, 3, 5 (analysed in the paper)', [1, 2, 3, 5]),
                    ('All five fielded arms', [1, 2, 3, 4, 5])]:
    keep = np.isin(arm_all, arms + [6])
    y, a = acq[keep], arm_all[keep]
    treated = (a != 6).astype(float)
    bp, Vp, dfp = ols_hc1(y, np.column_stack([np.ones(len(y)), treated]))
    sep = np.sqrt(Vp[1, 1])
    bj, Vj, dfj = ols_hc1(y, dummies(a, arms))
    Fj, pj = wald_f(bj, Vj, list(range(1, len(arms) + 1)), dfj)
    yt, at = y[treated == 1], a[treated == 1]
    be, Ve, dfe = ols_hc1(yt, dummies(at, arms[1:]))
    Fe, pe = wald_f(be, Ve, list(range(1, len(arms))), dfe)
    rows.append({'arms': label, 'n': int(keep.sum()), 'n_treated': int(treated.sum()),
                 'n_control': int((treated == 0).sum()),
                 'pooled_pp_more_true': 100 * bp[1],
                 'pooled_ci95_low_pp': 100 * (bp[1] - 1.96 * sep),
                 'pooled_ci95_high_pp': 100 * (bp[1] + 1.96 * sep),
                 'pooled_sd': bp[1] / acq_sd,
                 'pooled_ci95_low_sd': (bp[1] - 1.96 * sep) / acq_sd,
                 'pooled_ci95_high_sd': (bp[1] + 1.96 * sep) / acq_sd,
                 'pooled_p': 2 * special.stdtr(dfp, -abs(bp[1] / sep)),
                 'joint_F': Fj, 'joint_df1': len(arms), 'joint_df2': dfj, 'joint_p': pj,
                 'equality_among_treated_p': pe})
t_acq_pool = pd.DataFrame(rows)
for _, r in t_acq_pool.iterrows():
    dc_log(f"Acquiescence, {r['arms']}: pooled {r['pooled_pp_more_true']:+.2f} pp more 'true' "
           f"({r['pooled_sd']:+.3f} SD), p = {r['pooled_p']:.3f}; joint p = {r['joint_p']:.3f}; "
           f"equality among treated p = {r['equality_among_treated_p']:.3f}")

## EXPORT
readme = pd.DataFrame({'sheet': [
    'pairs', 'effect_signs', 'effect_signs_by_arm', 'pair_average', 'acq_by_arm', 'acq_pooled_joint', 'notes'],
    'description': [
    'Candidate opposite item pairs, control arm, raw codes. Genuine opposites should have raw_r < 0 and agree_both below the independence benchmark.',
    'Significant (p < .05) published Model 1 effects (Likert form, from the straightlining check) by grid and direction. More yea-saying cannot lower agreement.',
    'The same, by treatment arm (fielded numbering; the paper labels arm 5 "Treatment 4").',
    'Effect on the average of CB07d and CB07f, arms 1 and 5 vs control (OLS, HC1). Flat = consistent with genuine persuasion.',
    f'Effect of each arm on SB acquiescence (share answering "true", balanced across keying), HC1; SD = {acq_sd:.3f}, control mean = {acq[arm_all == 6].mean():.3f}.',
    'All treated arms pooled vs control, the joint test that each arm equals control, and equality among treated arms (HC1 Wald F).',
    'Diagnostic only: no treatment effect is re-estimated here and no respondent is dropped.']})
export_item = output + "\\direction_check_" + date + ".xlsx"
with pd.ExcelWriter(export_item, engine='openpyxl') as xw:
    for name, tab in [('README', readme), ('pairs', t_pairs), ('effect_signs', t_signs),
                      ('effect_signs_by_arm', t_signs_arm), ('pair_average', t_pairavg),
                      ('acq_by_arm', t_acq_arm), ('acq_pooled_joint', t_acq_pool)]:
        tab.to_excel(xw, sheet_name=name, index=False)
dc_log(f"Exported: direction_check_{date}.xlsx (2c output)")
dc_log("######################")
