import pickle as pkl
import numpy as np
import pandas as pd
import re
from datetime import datetime
from dateutil import parser
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.model_selection import RandomizedSearchCV
import xgboost
from scipy.stats import loguniform, randint, uniform
import pickle as pkl
from sklearn.model_selection import BaseCrossValidator

pd.options.mode.chained_assignment = None 

class CustomTimeSeriesCV(BaseCrossValidator):
    """Creates an iterator that contains the indices from each dataset based on the years given"""
    def __init__(self, years):
        self.years = years

    def split(self, X, y=None, groups=None):
        for train_years, test_years in self.years:
            train_indices = np.where(X['cycle'].isin(train_years))[0]
            test_indices = np.where(X['cycle'].isin(test_years))[0]
            yield train_indices, test_indices
        
    def get_n_splits(self, X=None, y=None, groups=None):
        return len(self.years) 


past_polls = pd.read_csv('../../data/raw_polls.csv')
days_counting = 50

office_type_dict = {
    "Pres-G": "President",
    "Sen-G": "Senate",
    "Gov-G": "Governor",
    "House-G": "House"    
}
state_name_to_abb = {
    'Alabama': 'AL', 'Alaska': 'AK', 'Arizona': 'AZ', 'Arkansas': 'AR',
    'California': 'CA', 'Colorado': 'CO', 'Connecticut': 'CT', 'Delaware': 'DE',
    'Florida': 'FL', 'Georgia': 'GA', 'Hawaii': 'HI', 'Idaho': 'ID', 'Illinois': 'IL',
    'Indiana': 'IN', 'Iowa': 'IA', 'Kansas': 'KS', 'Kentucky': 'KY', 'Louisiana': 'LA',
    'Maine': 'ME', 'Maryland': 'MD', 'Massachusetts': 'MA', 'Michigan': 'MI',
    'Minnesota': 'MN', 'Mississippi': 'MS', 'Missouri': 'MO', 'Montana': 'MT',
    'Nebraska': 'NE', 'Nevada': 'NV', 'New Hampshire': 'NH', 'New Jersey': 'NJ',
    'New Mexico': 'NM', 'New York': 'NY', 'North Carolina': 'NC', 'North Dakota': 'ND',
    'Ohio': 'OH', 'Oklahoma': 'OK', 'Oregon': 'OR', 'Pennsylvania': 'PA', 'Rhode Island': 'RI',
    'South Carolina': 'SC', 'South Dakota': 'SD', 'Tennessee': 'TN', 'Texas': 'TX',
    'Utah': 'UT', 'Vermont': 'VT', 'Virginia': 'VA', 'Washington': 'WA',
    'West Virginia': 'WV', 'Wisconsin': 'WI', 'Wyoming': 'WY'
}

#CLEANING PAST POLLS

#This filters out rows we do not want
past_polls = past_polls.query("time_to_election <= @days_counting & not @pd.isna(methodology)")
past_polls = past_polls[(past_polls['cand1_party'] == "DEM") & (past_polls['cand2_party'] == "REP")]
past_polls = past_polls[past_polls['type_simple'].isin(["Pres-G", "Sen-G", "Gov-G", "House-G"])]

#Adding important columns, X and Y
past_polls['office_type'] = past_polls['type_simple'].map(office_type_dict)
past_polls['state'] = past_polls['location'].apply(lambda x: x.split('-')[0])
past_polls['district'] = past_polls['location'].apply(lambda x: 0 if "-" not in x else x.split('-')[1])
past_polls['district'] = 1 if "1" in past_polls['state'] else past_polls['district']
past_polls['district'] = 2 if "2" in past_polls['state'] else past_polls['district']
past_polls['district'] = 3 if "3" in past_polls['state'] else past_polls['district']
past_polls['state'] = past_polls['state'].apply(lambda x: "ME" if re.match("M[0-9]", x) else x)
past_polls['state'] = past_polls['state'].apply(lambda x: "NE" if re.match("N[0-9]", x) else x)

past_polls = past_polls[['cycle', 'office_type', 'state', 'district', 'pollster_rating_id', 'methodology', 
                                     'partisan', 'samplesize', 'margin_poll']]
past_polls

#CLEANING CURRENT POLLS TO BE THE SAME
# Load the generic ballot polls data and transform it
uncleaned_current_genballot = pd.read_csv("https://projects.fivethirtyeight.com/polls-page/data/generic_ballot_polls.csv")
uncleaned_current_genballot = uncleaned_current_genballot.melt(id_vars=[col for col in uncleaned_current_genballot.columns if col not in ['dem', 'rep']],
                                                               value_vars=['dem', 'rep'],
                                                               var_name='party',
                                                               value_name='pct')
uncleaned_current_genballot['party'] = uncleaned_current_genballot['party'].str.upper()

# Load the other poll data files
president_polls = pd.read_csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls.csv")
senate_polls = pd.read_csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls.csv")
house_polls = pd.read_csv("https://projects.fivethirtyeight.com/polls-page/data/house_polls.csv")
governor_polls = pd.read_csv("https://projects.fivethirtyeight.com/polls-page/data/governor_polls.csv")

# Combine all poll data into one DataFrame
uncleaned_current = pd.concat([president_polls, senate_polls, house_polls, governor_polls, uncleaned_current_genballot], ignore_index=True)

#Only getting the polls that are within the last 50 days and after Biden dropped out
cleaned_current = uncleaned_current[
    ((pd.to_datetime('today') - pd.to_datetime(uncleaned_current['end_date'], format="%m/%d/%y")).dt.days <= days_counting) &
    ((uncleaned_current['office_type'] != "U.S. President") | 
     (pd.to_datetime(uncleaned_current['start_date'], format="%m/%d/%y") > pd.to_datetime('2024-07-21', format="%Y-%m-%d")))
].copy()

cleaned_current['seat_number'] = np.nan_to_num(cleaned_current['seat_number'], nan=0)

cleaned_current['seat_number'] = np.where(
    cleaned_current['state'].str.contains('CD-1'), 1,
    np.where(cleaned_current['state'].str.contains('CD-2'), 2, cleaned_current['seat_number'])
)

cleaned_current['state'] = cleaned_current['state'].str.replace(' CD-[0-9]', '', regex=True)

#Making sure polls are in the list of states (or general US poll)
cleaned_current = cleaned_current[(cleaned_current['state'].isin(state_name_to_abb.keys())) | (pd.isna(cleaned_current['state']))]
cleaned_current['office_type'] = np.where(cleaned_current['state'].isna(), 'U.S. President', cleaned_current['office_type'])

cleaned_current['num_polls'] = cleaned_current.groupby(['state', 'office_type', 'seat_number'], dropna=False)['poll_id'].transform('nunique')

# Filter out polls that don't include Harris and Trump
def filter_presidents(group):
    if group['office_type'].iloc[0] != 'U.S. President':
        return group
    elif group['answer'].str.contains('Harris').any() & group['answer'].str.contains('Trump').any():
        return group
    return pd.DataFrame()  # Return an empty DataFrame if the group does not meet the criteria


cleaned_current = cleaned_current.groupby('question_id').apply(filter_presidents).reset_index(drop=True)
cleaned_current = cleaned_current[['poll_id', 'pollster_rating_id', 'methodology', 'state', 'seat_number', 'question_id', 
         'sample_size', 'population_full', 'cycle', 'partisan', 'office_type', 'party', 'pct', 'answer', 'num_polls']]

#Ensuring the poll with the maximum total percentage is the one we use (almost always includes third party)
max_pct_sums = cleaned_current.groupby(['cycle', 'poll_id', 'pollster_rating_id', 'state', 'seat_number', 'office_type'], dropna=False).agg(
    total_pct=('pct', 'sum')
).reset_index()

max_pct_sums = max_pct_sums[max_pct_sums.groupby(['cycle', 'poll_id', 'state', 'seat_number', 'office_type'], dropna=False)['total_pct'].transform('max') == max_pct_sums['total_pct']]

# Merging back into the cleaned polls
cleaned_current = pd.merge(cleaned_current, max_pct_sums, on=['cycle', 'poll_id', 'pollster_rating_id', 'state', 'seat_number', 'office_type'], how='right')


cleaned_current = cleaned_current[cleaned_current['party'].isin(["DEM", "REP"])]


#Get poll value by party rather than by candidate
#We choose mean because there are sometimes multiple questions in the same poll (e.g. with/without 3-party) where the two main parties have the same sum
#So the max_pct_sums wouldn't take care of it -- we just take the mean
cleaned_current = cleaned_current.groupby(['poll_id', 'pollster_rating_id', 'methodology', 'partisan', 'state', 'seat_number', 
                                           'sample_size', 'population_full', 'cycle', 'office_type', 'party'], dropna=False).agg(
    pct=('pct', 'mean')
).reset_index()

    
#Pivoting so that we have one row per poll     
cleaned_current = cleaned_current[~cleaned_current['methodology'].isna()]
cleaned_current['state'] = cleaned_current['state'].fillna('Unknown State')
cleaned_current['partisan'] = cleaned_current['partisan'].fillna('Unknown Partisan')   
         
cleaned_current = cleaned_current.pivot_table(index=['poll_id', 'pollster_rating_id', 'methodology', 'state', 'partisan', 'seat_number',
                                                     'sample_size', 'population_full', 'cycle', 'office_type'],
                                              columns='party', values='pct').reset_index()

cleaned_current['state'] = np.where(cleaned_current['state'] == 'US', np.nan, cleaned_current['state'])
#Translating stuff to integers
cleaned_current['district'] = cleaned_current['seat_number'].astype(int)
cleaned_current['cycle'] = cleaned_current['cycle'].astype(int)
cleaned_current['pollster_rating_id'] = cleaned_current['pollster_rating_id'].astype(int)
cleaned_current['samplesize'] = cleaned_current['sample_size'].astype(int)

#We only want likely voter polls
cleaned_current = cleaned_current[cleaned_current['population_full'] == "lv"]
cleaned_current['partisan'] = np.where(cleaned_current['partisan'] == 'Unknown Partisan', np.NaN, cleaned_current['partisan'])
cleaned_current['margin_poll'] = cleaned_current['DEM'] - cleaned_current['REP']

cleaned_current = cleaned_current[['cycle', 'office_type', 'state', 'district', 'pollster_rating_id', 'methodology', 
                                   'partisan', 'samplesize',  'margin_poll']]

all_polls = pd.concat([past_polls, cleaned_current], ignore_index=True)

unique_methods = set()
for methods in all_polls['methodology']:
    unique_methods.update(methods.split('/'))
unique_methods.remove('Mixed')
unique_methods.remove("App Panel")

for method in unique_methods:
    all_polls[method] = all_polls['methodology'].apply(lambda x: 1 if method in x.split('/') else 0)

all_polls = all_polls.drop(columns=['methodology'])
all_polls['office_type'] = all_polls['office_type'].apply(lambda x: x.replace("U.S. ", ""))

def get_variance_bias(group_df):
    year = group_df['cycle'].iloc[0]
    predict_df = group_df.drop(columns=['state', 'district'])
    
    file_path_error = f'../../models/Polls_{year}_error.pkl'
    with open(file_path_error, 'rb') as file:
        var_model = pkl.load(file)

    file_path_bias = f'../../models/Polls_{year}_bias.pkl'
    with open(file_path_bias, 'rb') as file:
        bias_model = pkl.load(file)
    
    variance = var_model.predict(predict_df)
    bias = bias_model.predict(predict_df)
    return pd.DataFrame({'variance': variance, 'bias': bias})

all_polls = all_polls[(all_polls['cycle'] >= 2002) & (all_polls['cycle'] % 2 == 0)]
variance_bias_df = all_polls.groupby('cycle').apply(get_variance_bias).reset_index(drop=True)
all_polls = pd.concat([all_polls.reset_index(), variance_bias_df], axis=1)
all_polls['variance'] = np.where(all_polls['variance'] < 1, 1, all_polls['variance'])

def combine_polls(race_df):
    margins = race_df['margin_poll']
    variance = race_df['variance']
    bias = race_df['bias']
    
    weighted_estimate = np.average(margins, weights = 1 / variance) #Inverse weighted average
    weighted_variance = 1 / np.sum(1 / variance)
    weighted_lower_bound = weighted_estimate - 1.96 * np.sqrt(weighted_variance)
    weighted_upper_bound = weighted_estimate + 1.96 * np.sqrt(weighted_variance)
    
    adjusted_margins = margins - bias
    unweighted_esimate = np.mean(adjusted_margins)
    unweighted_variance = np.var(adjusted_margins, ddof=1)
    unweighted_lower_bound = unweighted_esimate - 1.96 * np.sqrt(unweighted_variance)
    unweighted_upper_bound = unweighted_esimate + 1.96 * np.sqrt(unweighted_variance)
    
    num_polls = len(margins)
    
    return pd.Series({'weighted_estimate': weighted_estimate, 
                         'weighted_ci_lower': weighted_lower_bound,
                         'weighted_ci_upper': weighted_upper_bound,
                         'unweighted_estimate': unweighted_esimate, 
                         'unweighted_ci_lower': unweighted_lower_bound,
                         'unweighted_ci_upper': unweighted_upper_bound, 
                         'num_polls': int(num_polls)})
    
poll_estimates = all_polls.groupby(['cycle', 'office_type', 'state', 'district']).apply(combine_polls).reset_index()
