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
from sklearn.svm import SVR
from scipy.stats import loguniform, randint, uniform
import pickle as pkl
from sklearn.model_selection import BaseCrossValidator


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


past_polls = pd.read_csv('data/raw_polls.csv')
days_counting = 50

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
past_polls = past_polls.query("time_to_election <= @days_counting & not @pd.isna(methodology)") #Filtering out rows with no methodology
past_polls = past_polls[(past_polls['cand1_party'] == "DEM") & (past_polls['cand2_party'] == "REP")] #Filtering out rows with no DEM vs REP
past_polls = past_polls[~past_polls['type_simple'].str.contains("-P")] #filtering out primaries

#Annoying amount of work to deal with M1-2 and N1-3, which are given differently!
past_polls['state'] = past_polls['location'].apply(lambda x: x.split('-')[0])
past_polls['district'] = past_polls['location'].apply(lambda x: 0 if "-" not in x else x.split('-')[1])
past_polls['district'] = 1 if "1" in past_polls['state'] else past_polls['district']
past_polls['district'] = 2 if "2" in past_polls['state'] else past_polls['district']
past_polls['district'] = 3 if "3" in past_polls['state'] else past_polls['district']
past_polls['state'] = past_polls['state'].apply(lambda x: "ME" if re.match("M[0-9]", x) else x)
past_polls['state'] = past_polls['state'].apply(lambda x: "NE" if re.match("N[0-9]", x) else x)

past_polls['office_type'] = past_polls['type_simple'].apply(lambda x: 
    'Senate' if 'Sen' in x else 
    'Governor' if 'Gov' in x else 
    'President' if 'Pres' in x else 
    'House' if 'House' in x else None)

#Combining genballot!
past_polls['office_type'] = past_polls.apply(
    lambda row: 'House' if row['office_type'] == 'President' and row['state'] == 'US' else row['office_type'], 
    axis=1
)

#Finish cleaning past polls
past_polls = past_polls[['cycle', 'office_type', 'state', 'district', 'pollster_rating_id', 'methodology', 
                                     'partisan', 'samplesize', 'margin_poll']]

#CLEANING CURRENT POLLS TO BE THE SAME
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
    (uncleaned_current['population_full'] == "lv") &
    ((uncleaned_current['office_type'] != "U.S. President") | 
     (pd.to_datetime(uncleaned_current['start_date'], format="%m/%d/%y") > pd.to_datetime('2024-07-21', format="%Y-%m-%d")))
].copy()



#Non-House races have a seat number of 0
cleaned_current['seat_number'] = np.nan_to_num(cleaned_current['seat_number'], nan=0)

cleaned_current['seat_number'] = np.where(
    cleaned_current['state'].str.contains('CD-1'), 1,
    np.where(cleaned_current['state'].str.contains('CD-2'), 2, cleaned_current['seat_number'])
)

cleaned_current['state'] = cleaned_current['state'].str.replace(' CD-[0-9]', '', regex=True)

#Making sure polls are in the list of states (or general US poll)
cleaned_current = cleaned_current[(cleaned_current['state'].isin(state_name_to_abb.keys())) | (pd.isna(cleaned_current['state']))]

cleaned_current['state'] = cleaned_current['state'].map(state_name_to_abb)
cleaned_current['state'] = cleaned_current['state'].fillna('US')

#Getting number of polls
cleaned_current['num_polls'] = cleaned_current.groupby(['state', 'office_type', 'seat_number'], dropna=False)['poll_id'].transform('nunique')

# Filter out polls that don't include Harris and Trump
def filter_presidents(group):
    if group['office_type'].iloc[0] != 'U.S. President':
        return group
    elif ((group['answer'].str.contains('Harris').any()) & (group['answer'].str.contains('Trump').any()) & (~group['answer'].str.contains('Biden').any()) & 
         (~group['answer'].str.contains('Kennedy').any())):
        return group
    return pd.DataFrame()  # Return an empty DataFrame if the group does not meet the criteria

#Applying the filter

cleaned_current = cleaned_current.groupby('question_id').apply(filter_presidents).reset_index(drop=True)

cleaned_current['office_type'] = np.where(cleaned_current['state'].isna(), 'U.S. House', cleaned_current['office_type']) #General US polls are counted as House polls

cleaned_current = cleaned_current[['poll_id', 'pollster_rating_id', 'methodology', 'state', 'seat_number', 'question_id', 
         'sample_size', 'population_full', 'cycle', 'partisan', 'office_type', 'party', 'pct', 'answer', 'num_polls']]

#Ensuring the poll with the maximum total percentage is the one we use (almost always includes third party)
max_pct_sums = cleaned_current.groupby(['cycle', 'poll_id', 'state', 'seat_number', 'office_type', 'question_id'], dropna=False).agg(
    total_pct=('pct', 'sum')
).reset_index()
 
max_pct_sums = max_pct_sums[max_pct_sums.groupby(['cycle', 'poll_id', 'state', 'seat_number', 'office_type'], dropna=False)['total_pct'].transform('max') == max_pct_sums['total_pct']]

max_pct_sums.to_csv("cleaned_data/max_pct_sums2.csv", index=False)
print(len(max_pct_sums))

# Merging back into the cleaned polls
cleaned_current = pd.merge(cleaned_current, max_pct_sums, on=['cycle', 'poll_id', 'state', 'seat_number', 'question_id', 'office_type'], how='right')
# Only caring about DEM and REP
cleaned_current = cleaned_current[cleaned_current['party'].isin(["DEM", "REP"])]

#Get poll value by party rather than by candidate
cleaned_current = cleaned_current.groupby(['poll_id', 'pollster_rating_id', 'methodology', 'partisan', 'question_id', 'state', 'seat_number', 
                                           'sample_size', 'population_full', 'cycle', 'office_type', 'party'], dropna=False).agg(
    pct=('pct', 'sum')
).reset_index()
                                        

#This time, not including question_id -- we want the median over all questions
#In a given poll
cleaned_current = cleaned_current.groupby(['poll_id', 'pollster_rating_id', 'methodology', 'partisan', 'state', 'seat_number', 
                                           'sample_size', 'population_full', 'cycle', 'office_type', 'party'], dropna=False).agg(
    pct=('pct', 'mean')
).reset_index()

cleaned_current['partisan'] = cleaned_current['partisan'].fillna('Unknown Partisan')   
cleaned_current['sample_size'] = cleaned_current['sample_size'].fillna(600)
cleaned_current['methodology'] = cleaned_current['methodology'].fillna('Unknown Methodology')


cleaned_current = cleaned_current.pivot_table(index=['poll_id', 'pollster_rating_id', 'methodology', 'state', 'partisan', 'seat_number',
                                                     'sample_size', 'population_full', 'cycle', 'office_type'],
                                              columns='party', values='pct').reset_index()

cleaned_current.to_csv("cleaned_data/cleaned_polls2.csv", index=False)

