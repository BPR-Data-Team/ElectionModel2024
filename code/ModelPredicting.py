import pandas as pd
import numpy as np
import pickle as pkl
import lightgbm as lgb
import shap
import matplotlib.pyplot as plt
import re
from datetime import date
from scipy.stats import multivariate_normal, Covariance, mode
import json
from collections import Counter

from sklearn.preprocessing import OneHotEncoder, OrdinalEncoder
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.model_selection import BaseCrossValidator

from hyperopt import fmin, tpe, hp, Trials, STATUS_OK
from hyperopt.early_stop import no_progress_loss

class CustomTimeSeriesCV(BaseCrossValidator):
    """Creates an iterator that contains the indices from each dataset based on the years given"""
    def __init__(self, years):
        self.years = years

    def split(self, X, y=None, groups=None):
        for train_years, test_years in self.years:
            train_indices = np.where(X['year'].isin(train_years))[0]
            test_indices = np.where(X['year'].isin(test_years))[0]
            yield train_indices, test_indices
        
    def get_n_splits(self, X=None, y=None, groups=None):
        return len(self.years) 
    
    
# Create fold structure so we can make a custom cross-validation for time-series
folds = [
    (range(2002, 2010, 2), [2010, 2012]),
    (range(2002, 2014, 2), [2014, 2016]),
    (range(2002, 2018, 2), [2018, 2020])
]

cv = CustomTimeSeriesCV(folds)

#Ignore warnings
np.seterr(divide='ignore', invalid='ignore')

#Categorical features that need to be one-hot encoded    
one_hot_fts = ['office_type']

#Rating is the only ordinal feature
ordinal_fts = ['final_rating']
ordinal_fts_ranking = ['Safe R', 'Likely R', 'Leans R', 'Toss-up', 'Leans D', 'Likely D', 'Safe D']

#Cont features that should be pass-throughed (and later scaled)
cont_fts = [
    "open_seat", "incumbent_differential", "special", "absenteeexcusereq", "pollhours", "avgpollhours", "minpollhours",
    "regdeadlines", "voteridlaws", "novoterid", "noallmailvote", "noearlyvote", "nofelonreg",
    "nofelonsregafterincar", "nonstrictid", "nonstrictphoto", "nopollplacereg", "nopr", "nosamedayreg",
    "nostateholiday", "pr16", "pr17", "pr175", "pr60", "pr90", "strictid", "strictphoto", "covi_num",
    "prev_dem_gen_tp", "prev_gen_margin", "weighted_genpoll", "weighted_genpoll_lower",
    "weighted_genpoll_upper", "unweighted_genpoll", "mean_specials_differential", 
    "house_chamber_margin", "senate_chamber_margin", "previous_cci", "current_cci", "change_cci",
    "previous_gas", "current_gas", "change_gas", "previous_unemployment", "current_unemployment",
    "change_unemployment",  "receipts", "from_committee_transfers", "disbursements",
    "to_committee_transfers", "beginning_cash", "ending_cash", "candidate_contributions",
    "individual_contributions", "unconvinced_pct", "phone_unweighted", "online_unweighted", "num_polls",
    "unweighted_estimate", "unweighted_ci_lower", "unweighted_ci_upper", "weighted_estimate",
    "weighted_ci_lower", "weighted_ci_upper", "white_pct", "black_pct", "asian_pct", "hispanic_pct",
    "median_income", "impoverished_pct", "median_age", "renting_pct", "inflation", "isMidterm",
    "genballot_predicted_margin", "genballot_predicted_lower", "genballot_predicted_upper",
    "poll_fundamental_agree",  'receipts_DEM', 'receipts_REP', 'disbursements_DEM', 'disbursements_REP', 
    'average_genballot', 'genballot_individual_predicted_margin', 'genballot_campaign5_predicted_margin', 
    'genballot_campaign10_predicted_margin', 'genballot_campaign15_predicted_margin', 
    'average_genballot_predicted_margin', 'expert_rating_democrat', 'finance_fundamental_agree'
]

#We don't care about individual features, we care about all features related to a specific category
shap_features = {
    "Past Elections": ['open_seat', 'incumbent_differential', 'weighted_genpoll', 'unweighted_genpoll',
        "weighted_genpoll_lower",
        "weighted_genpoll_upper", 'mean_specials_differential', 'genballot_predicted_margin',
       'specials_predicted_margin', 'poll_fundamental_agree', 'genballot_predicted_lower',
       'genballot_predicted_upper', 'prev_gen_margin', 'prev_dem_gen_tp', 
       'average_genballot', 'average_genballot_predicted_margin', 'genballot_individual_predicted_margin', 
       'genballot_campaign5_predicted_margin', 'genballot_campaign10_predicted_margin', 'genballot_campaign15_predicted_margin'],
    
    "Voting Regulations": ['voteridlaws', 'novoterid', 'nonstrictid', 'strictid', 'strictphoto', 
                    'nofelonreg', 'nofelonsregafterincar', 'nonstrictphoto', 'nopollplacereg', 'nosamedayreg', 'pr16',
                    'pr17', 'pr175', 'pr60', 'pr90', 'nopr', 'noallmailvote', 'noearlyvote',
                    'absenteeexcusereq', 'pollhours', 'avgpollhours', 'minpollhours', 'regdeadlines', 'nostateholiday', 'covi_num'],
    
    "Polls": ['unconvinced_pct', 'phone_unweighted', 'online_unweighted', 'num_polls',
       'unweighted_estimate', 'unweighted_ci_lower', 'unweighted_ci_upper',
       'weighted_estimate', 'weighted_ci_lower', 'weighted_ci_upper'], 
    
    "Demographics": ['white_pct', 'black_pct', 'asian_pct', 'hispanic_pct', 'median_income',
       'impoverished_pct', 'median_age', 'renting_pct'], 
    
    "Campaign Finance": ["receipts", "from_committee_transfers", "disbursements",
    "to_committee_transfers", "beginning_cash", "ending_cash", "candidate_contributions",
    "individual_contributions", 'receipts_DEM', 'receipts_REP', 'disbursements_DEM', 'disbursements_REP', 
    'finance_fundamental_agree'], 
        
    "Consumer Confidence Index": ['previous_cci', 'current_cci', 'change_cci'],
    
    "Gas Prices": ['previous_gas', 'current_gas', 'change_gas'], 
    
    "Unemployment & Inflation":  ['previous_unemployment','current_unemployment', 'change_unemployment', "inflation"],
    
    "Expert Ratings": ['final_rating', 'expert_rating_democrat'],
        
    "Composition of Congress/Presidency": ['democrat_in_presidency', 
              'house_chamber_margin', 'senate_chamber_margin'],
    
    "Other": ['special', 'isMidterm', 'expected_value', 'office_type']
}


data = pd.read_csv('cleaned_data/Engineered Dataset.csv')
X = data.drop(columns = ['margin'])
y = data['margin']
X_train, X_predict, y_train, y_predict = (X.loc[X['year'] < 2024, :], X.loc[X['year'] == 2024, :].reset_index(), 
                                        y.loc[X['year'] < 2024], y.loc[X['year'] == 2024].reset_index())

#Defining Preprocessor for the std model
preprocessor = ColumnTransformer([
        ('cat', OneHotEncoder(), one_hot_fts),
        ('ord', OrdinalEncoder(categories = [ordinal_fts_ranking], handle_unknown='use_encoded_value', 
                               unknown_value=np.nan), ordinal_fts),
        ('num', 'passthrough', cont_fts)])

num_models = 10

feature_names = preprocessor.fit(X_train).get_feature_names_out()

#Getting array with predictions
training_predictions_array = np.zeros((X_train.shape[0], num_models))
mean_training_predictions = np.zeros(X_train.shape[0])
predictions_array = np.zeros((X_predict.shape[0], num_models))
campaign_contributions = np.zeros((X_predict.shape[0], 101))
mean_predictions = np.zeros(X_predict.shape[0])
shap_contribution_array = np.zeros((X_predict.shape[0], len(feature_names) + 1))

def new_campaign_contributions(X : pd.DataFrame, update_num) -> pd.DataFrame:
    """Here, we change the X_predict >100 times with different campaign finance contributions, and then get the expected values
    For everything
    We will be only changing a few columns:
    1. Receipts_DEM / Receipts_REP
    2. Receipts = ln(Receipts_DEM / Receipts_REP)
    3. Disbursements_DEM / Disbursements_REP 
    4. Disbursements = ln(Disbursements_DEM / Disbursements_REP)
    5. receipts_genballot_interaction = genballot_predicted_margin * receipts
    6. disbursements_genballot_interaction = genballot_predicted_margin * disbursements
    7. finance_fundamental_agree = sign(genballot_predicted_margin * receipts)
    All increases to these values are done via individual_contributions_DEM/REP. Specifically, we add the same value to both 
    receipts_dem, disbursements_dem, and individual_contributions_dem. This is because we are assuming that the party receives all new cash from individuals, and spends all their money.

    X_predict: the original dataframe
    update_num: how much we are changing campaign finance values by
    e.g. if update_num is 1, we add D+40k to every house race and D+400k to each senate race. 
         if update_num <0, we add cash to republicans instead
    """
    X_update = X.copy(deep = True)
    
    new_individual_contributions_DEM = X_update['individual_contributions_DEM'].copy(deep = True)
    new_receipts_DEM = X_update['receipts_DEM'].copy(deep = True)
    new_disbursements_DEM = X_update['disbursements_DEM'].copy(deep = True)
    
    new_individual_contributions_REP = X_update['individual_contributions_REP'].copy(deep = True)
    new_receipts_REP = X_update['receipts_REP'].copy(deep = True)
    new_disbursements_REP = X_update['disbursements_REP'].copy(deep = True)
    
    if update_num > 0:
        new_individual_contributions_DEM += np.where(X_update['office_type'] == "Senate", 400_000 * update_num, 40_000 * update_num)
        new_receipts_DEM += np.where(X_update['office_type'] == "Senate", 400_000 * update_num, 40_000 * update_num)
        new_disbursements_DEM += np.where(X_update['office_type'] == "Senate", 400_000 * update_num, 40_000 * update_num)
    else:
        new_individual_contributions_REP -= np.where(X_update['office_type'] == "Senate", 400_000 * update_num, 40_000 * update_num)
        new_receipts_REP -= np.where(X_update['office_type'] == "Senate", 400_000 * update_num, 40_000 * update_num)
        new_disbursements_REP -= np.where(X_update['office_type'] == "Senate", 400_000 * update_num, 40_000 * update_num)
        
    X_update['disbursements_DEM'] = new_disbursements_DEM
    X_update['receipts_DEM'] = new_receipts_DEM
    X_update['individual_contributions_DEM'] = new_individual_contributions_DEM
    X_update['disbursements_REP'] = new_disbursements_REP
    X_update['receipts_REP'] = new_receipts_REP
    X_update['individual_contributions_REP'] = new_individual_contributions_REP
     
    # Calculate receipts with NA handling using np.where
    X_update['receipts'] = np.where(
        pd.isna(new_receipts_DEM),
        -6,
        np.where(
            pd.isna(new_receipts_REP),
            6,
            np.log(round(new_receipts_DEM) / round(new_receipts_REP))
        )
    )

    # Calculate disbursements with NA handling using np.where
    X_update['disbursements'] = np.where(
        pd.isna(new_disbursements_DEM),
        -6,
        np.where(
            pd.isna(new_disbursements_REP),
            6,
            np.log(round(new_disbursements_DEM) / round(new_disbursements_REP))
        )
    )

    # Calculate derived columns
    X_predict["receipts_genballot_interaction"] = X_predict["genballot_predicted_margin"] * X_predict["receipts"]
    X_predict["disbursements_genballot_interaction"] = X_predict["genballot_predicted_margin"] * X_predict["disbursements"]
    X_predict["finance_fundamental_agree"] = np.sign(X_predict["genballot_predicted_margin"] * X_predict["receipts"])
    
    return X_update


#Going through each model we trained to get a set of point estimates
for idx in range(num_models):
    file_path = f"models/Model_{idx}.pkl"

    # Open a file to write in binary mode????        
    with open(file_path, 'rb') as file:
        trained_pipe = pkl.load(file)
    
    training_predictions = trained_pipe.predict(X_train)
    predictions = trained_pipe.predict(X_predict)
    
    contributions = trained_pipe.predict(X_predict, pred_contrib = True)        
    
    shap_contribution_array += contributions
    training_predictions_array[:, idx] = training_predictions
    predictions_array[:, idx] = predictions

mean_training_predictions = np.mean(training_predictions_array, axis = 1)
mean_predictions = np.mean(predictions_array, axis = 1)
epistemic_std_predictions = np.std(predictions_array, axis = 1)
mean_shap_contributions = shap_contribution_array / num_models

#NOW WORKING WITH CAMPAIGN FINANCE DATA! THE NEW WAY, WITH THE NEW MODEL
with open("models/CampaignFinanceModel.pkl", 'rb') as file:
    campaign_pipe = pkl.load(file)
    
baseline_predictions = campaign_pipe.predict(X_predict)

for new_campaign in range(101):
    new_campaign_predict = new_campaign_contributions(X_predict, new_campaign - 50)
    campaign_contributions[:, new_campaign] = campaign_pipe.predict(new_campaign_predict) - baseline_predictions

#SHAP values are used for interpretation, but it's also used for correlation analysis for the final multivariate normal distribution
correlations = np.corrcoef(mean_shap_contributions)
def regularize_correlation_matrix(correlation_matrix, min_eigenvalue=1e-10):
    eigenvalues, eigenvectors = np.linalg.eigh(correlation_matrix)
    eigenvalues[eigenvalues < min_eigenvalue] = min_eigenvalue
    return eigenvectors @ np.diag(eigenvalues) @ eigenvectors.T
correlations = regularize_correlation_matrix(correlations)

shap_df = pd.DataFrame(mean_shap_contributions, columns = feature_names.tolist() + ['expected_value'])

#Checks if column is contained in a list of SHAP column names, dealing with the fact that, 
#After preprocessing, the column names are changed to include the preprocessing step
def list_contains_col(col, array):
    col_no_preprocessing = re.sub('^.*?__', '', col)
    return col_no_preprocessing in array

for key in shap_features:
    #Goes through each shap category and sums the SHAP values for each feature in that category
    shap_df[key] = shap_df[[col for col in shap_df.columns if list_contains_col(col, shap_features[key])]].sum(axis = 1)
    
shap_df['state'] = X_predict['state']
shap_df['district'] = X_predict['district']
shap_df['office_type'] = X_predict['office_type']

#Shap_df is a dataframe that has the three race identifiers and the SHAP values for each feature category
shap_df = shap_df[['state', 'district', 'office_type'] + list(shap_features.keys())]

#The following code was run once, but not again. It trains a standard-deviation predicting model and saves it in the models folder to be used in
#Future predictions. 

"""
#---- Now, we will train a standard-deviation predicting model -- dealing with Aleatoric Uncertainty (Bootstrapping gives us epistemic)
def neg_log_likelihood(y, y_pred, y_std):
    #Given a set of true values, predicted values, and standard deviations, returns the negative log likelihood.
    #We want to minimize this value to get the best standard deviations (based on aleatoric uncertainty).
    return np.mean(0.5 * np.log(2 * np.pi * y_std ** 2) + ((y - y_pred) ** 2 / (2 * y_std ** 2)))

ideal_std_predictions = np.abs(y_train - mean_training_predictions)

def std_objective(params):
    "Function that takes in hyperparameters and returns loss, that Hyperopt will minimize."        
    testing_loss = []
    for train_idx, test_idx in cv.split(X_train):                
        
        std_reg = lgb.LGBMRegressor(**params)
        pipe = Pipeline(steps = [
            ('preprocessing', preprocessor), 
            ('model', std_reg)])
        
        #Goes through each fold and calculates loss.
        pipe.fit(X_train.iloc[train_idx], ideal_std_predictions.iloc[train_idx])
        
        std_predictions = pipe.predict(X_train.iloc[test_idx])
        testing_loss.append(neg_log_likelihood(y_train.iloc[test_idx], mean_training_predictions[test_idx], std_predictions))
                    
    return {'loss': np.mean(testing_loss), 'status': STATUS_OK}

param_dict = {
    'boosting_type': 'dart',
    'num_leaves': hp.randint('num_leaves', 20, 70),  # Reduced the upper limit, 
    'n_estimators': hp.randint('n_estimators', 50, 200),  # Increased the range
    'learning_rate': hp.loguniform('learning_rate', -5, -2),  # Equivalent to about 0.0001 to 0.01
    'subsample_for_bin': hp.randint('subsample_for_bin', 20000, 200000),  # Narrowed the range
    'min_data_in_bin': hp.randint('min_data_in_bin', 1, 10), 
    'min_data_in_leaf': hp.randint('min_data_in_leaf', 1, 10),  # Reduced the upper limit
    'min_child_samples': hp.randint('min_child_samples', 20, 150),  # Increased the range for more regularization
    'reg_alpha': hp.uniform('reg_alpha', 0.0, 1.5),  # Increased upper limit for L1 regularization
    'reg_lambda': hp.uniform('reg_lambda', 0.0, 1.5),  # Increased upper limit for L2 regularization
    'colsample_bytree': hp.uniform('colsample_bytree', 0.4, 0.8),  # Reduced the upper limit
    'subsample': hp.uniform('subsample', 0.5, 0.8),  # Reduced the upper limit for more randomness
    'max_depth': hp.randint('max_depth', 2, 10),  # Added max_depth for additional control
    'drop_rate': hp.uniform('drop_rate', 0.05, 0.5),  # Added drop_rate for dart
    'skip_drop': hp.uniform('skip_drop', 0.1, 0.9),  # Added skip_drop for dart
    "verbose": -1,  # Keep verbose to -1 to reduce log clutter,  
    'n_jobs': 8
}

"Hyperopt uses the TPE algorithm to optimize hyperparameters. We use the no_progress_loss function to stop early if we don't see progress."
std_best_params = fmin(fn=std_objective,
                space=param_dict,
                algo=tpe.suggest,
                trials=Trials(),
                early_stop_fn = no_progress_loss(20))

#once we get the best params for each, we train each sequentially and then return the fitted versions.

std_y_train = np.abs(y_train - mean_training_predictions)
std_best_model = lgb.LGBMRegressor(**std_best_params, boosting_type = 'dart', verbose = -1, n_jobs = 8)
std_best_pipe = Pipeline(steps = [
    ('preprocessing', preprocessor), 
    ('model', std_best_model)])
std_best_pipe.fit(X_train, std_y_train)

with open("models/std_model.pkl", 'wb') as file:
    pkl.dump(std_best_pipe, file)"""

with open("models/std_model.pkl", 'rb') as file:
    std_best_pipe = pkl.load(file)
aleatoric_std_predictions = std_best_pipe.predict(X_predict)

days_until_election = (date(2024, 11, 5) - date.today()).days
aleatoric_increase = 0.03*days_until_election + 0.08 #We assume that aleatoric uncertainty decreases as we get closer to the election
#We chose 0.03 and 0.08 so ~4 months before the election, the std aleatoric uncertainty is multiplied by 2, and ~1 month before, it is multiplied by 1
#At this point, we now have the standard deviations for each prediction. We can now calculate the final predictions
final_std_predictions = np.sqrt(epistemic_std_predictions**2 + aleatoric_increase * aleatoric_std_predictions**2)
        

#Getting final race-level dataframe
predictions_df = pd.DataFrame()
predictions_df['state'] = X_predict['state']
predictions_df['district'] = X_predict['district']
predictions_df['office_type'] = X_predict['office_type']
for col in shap_features:
    predictions_df[col] = shap_df[col]

#In governor races, we don't have any campaign finance data, so we will add it (which is a small number) to the past elections
predictions_df.loc[predictions_df['office_type'] == "Governor", 'Past Elections'] += predictions_df.loc[predictions_df['office_type'] == "Governor", 'Campaign Finance']
predictions_df.loc[predictions_df['office_type'] == "Governor", 'Campaign Finance'] = 0
    
#Now working on getting the multivariate normal distribution with the std and the correlation matrix
cov_matrix = np.diag(final_std_predictions) @ correlations @ np.diag(final_std_predictions)


multinormal = multivariate_normal(mean_predictions, cov_matrix, allow_singular=True)
random_samples = multinormal.rvs(size = 100000).T
predictions_df['margins'] = random_samples.tolist()
predictions_df['median_margin'] = np.median(random_samples, axis = 1)
predictions_df['campaign'] = campaign_contributions.tolist()
predictions_df['campaign'] = predictions_df['campaign'].apply(lambda x: [round(i, 2) for i in x])
predictions_df['use_campaign'] = (((predictions_df['office_type'] == 'Senate') | (predictions_df['office_type'] == 'House')) & (predictions_df['state'] != 'US') & 
                                  X_predict['receipts_DEM'].notna() & X_predict['receipts_REP'].notna())


#Now need to add additional rows for house, senate, and president
senate_samples = random_samples[predictions_df['office_type'] == 'Senate']
US_senate = np.sum(senate_samples >= 0, axis = 0) + 30 #of the races we're not predicting, the margin is -10

house_samples = random_samples[predictions_df['office_type'] == 'House']
US_house = np.sum(house_samples >= 0, axis = 0) + 27

electoral_votes = pd.read_csv('cleaned_data/Electoral Votes Sheet.csv')
president_samples = random_samples[predictions_df['office_type'] == 'President']

presidential_df = predictions_df[predictions_df['office_type'] == 'President']
presidential_df = presidential_df.join(electoral_votes.set_index(['state', 'district']), on = ['state', 'district'])

#For the two maps on the website, we need to get the two most likely presidential outcomes (Harris wins which states, etc.)
#The following code does just that!
#----- BEGINNING OF LIKELY PRES CODE
def get_winner(margin):
    "Given a margin, returns the winner of the state."
    return 'Biden' if margin > 0 else 'Trump'

def create_state_map(margins):
    "Returns a frozenset of the states and the winner of the state for a single simulation."
    return frozenset((state, get_winner(margin)) for state, margin in margins.items())

presidential_df['state_district'] = np.where(presidential_df['district'] != 0,
                                             presidential_df['state'] + '-' + presidential_df['district'].astype(str),
                                             presidential_df['state'])
def calculate_evs(states, presidential_df):
    "Given a set of states, returns the total electoral votes won by those states."
    evs = 0
    for _, row in presidential_df.iterrows():
        state_district = row['state_district']
        if state_district in states:
            evs += row['electoral_votes']
    return evs


# Process the data
simulations = []
for _, row in presidential_df.iterrows():
    state = row['state']
    if row['district'] != 0:
        state += f"-{row['district']}"
    margins = row['margins']
    for i, margin in enumerate(margins):
        if len(simulations) <= i:
            simulations.append({})
        simulations[i][state] = margin
    
state_map_counter = Counter(create_state_map(sim) for sim in simulations)

# Create a DataFrame of the most common outcomes
top_outcomes = []

for rank, (state_map, count) in enumerate(state_map_counter.most_common(10)):
    biden_states = np.sort([state for state, winner in state_map if winner == 'Biden'])
    trump_states = np.sort([state for state, winner in state_map if winner == 'Trump'])
    
    biden_evs = calculate_evs(biden_states, presidential_df)
    trump_evs = calculate_evs(trump_states, presidential_df)
    
    top_outcomes.append({
        'Rank': rank,
        'Frequency': count,
        'Percentage': round((count / len(simulations) * 100), 1),
        'Biden States': biden_states.tolist(),
        'Trump States': trump_states.tolist(),
        'Biden EVs': biden_evs,
        'Trump EVs': trump_evs
    })

top_outcomes = pd.DataFrame(top_outcomes)

# Format the DataFrame
top_outcomes = top_outcomes.set_index('Rank')

top_outcomes.to_csv('cleaned_data/Top Presidential Outcomes.csv')

presidential_df = presidential_df.drop(columns = ['state_district'])

#---- END OF LIKELY PRES CODE

# Assuming president_samples is correctly filtered for presidential predictions
# Reshape the electoral votes to be broadcastable across the simulations
electoral_votes_broadcastable = presidential_df['electoral_votes'].values[:, np.newaxis]

# Now, president_samples should be shaped (52, number of simulations)
# Broadcast multiplication across simulations
democratic_electoral_votes = (president_samples >= 0) * electoral_votes_broadcastable

# Sum across states for each simulation
US_president = np.sum(democratic_electoral_votes, axis=0)

#Will add this after putting in names to the original predictions_df
US_rows = pd.DataFrame(
    data = {
        'state': ['US', 'US', 'US'], 
        'district': [0, 0, 0],
        'dem_name': ['Democrats', 'Democrats', 'Democrats'],
        'rep_name': ['Republicans', 'Republicans', 'Republicans'],
        'bin_bounds': [(40.5, 59.5), (1.5, 433.5), (3.5, 534.5)],
        'num_bins': [19, 54, 59],
        'office_type': ['Senate', 'House', 'President'],
        'threshold_winning': [50, 217.5, 269],
        'median_margin': [np.round(np.median(US_senate)), np.round(np.median(US_house)), np.round(np.median(US_president))],
        'margins': [US_senate.tolist(), US_house.tolist(), US_president.tolist()]
    }
)

name_df = pd.read_csv('cleaned_data/Names Dataset.csv').drop(columns = ['Unnamed: 0'])
predictions_df['std'] = final_std_predictions
predictions_df = predictions_df.join(name_df.set_index(['state', 'district', 'office_type']), on = ['state', 'district', 'office_type'], 
                                     how = 'outer')

#For nebraska senate, we are manually writing that we are not predicting the regular election
predictions_df.loc[(predictions_df['state'] == 'NE') & (predictions_df['office_type'] == "Senate"), 'weird'] = ''

#Adding all the margins manually would require too much memory, so we instead give how many simulations are in each bin
predictions_df['bin_bounds'] = predictions_df.apply(lambda x: (-100, 100), axis = 1)
predictions_df['num_bins'] = 50
predictions_df['threshold_winning'] = 0
predictions_df = pd.concat([predictions_df, US_rows], axis = 'rows')
predictions_df['bins'] = predictions_df.apply(lambda x: np.histogram(x['margins'], bins = x['num_bins'], range = x['bin_bounds'])[0], axis = 1)
predictions_df['bin_edges'] = predictions_df.apply(lambda x: np.histogram(x['margins'], bins = x['num_bins'], range = x['bin_bounds'])[1], axis = 1)

#Converting the bins and bin_edges to json strings, which can be easily saved in a csv
predictions_df['bins'] = predictions_df['bins'].apply(lambda x: json.dumps(x.tolist()))
predictions_df['bin_edges'] = predictions_df['bin_edges'].apply(lambda x: json.dumps(x.tolist()))

#Getting the number of simulations where each party wins
predictions_df['democrat_winning_num'] = predictions_df.apply(
    lambda x: np.sum(np.array(x['margins']) > x['threshold_winning']) , 
    axis = 1)
predictions_df['republican_winning_num'] = predictions_df.apply(
    lambda x: np.sum(np.array(x['margins']) < x['threshold_winning']), axis = 1)
predictions_df['tie_num'] = predictions_df.apply(
    lambda x: np.sum(np.array(x['margins']) == x['threshold_winning']), axis = 1)


predictions_df = predictions_df.drop(columns = ['margins', 'threshold_winning'])

# Getting predictions for today to add to the predictions over time dataframe
predictions_today = predictions_df.loc[:, ['state', 'district', 'office_type', 'median_margin']]
predictions_today['date'] = date.today().strftime("%m/%d/%Y")

# Creating the predictions over time dataframe
predictions_over_time = pd.read_csv('cleaned_data/Predictions over time.csv')

# Remove today's date if it exists in the DataFrame
today_date = date.today().strftime("%m/%d/%Y")
predictions_over_time = predictions_over_time[predictions_over_time['date'] != today_date]

# Concatenate the new predictions
predictions_over_time = pd.concat([predictions_over_time, predictions_today], ignore_index=True)

# Saving both dataframes
predictions_over_time.to_csv('cleaned_data/Predictions over time.csv', index=False)
predictions_df.to_csv('cleaned_data/Predictions.csv', index=False)
