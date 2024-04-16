import pandas as pd
import numpy as np
import pickle as pkl
import lightgbm as lgb
import xgboost
import re
import time
import shap

from sklearn.preprocessing import OneHotEncoder, OrdinalEncoder
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import make_pipeline, Pipeline
from sklearn.model_selection import BaseCrossValidator
from sklearn.metrics import  accuracy_score, median_absolute_error, make_scorer

from hyperopt import fmin, tpe, hp, Trials, STATUS_OK
from hyperopt.early_stop import no_progress_loss

np.seterr(divide='ignore', invalid='ignore')

#Categorical features that need to be one-hot encoded    
one_hot_fts = ['office_type']

#Rating is the only ordinal feature
ordinal_fts = ['final_rating']
ordinal_fts_ranking = ['Safe R', 'Likely R', 'Leans R', 'Toss-up', 'Leans D', 'Likely D', 'Safe D']

#Cont features that should be pass-throughed (and later scaled)
cont_fts = ['open_seat','incumbent_differential', 'absenteeexcusereq', 'special', 'isMidterm',
       'pollhours', 'avgpollhours', 'minpollhours', 'regdeadlines',
       'voteridlaws', 'novoterid', 'noallmailvote', 'noearlyvote',
       'nofelonreg', 'nofelonsregafterincar', 'nonstrictid', 'nonstrictphoto',
       'nopollplacereg', 'nopr', 'nosamedayreg', 'nostateholiday', 'pr16',
       'pr17', 'pr175', 'pr60', 'pr90', 'strictid', 'strictphoto', 'covi_num',
       'prev_gen_margin', 'prev_dem_gen_tp', 'weighted_genpoll', 'unweighted_genpoll',
       'mean_specials_differential', 'house_chamber_margin',
       'senate_chamber_margin', 'previous_cci', 'current_cci', 'change_cci',
       'previous_gas', 'current_gas', 'change_gas', 'previous_unemployment',
       'current_unemployment', 'change_unemployment', 'receipts_DEM',
       'receipts_REP', 'disbursements_DEM', 'disbursements_REP',
       'unconvinced_pct', 'phone_unweighted', 'online_unweighted', 'num_polls',
       'unweighted_estimate', 'unweighted_ci_lower', 'unweighted_ci_upper',
       'weighted_estimate', 'weighted_ci_lower', 'weighted_ci_upper',
       'white_pct', 'black_pct', 'asian_pct', 'hispanic_pct', 'median_income',
       'impoverished_pct', 'median_age', 'renting_pct', 'inflation',
       'isMidterm', 'receipts_ratio', 'disbursements_ratio', 'total_receipts',
       'total_disbursements', 'genballot_predicted_margin',
       'specials_predicted_margin', 'receipts_genballot_interaction', 'poll_fundamental_agree',
       'disbursements_genballot_interaction', 'gas_democrat_interaction', 'cci_democrat_interaction', 'genballot_predicted_lower',
       'genballot_predicted_upper', 'democrat_in_presidency', 'similar_poll_differential', 'combined_prediction']


shap_features = {
    "Electoral Dynamics": ['open_seat','incumbent_differential', 'weighted_genpoll', 'unweighted_genpoll',
       'mean_specials_differential', 'genballot_predicted_margin',
       'specials_predicted_margin', 'poll_fundamental_agree', 'genballot_predicted_lower',
       'genballot_predicted_upper'],
        
    "Voter ID Laws": ['voteridlaws', 'novoterid', 'nonstrictid', 'strictid', 'strictphoto'],
    
    "Voter Registration Laws": ['nofelonreg', 'nofelonsregafterincar', 'nonstrictphoto',
       'nopollplacereg', 'nosamedayreg', 'pr16',
       'pr17', 'pr175', 'pr60', 'pr90', 'nopr'], 
    
    "Mail-in Voting": ['noallmailvote', 'noearlyvote'],
    
    "Misceallaneous Voting Laws": ['absenteeexcusereq', 'pollhours', 'avgpollhours', 'minpollhours', 'regdeadlines', 'nostateholiday', 'covi_num'],
    
    "Polls from this race": ['unconvinced_pct', 'phone_unweighted', 'online_unweighted', 'num_polls',
       'unweighted_estimate', 'unweighted_ci_lower', 'unweighted_ci_upper',
       'weighted_estimate', 'weighted_ci_lower', 'weighted_ci_upper'], 
    
    "Demographics": ['white_pct', 'black_pct', 'asian_pct', 'hispanic_pct', 'median_income',
       'impoverished_pct', 'median_age', 'renting_pct'], 
    
    "Campaign Finance": ['receipts_DEM',
       'receipts_REP', 'disbursements_DEM', 'disbursements_REP','inflation',
       'receipts_ratio', 'disbursements_ratio', 'total_receipts',
       'total_disbursements', 'disbursements_genballot_interaction', 'receipts_genballot_interaction'], 
        
    "Consumer Confidence Index": ['previous_cci', 'current_cci', 'change_cci', 'cci_democrat_interaction'],
    
    "Gas Prices": ['previous_gas', 'current_gas', 'change_gas', 'gas_democrat_interaction'], 
    
    "Unemployment":  ['previous_unemployment','current_unemployment', 'change_unemployment'],
    
    "Polls from other races": ['similar_poll_differential'],
    
    "Expert Rating": ['final_rating'],
        
    "Composition of Congress/Presidency": ['democrat_in_presidency', 
              'house_chamber_margin', 'senate_chamber_margin'],
    
    "Type of election": ['office_type', 'special', 'isMidterm'],
    
    "Other": ['prev_gen_margin', 'prev_dem_gen_tp']
}

preprocessor = ColumnTransformer([
        ('cat', OneHotEncoder(), one_hot_fts),
        ('ord', OrdinalEncoder(categories = [ordinal_fts_ranking], handle_unknown='use_encoded_value', 
                               unknown_value=np.nan), ordinal_fts),
        ('num', 'passthrough', cont_fts)])

#Pre-decided number -- the number of trials we're running (probably 1000, on OSCAR)
num_trials = 2 

data = pd.read_csv('../cleaned_data/Engineered Dataset.csv')
X = data.drop(columns = ['margin'])
y = data['margin']
X_train, X_predict, y_train, y_predict = (X.loc[X['year'] < 2024, :], X.loc[X['year'] == 2024, :].reset_index(), 
                                        y.loc[X['year'] < 2024], y.loc[X['year'] == 2024].reset_index())

transformed_X_train = preprocessor.fit_transform(X_train)
transformed_X_predict = preprocessor.transform(X_predict)
col_names = preprocessor.get_feature_names_out()

#Getting array with predictions
predictions_array = np.zeros((X_predict.shape[0], num_trials))
shap_values_array = []
for idx in range(num_trials):
    file_path = f"../models/Model_{idx}.pkl"

    # Open a file to write in binary mode????        
    with open(file_path, 'rb') as file:
        trained_model = pkl.load(file)
    
    predictions = trained_model.predict(transformed_X_predict)
    
    Explainer = shap.TreeExplainer(trained_model)
    shap_values = Explainer.shap_values(transformed_X_predict)    
    
    predictions_array[:, idx] = predictions
    shap_values_array.append(shap_values)   
    
print(np.mean(shap_values_array, axis = 0).shape)
predictions_df = pd.DataFrame(predictions_array, columns = [f"Margin_{idx}" for idx in range(num_trials)])
predictions_df['state'] = X_predict['state']
predictions_df['district'] = X_predict['district']
predictions_df['office_type'] = X_predict['office_type']

predictions_df = predictions_df[['state', 'district', 'office_type'] + [f"Margin_{idx}" for idx in range(num_trials)]]


def list_contains_col(col, array):
    for item in array:
        if re.search(item, col) is not None:
            return True
    return False


shap_df = pd.DataFrame(np.mean(shap_values_array, axis = 0), columns = col_names)

for key in shap_features:
    shap_df[key] = shap_df[[col for col in shap_df.columns if list_contains_col(col, shap_features[key])]].sum(axis = 1)
    
shap_df['state'] = X_predict['state']
shap_df['district'] = X_predict['district']
shap_df['office_type'] = X_predict['office_type']
shap_df = shap_df[['state', 'district', 'office_type'] + list(shap_features.keys())]

shap_df.to_csv('../cleaned_data/SHAP_Values.csv', index = False)
predictions_df.to_csv('../cleaned_data/Predictions.csv', index = False)
        
print(shap_df.columns)
    
        
    