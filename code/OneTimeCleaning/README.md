# Information on all code

All code in this folder was ran once and not again. 

- COVI.R: Cleans data regarding Cost of Voting Index
- Governor, House, Senate, President: Self-explanatory -- each cleans data regarding those races. 
- pivotPredictionsOverTime.R: Used once (literally) to pivot data in python to make the predictions over time data work well. Is practically defunct now, since the data now remains in the same format each update.
- New Pollster Ratings.ipynb: The code that trained an XGBoost model for pollster ratings. This predicts the variance (error squared) and bias (signed error) of a poll given various facts (pollster, sample size, etc)
- SabatoRatings.R: Cleaning Sabato Crystal Ball expert ratings