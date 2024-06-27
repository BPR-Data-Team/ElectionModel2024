# Information on all code

All code in this folder was ran once and not again. 

- COVI.R: Cleans data regarding Cost of Voting Index
- Governor, House, Senate, President: Self-explanatory -- each cleans data regarding those races. 
- PastPolls.ipynb: Cleaned polls in previous years to bring them to the same format.
- pivotPredictionsOverTime.R: Used once (literally) to pivot data in python to make the predictions over time data work well. Is practically defunct now, since the data now remains in the same format each update.
- Pollster Ratings.ipynb: Ran an XGBoost algorithm on previous years' pollster errors to determine how well each pollster performs relative to others using the same pollster-agnostic features (sample size, type of poll, etc.) 
- SabatoRatings.R: Cleaning Sabato Crystal Ball expert ratings