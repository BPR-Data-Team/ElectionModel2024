# Information on all code

- OneTimeCleaning: All of the files for cleaning for data that do not change for the rest of the project, such as PVI data, cost-of-voting indices, etc.

- ContinuousCleaning: A folder containing all files for cleaning datasets that do change -- polls, economic indicators, etc.

- data_fetcher.py: A python script that fetches data from the internet and saves it to the data folder. You can run it with `python data_fetcher.py [filename]`. It will fetch the data from the internet and save it to the data folder. If you want to run the code, you will need to install the python package `requests` with `pip install requests` (or just run `pip install -r requirements.txt` to install all required packages).

- ModelPredicting.py: This python script runs the models every single day, on updated data created by ContinuousCleaning. It is long, but is also very heavily commented in hopes of making it easy to run by yourself. Once legal issues detailed in [Cleaned Data](../cleaned_data) are resolved (by either talking to them or the both of us), this file can be immediately run and you can see the results for yourself!

- ModelTrainingWithBootstrapping.ipynb: This python jupyter notebook contains all the code used to train the model. We train 10 models (each trained on bootstrapped data), and optimize hyperparameters via hyperopt. 
