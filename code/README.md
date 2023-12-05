# Information on all code

- pollster_ratings.R: A calculation using raw-polls.csv to determine various statistics on each pollster. The statistics are defined as follows:

| Statistic       | Description                                                                                                                                                                                  |
| --------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| avgbias         | The average distance between a pollster's predicted democratic margin and the actual democratic margin. Positive values mean a pollster leaned more democratic.                              |
| avg_error       | The average absolute value of the distance between a pollster's predicted margin and the actual margin.                                                                                      |
| pct_right       | The percent of races a pollster predicted correctly                                                                                                                                          |
| pct_dem         | The percent of races wherein a pollster predicted that the Democrat would win                                                                                                                |
| num_polls       | The total number of polls                                                                                                                                                                    |
| num_races       | The total number of races                                                                                                                                                                    |
| polls_per_race  | num_polls/num_races                                                                                                                                                                          |
| pct_outside_moe | Percent of results outside the MOE of the pollsters, calculated via their sample size                                                                                                        |
| dispersion      | The pollster's absolute value difference between their prediction and all other pollster's predictions (lower numbers mean the pollster "herded" closer to other pollsters)                  |
| relative error  | The average of a pollsters error minus the average error of all other pollsters in a race (negative numbers mean that this pollster was, on average, better than pollsters in the same race) |

- data_fetcher.py: A python script that fetches data from the internet and saves it to the data folder. You can run it with `python data_fetcher.py [filename]`. It will fetch the data from the internet and save it to the data folder. If you want to run the code, you will need to install the python package `requests` with `pip install requests` (or just run `pip install -r requirements.txt` to install all required packages).

- OneTimeCleaning: All of the files for cleaning for data that do not change for the rest of the project, such as PVI data, cost-of-voting indices, etc.

- ContinuousCleaning: A folder containing all files for cleaning datasets that do change -- polls, economic indicators, etc.
