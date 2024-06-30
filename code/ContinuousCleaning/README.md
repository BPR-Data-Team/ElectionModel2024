# Information on all code

- 538polls.R, CurrentPresPolls.R: Defunct code originally used to analyze 538's polls. We now use **PollingAverages.R** instead.
- FEC.R: Cleans incoming campaign finance data.
- Fundamentals: Cleans various fundamental data, such as inflation and unemployement.
- CensusDemographics.R: Cleans census data. It is continuously cleaned because sometime in September the new demographic data will be released, so we're simply automating that process!
- CombiningData.R: Combines all other data in continuous cleaning, and is called last when updating the model.