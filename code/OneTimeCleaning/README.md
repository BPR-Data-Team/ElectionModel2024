# This folder holds all the cleaning files that do not need to be updated over time, such as historical elections. 

All files with Historical in front of them hold cleaning for that office_type going back to 2002. 
HistoricalPresident also includes calculations for PVI, which we got from the Cook Political Review. 

## PVI Calculations

We do not publish our calculations for PVI in this github because we had to pay for the subscription and are not allowed to republish for free. We use PVI within each of the historical elections to calculate incumbent_differential. 

The formula for PVI is weirdly complicated. Over all jurisdictions "i", recent presidential elections 1 and 2 (with 1 being most recent) and general (gen) results for the entire country,

$PVI_i = 0.75 \left(\frac{DEM_{i, 1}}{DEM_{i, 1} + REP_{i, 1}} - \frac{DEM_{\text{gen}, 1}}{DEM_{\text{gen}, 1} + REP_{\text{gen}, 1}}\right) + \\
0.25 \left( \frac{DEM_{i, 2}}{DEM_{i, 2} + REP_{i, 2}} - \frac{DEM_{\text{gen}, 2}}{DEM_{\text{gen}, 2} + REP_{\text{gen}, 2}} \right)$

Positive values indicate that a jurisdiction leans democratic.

To calculate incumbent differential, we calculate the two-party share democratic margin, and then subtract twice the PVI for that jurisdiction and that election. 