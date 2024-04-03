# This folder holds all the cleaning files that do not need to be updated over time, such as historical elections. 

All files with Historical in front of them hold cleaning for that office_type going back to 2002. 
HistoricalPresident also includes calculations for PVI, which we got from the Cook Political Review. 

## PVI Calculations

We do not publish our calculations for PVI in this github because we had to pay for the subscription and are not allowed to republish for free. We use PVI within each of the historical elections to calculate incumbent_differential. 

The formula for PVI is weirdly complicated. Over all jurisdictions i, and elections t

$PVI_{i, t} = 0.75 \left(\frac{DEM_{i, t-1}}{DEM_{i, t-1} + REP_{i, t-1}} - \frac{DEM_{\text{gen}, t-1}}{DEM_{\text{gen}, 1} + REP_{\text{gen}, t-1}}\right) + \\
0.25 \left( \frac{DEM_{i, t-2}}{DEM_{i, t-2} + REP_{i, t-2}} - \frac{DEM_{\text{gen},t-2}}{DEM_{\text{gen}, t-2} + REP_{\text{gen}, t-2}} \right)$

Positive values indicate that a jurisdiction leans democratic.

Note: it very often seems that, by multiplying the PVI by two, one receives a "margin differential" -- how much greater the margin is than the generic margin. This is TRUE if there are no independents -- if there are independents, then it is no longer true.

To calculate incumbent differential for time t + 1, we use the following formula, for jurisdiction i and time t:

$\text{Diff}_{i, t} = 100\left(\frac{D_{i, t-1}}{D_{i, t}+R_{i, t-1}} - \frac{D_{t-1}}{D_{t-1} + R_{t-1}}\right) - PVI_{i, t}$

## Special Election Calculations

We have no reason to assume that republican inbumbents do better than democratic incumbents (or vice versa) so we can assume that

$0 = \mathbb{E}_i[\text{Diff}_{i, t}] = \mathbb{E}_i[100\frac{D_{i, t-1}}{D_{i, t-1}+R_{i, t-1}} - PVI_{i, t}] - 100\frac{D_t}{D_t + R_t}$

Obviously, our goal is to model $\frac{D_t}{D_t + R_t}$, the democrat two-party generic ballot results. We can utilize the LLN and special elections to do this, based on the previous formula:

$100\frac{D_t}{D_t + R_t} = \frac1n \sum_{i = 1}^n \left(100\frac{D_{i, t}}{D_{i, t}+R_{i, t}} - PVI_{i, t}\right)$
