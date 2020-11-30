# Diurnal-Phosphorus

Description of scripts 

Diurnal P - Import and Join.R - Imports, tidys, joins, then saves data (Only needs to be run once to create master dataframe frow which figures can be created) 

Diurnal P- Create figures.R - Create figures that attempt to visualize  

Diurnal P- Models.R - Attempts to model diel P pattern. Perhaps model P concentration uding Diel P model + other factors.

Diurnal P- Flow Scenarios.R - Calculates P load and FWM concentrations under alternative flow scenarios. 



Description of Figures

Date Range of RPA Data- Shows Date Range of RPA data. RPA data from stations from different time periods. 

Hourly TP Variation from the Daily Mean by Station- Shows diel P pattern at three differnet stations. Clear Diel P trend at all 3 stations 

Hourly Percent Variation in TP from the Daily Mean by Station-Shows diel P pattern at three different stations as percent. Clear Diel P trend at all 3 stations 

Hourly TP Variation from the Daily Mean by Station and Month- Shows diel P pattern at three different stations faceted by month. Clear Diel P trend at all 3 stations for almost all months!

TPO4 Deviation from Daily Mean by Station and Flow Category- Shows diel P pattern at three different stations under different flow conditions. Trend persists under all flow conditions!

PO4 vs Flow by Station and Season- TP vs Flow for wet/dry seasons. Not terribly informative

Hourly TP Variation from the Daily Mean by Station from days with continuous flow- Shows Diel P trend from only days of complete flow. Since Flow effects P and flow is majority during day could flow be was driver of Diel P signal? Linmiting to days of continoius flow mitgated this dynamic. Flow not driver of trend? 

TPO4 vs Flow by Station from Days of Continuous Flow- Flow vs TPO4 from days of continuous flow. Flow does effect overall tp concentrations but not Diel P pattern?

TPO4 Deviation from Daily Mean by Station and Flow Category from days of Continuous Flow- Flow does not appear to change the diel P trend from days of continuous flow. More evidence that changes in flow does not drive diel P pattern

TPO4 vs Flow by Station and Month from days with Continuous Flow- Diel P pattern doesn't change seasonaly. From days of continuous flow.

TP Variation from the Daily Mean Month with Daylight hours- Figure of Diel P with sunrise/setset displayed. Although kinda cool not sure if this really adds anything 

Rain Effects Variation from Daily Mean by Hour- Rain data is discrete near 0. This fig doesn't really show any noticeable trend with instantaneous rain instensity

Rain Effects Variation from Daily Mean by Hour boxplots- Same except boxplots- Still hard to see any trend. 

Rainy Days Affect on TP Variation- Rainy days do not seem to have effect on Diel P. Perhaps slightly diminished under heavy rains?

Max Daily Evaporation Effect on Diel P- Max daily evap does not seem to affect diel P pattern 

Wind Effect on P Variation from Daily Mean- There does seem to be slight effect of instantaneous wind speed on devaition from daily P mean! Higher winds are above daily mean

Max Daily Wind Effect on Diel P- Diel P pattern persistant for all wind categories

Stage Effect on Variation from Daily Mean- No noticble effect from stage on deviation from daily P mean

Max Daily Stage Effect on Diel P- Max daily stage did not seem to effect TP trend noticibly. 

Change in Stage Effect on Diel P- There is small but noticible decline in daily P when stage increases!

Change in Stage Effect on Diel P from days of over 1 inch stage change- Filtered to days where stage changed over 1 inch. Relationship between daily change in P and change in daily stage small and not strong but looks real.

Sonde Parameters vs Deviation in Daily P- Surprisingly did not see strong relationship between sonde parameters and devaition from daily mean P

Percent Flow by Hour- Most flow occurs during daytime when deviation from daily P is at Highest!

Percent Flow day or night- Column chart of percent flow. Same finding as above. 