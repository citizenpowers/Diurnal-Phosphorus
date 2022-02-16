# the objective of theis script it is to calcualte auto correlation for the series
library(tseries)
library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(maptools)
library(ggpmisc)
library(e1071)
library(purrr)
library(broom)
library(mgcv)
library(gratia)
library(zoo)

# Import Data ------------Step 1-------------------------------------------------

#RPA tidy data 
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") %>%
mutate(Flag=if_else(Station == "G334" & Date >"2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed  
filter(Flag ==FALSE)  %>% select(-Flag)



# Create vectors of devaitions from daily mean by station  -----------------

G384_full_days <- RPAs_with_Flow_Stage_Weather_Sonde %>%
filter(is.finite(Diff_24_hour_mean)) %>%
filter(Station=="G384") %>%
group_by(Date) %>%
summarise(n=n()) %>%
filter(n==8)

2256/8
G384_ts <- RPAs_with_Flow_Stage_Weather_Sonde %>%
filter(Station=="G384") %>%  
inner_join(G384_full_days, by="Date") 

#autocorrelation no lag
print(acf(G384_ts$Diff_24_hour_mean,pl=TRUE))

#autocorrelation lag 1
print(acf(G384_ts$Diff_24_hour_mean,lag=1,pl=TRUE))

#autocorrelation lag 2
print(acf(G384_ts$Diff_24_hour_mean,lag=2,pl=TRUE))

#autocorrelation lag 3
print(acf(G384_ts$Diff_24_hour_mean,lag=3,pl=TRUE))

#autocorrelation lag 4
print(acf(G384_ts$Diff_24_hour_mean,lag=4,pl=TRUE))

#autocorrelation lag 5
print(acf(G384_ts$Diff_24_hour_mean,lag=5,pl=TRUE))

#autocorrelation lag 6
print(acf(G384_ts$Diff_24_hour_mean,lag=6,pl=TRUE))

#autocorrelation lag 
acf(G384_ts$Diff_24_hour_mean,lag=7,pl=TRUE))

#autocorrelation lag 8
acf(G384_ts$TPO4,lag=8,correlation = TRUE,pl=FALSE)



