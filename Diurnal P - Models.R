
library(forecast)
library(moderndive)
library(feasts)
library(tsibble)
library(xts)
library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(maptools)
library(ggpmisc)




#Import RPA Flow and Stage and weather data
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv")



#create a TSibble object
RPA_Model_tsibble<- RPAs_with_Flow_Stage_Weather_Sonde %>%
filter(TPO4 < 60,Date >"2016-02-08",Date <"2017-09-02",abs(Diff_24_hour_mean) <10 ) %>%  #remove right skew, outliers
mutate(Date_time=ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York"))    %>%
select(Date_time,Station,Diff_24_hour_mean,TPO4)  %>%
distinct() %>%      #duplicates in data? follow up on
as_tsibble(index =Date_time, key = c("Station")) 

#Check for missing data
RPA_gaps <- RPA_Model_tsibble%>% count_gaps(.full = TRUE)

#visualize gaps
ggplot(RPA_gaps, aes(x = Station, colour = Station)) +geom_linerange(aes(ymin = .from, ymax = .to)) +geom_point(aes(y = .from)) +geom_point(aes(y = .to)) +coord_flip() +theme(legend.position = "bottom")

#fill gaps
RPA_Model_tsibble_full <- RPA_Model_tsibble %>% fill_gaps(.full = TRUE) %>% arrange(Date_time) %>% fill(TPO4,Diff_24_hour_mean)

#TPO4
RPA_Model_tsibble_full %>% gg_season(TPO4,period="day",na.rm=TRUE) + theme(legend.position = "none")


#deviation from daily mean
RPA_Model_tsibble_full %>% gg_season(Diff_24_hour_mean,period="day",na.rm=TRUE) + theme(legend.position = "none")






# Test code ---------------------------------------------------------------



#Plot of data to be modeled -TP by Station 
ggplot(RPAs_with_Flow_Model_data,aes(date,TPO4,color=Station))+geom_point(shape=1)+theme_bw()+
facet_wrap(~Station,nrow=3)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(0,100),breaks = seq(0,100,10))+
#scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="TPO4 by Station",y="TPO4  (ug/L)",x="Date")

#create data frame to model from
RPAs_with_Flow_Model_data <-RPAs_with_Flow_Stage_Weather_Sonde %>%
mutate(date=ISOdate(Year,match(Month,month.abb),Day,Hour,Minute,0,tz = "America/New_York"))   %>%
filter(TPO4<60) %>% #filter right sided skew
mutate(`Flow_Category`=`Flow Category`) %>%
#filter(Station =="G381") %>%
ungroup() 

# Fit linear regression model:
# Set random number generator seed value for reproducibility
set.seed(76)


# Randomly reorder the rows
RPAs_with_Flow_Model_data_shuffled <- RPAs_with_Flow_Model_data %>% 
sample_frac(size = 1, replace = FALSE)

# Train/test split
train <-RPAs_with_Flow_Model_data_shuffled %>%
slice(1:5000)

test <- RPAs_with_Flow_Model_data_shuffled %>%
  slice(5001:8080)

LM_TPO4_model <- lm(TPO4 ~Year+Month+`Flow_Category`+Hour, data = train)

#Assess accuracy of model
get_regression_summaries(LM_TPO4_model, digits = 3, print = FALSE)

checkresiduals(LM_TPO4_model)
summary(LM_TPO4_model)

#other models
#create time series DF
RPAs_with_Flow_Model_data_time_series <- filter(RPAs_with_Flow_Model_data ,Station=="G381") %>%
select(date,TPO4)   

RPAs_with_Flow_ts <-as.ts(na.omit(RPAs_with_Flow_Model_data_time_series))

RPAs_with_Flow_xts <-xts(na.omit(RPAs_with_Flow_Model_data_time_series[,-1]),order.by = RPAs_with_Flow_Model_data_time_series$date)

rosewine.train.ts <- window(RPAs_with_Flow_ts, start=date[1], end=date[5552])


#view data to be modeled
autoplot(RPAs_with_Flow_ts)
autoplot(RPAs_with_Flow_xts)

# Fit naive model:
RPAs_with_Flow_niave <-forecast::naive(RPAs_with_Flow_ts,h=10)
# Fit simple exponential smoothing
RPAs_with_Flow_ses <- ses(RPAs_with_Flow_xts,h=50)
#holt model
Hult_add <- hw(RPAs_with_Flow_xts,seasonal="additive")
Hult_mult <- hw(RPAs_with_Flow_xts,seasonal="multiplicative")
#ARIMA models
RPAs_with_Flow_ARIMA <- auto.arima(RPAs_with_Flow_xts)
#MSTS Model
RPAs_with_Flow_MSTS <- msts(RPAs_with_Flow_xts, seasonal.periods = c(24, 24*31))
#TBATS model
RPAs_with_Flow_TBATS <- RPAs_with_Flow_xts %>% log() %>% tbats(use.box.cox = FALSE, use.trend = TRUE, use.damped.trend = TRUE)

#Plot models
RPAs_with_Flow_niave %>% forecast(h=10) %>% autoplot()   #niave
RPAs_with_Flow_ses %>% forecast(h=50) %>% autoplot()    #ses
RPAs_with_Flow_ARIMA  %>% forecast(h=50) %>% autoplot()    #ARIMA
RPAs_with_Flow_MSTS  %>% forecast(h=50) %>% autoplot()    #MSTS
RPAs_with_Flow_TBATS  %>% forecast(h=50) %>% autoplot()    #TBATS

#assess models
checkresiduals(RPAs_with_Flow_niave)
checkresiduals(RPAs_with_Flow_ses)
checkresiduals(RPAs_with_Flow_ARIMA)
checkresiduals(RPAs_with_Flow_MSTS)
checkresiduals(RPAs_with_Flow_TBATS)

summary(RPAs_with_Flow_niave)
summary(RPAs_with_Flow_ses)
summary(RPAs_with_Flow_ARIMA)
summary(RPAs_with_Flow_MSTS)
summary(RPAs_with_Flow_TBATS)