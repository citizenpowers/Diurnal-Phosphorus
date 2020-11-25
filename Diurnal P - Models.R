
library(tidymodels) 
library(modelr)
library(caret)
library(broom)
library(splines)
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



# Import data -------------------------------------------------------------


#Import RPA Flow and Stage and weather data
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv")


# Tidy Model Method using spline ------------------------------------------------------------

#remove outliers
RPA_Tidy_model_data<- RPAs_with_Flow_Stage_Weather_Sonde %>%
filter(TPO4 < 60,abs(Diff_24_hour_mean) <10 )   #remove right skew, outliers

RPA_Tidy_model_data_hourly_mean <- RPA_Tidy_model_data %>%
group_by(Station,Hour) %>%
summarise(`TPO4 mean`=mean(TPO4,na.rm=TRUE),`Avg Deviation from Daily mean`=mean(Diff_24_hour_mean,na.rm=TRUE) )


RPA_Tidy_model_data_train <- RPA_Tidy_model_data %>% dplyr::sample_frac(.75)
RPA_Tidy_model_data_test  <- dplyr::anti_join(RPA_Tidy_model_data, RPA_Tidy_model_data_train, by = c("Station","Date","Hour"))

#spline models
mod1 <- lm(Diff_24_hour_mean ~ ns(Hour, 1), data = RPA_Tidy_model_data_train)
mod2 <- lm(Diff_24_hour_mean ~ ns(Hour, 2), data = RPA_Tidy_model_data_train)
mod3 <- lm(Diff_24_hour_mean ~ ns(Hour, 3), data = RPA_Tidy_model_data_train)  #i like model3 the best of spline models
mod4 <- lm(Diff_24_hour_mean ~ ns(Hour, 4), data = RPA_Tidy_model_data_train)
mod5 <- lm(Diff_24_hour_mean ~ ns(Hour, 5), data = RPA_Tidy_model_data_train)

grid <- RPA_Tidy_model_data_train  %>% 
data_grid(Hour = seq_range(Hour, n = 500, expand = 0.1)) %>% 
gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "Diff_24_hour_mean")

#visualize models on trainign data
ggplot(RPA_Tidy_model_data_train, aes(Hour, Diff_24_hour_mean)) + 
geom_point() + geom_line(data=RPA_Tidy_model_data_hourly_mean, aes(Hour,`Avg Deviation from Daily mean`), color="blue")+
geom_line(data = grid, colour = "red") + theme_bw()+
facet_wrap(~model)

#visualize models on test data
ggplot(RPA_Tidy_model_data_test, aes(Hour, Diff_24_hour_mean)) + 
geom_point() + geom_line(data=RPA_Tidy_model_data_hourly_mean, aes(Hour,`Avg Deviation from Daily mean`), color="blue")+
geom_line(data = grid, colour = "red") + theme_bw()+
facet_wrap(~ model)

#predict TPO4 using model3
RPA_Tidy_model_predict <-  RPA_Tidy_model_data %>%
add_predictions(mod3) %>%
mutate(date_time=ISOdate(Year,match(Month,month.abb),Day,Hour,Minute,0,tz = "America/New_York"),`Predicted TPO4`=`24_hour_mean`+pred)

#visualize predictions over entire time series
ggplot(RPA_Tidy_model_predict, aes(date_time,TPO4,color=Station))+geom_point()+facet_wrap(~Station,ncol=1)+theme_bw()+
geom_point(data=RPA_Tidy_model_predict, aes(date_time,`Predicted TPO4`), color="black")

#visualize predictions over short time 
ggplot(RPA_Tidy_model_predict, aes(date_time,TPO4,color=Station))+geom_point()+facet_wrap(~Station,ncol=1)+theme_bw()+
geom_line(data=RPA_Tidy_model_predict, aes(date_time,`Predicted TPO4`), color="black")+scale_x_datetime(limits = ymd_hms(c("2014-07-01 00:00:00", "2014-07-30 00:00:00")))


RPA_Tidy_model_data_test_pred <- RPA_Tidy_model_data_test %>%
add_predictions(mod3) 
  
Model_performance <- data.frame(RMSE = RMSE(RPA_Tidy_model_data_test_pred$pred, RPA_Tidy_model_data_test_pred$Diff_24_hour_mean),R2 = R2(RPA_Tidy_model_data_test_pred$pred, RPA_Tidy_model_data_test_pred$Diff_24_hour_mean))

print(Model_performance)









# Test code ---------------------------------------------------------------



#create a TSibble object
RPA_Model_tsibble<- RPAs_with_Flow_Stage_Weather_Sonde %>%
filter(TPO4 < 60,Date >"2016-02-08",Date <"2017-09-02",abs(Diff_24_hour_mean) <10 ) %>%  #remove right skew, outliers
mutate(Date_time=ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York"))    %>%
select(Date_time,Station,Diff_24_hour_mean)  %>%
distinct() %>%      #duplicates in data? follow up on
as_tsibble(index =Date_time, key = c("Station")) 

#Check for missing data
RPA_gaps <- RPA_Model_tsibble%>% count_gaps(.full = TRUE)

#visualize gaps
ggplot(RPA_gaps, aes(x = Station, colour = Station)) +geom_linerange(aes(ymin = .from, ymax = .to)) +geom_point(aes(y = .from)) +geom_point(aes(y = .to)) +coord_flip() +theme(legend.position = "bottom")

#fill gaps
RPA_Model_tsibble_full <- RPA_Model_tsibble %>% fill_gaps(.full = TRUE) %>% arrange(Date_time) %>% fill(Diff_24_hour_mean) 

#TPO4
RPA_Model_tsibble_full %>% gg_season(TPO4,period="day",na.rm=TRUE) + theme(legend.position = "none")

#deviation from daily mean
RPA_Model_tsibble_full %>% gg_season(Diff_24_hour_mean,period="day",na.rm=TRUE) + theme(legend.position = "none")

autoplot(RPA_Model_tsibble_full) +
autolayer(naive(RPA_Model_tsibble_full, h=11),series="Naïve", PI=FALSE) +
autolayer(snaive(RPA_Model_tsibble_full, h=11),series="Seasonal naïve", PI=FALSE) 

test3 <-test  %>% forecast(h=50) #%>% autoplot()   #niave


fc <- hw(subset(hyndsight,end=length(hyndsight)-35),
         damped = TRUE, seasonal="multiplicative", h=35)
  autoplot(hyndsight) +
  autolayer(fc, series="HW multi damped", PI=FALSE)+
  guides(colour=guide_legend(title="Daily forecasts"))
  
  
test4  <-  hw(RPA_Model_tsibble_full, damped = TRUE, seasonal="multiplicative", h=50)


test <-naive(RPA_Model_tsibble_full, h=50)
test1 <-snaive(RPA_Model_tsibble_full, h=11)


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