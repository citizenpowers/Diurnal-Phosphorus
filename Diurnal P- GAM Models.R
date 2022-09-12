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
library(ggeffects)


#The Goal of this script is to extract the model characteristics from the data set under different environmental conditions
#Step 1 Import data
#Step 2 
#Step 2
#Step 3 
#Step 4


# Import Data ------------Step 1-------------------------------------------------

#RPA tidy data 
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") %>%
mutate(Flag=if_else(Station == "G334" & Date >"2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed  
filter(Flag ==FALSE)  %>% select(-Flag) %>%
rename(Wind="WIND BELLEGLADE") 

#Import Flow Data (needed to create DF of days of complete flow)
Combined_BK_Flow <- read_csv("Data/Combined_BK_Flow.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d"),  Inflow = col_number(), `Inflow HLR` = col_number(), Outflow = col_number(), `Outflow HLR` = col_number()))


# Create Models Deviation from 24 hour mean -------------------------------------------------------

GAM_Time = gam(Diff_24_hour_mean ~ s(Time), data = RPAs_with_Flow_Stage_Weather_Sonde)
GAM_Wind=gam(Diff_24_hour_mean ~ s(`Wind`), data = RPAs_with_Flow_Stage_Weather_Sonde)
GAM_Time_Wind=gam(Diff_24_hour_mean ~ s(Time)+s(`Wind`), data = RPAs_with_Flow_Stage_Weather_Sonde)
GAM_Time_Wind_interact=gam(Diff_24_hour_mean ~ s(Time,by=Wind), data = RPAs_with_Flow_Stage_Weather_Sonde)

test <-summary(GAM_Time)
summary(GAM_Time_Wind)
summary(GAM_Time_Wind_interact)
summary(GAM_Wind)

#plot models 
gratia::draw(GAM_Hour)
plot(ggeffects::ggpredict(GAM_Time), facets = TRUE)
plot(ggeffects::ggpredict(GAM_Time_Wind), facets = TRUE)
ggplot(data = drop_na(RPAs_with_Flow_Stage_Weather_Sonde,Diff_24_hour_mean), aes(y =Diff_24_hour_mean, x = Time)) +geom_point() + geom_line(aes(y = fitted(GAM_Time)), colour = "red",size = 1.2)+theme_bw()+coord_cartesian(ylim = c(-10, 10))

#test AIC
AIC(GAM_Time,GAM_Time_Wind)

#Outflow HLR has no effect on deviation from TP daily mean
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Outflow HLR`,Diff_24_hour_mean,color=Station_ID,fill=Station_ID))+geom_point()+coord_cartesian(ylim = c(-10, 10))+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
#Inflow HLR has no effect on deviation from TP daily mean
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Inflow HLR`,Diff_24_hour_mean,color=Station_ID,fill=Station_ID))+geom_point()+coord_cartesian(ylim = c(-10, 10))+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
#Inflow Stage has no effect on deviation from TP daily mean
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Inflow Stage`,Diff_24_hour_mean,color=Station_ID,fill=Station_ID))+geom_point()+coord_cartesian(ylim = c(-10, 10))+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
#Outflow Stage has no effect on deviation from TP daily mean
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Outflow Stage`,Diff_24_hour_mean,color=Station_ID,fill=Station_ID))+geom_point()+coord_cartesian(ylim = c(-10, 10))+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
#Evaporation has no effect on deviation from TP daily mean
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`EVAP S7`,Diff_24_hour_mean,color=Station_ID,fill=Station_ID))+geom_point()+coord_cartesian(ylim = c(-10, 10))+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
#Rain has no effect on deviation from TP daily mean
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Rain S7`,Diff_24_hour_mean,color=Station_ID,fill=Station_ID))+geom_point()+coord_cartesian(ylim = c(-10, 10))+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
#Wind looks to have an apparent affect on deviation from daily mean
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Wind`,Diff_24_hour_mean,color=Station_ID,fill=Station_ID))+geom_point()+coord_cartesian(ylim = c(-10, 10))+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
#Time has an affect on deviation from daily mean
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Time`,Diff_24_hour_mean,color=Station_ID,fill=Station_ID))+geom_point()+coord_cartesian(ylim = c(-10, 10))+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
#Wind vs Time
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Time`,Wind,color=Station_ID,fill=Station_ID))+geom_point()+coord_cartesian()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")


RPAs_with_Flow_Stage_Weather_Sonde %>%
group_by(Station_ID) %>%
summarise(n=n(),`observations of temp`=sum(is.finite(`Avg Hourly Temp`),na.rm=FALSE))



ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Outflow HLR`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Inflow HLR`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Outflow HLR`,`Outflow Stage`,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Outflow HLR`,`Inflow Stage`,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Inflow Stage`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")


# Models from data from only single station----------------------------------------------------

#build model
GAM_Time <- function(data) {gam(Diff_24_hour_mean ~ s(Time), data = data)}
GAM_Wind <- function(data) {gam(Diff_24_hour_mean ~ s(`Wind`), data = data)}
GAM_Time_Wind <- function(data) {gam(Diff_24_hour_mean ~ s(Time)+s(`Wind`), data = data)}


#nest data by station and run model
GAM_Models <- RPAs_with_Flow_Stage_Weather_Sonde %>% 
group_by(Station) %>% 
nest() %>%
mutate(model = map(data, GAM_Time),
       model_wind=map(data,GAM_Wind))

#summarize model in nested DF
GAM_Models_summary <- GAM_Models %>% 
mutate(
glance = map(model, broom::glance),
tidy = map(model, broom::tidy),
augment = map(model, broom::augment),
r.squared=map(model,~summary(.)$r.sq)) %>%
mutate(
glance_wind = map(model_wind, broom::glance),
tidy_wind = map(model_wind, broom::tidy),
augment_wind = map(model_wind, broom::augment),
r.squared_wind=map(model_wind,~summary(.)$r.sq))

#Unnest data summaries 
GAM_Models_summary %>% 
unnest(glance) %>% select(df,AIC,BIC,deviance,df.residual,nobs)

GAM_Models_summary %>% 
unnest(tidy) %>% select(edf,ref.df,statistic,p.value)

GAM_Models_summary %>% 
unnest(augment) 

r.squared <- GAM_Models_summary %>% select(Station,r.squared) %>%
unnest(r.squared)  %>%
arrange(desc(r.squared))

Unnested_summary <- r.squared %>% 
left_join(GAM_Models_summary %>% unnest(glance) %>% select(df,AIC,BIC,deviance,df.residual,nobs),by="Station") %>% #join with glance summary
left_join(GAM_Models_summary %>% unnest(tidy) %>% select(edf,ref.df,statistic,p.value),by="Station") #join with tidy summary

Unnested_summary_Wind <- GAM_Models_summary %>% select(Station,r.squared_wind) %>% unnest(r.squared_wind)  %>% arrange(desc(r.squared_wind)) %>%
left_join(GAM_Models_summary %>% unnest(glance_wind) %>% select(df,AIC,BIC,deviance,df.residual,nobs),by="Station") %>% #join with glance summary
left_join(GAM_Models_summary %>% unnest(tidy_wind) %>% select(edf,ref.df,statistic,p.value),by="Station") #join with tidy summary

par(mfrow = c(2, 2))
plot(GAM_Time_Wind, page = 1, all.terms = TRUE)


AIC(GAM_Time,GAM_Time_Wind)
k.check(GAM_Time)

par(mfrow = c(2, 2))
gam.check(GAM_Time)
