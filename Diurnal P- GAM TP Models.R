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


#The Goal of this script is to find which factors affect TP
#followed general instructions from http://r.qcbs.ca/workshop08/book-en/how-gams-work.html and https://m-clark.github.io/generalized-additive-models/application.html


#Step 1 Import data
#Step 2 Use scatterplots to find factors which might be causal
#Step 2 Create models using possible causal factors
#Step 3 Evaluate models
#Step 4 Verify model assumptions


# Import Data ------------Step 1-------------------------------------------------

#RPA tidy data 
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") %>%
mutate(Flag=if_else(Station == "G334" & Date >"2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed  
filter(Flag ==FALSE)  %>% select(-Flag) %>%
rename(Wind="WIND BELLEGLADE",HLRin="Inflow HLR",HLRout="Outflow HLR",Flowpath="Flowpath Region") %>% #models will not accept variables with blank spaces as input 
mutate(Station_ID=as.factor(Station_ID),Flowpath=as.factor(Flowpath)) %>%
mutate(Month=as.factor(month(Date, label=TRUE, abbr=TRUE))) %>%
mutate(Day=yday(Date)) %>%
filter(TPO4>0)


#Import Flow Data (needed to create DF of days of complete flow)
Combined_BK_Flow <- read_csv("Data/Combined_BK_Flow.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d"),  Inflow = col_number(), `Inflow HLR` = col_number(), Outflow = col_number(), `Outflow HLR` = col_number()))



# Create scatterplots ----------------------------------------------------


ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`HLRout`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_grid(`Flowpath Region`~Flowway,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`HLRin`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`HLRout`,`Outflow Stage`,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`HLRout`,`Inflow Stage`,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Inflow Stage`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Wind`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Flowpath`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Month`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")

ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(TPO4))+geom_density()



# Build Model all stations included ---------------------------------------

test <-RPAs_with_Flow_Stage_Weather_Sonde %>%
group_by(Station_ID,Month) %>%
count()

TP_GAM_month <- gam(TPO4 ~s(Month),method="REML",data =RPAs_with_Flow_Stage_Weather_Sonde,select=TRUE)

TP_GAM_day <- gam(TPO4 ~s(Day,bs="gp")+s(Time,bs="cc")+s(Station_ID,bs="re")+s(HLRin,k=10)+s(HLRout,k=10)+Wind,method="REML",select=TRUE,data =RPAs_with_Flow_Stage_Weather_Sonde)
summary(TP_GAM_day)
plot(TP_GAM_day, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
plot(ggeffects::ggpredict(TP_GAM_day), facets = TRUE)



TP_GAM_All_Stations <- gam(TPO4 ~ s(Time)+s(HLRin,k=60)+s(HLRout,k=60)+s(Day,k=60)+s(Station_ID)+Flowpath+Wind,data =RPAs_with_Flow_Stage_Weather_Sonde)
summary(TP_GAM_All_Stations)

TP_Stations <- gam(TPO4 ~ Station_ID, data =RPAs_with_Flow_Stage_Weather_Sonde)
summary(TP_Stations)

concurvity(TP_GAM_All_Stations)
gam.check(TP_GAM_All_Stations, k.rep = 1000)

plot(TP_GAM_day, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)

plot(TP_GAM_All_Stations, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
plot(ggeffects::ggpredict(TP_GAM_All_Stations), facets = TRUE)
gratia::draw(TP_Stations)

# Build Models -------Model built for each station individually-------------------------------------------------
TP_GAM_Time <- function(data) {gam(TPO4 ~ s(Time), data = data)}    #function of tp~time
TP_GAM_HLRin <- function(data) {gam(TPO4 ~ s(`HLRin`), data = data)}   #
TP_GAM_HLRout <- function(data) {gam(TPO4 ~ s(`HLRout`), data = data)}


#nest data by station and run model
TP_GAM_Models <- RPAs_with_Flow_Stage_Weather_Sonde %>% 
group_by(Station) %>% 
nest() %>%
mutate(TP_GAM_Time = map(data, TP_GAM_Time),
       TP_GAM_HLRin=map(data,TP_GAM_HLRin),
       TP_GAM_HLRout=map(data,TP_GAM_HLRout))

#summarize model in nested DF
TP_GAM_Models_summary <- TP_GAM_Models %>% 
mutate(TP_Time_glance = map(TP_GAM_Time,glance),TP_Time_tidy = map(TP_GAM_Time,tidy),TP_Time_augment = map(TP_GAM_Time,augment),TP_Time_r.squared=map(TP_GAM_Time ,~summary(.)$r.sq)) %>%
mutate(TP_HLRin_glance = map(TP_GAM_HLRin,glance),TP_HLRin_tidy = map(TP_GAM_HLRin,tidy),TP_HLRin_augment = map(TP_GAM_HLRin,augment),TP_HLRin_r.squared=map(TP_GAM_HLRin,~summary(.)$r.sq)) %>%
mutate(TP_HLRout_glance = map(TP_GAM_HLRout,glance),TP_HLRout_tidy = map(TP_GAM_HLRout,tidy),TP_HLRout_augment = map(TP_GAM_HLRout,augment),TP_HLRout_r.squared=map(TP_GAM_HLRout,~summary(.)$r.sq))

TP_Time_summary <- cbind(TP_GAM_Models_summary %>% select(Station,TP_Time_r.squared) %>% unnest(TP_Time_r.squared)  %>% arrange(desc(TP_Time_r.squared)) %>%
left_join(TP_GAM_Models_summary %>% unnest(TP_Time_glance) %>% select(df,AIC,BIC,deviance,df.residual,nobs),by="Station") %>% #join with glance summary
left_join(TP_GAM_Models_summary %>% unnest(TP_Time_tidy) %>% select(edf,ref.df,statistic,p.value),by="Station"),model="Time") #join with tidy summary

TP_HLRin_summary <- cbind(TP_GAM_Models_summary %>% select(Station,TP_HLRin_r.squared) %>% unnest(TP_HLRin_r.squared)  %>% arrange(desc(TP_HLRin_r.squared)) %>%
left_join(TP_GAM_Models_summary %>% unnest(TP_HLRin_glance) %>% select(df,AIC,BIC,deviance,df.residual,nobs),by="Station") %>% #join with glance summary
left_join(TP_GAM_Models_summary %>% unnest(TP_HLRin_tidy) %>% select(edf,ref.df,statistic,p.value),by="Station"),model="HLRin") #join with tidy summary

TP_HLRout_summary <- cbind(TP_GAM_Models_summary %>% select(Station,TP_HLRout_r.squared) %>% unnest(TP_HLRout_r.squared)  %>% arrange(desc(TP_HLRout_r.squared)) %>%
left_join(TP_GAM_Models_summary %>% unnest(TP_HLRout_glance) %>% select(df,AIC,BIC,deviance,df.residual,nobs),by="Station") %>% #join with glance summary
left_join(TP_GAM_Models_summary %>% unnest(TP_HLRout_tidy) %>% select(edf,ref.df,statistic,p.value),by="Station"),model="HLRout") #join with tidy summary


All_models_summary <- bind_rows(TP_Time_summary,TP_HLRin_summary,TP_HLRout_summary)
