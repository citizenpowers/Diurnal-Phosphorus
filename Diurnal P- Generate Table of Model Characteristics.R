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
library(e1071)
library(purrr)
library(broom)
library(mgcv)
library(gratia)
library(zoo)


#The Goal of this script is to extract the model characteristics from the data set under different environmental conditions
#Step 1 Import data
#Step 2 Add data to boundaries of day
#Step 2 Run functions at bottom of script
#Step 3 Filter data by environmental condition and run through model and gather the parameters
#Step 4 Combine Paramters into table for publication


# Import Data ------------Step 1-------------------------------------------------

#RPA tidy data 
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") 

#Import Flow Data (needed to create DF of days of complete flow)
Combined_BK_Flow <- read_csv("Data/Combined_BK_Flow.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d"),  Inflow = col_number(), `Inflow HLR` = col_number(), Outflow = col_number(), `Outflow HLR` = col_number()))


# Add data to boundaries of day -------Step 2------------------------------------

#all observations
RPAs_Sorted_negative_time <-RPAs_with_Flow_Stage_Weather_Sonde%>%
mutate(Time=Time-24)  

RPAs_Sorted_added_time <-RPAs_with_Flow_Stage_Weather_Sonde %>%
mutate(Time=Time+24)  

RPAS_extra_time <- RPAs_with_Flow_Stage_Weather_Sonde %>%
rbind(RPAs_Sorted_negative_time,RPAs_Sorted_added_time)

#Days of continious flow
Days_with_continual_flow <- Combined_BK_Flow %>%
group_by(`Flowway`,Date) %>%
summarize(`Min Outflow`=min(Outflow,na.rm=TRUE),`Mean Outflow`=mean(Outflow,na.rm=TRUE),`Max Outflow`=max(Outflow,na.rm=TRUE),`Min Inflow`=min(Inflow,na.rm=TRUE),`Mean Inflow`=mean(Inflow,na.rm=TRUE),`Max Inflow`=max(Inflow,na.rm=TRUE)) %>% 
mutate(`Continuous OutFlow`=ifelse(`Min Outflow`>=`Mean Outflow`*.66 &`Max Outflow` <=`Mean Outflow`*1.33,TRUE,FALSE)) %>% #days with all outflow within 33% of mean
mutate(`Continuous InFlow`=ifelse(`Min Inflow`>=`Mean Inflow`*.66 &`Max Inflow` <=`Mean Inflow`*1.33,TRUE,FALSE)) %>%   #days with all inflow within 33% of mean
select(`Flowway`,Date,`Continuous OutFlow`,`Continuous InFlow`)

#RPAS with flow data from days of continuous flow only
RPAs_with_Flow_Complete_Days <-  RPAs_with_Flow_Stage_Weather_Sonde  %>%
left_join(Days_with_continual_flow)

#add data at boundaries to continious days DF
Complete_Days_negative_time <-RPAs_with_Flow_Complete_Days %>%
mutate(Time=Time-24)  

Complete_Days_added_time <-RPAs_with_Flow_Complete_Days  %>%
mutate(Time=Time+24)  

Complete_Days_extra_time <- RPAs_with_Flow_Complete_Days%>%
rbind(Complete_Days_negative_time,Complete_Days_added_time)

# Functions -----------------Step 2---------------------------------------------------

GAM_MODEL <- function(df,min_obs)
{

if(nrow(filter(df,Station=="G333"))>min_obs) {
G333_model_GAM <- gam(data=filter(df,Station=="G333"),Diff_24_hour_mean ~ s(Time, bs="cs"))
G333_Predicted_GAM<- filter(df,Station=="G333")  %>% add_fitted(G333_model_GAM,value="Predicted")}
  
if(nrow(filter(df,Station=="G334"))>min_obs) {
G334_model_GAM <- gam(data=filter(df,Station=="G334"),Diff_24_hour_mean ~ s(Time, bs="cs"))
G334_Predicted_GAM<- filter(df,Station=="G334")  %>% add_fitted(G334_model_GAM,value="Predicted")}

if(nrow(filter(df,Station=="G381"))>min_obs) {  
G381_model_GAM <- gam(data=filter(df,Station=="G381"),Diff_24_hour_mean ~ s(Time, bs="cs")) 
G381_Predicted_GAM<- filter(df,Station=="G381")  %>% add_fitted(G381_model_GAM,value="Predicted")}

if(nrow(filter(df,Station=="G384"))>min_obs) {
G384_model_GAM <- gam(data=filter(df,Station=="G384"),Diff_24_hour_mean ~ s(Time, bs="cs"))
G384_Predicted_GAM<- filter(df,Station=="G384")  %>% add_fitted(G384_model_GAM,value="Predicted")}

if(nrow(filter(df,Station=="G379"))>min_obs) { 
G379_model_GAM <- gam(data=filter(df,Station=="G379"),Diff_24_hour_mean ~ s(Time, bs="cs"))
G379_Predicted_GAM<- filter(df,Station=="G379")  %>% add_fitted(G379_model_GAM,value="Predicted")}

if(nrow(filter(df,Station=="G380"))>min_obs) {
G380_model_GAM <- gam(data=filter(df,Station=="G380"),Diff_24_hour_mean ~ s(Time, bs="cs"))
G380_Predicted_GAM<- filter(df,Station=="G380")  %>% add_fitted(G380_model_GAM,value="Predicted")}

if(nrow(filter(df,Station=="G378"))>min_obs) {
G378_model_GAM <- gam(data=filter(df,Station=="G378"),Diff_24_hour_mean ~ s(Time, bs="cs"))
G378_Predicted_GAM<- filter(df,Station=="G378")  %>% add_fitted(G378_model_GAM,value="Predicted")}

if(nrow(filter(df,Station=="G377"))>min_obs) {
G377_model_GAM <- gam(data=filter(df,Station=="G377"),Diff_24_hour_mean ~ s(Time, bs="cs"))
G377_Predicted_GAM<- filter(df,Station=="G377")  %>% add_fitted(G377_model_GAM,value="Predicted") } 


  
Model_predictions_GAM <- data.frame(`Flowway`=as.character(),`Flowpath Region`=as.character(),Station=as.character(),Time=as.numeric(),Diff_24_hour_mean=as.numeric(),`variable`=as.character())
  
if(exists("G377_Predicted_GAM")) {Model_predictions_GAM <-rbind(Model_predictions_GAM,G377_Predicted_GAM)} 
if(exists("G333_Predicted_GAM")) {Model_predictions_GAM <-rbind(Model_predictions_GAM,G333_Predicted_GAM)} 
if(exists("G384_Predicted_GAM")) {Model_predictions_GAM <-rbind(Model_predictions_GAM,G384_Predicted_GAM)} 
if(exists("G381_Predicted_GAM")) {Model_predictions_GAM <-rbind(Model_predictions_GAM,G381_Predicted_GAM)} 
if(exists("G379_Predicted_GAM")) {Model_predictions_GAM <-rbind(Model_predictions_GAM,G379_Predicted_GAM)} 
if(exists("G380_Predicted_GAM")) {Model_predictions_GAM <-rbind(Model_predictions_GAM,G380_Predicted_GAM)} 
if(exists("G378_Predicted_GAM")) {Model_predictions_GAM <-rbind(Model_predictions_GAM,G378_Predicted_GAM)} 
if(exists("G334_Predicted_GAM")) {Model_predictions_GAM <-rbind(Model_predictions_GAM,G334_Predicted_GAM)} 

Model_predictions_GAM %>%
mutate(Time=as.numeric(Time),Diff_24_hour_mean=as.numeric(Diff_24_hour_mean),Predicted=as.numeric(Predicted)) %>%
filter(Station!="Time")
  
return (Model_predictions_GAM)
}

GAM_Extract_Parameters <- function(df) 
{
  Model_parameters_GAM<- df %>%
    group_by(Flowway,`Flowpath Region`,Station) %>%
    filter(between(Time,0,24)) %>%
    mutate(max=max(`Predicted`,na.rm = TRUE),min=min(`Predicted`,na.rm = TRUE)) %>%
    mutate(`Max Time`=ifelse(`Predicted`==max,Time,NA),`Min Time`=ifelse(`Predicted`==min,Time,NA)) %>%
    summarise(n=n(),
              Max=as.numeric(format(max(max,na.rm =TRUE),scientific=FALSE)),
              `Min`=as.numeric(format(min(min,na.rm=TRUE),scientific = FALSE)),
              `Max Time`=paste(sep = "",as.character(trunc(max(`Max Time`,na.rm = TRUE))),":",as.character(formatC(max(`Max Time`,na.rm=TRUE)%%1*60,width = 2, format = "d", flag = "0"))),
              `Min Time`=paste(sep = "",as.character(trunc(min(`Min Time`,na.rm = TRUE))),":",as.character(formatC(min(`Min Time`,na.rm = TRUE)%%1*60,width = 2, format = "d", flag = "0")))) %>%
    mutate(Amplitude=as.numeric(format(as.numeric(Max)-as.numeric(`Min`),scientific = FALSE))) %>%
    mutate(across(where(is.numeric),~round(.,3))) %>%
    mutate(Model="GAM")
}

LOESS_MODEL <-function(df,span_width,min_obs)
{
  if(nrow(filter(df,Station=="G333"))>min_obs) {
  G333_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G333"),span = span_width,method.args = list(family = "symmetric",degree=2))
  G333_Predicted<- filter(df,Station=="G333") %>% mutate(Predicted=predict(data=.,G333_model, Time, se = FALSE))}
  
  if(nrow(filter(df,Station=="G334"))>min_obs) {
  G334_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G334"),span = span_width,method.args = list(family = "symmetric",degree=2)) 
  G334_Predicted<- filter(df,Station=="G334") %>% mutate(Predicted=predict(data=.,G334_model, Time, se = FALSE))}
  
  if(nrow(filter(df,Station=="G381"))>min_obs) {
  G381_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G381"),span = span_width,method.args = list(family = "symmetric",degree=2))
  G381_Predicted<- filter(df,Station=="G381") %>% mutate(Predicted=predict(data=.,G381_model, Time, se = FALSE))}

  if(nrow(filter(df,Station=="G384"))>min_obs) {
  G384_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G384"),span = span_width,method.args = list(family = "symmetric",degree=2))
  G384_Predicted<- filter(df,Station=="G384") %>% mutate(Predicted=predict(data=.,G384_model, Time, se = FALSE))}
  
  if(nrow(filter(df,Station=="G379"))>min_obs) {
  G379_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G379"),span = span_width,method.args = list(family = "symmetric",degree=2))
  G379_Predicted<- filter(df,Station=="G379") %>% mutate(Predicted=predict(data=.,G379_model, Time, se = FALSE))}
  
  if(nrow(filter(df,Station=="G380"))>min_obs) {
  G380_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G380"),span = span_width,method.args = list(family = "symmetric",degree=2))
  G380_Predicted<- filter(df,Station=="G380") %>% mutate(Predicted=predict(data=.,G380_model, Time, se = FALSE))}
  
  if(nrow(filter(df,Station=="G378"))>min_obs) {
  G378_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G378"),span = span_width,method.args = list(family = "symmetric",degree=2))
  G378_Predicted<- filter(df,Station=="G378") %>% mutate(Predicted=predict(data=.,G378_model, Time, se = FALSE))}
  
  if(nrow(filter(df,Station=="G377"))>min_obs) {
  G377_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G377"),span = span_width,method.args = list(family = "symmetric",degree=2))
  G377_Predicted<- filter(df,Station=="G377") %>% mutate(Predicted=predict(data=.,G377_model, Time, se = FALSE))}
  
  Model_predictions <- data.frame(`Flowway`=as.character(),`Flowpath Region`=as.character(),Station=as.character(),Time=as.numeric(),Diff_24_hour_mean=as.numeric(),`variable`=as.character(),Predicted=as.numeric())
  
  if(exists("G377_Predicted")) {Model_predictions <-rbind(Model_predictions,G377_Predicted)} 
  if(exists("G333_Predicted")) {Model_predictions <-rbind(Model_predictions,G333_Predicted)} 
  if(exists("G384_Predicted")) {Model_predictions <-rbind(Model_predictions,G384_Predicted)} 
  if(exists("G381_Predicted")) {Model_predictions <-rbind(Model_predictions,G381_Predicted)} 
  if(exists("G379_Predicted")) {Model_predictions <-rbind(Model_predictions,G379_Predicted)} 
  if(exists("G380_Predicted")) {Model_predictions <-rbind(Model_predictions,G380_Predicted)} 
  if(exists("G378_Predicted")) {Model_predictions <-rbind(Model_predictions,G378_Predicted)} 
  if(exists("G334_Predicted")) {Model_predictions <-rbind(Model_predictions,G334_Predicted)} 
 
  rename(Model_predictions,`LOESS Prediction`="Predicted")
}


LOESS_Extract_Parameters <- function(df) 
{  
  Model_parameters<- df%>%
    group_by(Flowway,`Flowpath Region`,Station) %>%
    filter(between(Time,0,24)) %>%
    mutate(max=max(`LOESS Prediction`,na.rm = TRUE),min=min(`LOESS Prediction`,na.rm = TRUE)) %>%
    mutate(`Max Time`=ifelse(`LOESS Prediction`==max,Time,NA),`Min Time`=ifelse(`LOESS Prediction`==min,Time,NA)) %>%
    summarise(n=n(),
              Max=as.numeric(format(max(max,na.rm =TRUE),scientific=FALSE)),
              `Min`=as.numeric(format(min(min,na.rm=TRUE),scientific = FALSE)),
              `Max Time`=paste(sep = "",as.character(trunc(max(`Max Time`,na.rm = TRUE))),":",as.character(formatC(max(`Max Time`,na.rm=TRUE)%%1*60,width = 2, format = "d", flag = "0"))),
              `Min Time`=paste(sep = "",as.character(trunc(min(`Min Time`,na.rm = TRUE))),":",as.character(formatC(min(`Min Time`,na.rm = TRUE)%%1*60,width = 2, format = "d", flag = "0")))) %>%
    mutate(Amplitude=as.numeric(format(as.numeric(Max)-as.numeric(`Min`),scientific = FALSE))) %>%
    mutate(across(where(is.numeric),~round(.,3))) %>%
    mutate(Model="LOESS")
}


# Filter Data Set by Environmental Conditions -----Step 3------------------------

Span_width <-0.5  #With of span in LOESS model
Min_Obs <- 50 #Minimum observations required for data to be modelled 

All_Observations <- rbind(RPAS_extra_time %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters()) %>% mutate(`Condition`="All Observations")

#Inflow Stage
Inflow_Stage_10_11 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="10.5-11 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="10.5-11 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="10.5-11 Max Daily Stage ft")
Inflow_Stage_11_12 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="11-12 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="11-12 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="11-12 Max Daily Stage ft")
Inflow_Stage_12_13 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="12-13 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="12-13 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="12-13 Max Daily Stage ft")
Inflow_Stage_13 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="13+ Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="13+ Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="13+ Max Daily Stage ft")

Inflow_Stage_All <- rbind(Inflow_Stage_10_11,Inflow_Stage_11_12) %>% rbind(Inflow_Stage_12_13) %>% rbind(Inflow_Stage_13)

ggplot(Inflow_Stage_All,aes(`Max Daily Inflow Stage`,Amplitude,fill=Model))+geom_point(shape=21,size=2)+facet_grid(`Flowpath Region`~Flowway)+theme_bw()

#Outflow Stage
Outflow_Stage_0 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="< 10.5 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="< 10.5 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="< 10.5 Max Daily Stage ft")
Outflow_Stage_1 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="10.5-11 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="10.5-11 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="10.5-11 Max Daily Stage ft")
Outflow_Stage_2 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="11-12 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="11-12 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="11-12 Max Daily Stage ft")
Outflow_Stage_3 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="12-13 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="12-13 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="12-13 Max Daily Stage ft")
Outflow_Stage_4 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="13+ Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="13+ Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="13+ Max Daily Stage ft")

Outflow_Stage_All <- rbind(Outflow_Stage_0,Outflow_Stage_1) %>% rbind(Outflow_Stage_2) %>% rbind(Outflow_Stage_3) %>% rbind(Outflow_Stage_4)

ggplot(Outflow_Stage_All,aes(`Max Daily Outflow Stage`,Amplitude,fill=Model))+geom_point(shape=21,size=3,alpha=.5)+facet_grid(`Flowpath Region`~Flowway)+theme_bw()+ theme(axis.text.x=element_text(angle=90,hjust=1))

#Inflow HLR Category
#Inflow_HLR_Stage_3 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="Reverse Flow") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="Reverse Flow") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Inflow HLR Category`="Reverse Flow")
Inflow_HLR_Stage_0 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0-.1 (cm/day)") %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0-.1 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="0-.1 (cm/day)")
Inflow_HLR_Stage_1 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0.1-10 (cm/day)") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0.1-10 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="0.1-10 (cm/day)")
Inflow_HLR_Stage_2 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="10+ (cm/day)") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="10+ (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="10+ (cm/day)")

Inflow_HLR_All <- rbind(Inflow_HLR_Stage_0,Inflow_HLR_Stage_1) %>% rbind(Inflow_HLR_Stage_2) #%>% rbind(Inflow_HLR_Stage_3)

ggplot(Inflow_HLR_All,aes(`Inflow HLR Category`,Amplitude,fill=Model))+geom_point(shape=21,size=2)+facet_grid(`Flowpath Region`~Flowway)+theme_bw()+ theme(axis.text.x=element_text(angle=90,hjust=1))

#Outflow HLR Category
#Outflow_HLR_Stage_0 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="Reverse Flow") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="Reverse Flow") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Outflow HLR Category`="Reverse Flow")
Outflow_HLR_1 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0-.1 (cm/day)") %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0-.1 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="0-.1 (cm/day)")
Outflow_HLR_2 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0.1-10 (cm/day)") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0.1-10 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="0.1-10 (cm/day)")
Outflow_HLR_3 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="10+ (cm/day)") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="10+ (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="10+ (cm/day)")

Outflow_HLR_All <- rbind(Outflow_HLR_1,Outflow_HLR_2)  %>% rbind(Outflow_HLR_3)

ggplot(Outflow_HLR_All,aes(`Outflow HLR Category`,Amplitude,fill=Model))+geom_point(shape=21,size=2)+facet_grid(`Flowpath Region`~Flowway)+theme_bw()+ theme(axis.text.x=element_text(angle=90,hjust=1))

#Days of continious flow
Complete_inflow_days <-Complete_Days_extra_time %>% filter(`Continuous InFlow`=="TRUE")
Complete_outflow_days <-Complete_Days_extra_time %>% filter(`Continuous OutFlow`=="TRUE")
Complete_flow_days <-Complete_Days_extra_time %>% filter(`Continuous OutFlow`=="TRUE") %>% filter(`Continuous InFlow`=="TRUE")

#Continous inflow
Cont_inflow_days <- rbind(Complete_inflow_days %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),Complete_inflow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters()) %>% mutate(`Condition`="Continuous Inflow")
#continous outflow
Cont_outflow_days <- rbind(Complete_outflow_days %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),Complete_outflow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters()) %>% mutate(`Condition`="Continuous Outflow")
#continous inflow and outflow
Cont_flow_days <- rbind(Complete_flow_days %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),Complete_flow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters()) %>% mutate(`Condition`="Continuous Inflow and Outflow")




# Combine all tables into one -----------------------------------------------

All_data_table <- All_Observations %>% 
rbind(Cont_inflow_days) %>% 
rbind(Inflow_Stage_All) %>% 
rbind(Outflow_Stage_All) %>% 
rbind(Inflow_HLR_All) %>% 
rbind(Outflow_HLR_All) %>% 
rbind(Cont_inflow_days) %>%
rbind(Cont_outflow_days) %>% 
rbind(Cont_flow_days)

write.csv(All_data_table,"Data/Extracted Model Parameters.csv",row.names=FALSE)


# Calculate Confidence Intervals ------------------------------------------
#no known method to calculate CI for LOESS models

df <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`)
G384_model_GAM <- gam(data=filter(df,Station=="G384"),Diff_24_hour_mean ~ s(Time, bs="cs"),se.fit=TRUE)
G384_Predicted_GAM<- filter(df,Station=="G384")  %>% add_fitted(G384_model_GAM,value="Predicted") %>% arrange(Time)
G384_CI <- filter(df,Station=="G384") %>% smooth_estimates(G384_model_GAM,data=.) %>% add_confint(coverage = 0.95) %>% arrange(Time)
G384_Predicted_CI <-cbind(G384_Predicted_GAM,select(G384_CI,upper_ci,lower_ci))


filter(G384_CI,between(Time,0,24)) %>%
summarise(n=n(),se_max=max(se),se_min=min(se),avg_se=mean(se))

ggplot(G384_Predicted_CI,aes(Time,Predicted,ymin=upper_ci,ymax=lower_ci))+geom_point()+geom_errorbar()




