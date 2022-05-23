
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


#The Goal of this script is to extract the model characteristics from the data set under different environmental conditions
#Step 1 Import data
#Step 2 Add data to boundaries of day
#Step 2 Run functions at bottom of script
#Step 3 Filter data by environmental condition and run through model and gather the parameters
#Step 4 Combine Parameters into table for publication


# Import Data ------------Step 1-------------------------------------------------

#RPA tidy data 
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") %>%
mutate(Flag=if_else(Station == "G334" & Date >"2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed  
filter(Flag ==FALSE)  %>% select(-Flag)

#Import Flow Data (needed to create DF of days of complete flow)
Combined_BK_Flow <- read_csv("Data/Combined_BK_Flow.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d"),  Inflow = col_number(), `Inflow HLR` = col_number(), Outflow = col_number(), `Outflow HLR` = col_number()))


# Add data to boundaries of day -------Step 2------------------------------------

#all observations
RPAs_Sorted_negative_time <-RPAs_with_Flow_Stage_Weather_Sonde%>%
mutate(Time=Time-24)  

RPAs_Sorted_added_time <-RPAs_with_Flow_Stage_Weather_Sonde %>%
mutate(Time=Time+24)  

RPAS_extra_time <- RPAs_with_Flow_Stage_Weather_Sonde %>%
rbind(RPAs_Sorted_negative_time,RPAs_Sorted_added_time) %>%
mutate(`Month`=factor(`Month`,levels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) %>%
filter(between(Time,-6,30))  #filter time between -10 and 34
  

#Days of continuous flow
Days_with_continual_flow <- Combined_BK_Flow %>%
group_by(`Flowway`,Date) %>%
summarise(`Min Outflow`=min(Outflow,na.rm=TRUE),`Mean Outflow`=mean(Outflow,na.rm=TRUE),`Max Outflow`=max(Outflow,na.rm=TRUE),`Min Inflow`=min(Inflow,na.rm=TRUE),`Mean Inflow`=mean(Inflow,na.rm=TRUE),`Max Inflow`=max(Inflow,na.rm=TRUE)) %>% 
mutate(`Continuous OutFlow`=ifelse(`Min Outflow`>=`Mean Outflow`*.66 &`Max Outflow` <=`Mean Outflow`*1.33,TRUE,FALSE)) %>% #days with all outflow within 33% of mean
mutate(`Continuous InFlow`=ifelse(`Min Inflow`>=`Mean Inflow`*.66 &`Max Inflow` <=`Mean Inflow`*1.33,TRUE,FALSE)) %>%   #days with all inflow within 33% of mean
select(`Flowway`,Date,`Continuous OutFlow`,`Continuous InFlow`)

#RPAS with flow data from days of continuous flow only
RPAs_with_Flow_Complete_Days <-  RPAs_with_Flow_Stage_Weather_Sonde  %>%
left_join(Days_with_continual_flow)

#add data at boundaries to continuous days DF
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
  G333_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G333"),span = span_width,method.args = list(family = "symmetric",degree=2,iterations=4))
  G333_Predicted<- filter(df,Station=="G333") %>% mutate(Predicted=predict(data=.,G333_model, Time, se = FALSE)) %>% mutate(SE=predict(data=.,G333_model, Time, se = TRUE)$se.fit)}
  else {G333_Predicted<- filter(df,Station=="G333") %>% mutate(Predicted=NA,SE=NA)}
  
  if(nrow(filter(df,Station=="G334"))>min_obs) {
  G334_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G334"),span = span_width,method.args = list(family = "symmetric",degree=2,iterations=4))
  G334_Predicted<- filter(df,Station=="G334") %>% mutate(Predicted=predict(data=.,G334_model, Time, se = FALSE)) %>% mutate(SE=predict(data=.,G334_model, Time, se = TRUE)$se.fit)}
  else {G334_Predicted<- filter(df,Station=="G334") %>% mutate(Predicted=NA,SE=NA)}
  
  if(nrow(filter(df,Station=="G381"))>min_obs) {
  G381_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G381"),span = span_width,method.args = list(family = "symmetric",degree=2,iterations=4))
  G381_Predicted<- filter(df,Station=="G381") %>% mutate(Predicted=predict(data=.,G381_model, Time, se = FALSE)) %>% mutate(SE=predict(data=.,G381_model, Time, se = TRUE)$se.fit)}
  else {G381_Predicted<- filter(df,Station=="G381") %>% mutate(Predicted=NA,SE=NA)}
  
  if(nrow(filter(df,Station=="G384"))>min_obs) {
  G384_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G384"),span = span_width,method.args = list(family = "symmetric",degree=2,iterations=4))
  G384_Predicted<- filter(df,Station=="G384") %>% mutate(Predicted=predict(data=.,G384_model, Time, se = FALSE)) %>% mutate(SE=predict(data=.,G384_model, Time, se = TRUE)$se.fit)}
  else {G384_Predicted<- filter(df,Station=="G384") %>% mutate(Predicted=NA,SE=NA)}
  
  if(nrow(filter(df,Station=="G379"))>min_obs) {
  G379_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G379"),span = span_width,method.args = list(family = "symmetric",degree=2,iterations=4))
  G379_Predicted<- filter(df,Station=="G379") %>% mutate(Predicted=predict(data=.,G379_model, Time, se = FALSE)) %>% mutate(SE=predict(data=.,G379_model, Time, se = TRUE)$se.fit)}
  else {G379_Predicted<- filter(df,Station=="G379") %>% mutate(Predicted=NA,SE=NA)}
  
  if(nrow(filter(df,Station=="G380"))>min_obs) {
  G380_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G380"),span = span_width,method.args = list(family = "symmetric",degree=2,iterations=4))
  G380_Predicted<- filter(df,Station=="G380") %>% mutate(Predicted=predict(data=.,G380_model, Time, se = FALSE)) %>% mutate(SE=predict(data=.,G380_model, Time, se = TRUE)$se.fit)}
  else {G380_Predicted<- filter(df,Station=="G380") %>% mutate(Predicted=NA,SE=NA)}
  
  if(nrow(filter(df,Station=="G378"))>min_obs) {
  G378_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G378"),span = span_width,method.args = list(family = "symmetric",degree=2,iterations=4))
  G378_Predicted<- filter(df,Station=="G378") %>% mutate(Predicted=predict(data=.,G378_model, Time, se = FALSE)) %>% mutate(SE=predict(data=.,G378_model, Time, se = TRUE)$se.fit)}
  else {G378_Predicted<- filter(df,Station=="G378") %>% mutate(Predicted=NA,SE=NA)}
  
  if(nrow(filter(df,Station=="G377"))>min_obs) {
  G377_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G377"),span = span_width,method.args = list(family = "symmetric",degree=2,iterations=4))
  G377_Predicted<- filter(df,Station=="G377") %>% mutate(Predicted=predict(data=.,G377_model, Time, se = FALSE)) %>% mutate(SE=predict(data=.,G377_model, Time, se = TRUE)$se.fit)}
  else {G377_Predicted<- filter(df,Station=="G377") %>% mutate(Predicted=NA,SE=NA)}
  
  Model_predictions <- data.frame(`Flowway`=as.character(),`Flowpath Region`=as.character(),Station=as.character(),Time=as.numeric(),Diff_24_hour_mean=as.numeric(),`variable`=as.character(),Predicted=as.numeric(),SE=as.numeric())
  
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
              `Min Time`=paste(sep = "",as.character(trunc(min(`Min Time`,na.rm = TRUE))),":",as.character(formatC(min(`Min Time`,na.rm = TRUE)%%1*60,width = 2, format = "d", flag = "0"))),
              `Mean SE`=mean(SE,na.rm=TRUE)) %>%
    mutate(Amplitude=as.numeric(format(as.numeric(Max)-as.numeric(`Min`),scientific = FALSE))) %>%
    mutate(across(where(is.numeric),~round(.,3))) %>%
    mutate(Model="LOESS")
  }



# Filter Data Set by Environmental Conditions -----Step 3  LOESS and GAM------------------------

Span_width <-0.5  #With of span in LOESS model
Min_Obs <-50 #Minimum observations required for data to be modeled 

All_Observations <- rbind(RPAS_extra_time %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters()) %>% mutate(`Condition`="All Observations")

#Inflow Stage
Inflow_Stage_10_11 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="10.5-11 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="10.5-11 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="10.5-11 Max Daily Inflow Stage ft")
Inflow_Stage_11_12 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="11-12 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="11-12 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="11-12 Max Daily Inflow Stage ft")
Inflow_Stage_12_13 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="12-13 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="12-13 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="12-13 Max Daily Inflow Stage ft")
Inflow_Stage_13 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="13+ Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="13+ Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="13+ Max Daily Inflow Stage ft")

Inflow_Stage_All <- rbind(Inflow_Stage_10_11,Inflow_Stage_11_12) %>% rbind(Inflow_Stage_12_13) %>% rbind(Inflow_Stage_13)

#ggplot(Inflow_Stage_All,aes(`Max Daily Inflow Stage`,Amplitude,fill=Model))+geom_point(shape=21,size=2)+facet_grid(`Flowpath Region`~Flowway)+theme_bw()

#Outflow Stage
Outflow_Stage_0 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="< 10.5 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="< 10.5 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="< 10.5 Max Daily Outflow Stage ft")
Outflow_Stage_1 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="10.5-11 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="10.5-11 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="10.5-11 Max Daily Outflow Stage ft")
Outflow_Stage_2 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="11-12 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="11-12 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="11-12 Max Daily Outflow Stage ft")
Outflow_Stage_3 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="12-13 Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="12-13 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="12-13 Max Daily Outflow Stage ft")
Outflow_Stage_4 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="13+ Max Daily Stage ft") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="13+ Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="13+ Max Daily Outflow Stage ft")

Outflow_Stage_All <- rbind(Outflow_Stage_0,Outflow_Stage_1) %>% rbind(Outflow_Stage_2) %>% rbind(Outflow_Stage_3) %>% rbind(Outflow_Stage_4)

#ggplot(Outflow_Stage_All,aes(`Max Daily Outflow Stage`,Amplitude,fill=Model))+geom_point(shape=21,size=3,alpha=.5)+facet_grid(`Flowpath Region`~Flowway)+theme_bw()+ theme(axis.text.x=element_text(angle=90,hjust=1))

#Inflow HLR Category
#Inflow_HLR_Stage_3 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="Reverse Flow") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="Reverse Flow") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Inflow HLR Category`="Reverse Flow")
Inflow_HLR_Stage_0 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0-.1 (cm/day)") %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0-.1 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="Inflow 0-.1 (cm/day)")
Inflow_HLR_Stage_1 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0.1-10 (cm/day)") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0.1-10 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="Inflow 0.1-10 (cm/day)")
Inflow_HLR_Stage_2 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="10+ (cm/day)") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="10+ (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="Inflow 10+ (cm/day)")

Inflow_HLR_All <- rbind(Inflow_HLR_Stage_0,Inflow_HLR_Stage_1) %>% rbind(Inflow_HLR_Stage_2) #%>% rbind(Inflow_HLR_Stage_3)

#ggplot(Inflow_HLR_All,aes(`Inflow HLR Category`,Amplitude,fill=Model))+geom_point(shape=21,size=2)+facet_grid(`Flowpath Region`~Flowway)+theme_bw()+ theme(axis.text.x=element_text(angle=90,hjust=1))

#Outflow HLR Category
#Outflow_HLR_Stage_0 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="Reverse Flow") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="Reverse Flow") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Outflow HLR Category`="Reverse Flow")
Outflow_HLR_1 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0-.1 (cm/day)") %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0-.1 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="Outflow 0-.1 (cm/day)")
Outflow_HLR_2 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0.1-10 (cm/day)") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0.1-10 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="Outflow 0.1-10 (cm/day)")
Outflow_HLR_3 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="10+ (cm/day)") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="10+ (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Condition`="Outflow 10+ (cm/day)")

Outflow_HLR_All <- rbind(Outflow_HLR_1,Outflow_HLR_2)  %>% rbind(Outflow_HLR_3)

#ggplot(Outflow_HLR_All,aes(`Outflow HLR Category`,Amplitude,fill=Model))+geom_point(shape=21,size=2)+facet_grid(`Flowpath Region`~Flowway)+theme_bw()+ theme(axis.text.x=element_text(angle=90,hjust=1))

#Days of continuous flow
Complete_inflow_days <-Complete_Days_extra_time %>% filter(`Continuous InFlow`=="TRUE")%>% filter (between(Time,0,24))
Complete_outflow_days <-Complete_Days_extra_time %>% filter(`Continuous OutFlow`=="TRUE")%>% filter (between(Time,0,24))
Complete_flow_days <-Complete_Days_extra_time %>% filter(`Continuous OutFlow`=="TRUE") %>% filter(`Continuous InFlow`=="TRUE") %>% filter (between(Time,0,24))

#Continuous inflow
Cont_inflow_days <- rbind(Complete_inflow_days %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),Complete_inflow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters()) %>% mutate(`Condition`="Continuous Inflow")
#continuous outflow
Cont_outflow_days <- rbind(Complete_outflow_days %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),Complete_outflow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters()) %>% mutate(`Condition`="Continuous Outflow")
#continuous inflow and outflow
Cont_flow_days <- rbind(Complete_flow_days %>% GAM_MODEL(.,Min_Obs ) %>% GAM_Extract_Parameters(),Complete_flow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters()) %>% mutate(`Condition`="Continuous Inflow and Outflow")

# Filter Data Set by Environmental Conditions -----Step 3-------LOESS only------------


Span_width <-0.5  #With of span in LOESS model
Min_Obs <-50 #Minimum observations required for data to be modeled 

All_Observations <- RPAS_extra_time %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="All Observations")

test<-  RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="10.5-11 Max Daily Stage ft") %>% group_by(Station) %>% summarise(n=n())

#Inflow Stage
Inflow_Stage_10_11 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="10.5-11 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="10.5-11 Max Daily Inflow Stage ft")
Inflow_Stage_11_12 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="11-12 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters()  %>% mutate(`Condition`="11-12 Max Daily Inflow Stage ft")
Inflow_Stage_12_13 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="12-13 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters()  %>% mutate(`Condition`="12-13 Max Daily Inflow Stage ft")
Inflow_Stage_13 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="13+ Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters()  %>% mutate(`Condition`="13+ Max Daily Inflow Stage ft")

Inflow_Stage_All <- rbind(Inflow_Stage_10_11,Inflow_Stage_11_12) %>% rbind(Inflow_Stage_12_13) %>% rbind(Inflow_Stage_13)

#Outflow Stage
Outflow_Stage_0 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="< 10.5 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="< 10.5 Max Daily Outflow Stage ft")
Outflow_Stage_1 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="10.5-11 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="10.5-11 Max Daily Outflow Stage ft")
Outflow_Stage_2 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="11-12 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="11-12 Max Daily Outflow Stage ft")
Outflow_Stage_3 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="12-13 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="12-13 Max Daily Outflow Stage ft")
Outflow_Stage_4 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="13+ Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="13+ Max Daily Outflow Stage ft")

Outflow_Stage_All <- rbind(Outflow_Stage_0,Outflow_Stage_1) %>% rbind(Outflow_Stage_2) %>% rbind(Outflow_Stage_3) %>% rbind(Outflow_Stage_4)

#Inflow HLR Category
#Inflow_HLR_Stage_3 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="Reverse Flow") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="Reverse Flow") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Inflow HLR Category`="Reverse Flow")
Inflow_HLR_Stage_0 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0-.1 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="Inflow 0-.1 (cm/day)")
Inflow_HLR_Stage_1 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0.1-10 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="Inflow 0.1-10 (cm/day)")
Inflow_HLR_Stage_2 <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="10+ (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="Inflow 10+ (cm/day)")

Inflow_HLR_All <- rbind(Inflow_HLR_Stage_0,Inflow_HLR_Stage_1) %>% rbind(Inflow_HLR_Stage_2) #%>% rbind(Inflow_HLR_Stage_3)

#Outflow HLR Category
#Outflow_HLR_Stage_0 <- rbind(RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="Reverse Flow") %>% GAM_MODEL(.,Min_Obs) %>% GAM_Extract_Parameters(),RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="Reverse Flow") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() ) %>% mutate(`Outflow HLR Category`="Reverse Flow")
Outflow_HLR_1 <-  RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0-.1 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters()  %>% mutate(`Condition`="Outflow 0-.1 (cm/day)")
Outflow_HLR_2 <-  RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0.1-10 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters()  %>% mutate(`Condition`="Outflow 0.1-10 (cm/day)")
Outflow_HLR_3 <-  RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="10+ (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="Outflow 10+ (cm/day)")

Outflow_HLR_All <- rbind(Outflow_HLR_1,Outflow_HLR_2)  %>% rbind(Outflow_HLR_3)

#Days of continuous flow
Complete_inflow_days <-Complete_Days_extra_time %>% filter(`Continuous InFlow`=="TRUE")%>% filter (between(Time,0,24))
Complete_outflow_days <-Complete_Days_extra_time %>% filter(`Continuous OutFlow`=="TRUE")%>% filter (between(Time,0,24))
Complete_flow_days <-Complete_Days_extra_time %>% filter(`Continuous OutFlow`=="TRUE") %>% filter(`Continuous InFlow`=="TRUE") %>% filter (between(Time,0,24))

#Continuous inflow
Cont_inflow_days <- Complete_inflow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="Continuous Inflow")
#continuous outflow
Cont_outflow_days <- Complete_outflow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="Continuous Outflow")
#continuous inflow and outflow
Cont_flow_days <- Complete_flow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% LOESS_Extract_Parameters() %>% mutate(`Condition`="Continuous Inflow and Outflow")


1415+5440+2131+1901+3282+2788+2365+5700 #total
979+5107+1378+1239+2045+1370+989+2318 #inflow
738+3956+1040+860+1898+1434+1272+2899 #outflow

557+3247+693+581+1336+848+658+1325 #inflow and outflow
290+2221+277
# Combine all tables into one -----------------------------------------------

All_data_table <- All_Observations %>% 
rbind(Cont_inflow_days) %>%
rbind(Cont_outflow_days) %>% 
rbind(Cont_flow_days) %>%
rbind(Inflow_Stage_All) %>% 
rbind(Outflow_Stage_All) %>% 
rbind(Inflow_HLR_All) %>% 
rbind(Outflow_HLR_All)  

write.csv(All_data_table,"Data/Extracted Model Parameters added SE.csv",row.names=FALSE)

#Publication format table STA2 LOESS
STA2_LOESS_table <- All_data_table %>%
ungroup() %>%  
select(Station,Amplitude,Condition, n,`Min Time`,`Max Time`,`Mean SE`,Model) %>%  
filter(Station %in% c("G333","G334"),Model=="LOESS") %>%
select(-Model)  %>% 
pivot_wider(names_from = Station, values_from = c(n,Amplitude, `Min Time`,`Max Time`,`Mean SE`)) %>%
select(Condition,n_G333,Amplitude_G333,`Min Time_G333`,`Max Time_G333`,`Mean SE_G333`,n_G334,Amplitude_G334,`Min Time_G334`,`Max Time_G334`,`Mean SE_G334`)

write.csv(STA2_LOESS_table,"Data/STA-2 Model Pub LOESS Table.csv",row.names=FALSE)

#Publication format table STA34 central FW LOESS
STA34_FW2_LOESS_table <- All_data_table %>%
ungroup() %>%  
select(Station,Amplitude,Condition, n,`Min Time`,`Max Time`,`Mean SE`,Model) %>%  
filter(Station %in% c("G377","G378","G379"),Model=="LOESS") %>%
select(-Model)  %>% 
pivot_wider(names_from = Station, values_from = c(n,Amplitude, `Min Time`,`Max Time`,`Mean SE`)) %>%
select(Condition,n_G377,Amplitude_G377,`Min Time_G377`,`Max Time_G377`,`Mean SE_G377`,n_G378,Amplitude_G378,`Min Time_G378`,`Max Time_G378`,`Mean SE_G378`,n_G379,Amplitude_G379,`Min Time_G379`,`Max Time_G379`,`Mean SE_G379`)
  
write.csv(STA34_FW2_LOESS_table,"Data/STA-34 FW Central Model Pub LOESS Table.csv",row.names=FALSE)

#Publication format table STA34 Western LOESS
STA34_FW3_LOESS_table <- All_data_table %>%
ungroup() %>%  
select(Station,Amplitude,Condition, n,`Min Time`,`Max Time`,`Mean SE`,Model) %>%  
filter(Station %in% c("G380","G384","G381"),Model=="LOESS") %>%
select(-Model)  %>% 
pivot_wider(names_from = Station, values_from = c(n,Amplitude, `Min Time`,`Max Time`,`Mean SE`)) %>%
select(Condition,n_G380,Amplitude_G380,`Min Time_G380`,`Max Time_G380`,`Mean SE_G380`,n_G384,Amplitude_G384,`Min Time_G384`,`Max Time_G384`,`Mean SE_G384`,n_G381,Amplitude_G381,`Min Time_G381`,`Max Time_G381`,`Mean SE_G381`)

write.csv(STA34_FW3_LOESS_table,"Data/STA-34 FW West Model Pub LOESS Table.csv",row.names=FALSE)

# Publication Plots -------------------------------------------------------

fig_2_fit<- LOESS_MODEL(RPAS_extra_time,Span_width,Min_Obs)

#LOESS Visualized All Stations Figure #2
ggplot(RPAS_extra_time,aes(Time,Diff_24_hour_mean,color=Station))+
geom_ribbon(data =fig_2_fit,aes(Time,ymax=`LOESS Prediction`+SE,ymin=`LOESS Prediction`-SE),color="grey80",size=1,fill="grey80")+geom_point(shape=1,alpha=.5)+
geom_line(data =fig_2_fit,aes(Time,`LOESS Prediction`),color="black",size=1)+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(breaks = seq(-10,10,1))+scale_x_continuous(breaks = seq(0,24,4))+
coord_cartesian(ylim = c(-10,10),xlim = c(1,23))+theme_bw()+ guides(colour=FALSE)+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.1),panel.spacing.x = unit(.5, "lines"))+ #geom_vline(xintercept = c(0,24),color="blue")+
labs(y=expression("Deviation from daily mean TP"~(mu~g~L^-1)),x="Hour")

ggsave("Figures/Pub Fig 2- Hourly TP Variation from the Daily Mean by Station.jpeg", plot = last_plot(), width = 8, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#Inflow Stage combine model predictions 
Inflow_Stage_10_11_fig <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="10.5-11 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>%  mutate(`Condition`="10.5-11 Max Daily Inflow Stage ft")
Inflow_Stage_11_12_fig <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="11-12 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% mutate(`Condition`="11-12 Max Daily Inflow Stage ft")
Inflow_Stage_12_13_fig <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="12-13 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% mutate(`Condition`="12-13 Max Daily Inflow Stage ft")
Inflow_Stage_13_fig <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Inflow Stage`) %>% filter(`Max Daily Inflow Stage`=="13+ Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs)  %>% mutate(`Condition`="13+ Max Daily Inflow Stage ft")

Inflow_Stage_All_fig <- rbind(Inflow_Stage_10_11_fig,Inflow_Stage_11_12_fig) %>% rbind(Inflow_Stage_12_13_fig) %>% rbind(Inflow_Stage_13_fig)

#LOESS Visualized All Stations SI #1
ggplot(Inflow_Stage_All_fig,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
geom_ribbon(data =Inflow_Stage_All_fig,aes(Time,ymax=`LOESS Prediction`+SE,ymin=`LOESS Prediction`-SE),color="grey80",size=1,fill="grey80")+geom_point(shape=1,alpha=.5)+
geom_line(data =Inflow_Stage_All_fig,aes(Time,`LOESS Prediction`),color="black",size=1)+  
facet_grid(`Max Daily Inflow Stage`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(breaks = seq(-10,10,1))+scale_x_continuous(breaks = seq(0,24,4))+
coord_cartesian(ylim = c(-10,10),xlim = c(1,23))+theme_bw()+ guides(colour=FALSE)+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.1),panel.spacing.x = unit(.5, "lines"))+ #geom_vline(xintercept = c(0,24),color="blue")+
labs(y=expression("Deviation from daily mean TP"~(mu~g~L^-1)),x="Hour")

ggsave("Figures/Pub SI 1- Hourly TP Variation from the Daily Mean by Station and Inflow Stage Condition.jpeg", plot = last_plot(), width = 8, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Outflow Stage combine model predictions 
Outflow_Stage_0_fig <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="< 10.5 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>%mutate(`Condition`="< 10.5 Max Daily Outflow Stage ft")
Outflow_Stage_1_fig <-  RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="10.5-11 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% mutate(`Condition`="10.5-11 Max Daily Outflow Stage ft")
Outflow_Stage_2_fig <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="11-12 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs)%>% mutate(`Condition`="11-12 Max Daily Outflow Stage ft")
Outflow_Stage_3_fig <-RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="12-13 Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% mutate(`Condition`="12-13 Max Daily Outflow Stage ft")
Outflow_Stage_4_fig <-  RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Max Daily Outflow Stage`) %>% filter(`Max Daily Outflow Stage`=="13+ Max Daily Stage ft") %>% LOESS_MODEL(.,Span_width,Min_Obs)  %>% mutate(`Condition`="13+ Max Daily Outflow Stage ft")

Outflow_Stage_All_fig <- rbind(Outflow_Stage_0_fig,Outflow_Stage_1_fig) %>% rbind(Outflow_Stage_2_fig) %>% rbind(Outflow_Stage_3_fig) %>% rbind(Outflow_Stage_4_fig)

#LOESS Visualized All Stations SI #2
ggplot(Outflow_Stage_All_fig ,aes(Time,Diff_24_hour_mean,color=Station))+
geom_ribbon(data =Outflow_Stage_All_fig,aes(Time,ymax=`LOESS Prediction`+SE,ymin=`LOESS Prediction`-SE),color="grey80",size=1,fill="grey80")+geom_point(shape=1,alpha=.5)+
geom_line(data =Outflow_Stage_All_fig,aes(Time,`LOESS Prediction`),color="black",size=1)+
facet_grid(`Max Daily Outflow Stage`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(breaks = seq(-10,10,1))+scale_x_continuous(breaks = seq(0,24,4))+
coord_cartesian(ylim = c(-10,10),xlim = c(1,23))+theme_bw()+ guides(colour=FALSE)+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.1),panel.spacing.x = unit(.5, "lines"))+ #geom_vline(xintercept = c(0,24),color="blue")+
labs(y=expression("Deviation from daily mean TP"~(mu~g~L^-1)),x="Hour")

ggsave("Figures/Pub SI 2- Hourly TP Variation from the Daily Mean by Station and Outflow Stage Condition.jpeg", plot = last_plot(), width = 8, height = 9, units = "in", dpi = 300, limitsize = TRUE)


#Inflow HLR Category
Inflow_HLR_Stage_0_fig <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0-.1 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>%  mutate(`Condition`="Inflow 0-.1 (cm/day)")
Inflow_HLR_Stage_1_fig <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="0.1-10 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>%  mutate(`Condition`="Inflow 0.1-10 (cm/day)")
Inflow_HLR_Stage_2_fig <- RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Inflow HLR Category`) %>% filter(`Inflow HLR Category`=="10+ (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>%  mutate(`Condition`="Inflow 10+ (cm/day)")

Inflow_HLR_All_fig <- rbind(Inflow_HLR_Stage_0_fig,Inflow_HLR_Stage_1_fig) %>% rbind(Inflow_HLR_Stage_2_fig) #%>% rbind(Inflow_HLR_Stage_3)

#LOESS Visualized All Stations SI #3
ggplot(Inflow_HLR_All_fig ,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
geom_ribbon(data =Inflow_HLR_All_fig,aes(Time,ymax=`LOESS Prediction`+SE,ymin=`LOESS Prediction`-SE),color="grey80",size=1,fill="grey80")+geom_point(shape=1,alpha=.5)+
geom_line(data =Inflow_HLR_All_fig,aes(Time,`LOESS Prediction`),color="black",size=1)+
facet_grid(`Inflow HLR Category`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(breaks = seq(-10,10,1))+scale_x_continuous(breaks = seq(0,24,4))+
coord_cartesian(ylim = c(-10,10),xlim = c(1,23))+theme_bw()+ guides(colour=FALSE)+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.1),panel.spacing.x = unit(.5, "lines"))+ #geom_vline(xintercept = c(0,24),color="blue")+
labs(y=expression("Deviation from daily mean TP"~(mu~g~L^-1)),x="Hour")

ggsave("Figures/Pub SI 3- Hourly TP Variation from the Daily Mean by Station and Inflow HLR Condition.jpeg", plot = last_plot(), width = 8, height = 6, units = "in", dpi = 300, limitsize = TRUE)

#Outflow HLR Category
Outflow_HLR_1_fig <-  RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0-.1 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% mutate(`Condition`="Outflow 0-.1 (cm/day)")
Outflow_HLR_2_fig <-  RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="0.1-10 (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>% mutate(`Condition`="Outflow 0.1-10 (cm/day)")
Outflow_HLR_3_fig <-  RPAS_extra_time %>% select(Flowway,`Flowpath Region`,Station,Time,Diff_24_hour_mean,`Outflow HLR Category`) %>% filter(`Outflow HLR Category`=="10+ (cm/day)") %>% LOESS_MODEL(.,Span_width,Min_Obs) %>%  mutate(`Condition`="Outflow 10+ (cm/day)")

Outflow_HLR_All_fig <- rbind(Outflow_HLR_1_fig,Outflow_HLR_2_fig)  %>% rbind(Outflow_HLR_3_fig)

#LOESS Visualized All Stations SI #4
ggplot(Outflow_HLR_All_fig ,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
geom_ribbon(data =Outflow_HLR_All_fig,aes(Time,ymax=`LOESS Prediction`+SE,ymin=`LOESS Prediction`-SE),color="grey80",size=1,fill="grey80")+geom_point(shape=1,alpha=.5)+
geom_line(data =Outflow_HLR_All_fig,aes(Time,`LOESS Prediction`),color="black",size=1)+
facet_grid(`Outflow HLR Category`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(breaks = seq(-10,10,1))+scale_x_continuous(breaks = seq(0,24,4))+
coord_cartesian(ylim = c(-10,10),xlim = c(1,23))+theme_bw()+ guides(colour=FALSE)+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.1),panel.spacing.x = unit(.5, "lines"))+ #geom_vline(xintercept = c(0,24),color="blue")+
labs(y=expression("Deviation from daily mean TP"~(mu~g~L^-1)),x="Hour")

ggsave("Figures/Pub SI 4- Hourly TP Variation from the Daily Mean by Station and Outflow HLR Condition.jpeg", plot = last_plot(), width = 8, height = 6, units = "in", dpi = 300, limitsize = TRUE)

#Continuous inflow
Cont_inflow_days_fig <- Complete_inflow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% mutate(`Condition`="Continuous Inflow")
#continuous outflow
Cont_outflow_days_fig <- Complete_outflow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>%  mutate(`Condition`="Continuous Outflow")
#continuous inflow and outflow
Cont_flow_days_fig <- Complete_flow_days %>% LOESS_MODEL(.,Span_width ,Min_Obs) %>% mutate(`Condition`="Continuous Inflow and Outflow")

Cont_flow_days_all_fig <- rbind(Cont_inflow_days_fig,Cont_outflow_days_fig)  %>% rbind(Cont_flow_days_fig) %>% mutate(Condition=factor(Condition,levels = c("Continuous Inflow","Continuous Outflow","Continuous Inflow and Outflow")))

#LOESS Visualized All Stations SI #5
ggplot(Cont_flow_days_all_fig ,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
geom_ribbon(data =Cont_flow_days_all_fig,aes(Time,ymax=`LOESS Prediction`+SE,ymin=`LOESS Prediction`-SE),color="grey80",size=1,fill="grey80")+geom_point(shape=1,alpha=.5)+
geom_line(data =Cont_flow_days_all_fig,aes(Time,`LOESS Prediction`),color="black",size=1)+
facet_grid(`Condition`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(breaks = seq(-10,10,1))+scale_x_continuous(breaks = seq(0,24,4))+
coord_cartesian(ylim = c(-10,10),xlim = c(1,23))+theme_bw()+ guides(colour=FALSE)+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.1),panel.spacing.x = unit(.5, "lines"))+ #geom_vline(xintercept = c(0,24),color="blue")+
labs(y=expression("Deviation from daily mean TP"~(mu~g~L^-1)),x="Hour")

ggsave("Figures/Pub SI 5- Hourly TP Variation from the Daily Mean by Station and continuous flow Condition.jpeg", plot = last_plot(), width = 8, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#LOESS Visualized All Stations by month SI #6 (need LOESS predictions by month)
ggplot(RPAS_extra_time ,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
geom_smooth(color="black",method = "loess",span=0.5,method.args = list(family = "symmetric",degree=2,iterations=4))+  
facet_grid(Station~Month)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(breaks = seq(-10,10,2))+scale_x_continuous(breaks = seq(0,24,4))+
coord_cartesian(ylim = c(-10,10),xlim = c(1,23))+theme_bw()+ guides(colour=FALSE)+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.1),panel.spacing.x = unit(.5, "lines"))+ #geom_vline(xintercept = c(0,24),color="blue")+
labs(y=expression("Deviation from daily mean TP"~(mu~g~L^-1)),x="Hour")

ggsave("Figures/Pub SI 6- Hourly TP Variation from the Daily Mean by Station and Month.jpeg", plot = last_plot(), width = 8, height = 8, units = "in", dpi = 300, limitsize = TRUE)




# Test --------------------------------------------------------------------


#LOESS Visualized All Stations no flow SI #7 (need LOESS predictions)
no_flow <- RPAS_extra_time %>% filter(`Inflow HLR` <0.001) %>% filter(`Outflow HLR` <0.001)

ggplot(no_flow ,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
#geom_line(aes(Time,`LOESS Prediction`) ,color="black",size=1)+
geom_smooth(color="black",method = "loess",span=0.5)+  
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(breaks = seq(-10,10,2))+scale_x_continuous(breaks = seq(0,24,4))+
coord_cartesian(ylim = c(-10,10),xlim = c(1,23))+theme_bw()+ guides(colour=FALSE)+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.1),panel.spacing.x = unit(.5, "lines"))+ #geom_vline(xintercept = c(0,24),color="blue")+
labs(y="Deviation from daily mean (P ug/L)",x="Hour")

ggsave("Figures/Pub SI 5- Hourly TP Variation from the Daily Mean by Station and continuous flow Condition.jpeg", plot = last_plot(), width = 8, height = 8, units = "in", dpi = 300, limitsize = TRUE)

Flow_vs_inflow_HLR <- RPAs_with_Flow_Stage_Weather_Sonde %>% filter(`Flowpath Region` == "Inflow") 

Flow_vs_outflow_HLR <- RPAs_with_Flow_Stage_Weather_Sonde %>% filter(`Flowpath Region` == "Outflow") %>% filter

ggplot(Flow_vs_inflow_HLR ,aes(`Inflow HLR`,TPO4,color=Station))+geom_point(shape=1)+
geom_smooth(color="black")+  scale_x_continuous(breaks = seq(0,30,3))+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+ coord_cartesian(ylim = c(0,200))+theme_bw()+ 
guides(colour=FALSE)+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.1),panel.spacing.x = unit(.5, "lines"))+ #geom_vline(xintercept = c(0,24),color="blue")+
labs(y="TP (P ug/L)",x="HLR (cm/day)")

ggplot(Flow_vs_outflow_HLR ,aes(`Inflow HLR`,TPO4,color=Station))+geom_point(shape=1)+
geom_smooth(color="black",method="loess")+  scale_x_continuous(breaks = seq(0,30,3))+scale_y_continuous(breaks = seq(0,90,10))+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+ coord_cartesian(ylim = c(1,90),xlim = c(0,30))+theme_bw()+ 
guides(colour=FALSE)+  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.1),panel.spacing.x = unit(.5, "lines"))+ #geom_vline(xintercept = c(0,24),color="blue")+
labs(y="TP (P ug/L)",x="HLR (cm/day)")

G380C_test <- filter(RPAS_extra_time,Station=="G380") 

#Test the effect of span witdth on LOESS
ggplot(G380C_test,aes(Time,Diff_24_hour_median,color=Station_ID,fill=Station_ID))+geom_point(shape=21)+
#geom_smooth(method="loess",color="purple",fill="grey",span=.5)+   
#geom_smooth(method="loess",color="orange",fill="grey",span=1)+   
geom_smooth(method="loess",color="blue",fill="grey",span=.25)+   
#geom_smooth(method="loess",color="red",fill="grey",span=.1)+   
#geom_smooth(method="loess",color="yellow",fill="grey",span=8)+   
coord_cartesian(ylim = c(-10,10))+theme_bw()

#test boundary effects
G380C_test_0_24 <- filter(G380C_test,between(Time,0,24)) 
G380C_test_n4_28 <- filter(G380C_test,between(Time,-4,28)) 
G380C_test_8_20 <- filter(G380C_test,between(Time,8,20)) 
G380C_test_n10_34 <- filter(G380C_test,between(Time,-10,34)) 

ggplot(G380C_test,aes(Time,Diff_24_hour_median,color=Station_ID,fill=Station_ID))+#geom_point(shape=21)+
geom_smooth(data=G380C_test_0_24,method="loess",color="purple",fill="grey",span=.25,method.args = list(family = "symmetric",degree=2))+   
geom_smooth(data=G380C_test_n4_28,method="loess",color="orange",fill="grey",span=.25,method.args = list(family = "symmetric",degree=2))+   
geom_smooth(data=G380C_test_8_20,method="loess",color="blue",fill="grey",span=.25,method.args = list(family = "symmetric",degree=2))+   
geom_smooth(method="loess",color="red",fill="grey",span=.25,method.args = list(family = "symmetric",degree=2))+   
geom_smooth(data=G380C_test_n10_34, method="loess",color="yellow",fill="grey",span=.25,method.args = list(family = "symmetric",degree=2))+   
geom_point(data = G380_Predicted_test,aes(Time,Predicted))+  
coord_cartesian(ylim = c(-10,10),xlim = c(0,24))+theme_bw()

G380_model_test <- loess(Diff_24_hour_median ~ Time ,filter(RPAS_extra_time,Station=="G380"),span =.25,method.args = list(family = "symmetric",degree=2))
G380_Predicted_test<- filter(RPAS_extra_time,Station=="G380") %>% mutate(Predicted=predict(data=.,G380_model_test, Time, se = FALSE))

G380_model_GAM_test <- gam(data=filter(RPAS_extra_time,Station=="G380"),Diff_24_hour_mean ~ s(Time, bs="cs"))
G380_Predicted_GAM_test <- filter(RPAS_extra_time,Station=="G380")  %>% add_fitted(G380_model_GAM_test,value="Predicted")


#Test number of points

G380C_test_1001 <- slice(G380C_test,1:1001)
G380C_test_999 <- slice(G380C_test,1:999) 
G380C_test_500<- slice(G380C_test,1:500) 

ggplot(G380C_test,aes(Time,Diff_24_hour_median,color=Station_ID,fill=Station_ID))+#geom_point(shape=21)+
#geom_smooth(data=G380C_test_1001 ,method="loess",formula =y ~ x,color="purple",fill="grey",span=.25,method.args = list(family = "symmetric",degree=2))+   
#geom_smooth(data=G380C_test_999 ,method="loess",formula =y ~ x,color="orange",fill="grey",span=.25,method.args = list(family = "symmetric",degree=2))+   
#geom_smooth(data=G380C_test_500 ,method="loess",formula =y ~ x,color="blue",fill="grey",span=.25,method.args = list(family = "symmetric",degree=2),se=FALSE)+  
geom_smooth(method="loess",color="red",fill="grey",formula =y ~ x,span=.25,method.args = list(family = "symmetric",degree=2),se=FALSE)+  
geom_smooth(method="loess",color="orange",fill="grey",formula =y ~ x,span=.25,method.args = list(family = "gaussian",degree=2),se=FALSE)+     
geom_point(data = G380_Predicted_test,aes(Time,Predicted),color="purple")+  
#geom_point(data = G380_Predicted_GAM_test,aes(Time,Predicted),color="grey")+    
coord_cartesian(ylim = c(-10,10),xlim = c(0,24))+theme_bw()

#Test LOESS predicted vs ggplot loess

LOESS_test <- RPAS_extra_time %>% LOESS_MODEL(.,.25 )

ggplot(LOESS_test,aes(Time,`LOESS Prediction`,color=Station,fill=Station))+geom_point(shape=21)+facet_wrap(~Station)+
geom_smooth(method="loess",color="orange",fill="grey",formula =y ~ x,span=.25,method.args = list(family = "gaussian",degree=2),se=FALSE) +  
coord_cartesian(ylim = c(-10,10),xlim = c(0,24))+theme_bw()  + geom_hline(yintercept=0)
  
#outflow LOESS
ggplot(filter(Outflow_Stage_All,Model=="LOESS"),aes(Condition,Amplitude,fill=Station))+geom_col(color="black")+facet_grid(`Flowpath Region`~Flowway)+theme_bw()+
theme(axis.text.x=element_text(angle=90))

ggplot(RPAS_extra_time,aes(Time,Diff_24_hour_median,color=`Max Daily Outflow Stage`))+
geom_smooth(method="loess",span=.25,method.args = list(family = "gaussian",degree=2),se=FALSE)+scale_y_continuous(breaks= seq(-15,15,1))+
geom_smooth(method="gam",linetype="dashed",se = FALSE)+
facet_grid(`Flowpath Region`~Flowway)+theme_bw()+coord_cartesian(ylim = c(-15,15),xlim = c(0,24))+ geom_hline(yintercept=0)+
theme(axis.text.x=element_text(angle=90))

#inflow LOESS
ggplot(Inflow_Stage_All,aes(Condition,Amplitude,fill=Model))+geom_col(color="black",position = "dodge")+facet_grid(`Flowpath Region`~Flowway)+theme_bw()+
theme(axis.text.x=element_text(angle=90))

ggplot(RPAS_extra_time,aes(Time,Diff_24_hour_median,color=`Max Daily Inflow Stage`))+
geom_smooth(method="loess",fill="grey",span=.25,method.args = list(family = "gaussian",degree=2),se=FALSE)+scale_y_continuous(breaks= seq(-15,15,1))+
geom_smooth(method="gam",linetype="dashed",se = FALSE)+
facet_grid(`Flowpath Region`~Flowway)+theme_bw()+coord_cartesian(ylim = c(-15,15),xlim = c(0,24))+ geom_hline(yintercept=0)+
theme(axis.text.x=element_text(angle=90))


#Hourly TP Variation from the Daily Median by Station
ggplot(RPAS_extra_time,aes(Time,Diff_24_hour_median,color=Station,fill=Station))+#geom_point(shape=21)+
#geom_smooth(method="loess",color="black",fill="grey",span=.25,method.args = list(family = "symmetric",degree=2))+
geom_smooth(method="loess",color="purple",fill="grey",span=.25,method.args = list(family = "symmetric",degree=2))+ 
geom_point(data=LOESS_test,aes(Time,`LOESS Prediction`,fill=Station,color=Station),size=.5)+
#geom_smooth(aes(Time,Diff_24_hour_mean),method="loess",color="green",fill="grey",method.args = list(family = "symmetric",degree=2))+
#geom_smooth(method="gam",color="blue")+
#geom_smooth(aes(Time,Diff_24_hour_mean),method="gam",color="red")+
theme_bw()+facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2",guide = 'none')+scale_fill_brewer( type = "qual", palette = "Set2")+
geom_hline(yintercept=0)+#scale_x_continuous(limits = c(-10,34))+
coord_cartesian(ylim = c(-10,10))+guides(fill=guide_legend(title="Station"))+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
labs(title="Deviation from Daily Median by Hour",y="TPO4 Deviation from daily median (ug/L)",x="Hour")

ggsave("Figures/Hourly TP Variation from the Daily Median.jpeg", plot = last_plot(), width = 8, height = 5, units = "in", dpi = 300, limitsize = TRUE)

#Hourly TP Variation from the Daily Mean by Station
ggplot(RPAS_extra_time,aes(Time,Diff_24_hour_mean,color=Station_ID,fill=Station_ID))+geom_point(shape=21)+
#geom_smooth(method="loess",color="black",fill="grey",method.args = list(family = "symmetric",degree=2))+
geom_smooth(method="gam",color="blue",fill = "grey")+
theme_bw()+facet_grid(~Station_ID)+scale_colour_brewer( type = "qual", palette = "Set2",guide = 'none')+scale_fill_brewer( type = "qual", palette = "Set2")+
geom_hline(yintercept=0)+
coord_cartesian(xlim = c(0,24),ylim = c(-10,10))+guides(fill=guide_legend(title="Station"))+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
labs(title="Deviation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Hourly TP Variation from the Daily Median.jpeg", plot = last_plot(), width = 8, height = 5, units = "in", dpi = 300, limitsize = TRUE)

#Hourly TP Variation from the Daily Mean by Station
ggplot(RPAS_extra_time,aes(Time,Diff_24_hour_mean,color=Station_ID,fill=Station_ID))+geom_point(shape=21)+
geom_smooth(method="loess",color="black",fill="grey",method.args = list(family = "symmetric",degree=2))+
geom_smooth(method="loess",color="red")+ 
geom_smooth(method="gam",color="blue")+
theme_bw()+facet_grid(~Station_ID)+scale_colour_brewer( type = "qual", palette = "Set2",guide = 'none')+scale_fill_brewer( type = "qual", palette = "Set2")+
geom_hline(yintercept=0)+
coord_cartesian(ylim = c(-10,10))+guides(fill=guide_legend(title="Station"))+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
labs(title="Deviation from Daily Median by Hour",y="TPO4 Deviation from daily median (ug/L)",x="Hour")


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




