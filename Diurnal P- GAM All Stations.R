#goal of this script is to create a GAM model using data from all stations and flowways for seasonality, time of day and influence of HLRin, HLRout, and water depth 

library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(ggpmisc)
library(purrr)
library(broom)
library(mgcv)
library(gratia)
library(zoo)
library(ggeffects)
library(mgcViz)
library(stats)
library(dbhydroR)
library(cowplot)
library(gridExtra)
library(ggplotify)
library(ggpubr)
library(itsadug)
# Import Data -------------------------------------------------------------


RPA_GAM_Data <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") %>%
mutate(Flag=if_else(Station == "G334" & Date >="2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed  
filter(Flag ==FALSE)  %>% select(-Flag) %>%
rename(Wind="WIND BELLEGLADE",HLRin="Inflow HLR",HLRout="Outflow HLR",Flowpath="Flowpath Region",TEMP="Temp S7",Rain="Rain S7") %>% #models will not accept variables with blank spaces as input 
mutate(Station_ID=as.factor(Station_ID),Flowpath=as.factor(Flowpath),Month=as.factor(month(Date, label=TRUE, abbr=TRUE)),Day=yday(Date),Year=as.factor(Year)) %>%
mutate(`Mean_Depth` = case_when(Flowway=="STA-3/4 Central" & Flowpath=="Outflow"~`Outflow Stage`-9.4,
                                Flowway=="STA-3/4 Western"& Flowpath=="Outflow"~`Outflow Stage`-9.7,
                                Flowway=="STA-2 Central"& Flowpath=="Outflow"~`Outflow Stage`-9.5,
                                Flowway=="STA-3/4 Central" & Flowpath=="Inflow"~`Inflow Stage`-9.4,
                                Flowway=="STA-3/4 Western" & Flowpath=="Inflow"~`Inflow Stage`-9.8,
                                Flowway=="STA-2 Central" & Flowpath=="Inflow"~`Inflow Stage`-9.5,
                                Flowway=="STA-3/4 Central" & Flowpath=="Midflow"~`Inflow Stage`-9.4,
                                Flowway=="STA-3/4 Western" & Flowpath=="Midflow"~`Inflow Stage`-9.75)) %>%  #Calculate mean depth using average ground stage
select(Station_ID,Flowway,Flowpath,Date,Year,Day,Time,HLRout,HLRin,Mean_Depth,TPO4 ) %>%
filter(TPO4>0,TPO4<200)

# Tidy Data ---------------------------------------------------------------
#Create training and test data sets
set.seed(1)

#create ID column
RPA_GAM_Data$id <- 1:nrow(RPA_GAM_Data )

#use 70% of dataset as training set and 30% as test set 
RPA_GAM_Data_train <- RPA_GAM_Data %>% dplyr::sample_frac(0.70)
RPA_GAM_Data_test  <- dplyr::anti_join(RPA_GAM_Data, RPA_GAM_Data_train, by = 'id')



# Create Models -----------------------------------------------------------

#Seasonality random smooths by station 
GAM_1 <- gam(TPO4~Flowpath+s(Day,by=Flowpath)+s(Day,Station_ID,bs="fs",m=1) ,data =RPA_GAM_Data_train ,method="REML",family="gaussian")
summary(GAM_1)  #Deviance 63.6 %
gam.check(GAM_1)
check(getViz(GAM_1))
concurvity(GAM_1)
plot(GAM_1, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(GAM_1, file="./Data/Model/GAM_1.rda")

#Sesaonality random intercept for flowpath
GAM_2 <- gam(TPO4~Flowpath+s(Day,bs="cc") ,data =RPA_GAM_Data_train ,method="REML",family="gaussian",knots=list(Day=c(0, 365)))
summary(GAM_2)  #Deviance 47 %
gam.check(GAM_2)
check(getViz(GAM_2))
concurvity(GAM_2)
plot(GAM_2, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(GAM_2, file="./Data/Model/GAM_2.rda")

#Seasonality + HLRout random intercept for flowpath
GAM_3 <- gam(TPO4~Flowpath+s(Day)+s(HLRout,by=Flowpath,k=5) ,data =RPA_GAM_Data_train ,method="REML",family="gaussian")
summary(GAM_3)  #Deviance 55 %
gam.check(GAM_3)
check(getViz(GAM_2))
concurvity(GAM_2)
plot(GAM_3, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(GAM_3, file="./Data/Model/GAM_3.rda")

#Seasonality + HLRout by flowpath+HLRin by flowpath +random intercept for flowpath
GAM_4 <- gam(TPO4~Flowpath+s(Day)+s(HLRout,by=Flowpath,k=5)+s(HLRin,by=Flowpath) ,data =RPA_GAM_Data_train ,method="REML",family="gaussian")
summary(GAM_4)  #Deviance 57.7 %
gam.check(GAM_4)
check(getViz(GAM_4))
concurvity(GAM_4)
plot(GAM_4, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(GAM_4, file="./Data/Model/GAM_4.rda")

#Seasonality + HLRout by flowpath +HLRin +random intercept for flowpath
GAM_5 <- gam(TPO4~Flowpath+s(Day)+s(HLRout,by=Flowpath,k=5)+s(HLRin) ,data =RPA_GAM_Data_train ,method="REML",family="gaussian")
summary(GAM_5)  #Deviance 55.8 %
gam.check(GAM_5)
check(getViz(GAM_5))
concurvity(GAM_5)
plot(GAM_5, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(GAM_5, file="./Data/Model/GAM_5.rda")

#Seasonality + HLRout by flowpath +HLRin +mean depth + random intercept for flowpath
GAM_6 <- gam(TPO4~Flowpath+s(Day)+s(HLRout,by=Flowpath,k=5)+s(HLRin)+s(Mean_Depth) ,data =RPA_GAM_Data_train ,method="REML",family="gaussian")
summary(GAM_6)  #Deviance 58.2 %
gam.check(GAM_6)
check(getViz(GAM_6))
concurvity(GAM_6)
plot(GAM_6, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(GAM_6, file="./Data/Model/GAM_6.rda")


#Seasonality + HLRout by flowpath +HLRin +mean depth by flowpath +random intercept for flowpath
GAM_7 <- gam(TPO4~Flowpath+s(Day)+s(HLRout,by=Flowpath,k=5)+s(HLRin)+s(Mean_Depth,by=Flowpath) ,data =RPA_GAM_Data_train ,method="REML",family="gaussian")
summary(GAM_7)  #Deviance 61 %
gam.check(GAM_7)
check(getViz(GAM_7))
concurvity(GAM_7)
plot(GAM_7, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(GAM_7, file="./Data/Model/GAM_7.rda")


#Seasonality + HLRout by flowpath +HLRin + mean depth +time of day random intercept for flowpath
GAM_8 <- gam(TPO4~Flowpath+s(Day)+s(HLRout,by=Flowpath,k=5)+s(HLRin)+s(Mean_Depth)+s(Time) ,data =RPA_GAM_Data_train ,method="REML",family="gaussian")
summary(GAM_8)  #Deviance 58.4 %
gam.check(GAM_8)
check(getViz(GAM_8))
concurvity(GAM_8)
plot(GAM_8, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(GAM_8, file="./Data/Model/GAM_8.rda")

#Seasonality + HLRout by flowpath +HLRin + mean depth +time of day by flowpath random intercept for flowpath
GAM_9 <- gam(TPO4~Flowpath+s(Day)+s(HLRout,by=Flowpath,k=5)+s(HLRin)+s(Mean_Depth)+s(Time,by=Flowpath) ,data =RPA_GAM_Data_train ,method="REML",family="gaussian")
summary(GAM_9)  #Deviance 58.6 %
gam.check(GAM_9)
check(getViz(GAM_8))
concurvity(GAM_9)
plot(GAM_9, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(GAM_9, file="./Data/Model/GAM_9.rda")

#Seasonality + HLRout by flowpath +HLRin + mean depth +time of day random intercept for flowpath
GAM_10 <- gam(TPO4~Flowpath+s(Day,bs="cc")+s(HLRout,by=Flowpath,k=10)+s(HLRin)+s(Mean_Depth)+s(Time) ,data =RPA_GAM_Data_train ,method="REML",family="gaussian",knots=list(Day=c(0, 366),Time=c(0,24)))
summary(GAM_10)  #Deviance 58.4 %
gam.check(GAM_10)
check(getViz(GAM_10))
concurvity(GAM_10)
plot(GAM_10, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(GAM_10, file="./Data/Model/GAM_10.rda")

#Seasonality + HLRout by flowpath +HLRin + mean depth +time of day random intercept for flowpath
GAM_11 <- gam(TPO4~Flowpath+s(Day,bs="cc")+s(HLRout,by=Flowpath,k=5)+s(HLRin)+s(Mean_Depth,by=Flowpath)+s(Time) ,data =RPA_GAM_Data_train ,method="REML",family="gaussian",knots=list(Day=c(0, 366),Time=c(0,24)))
summary(GAM_11)  #Deviance 61.2 %
gam.check(GAM_11)
check(getViz(GAM_10))
concurvity(GAM_8)
plot(GAM_11, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(GAM_8, file="./Data/Model/GAM_8.rda")



# Test Plots --------------------------------------------------------------

#Test of mean flow depths 
ggplot(RPA_GAM_Data,aes(Mean_Depth))+geom_histogram()+facet_grid(Flowway~Flowpath,scales="free")
ggplot(RPA_GAM_Data,aes(Date,Mean_Depth,color=Station_ID,shape=Flowpath))+geom_point()+theme_bw()

ggplot(RPA_GAM_Data,aes(Date,TPO4,color=Station_ID))+facet_grid(Flowway~Flowpath,scales="free")+geom_point()+theme_bw()

