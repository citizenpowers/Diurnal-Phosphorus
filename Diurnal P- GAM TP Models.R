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
library(mgcViz)
library(ggh4x)  #helper package that allows faceting of theoretical distributions 
library(stats)
library(itsadug)

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
rename(Wind="WIND BELLEGLADE",HLRin="Inflow HLR",HLRout="Outflow HLR",Flowpath="Flowpath Region",TEMP="Temp S7",Rain="Rain S7") %>% #models will not accept variables with blank spaces as input 
mutate(Station_ID=as.factor(Station_ID),Flowpath=as.factor(Flowpath)) %>%
mutate(Month=as.factor(month(Date, label=TRUE, abbr=TRUE))) %>%
mutate(Day=yday(Date)) %>%
filter(TPO4>0)


# Create scatterplots ----------------------------------------------------

ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(date,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black") #long-term trend
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`HLRout`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`HLRin`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`HLRout`,`Outflow Stage`,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`HLRout`,`Inflow Stage`,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Inflow Stage`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Wind`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Flowpath`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`Month`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`TEMP`,TPO4,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black")

ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(TPO4))+geom_density()



# Build Model all stations included ---------------------------------------

test <-RPAs_with_Flow_Stage_Weather_Sonde %>%
group_by(Station_ID,Month) %>%
count()

TP_GAM_month <- gam(TPO4 ~s(Month),method="REML",data =RPAs_with_Flow_Stage_Weather_Sonde,select=TRUE)

TP_GAM_day <- gam(TPO4 ~s(Day,bs="gp")+s(Time,bs="cc")+s(Station_ID,bs="re")+s(HLRin,k=10)+s(HLRout,k=10)+Wind,method="REML",select=TRUE,data =RPAs_with_Flow_Stage_Weather_Sonde)
summary(TP_GAM_day)
plot(TP_GAM_day, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(TP_GAM_day)
gam.check(TP_GAM_day)


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



# Effects  HLR in -------------------------------------------

STA34_only  <-RPAs_with_Flow_Stage_Weather_Sonde %>% filter(Flowway !="STA-2 Central")

#Single common smoother mod_G
HLRin_Station_mod_G <- gam(TPO4 ~s(HLRin,bs="tp")+s(Station_ID,bs="re"), data =RPAs_with_Flow_Stage_Weather_Sonde,method="REML",family="gaussian")
summary(HLRin_Station_mod_G)  #Deviance 61.2%
plot(HLRin_Station_mod_G, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRin_Station_mod_G)
gam.check(HLRin_Station_mod_G,pages=1)
check(getViz(HLRin_Station_mod_G))

#Global smoother and group level smoothers with same wiggliness  Mod_GS
HLRin_Station_mod_GS <- gam(TPO4 ~s(HLRin,m=2)+s(HLRin,Station_ID,bs="fs",m=2), data =RPAs_with_Flow_Stage_Weather_Sonde,method="REML",family="Gamma")
summary(HLRin_Station_mod_GS)  #Deviance 65.3%
plot(HLRin_Station_mod_GS, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRin_Station_mod_GS)
gam.check(HLRin_Station_mod_GS,pages=1)
check(getViz(HLRin_Station_mod_GS))

#global smoother and group level smoother with individual wiggliness Mod_GI
HLRin_Station_mod_GI <- gam(TPO4 ~s(HLRin,bs="tp")+s(HLRin,by=Station_ID,bs="tp")+s(Station_ID,bs="re"), data =RPAs_with_Flow_Stage_Weather_Sonde,method="REML",family="gaussian")
summary(HLRin_Station_mod_GI)  #Deviance 65.3%
plot(HLRin_Station_mod_GI, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRin_Station_mod_GI)
gam.check(HLRin_Station_mod_GI,pages=1)
check(getViz(HLRin_Station_mod_GI))

#group level smoother with same wiggliness Mod_S
HLRin_Station_mod_S <- gam(TPO4 ~s(HLRin,Station_ID,bs="fs",m=2), data =RPAs_with_Flow_Stage_Weather_Sonde,method="REML",family="Gamma")
summary(HLRin_Station_mod_S)  #Deviance 65.3%
plot(HLRin_Station_mod_S, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRin_Station_mod_S)
gam.check(HLRin_Station_mod_S,pages=1)
check(getViz(HLRin_Station_mod_S))

#global smoother and group level smoother with individual wiggliness Mod_I
HLRin_Station_mod_I <- gam(TPO4 ~s(HLRin,by=Station_ID, bs="tp",m=2)+s(Station_ID,bs="re"), data =RPAs_with_Flow_Stage_Weather_Sonde,method="REML",family="Gamma")
summary(HLRin_Station_mod_GI)  #Deviance 65.3%
plot(HLRin_Station_mod_GI, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRin_Station_mod_GI)
gam.check(HLRin_Station_mod_GI,pages=1)
check(getViz(HLRin_Station_mod_GI))



#factor model- individual smooths for each station
HLRin_Station_Factor <- gam(TPO4 ~ s(HLRin, Station_ID, bs="fs")+Station_ID, data=RPAs_with_Flow_Stage_Weather_Sonde, method = "REML",family="Gamma")
summary(HLRin_Station_Factor)  #deviance 50%
vis.gam(HLRin_Station_Factor, view = c("HLRin", "Station_ID"), plot.type = "persp",theta=90)
plot(HLRin_Station_Factor,shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)

#factor model- individual smooths for each position in flowpath
HLRin_Flowpath_Factor <- gam(TPO4 ~ s(HLRin, Flowpath, bs="fs")+Flowpath, data=RPAs_with_Flow_Stage_Weather_Sonde, method = "REML",family="Gamma")
summary(HLRin_Flowpath_Factor) #deviance 57.4%
vis.gam(HLRin_Flowpath_Factor, view = c("HLRin", "Flowpath"), plot.type = "persp",theta=240)
concurvity(HLRin_Flowpath_Factor)
gam.check(HLRin_Flowpath_Factor)
check(getViz(HLRin_Flowpath_Factor))




# Effects of HLRout -------------------------------------------------------

#reduce data to discharge stations only 
Stage_discharge_data <- filter(RPAs_with_Flow_Stage_Weather_Sonde,`Flowpath Region`=="Outflow") %>% rename(Stage_Out="Outflow Stage") %>% filter(TPO4<200) %>% mutate(Year=as.factor(Year))
Stage_discharge_data_34only <- filter(RPAs_with_Flow_Stage_Weather_Sonde,`Flowpath Region`=="Outflow") %>% rename(Stage_Out="Outflow Stage") %>% filter(TPO4<200) %>% filter(Station_ID !="G334")


#Single common smoother mod_G
HLRout_Station_mod_G <- gam(TPO4 ~s(HLRout,bs="tp")+s(Station_ID,bs="re"), data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Station_mod_G)  #Deviance 31%
plot(HLRout_Station_mod_G, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRout_Station_mod_G)
gam.check(HLRout_Station_mod_G,pages=1)
check(getViz(HLRout_Station_mod_G))

#Global smoother and group level smoothers with same wiggliness  Mod_GS
HLRout_Station_mod_GS <- gam(TPO4 ~s(HLRout,m=2)+s(HLRout,Station_ID,bs="fs",m=2), data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Station_mod_GS)  #Deviance 38.2%
plot(HLRout_Station_mod_GS, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRout_Station_mod_GS)
gam.check(HLRout_Station_mod_GS,pages=1)
check(getViz(HLRout_Station_mod_GS))

#global smoother and group level smoother with individual wiggliness Mod_GI
HLRout_Station_mod_GI <- gam(TPO4 ~s(HLRout,bs="tp")+s(HLRout,by=Station_ID,bs="tp")+s(Station_ID,bs="re"), data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Station_mod_GI)  #Deviance 38.2%
plot(HLRout_Station_mod_GI, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRout_Station_mod_GI)
gam.check(HLRout_Station_mod_GI,pages=1)
check(getViz(HLRout_Station_mod_GI))

#group level smoother with same wiggliness Mod_S
HLRout_Station_mod_S <- gam(TPO4 ~s(HLRout,Station_ID,bs="fs",m=2), data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Station_mod_S)  #Deviance38.2%
plot(HLRout_Station_mod_S, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRout_Station_mod_S)
gam.check(HLRout_Station_mod_S,pages=1)
check(getViz(HLRout_Station_mod_S))

#global smoother and group level smoother with individual wiggliness Mod_I
HLRout_Station_mod_I <- gam(TPO4 ~s(HLRout,by=Station_ID, bs="tp",m=2)+s(Station_ID,bs="re"), data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Station_mod_GI)  #Deviance 38.2%
plot(HLRout_Station_mod_GI, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRout_Station_mod_GI)
gam.check(HLRout_Station_mod_GI,pages=1)
check(getViz(HLRout_Station_mod_GI))

#Global smoother for HLRout and Day of year
HLRout_Station_Day_mod_G <- gam(TPO4 ~s(HLRout,bs="tp")+s(Station_ID,bs="re")+s(Day,bs="tp",k=20)+ti(Day,by=Station_ID,bs="tp")+s(Time,bs="cc"), data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Station_Day_mod_G)  #Deviance 78%
plot(HLRout_Station_Day_mod_G, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRout_Station_Day_mod_G)
gam.check(HLRout_Station_Day_mod_G,pages=1)
check(getViz(HLRout_Station_Day_mod_G))

#Global smoother for HLRout and Day of year       *****Winner******              
HLRout_Station_Day_Time_mod_G <- gam(TPO4 ~s(HLRout,bs="tp",k=5)+s(Day,bs="cc",k=10)+s(Station_ID,bs="re")+s(Year,bs="re")+ti(Day,by=Station_ID,bs="tp")+ti(Time,by=Station_ID,bs="cc"), data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Station_Day_Time_mod_G)  #Deviance 77.9%
plot(HLRout_Station_Day_Time_mod_G, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
plot(HLRout_Station_Day_Time_mod_G, pages = 1, scheme = 2, shade = TRUE, scale = 0)
concurvity(HLRout_Station_Day_Time_mod_G)
gam.check(HLRout_Station_Day_Time_mod_G,pages=1)
check(getViz(HLRout_Station_Day_Time_mod_G))
check_resid(HLRout_Station_Day_Time_mod_G, split_by=c("Station_ID", "Day"))
vis.gam(HLRout_Station_Day_Time_mod_G, view = c("HLRout", "Station_ID"), plot.type = "persp",theta=150, ticktype="detailed")  #3D HLR and station
vis.gam(HLRout_Station_Day_Time_mod_G, view = c("Day", "Station_ID"), plot.type = "persp",theta=150, ticktype="detailed")  #3D Time and station
vis.gam(HLRout_Station_Day_Time_mod_G, view = c("Time", "Station_ID"), plot.type = "persp",theta=150, ticktype="detailed")  #3D Time and station
vis.gam(HLRout_Station_Day_Time_mod_G, view = c("HLRout", "Time"), plot.type = "persp",theta=60,too.far=.1, ticktype="detailed")  #3D HLR and station
glance(HLRout_Station_Day_Time_mod_G)
tidy(HLRout_Station_Day_Time_mod_G)
augment(HLRout_Station_Day_Time_mod_G)

#save model
saveRDS(HLRout_Station_Day_Time_mod_G, file="./Data/Model/HLRout_Station_Day_Time_mod_G.rda")


HLRout_Day <- gam(TPO4 ~te(Day,HLRout,bs=c("cc","tp"),m=2)+t2(Day,HLRout,Station_ID, bs=c("cc", "tp", "re"), k=c(10, 10, 6), m=2, full=TRUE),data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Day)  #Deviance 80.7%
plot(HLRout_Day, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(HLRout_Day, view = c("HLRout", "Day"), plot.type = "persp",theta=75, ticktype="detailed")  #3D HLR Day
gam.check(HLRout_Day,pages=1)


HLRout_Day_mod2 <- gam(TPO4 ~t2(Day,HLRout,Station_ID, bs=c("cc", "tp", "re"), k=c(10, 10, 6), m=2, full=TRUE),data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Day_mod2)  #Deviance 80.7%
plot(HLRout_Day_mod2, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(HLRout_Day_mod2, view = c("HLRout", "Day"), plot.type = "persp",theta=45, ticktype="detailed")  #3D HLR Day
gam.check(HLRout_Day_mod2,pages=1)

HLRout_Day_mod3 <- gam(TPO4 ~t2(Day,HLRout,Station_ID, bs=c("cc", "tp", "re"), k=c(10, 10, 6), m=2, full=TRUE)+s(Station_ID,bs="re")+s(Year,bs="re"),data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Day_mod3)  #Deviance 81.9%
plot(HLRout_Day_mod3, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(HLRout_Day_mod3, view = c("HLRout", "Day"), plot.type = "persp",theta=45, ticktype="detailed")  #3D HLR Day
gam.check(HLRout_Day_mod3,pages=1)

HLRout_Day_mod4 <- gam(TPO4 ~t2(Day,HLRout,Station_ID, bs=c("cc", "tp", "re"), k=c(10, 10, 6), m=2, full=TRUE)+s(Station_ID,bs="re")+s(Year,bs="re")+s(Time,bs="cc"),data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Day_mod4)  #Deviance 82.5%
plot(HLRout_Day_mod4, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(HLRout_Day_mod4, view = c("HLRout", "Day"), plot.type = "persp",theta=45, ticktype="detailed")  #3D HLR Day
gam.check(HLRout_Day_mod4,pages=1)

HLRout_Day_mod5 <- gam(TPO4 ~t2(Day,HLRout,Station_ID, bs=c("cc", "tp", "re"), k=c(10, 10, 6), m=2, full=TRUE)+s(Station_ID,bs="re")+s(Year,bs="re")+s(Time,bs="cc"),data =Stage_discharge_data ,method="REML",family="gaussian",knots=list(Day=c(0, 365),Time=c(0,24)))
summary(HLRout_Day_mod5)  #Deviance 82.5%
plot(HLRout_Day_mod5, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(HLRout_Day_mod2, view = c("HLRout", "Day"), plot.type = "persp",theta=45, ticktype="detailed")  #3D HLR Day
gam.check(HLRout_Day_mod5,pages=1)

HLRout_Day_mod6 <- gam(TPO4 ~te(Day,HLRout,bs=c("cc","tp"),m=2)+t2(Day,HLRout,Station_ID, bs=c("cc", "tp", "re"), k=c(10, 10, 6), m=2, full=TRUE)+s(Station_ID,bs="re")+s(Year,bs="re")+s(Time,bs="cc"),data =Stage_discharge_data ,method="REML",family="gaussian",knots=list(Day=c(0, 365),Time=c(0,24)))
summary(HLRout_Day_mod6)  #Deviance 82.8%
plot(HLRout_Day_mod6, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(HLRout_Day_mod6, view = c("HLRout", "Day"), plot.type = "persp",theta=45, ticktype="detailed")  #3D HLR Day
gam.check(HLRout_Day_mod6,pages=1)
check(getViz(HLRout_Day_mod6))

Day_time <- gam(TPO4 ~te(Day,Time,bs=c("cc","cc"),m=2)+t2(Day,Time,Station_ID, bs=c("cc", "tp", "re"), k=c(10, 10, 6), m=2, full=TRUE)+s(Station_ID,bs="re")+s(Time,bs="cc"),data =Stage_discharge_data ,method="REML",family="gaussian",knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Day_time)  #Deviance 73.5%
plot(Day_time, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Day_time, view = c("Day", "Time"), plot.type = "persp",theta=45, ticktype="detailed")  #3D HLR Day
gam.check(Day_time,pages=1)
check(getViz(Day_time))

Station_Year_HLR_Time_mod_I <-gam(TPO4 ~s(Day,by=Station_ID)+s(Station_ID,Year,bs="re")+s(HLRout,by=Station_ID)+s(Time,bs="cc"),data =Stage_discharge_data ,method="REML",family="gaussian",knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Station_Year_HLR_Time_mod_I)  #Devi  ance 81.6%
yplot(Station_Year_HLR_Time_mod_I, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Station_Year_HLR_Time_mod_I, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.1, ticktype="detailed")  #3D HLR Day
gam.check(Station_Year_HLR_Time_mod_I,pages=1)
check(getViz(Station_Year_HLR_Time_mod_I)) 
  
Station_Year_HLR_Time_mod_I_Gamma <-gam(TPO4 ~s(Day,by=Station_ID)+s(Station_ID,Year,bs="re")+s(HLRout,by=Station_ID)+s(Time,bs="cc"),data =Stage_discharge_data ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Station_Year_HLR_Time_mod_I_Gamma)  #Deviance 79 %
plot(Station_Year_HLR_Time_mod_I_Gamma, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Station_Year_HLR_Time_mod_I_Gamma, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.1, ticktype="detailed")  #3D HLR Day
gam.check(Station_Year_HLR_Time_mod_I_Gamma,pages=1)
check(getViz(Station_Year_HLR_Time_mod_I_Gamma)) 
glance(Station_Year_HLR_Time_mod_I_Gamma)

Station_Year_HLRout_HLRin_Time_mod_I_Gamma <-gam(TPO4 ~s(Day,by=Station_ID)+s(Station_ID,Year,bs="re")+s(HLRout,by=Station_ID)+s(HLRin,by=Station_ID)+s(Time,bs="cc"),data =Stage_discharge_data ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Station_Year_HLRout_HLRin_Time_mod_I_Gamma)  #Deviance 79.6 %
plot(Station_Year_HLRout_HLRin_Time_mod_I_Gamma, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Station_Year_HLRout_HLRin_Time_mod_I_Gamma, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.1, ticktype="detailed")  #3D HLR Day
vis.gam(Station_Year_HLRout_HLRin_Time_mod_I_Gamma, view = c("HLRin", "Time"), plot.type = "persp",theta=240,too.far=.1, ticktype="detailed")  #3D HLR Day
gam.check(Station_Year_HLRout_HLRin_Time_mod_I_Gamma,pages=1)
check(getViz(Station_Year_HLRout_HLRin_Time_mod_I_Gamma)) 

Mod_1 <-gam(TPO4 ~s(Day,by=Station_ID),data =Stage_discharge_data ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 365),Time=c(0,24)))



#Global smoother for HLRout and Day
HLRout_Station_Day_Time_mod_G_34only <- gam(TPO4 ~s(HLRout,bs="tp",k=5)+s(Day,bs="cc",k=10)+te(Day,by=Year,bs=c("cc","tp"))+te(Day,by=Station_ID,bs="tp")+te(Time,by=Station_ID,bs="cc"), data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Station_Day_Time_mod_G_34only)  #Deviance 75.5%
plot(HLRout_Station_Day_Time_mod_G_34only, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
concurvity(HLRout_Station_Day_Time_mod_G_34only)
gam.check(HLRout_Station_Day_Time_mod_G_34only,pages=1)
check(getViz(HLRout_Station_Day_Time_mod_G_34only))
plot(HLRout_Station_Day_Time_mod_G_34only, pages = 1, scheme = 2, shade = TRUE, scale = 0)
vis.gam(HLRout_Station_Day_Time_mod_G_34only, view = c("HLRout", "Station_ID"), plot.type = "persp",theta=150,ticktype="detailed")  #3D HLR and station
vis.gam(HLRout_Station_Day_Time_mod_G_34only, view = c("Day", "Year"), plot.type = "persp",theta=200, ticktype="detailed")  #3D HLR and station
vis.gam(HLRout_Station_Day_Time_mod_G_34only, view = c("HLRout", "Time"), plot.type = "persp",theta=135,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(HLRout_Station_Day_Time_mod_G_34only, view = c("HLRout", "Day"), plot.type = "persp",theta=45,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(HLRout_Station_Day_Time_mod_G_34only, view = c("HLRout", "Year"), plot.type = "persp",theta=45, ticktype="detailed")  #3D HLR and station
glance(HLRout_Station_Day_Time_mod_G_34only)
ggplot(Stage_discharge_data ,aes(Day,TPO4,color=as.factor(Year),fill=as.factor(Year)))+geom_point(shape=21)+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="black") #long-term trend

#Temp used instead of day
HLRout_Station_Temp_Time_mod_G <- gam(TPO4 ~s(HLRout,bs="tp",k=5)+s(TEMP,bs="tp",k=10)+s(Station_ID,bs="re")+s(Year,bs="re")+ti(TEMP,by=Station_ID,bs="tp")+ti(Time,by=Station_ID,bs="cc"), data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Station_Temp_Time_mod_G)  #Deviance 49.8%
plot(HLRout_Station_Temp_Time_mod_G, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(HLRout_Station_Temp_Time_mod_G, view = c("TEMP", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station

#Temp and day used
HLRout_Station_Temp_Day_Time_mod_G <- gam(TPO4 ~s(HLRout,bs="tp",k=5)+s(Day,bs="tp",k=10)+s(TEMP,bs="tp")+s(Station_ID,bs="re")+s(Year,bs="re")+ti(Day,by=Station_ID,bs="tp")+ti(Time,by=Station_ID,bs="cc"), data =Stage_discharge_data ,method="REML",family="gaussian")
summary(HLRout_Station_Temp_Day_Time_mod_G)  #Deviance 77.7%
plot(HLRout_Station_Temp_Day_Time_mod_G, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(HLRout_Station_Temp_Day_Time_mod_G, view = c("TEMP", "Time"), plot.type = "persp",theta=240,too.far=.05, ticktype="detailed")  #3D HLR and station



#model TPO4 by outflow discharge for each individual stations
HLRout_Station <- gam(TPO4 ~s(HLRout,by=Station_ID)+Station_ID, data =Stage_discharge_data ,family="Gamma")
summary(HLRout_Station)  #deviation 69.2%
plot(HLRout_Station,shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRout_Station)
gam.check(HLRout_Station)
check(getViz(HLRout_Station))

#factor model- individual smooths for each position in flowpath  gaussian distribution
HLRout_Flowpath_Factor <- gam(TPO4 ~ s(HLRout, Flowpath, bs="fs"), data=Stage_discharge_data , method = "REML" ,family="gaussian")
summary(HLRout_Flowpath_Factor)
vis.gam(HLRout_Flowpath_Factor, view = c("HLRout", "Flowpath"), plot.type = "persp",theta=300)
concurvity(HLRout_Flowpath_Factor)
gam.check(HLRout_Flowpath_Factor)
check(getViz(HLRout_Flowpath_Factor))

#factor model- individual smooths for each position in flowpath gamma distribution
HLRout_Flowpath_Factor_gamma <- gam(TPO4 ~ s(HLRout, Flowpath, bs="fs")+Flowpath, data=Stage_discharge_data , method = "REML" ,family=Gamma(link="log"))
summary(HLRout_Flowpath_Factor_gamma)   #devaition 60.5
vis.gam(HLRout_Flowpath_Factor_gamma, view = c("HLRout", "Flowpath"), plot.type = "persp",theta=200)
concurvity(HLRout_Flowpath_Factor_gamma)
gam.check(HLRout_Flowpath_Factor_gamma)
check(getViz(HLRout_Flowpath_Factor_gamma))
plot(HLRout_Flowpath_Factor_gamma,shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)


# Effects from HLRin and HLR out  -----------------------------------------



#factor model using inflow and outflow HLRs 
HLRout_HLRin_flowpath <- gam(TPO4 ~s(HLRout,by=Flowpath)+s(HLRin,by=Flowpath)+Flowpath, data =STA34_only,method = "REML" ,family="Gamma")
summary(HLRout_HLRin_flowpath)
plot(HLRout_HLRin_flowpath,shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRout_HLRin_flowpath)
gam.check(HLRout_HLRin_flowpath)
check(getViz(HLRout_HLRin_flowpath))

HLRout_HLRin_Station_time <- gam(TPO4 ~s(HLRout,by=Station_ID)+s(HLRin,by=Station_ID)+Station_ID, data =RPAs_with_Flow_Stage_Weather_Sonde)
summary(HLRout_HLRin_Station_time)
plot(HLRout_HLRin_Station_time,shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(HLRout_HLRin_Station_time)
gam.check(HLRout_HLRin_Station_time)
getViz(HLRout_HLRin_Station_time)
check(getViz(HLRout_HLRin_Station_time))

# Interaction between outflow stage and discharge for outflow stations --------
Stage_discharge_data <- filter(RPAs_with_Flow_Stage_Weather_Sonde,Flowpath=="Outflow",Station_ID!="G334") %>% rename(Stage_Out="Outflow Stage")

Stage_discharge_mod <- gam(TPO4 ~te(HLRout,Stage_Out)+Station_ID, data= Stage_discharge_data)
summary(Stage_discharge_mod)

plot(Stage_discharge_mod)
plot(Stage_discharge_mod,scheme = 2)
vis.gam(Stage_discharge_mod, view = c("HLRout","Stage_Out"), plot.type = "contour",too.far=.05)
vis.gam(Stage_discharge_mod, view = c("HLRout","Stage_Out"),theta=40)
concurvity(Stage_discharge_mod)
gam.check(Stage_discharge_mod)
getViz(Stage_discharge_mod)
check(getViz(Stage_discharge_mod))


# Interaction between inflow and stage for inflow station at STA34 --------
Stage_Inflow_data <- filter(RPAs_with_Flow_Stage_Weather_Sonde,Flowpath=="Inflow",Station_ID!="G333") %>% rename(Stage_In="Inflow Stage")

Stage_Inflow_mod <- gam(TPO4 ~s(HLRin,Stage_In)+Station_ID, data= Stage_Inflow_data)
summary(Stage_Inflow_mod )
plot(Stage_Inflow_mod )
plot(Stage_Inflow_mod ,scheme = 2)
vis.gam(Stage_Inflow_mod, view = c("HLRin","Stage_In"), plot.type = "contour",too.far=.05)
vis.gam(Stage_Inflow_mod , view = c("HLRin","Stage_In"),theta=330)



# Build Models -------Model built for each station individually-------------------------------------------------
TP_GAM_Time <- function(data) {gam(TPO4 ~ s(Time,bs="cc")+s(HLRin)+s(HLRout)+s(Day), data = data,method = "REML",family="gaussian")}    #function of tp~time
TP_GAM_HLRin <- function(data) {gam(TPO4 ~ s(`HLRin`), data = data)}   #
TP_GAM_HLRout <- function(data) {gam(TPO4 ~ s(`HLRout`), data = data)}


#nest data by station and run model
TP_GAM_Models <- RPAs_with_Flow_Stage_Weather_Sonde %>% 
group_by(Station) %>% 
nest() %>%
mutate(TP_GAM_Time = map(data, TP_GAM_Time),  TP_GAM_HLRin=map(data,TP_GAM_HLRin), TP_GAM_HLRout=map(data,TP_GAM_HLRout))

#summarize model in nested DF
TP_GAM_Models_summary <- TP_GAM_Models %>% 
mutate(TP_Time_glance = map(TP_GAM_Time,glance),TP_Time_tidy = map(TP_GAM_Time,tidy),TP_Time_augment = map(TP_GAM_Time,augment),TP_Time_r.squared=map(TP_GAM_Time ,~summary(.)$r.sq),TP_Time_dev.expl=map(TP_GAM_Time ,~summary(.)$dev.expl)) %>%
mutate(TP_HLRin_glance = map(TP_GAM_HLRin,glance),TP_HLRin_tidy = map(TP_GAM_HLRin,tidy),TP_HLRin_augment = map(TP_GAM_HLRin,augment),TP_HLRin_r.squared=map(TP_GAM_HLRin,~summary(.)$r.sq)) %>%
mutate(TP_HLRout_glance = map(TP_GAM_HLRout,glance),TP_HLRout_tidy = map(TP_GAM_HLRout,tidy),TP_HLRout_augment = map(TP_GAM_HLRout,augment),TP_HLRout_r.squared=map(TP_GAM_HLRout,~summary(.)$r.sq))

TP_Time_summary <- cbind(TP_GAM_Models_summary %>% select(Station,TP_Time_r.squared,TP_Time_dev.expl) %>%  arrange(desc(TP_Time_r.squared)) %>%
left_join(TP_GAM_Models_summary %>% unnest(TP_Time_glance) %>% select(df,AIC,BIC,deviance,df.residual,nobs),by="Station") %>% #join with glance summary
left_join(TP_GAM_Models_summary %>% unnest(TP_Time_tidy) %>% select(edf,ref.df,statistic,p.value),by="Station"),model="Time") #join with tidy summary

TP_HLRin_summary <- cbind(TP_GAM_Models_summary %>% select(Station,TP_HLRin_r.squared) %>% unnest(TP_HLRin_r.squared)  %>% arrange(desc(TP_HLRin_r.squared)) %>%
left_join(TP_GAM_Models_summary %>% unnest(TP_HLRin_glance) %>% select(df,AIC,BIC,deviance,df.residual,nobs),by="Station") %>% #join with glance summary
left_join(TP_GAM_Models_summary %>% unnest(TP_HLRin_tidy) %>% select(edf,ref.df,statistic,p.value),by="Station"),model="HLRin") #join with tidy summary

TP_HLRout_summary <- cbind(TP_GAM_Models_summary %>% select(Station,TP_HLRout_r.squared) %>% unnest(TP_HLRout_r.squared)  %>% arrange(desc(TP_HLRout_r.squared)) %>%
left_join(TP_GAM_Models_summary %>% unnest(TP_HLRout_glance) %>% select(df,AIC,BIC,deviance,df.residual,nobs),by="Station") %>% #join with glance summary
left_join(TP_GAM_Models_summary %>% unnest(TP_HLRout_tidy) %>% select(edf,ref.df,statistic,p.value),by="Station"),model="HLRout") #join with tidy summary


All_models_summary <- bind_rows(TP_Time_summary,TP_HLRin_summary,TP_HLRout_summary)


# Model TP by Flow and temp -----------------------------------------------

#temp timeseries
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(Date,`TEMP`))+geom_point()+facet_wrap(~Station_ID,scales="free")
ggplot(filter(RPAs_with_Flow_Stage_Weather_Sonde,HLRout>0) ,aes(HLRout,`TEMP`))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(color="red")
ggplot(RPAs_with_Flow_Stage_Weather_Sonde ,aes(`TPO4`))+facet_wrap(~Station,scales="free")+
geom_histogram(aes(y = after_stat(density)),binwidth=2)+ 
stat_theodensity(aes(colour = "Normal"),distri = "norm")+ 
stat_theodensity(aes(colour="Gamma"),distri = "gamma")  +
stat_theodensity(aes(colour="Log Normal"),distri = "lnorm") +  
stat_theodensity(aes(colour="cauchy"),distri = "cauchy")   +
  stat_theodensity(aes(colour="exp"),distri = "exp")     

#for outflow data only
#Temp_HLRout_te <- y ~ s(x1) + s(x2) + ti(x1, x2)+

Temp_HLRout_te_gamma  <- gam(TPO4 ~ s(HLRout)+s(TEMP)+ti(HLRout,TEMP), data=Stage_discharge_data, method = "REML",family="Gamma")
summary(Temp_HLRout_te_gamma  ) #deviance 
vis.gam(Temp_HLRout_te_gamma  , view = c("HLRout", "TEMP"), plot.type = "persp",theta=300)
vis.gam(Temp_HLRout_te_gamma, view = c("HLRout", "TEMP"), plot.type = "contour",too.far=.05)
concurvity(HLRin_Flowpath_Factor)
gam.check(HLRin_Flowpath_Factor) 
check(getViz(HLRin_Flowpath_Factor))

Temp_HLRout_te_gaussian  <- gam(TPO4 ~ s(HLRout)+s(TEMP)+ti(HLRout,TEMP), data=Stage_discharge_data, method = "REML",family="gaussian")
summary(Temp_HLRout_te_gaussian  ) #deviance 
vis.gam(Temp_HLRout_te_gaussian  , view = c("HLRout", "TEMP"), plot.type = "persp",theta=120)
vis.gam(Temp_HLRout_te_gaussian , view = c("HLRout", "TEMP"), plot.type = "contour",too.far=.05)
plot(Temp_HLRout_te_gaussian, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE, cex = 1)

Temp_day_te_gaussian <-gam(TPO4 ~ s(Day)+s(TEMP)+ti(Day,TEMP), data=Stage_discharge_data, method = "REML",family="gaussian")
summary(Temp_day_te_gaussian  ) #deviance 
vis.gam(Temp_day_te_gaussian  , view = c("Day", "TEMP"), plot.type = "persp",theta=150,too.far=.05)
vis.gam(Temp_day_te_gaussian , view = c("Day", "TEMP"), plot.type = "contour",too.far=.05)
plot(Temp_day_te_gaussian, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)

Temp_gaussian <- gam(TPO4 ~ s(TEMP), data=Stage_discharge_data, method = "REML",family="gaussian")
summary(Temp_gaussian  ) #deviance 
plot(Temp_gaussian, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)

Rain_gaussian <- gam(TPO4 ~ s(Rain), data=Stage_discharge_data, method = "REML",family="gaussian")
summary(Temp_gaussian  ) #deviance 
plot(Temp_gaussian, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)

Wind_gaussian <- gam(TPO4 ~ s(Wind,Station_ID, bs="fs"), data=Stage_discharge_data, method = "REML",family="gaussian")
summary(Wind_gaussian  ) #deviance 
plot(Wind_gaussian, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)

Hour_gaussian <- gam(TPO4 ~ s(Hour,Station_ID, bs="fs")+Station_ID, data=Stage_discharge_data, method = "REML",family="gaussian")
summary(Hour_gaussian  ) #deviance 
plot(Hour_gaussian, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)




# model TP by day of year -------------------------------------------------
Day_gaussian <- gam(TPO4 ~ s(Day,Station_ID, bs="fs")+Station_ID+s(HLRout,Station_ID, bs="fs")+s(TEMP)+s(Hour,bs="cc")+s(Wind), data=Stage_discharge_data, method = "REML",family="gaussian")
summary(Day_gaussian) #deviance 
plot(Day_gaussian , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE, cex = 1)
concurvity(Day_gaussian)
gam.check(Day_gaussian) 
check(getViz(Day_gaussian))



Day_gaussian_gamma <- gam(TPO4 ~ s(Day,Station_ID, bs="fs")+Station_ID+s(HLRout,Station_ID, bs="fs")+s(TEMP)+s(Hour,bs="cc")+s(Wind), data=Stage_discharge_data, method = "REML",family="Gamma")
summary(Day_gaussian_gamma) #deviance 
plot(Day_gaussian_gamma , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE, cex = 1)
concurvity(Day_gaussian_gamma)
gam.check(Day_gaussian_gamma) 
check(getViz(Day_gaussian_gamma))

# TPO4 by hour model  ---------------------------------------------------------
#Single common smoother mod_G
Hour_Station_mod_G <- gam(TPO4 ~s(Hour,bs="tp")+s(Station_ID,bs="re"), data =RPAs_with_Flow_Stage_Weather_Sonde,method="REML",family="gaussian")
test <-summary(Hour_Station_mod_G)  #Deviance 39%
plot(Hour_Station_mod_G, shade = TRUE, pages = 1,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(Hour_Station_mod_G)
gam.check(Hour_Station_mod_G,pages=1)
check(getViz(Hour_Station_mod_G))
glance(Hour_Station_mod_G)
tidy(Hour_Station_mod_G)

#Global smoother and group level smoothers with same wiggliness  Mod_GS
Hour_Station_mod_GS <- gam(TPO4 ~s(Hour,m=2)+s(Hour,Station_ID,bs="fs",m=2), data =RPAs_with_Flow_Stage_Weather_Sonde,method="REML",family="gaussian")
summary(Hour_Station_mod_GS)  #Deviance 39%
plot(HLRin_Station_mod_GS, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(Hour_Station_mod_GS)
gam.check(Hour_Station_mod_GS,pages=1)
check(getViz(Hour_Station_mod_GS))

#global smoother and group level smoother with individual wiggliness Mod_GI
Hour_Station_mod_GI <- gam(TPO4 ~s(Hour,bs="tp")+s(Hour,by=Station_ID,bs="tp")+s(Station_ID,bs="re"), data =RPAs_with_Flow_Stage_Weather_Sonde,method="REML",family="gaussian")
summary(Hour_Station_mod_GI)  #Deviance 39.1%
plot(Hour_Station_mod_GI, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(Hour_Station_mod_GI)
gam.check(Hour_Station_mod_GI,pages=1)
check(getViz(Hour_Station_mod_GI))

#group level smoother with same wiggliness Mod_S
Hour_Station_mod_S <- gam(TPO4 ~s(Hour,Station_ID,bs="fs",m=2), data =RPAs_with_Flow_Stage_Weather_Sonde,method="REML",family="gaussian")
summary(Hour_Station_mod_S)  #Deviance 39%
plot(Hour_Station_mod_S, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(Hour_Station_mod_S)
gam.check(Hour_Station_mod_S,pages=1)
check(getViz(Hour_Station_mod_S))

#group level smoother with individual wiggliness Mod_I
Hour_Station_mod_I <- gam(TPO4 ~s(Hour,by=Station_ID, bs="tp",m=2)+s(Station_ID,bs="re"), data =RPAs_with_Flow_Stage_Weather_Sonde,method="REML",family="gaussian")
summary(Hour_Station_mod_I)  #Deviance 39%
plot(Hour_Station_mod_I,shade=TRUE)
concurvity(Hour_Station_mod_I)
gam.check(Hour_Station_mod_I,pages=1)
check(getViz(Hour_Station_mod_I))




# Model as suggested  by  https://stats.stackexchange.com/questions/244042/trend-in-irregular-time-series-data
Complete_model <- gam(TPO4 ~s(Time)+s(Day)+Station_ID, data =RPAs_with_Flow_Stage_Weather_Sonde, method = "REML" ,family="gaussian")
summary(Complete_model)
plot(Complete_model,shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE, residuals = TRUE,pch = 1, cex = 1)
concurvity(Complete_model)
gam.check(Complete_model)
getViz(Complete_model)
check(getViz(Complete_model))



