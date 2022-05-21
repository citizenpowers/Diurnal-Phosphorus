

library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(ggpmisc)
library(zoo)
library(viridis)
library(Hmisc)
library(boot)
library(broom)
library(boot.pval)

citation("zoo")

# Import data for RPA analysis -------------------------------------------------------------
#RPA tidy data
RPAs_Sorted <- read_csv("Data/RPAs Sorted.csv") %>%
mutate(Flag=if_else(Station == "G334" & Date >"2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed
filter(Flag ==FALSE)  

#Import Flow Data
Combined_BK_Flow <- read_csv("Data/Combined_BK_Flow.csv", col_types = cols(Flow = col_number(),HLR = col_number()))

#Import RPA Flow and Stage and weather data
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv")


# Outflow TP Load scenarios (Cumulative TP load and Flow by Date)----------------------------------------------------

#DF with days of TP sample collections of at least 4 samples. 
Days_with_TPO4 <- RPAs_Sorted %>%                  
filter(`Flowpath Region`=="Outflow") %>%  
group_by(`Flowway`,Date) %>%
summarise(`Has Sample`=sum(!is.na(TPO4))) %>%
filter(`Has Sample`>3)

#create DF of TP interpolated by hour
Outflow_TP_Load_Scenarios_wide1<- Combined_BK_Flow  %>%
left_join(filter(select(RPAs_Sorted,2:14),`Flowpath Region`=="Outflow") ,by=c("Date","Hour","Flowway")) %>%   #join TP data to flow data
group_by(Flowway)  %>%
mutate(`Date_Time`=ymd_hms(ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York")),`TP interpolated`=TPO4) %>%   #create hourly date time index
mutate(`TP interpolated`=na.approx(`TP interpolated`,along=index(`Date_Time`),na.rm=FALSE))  %>%                                                    #Interpolate TP by hour
#mutate(`Hourly TP LOAD`=if_else(is.finite(Outflow),`TP interpolated`/1000*Outflow*3600*28.31/1000000,0)) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
filter(is.finite(`TP interpolated`))   %>%
semi_join(Days_with_TPO4,by=c("Date","Flowway"))             #eliminate days from which no TP sample was collected. 

#DF of complete days. Only days with 24 hours of data
Complete_days <-  Outflow_TP_Load_Scenarios_wide1 %>%
group_by(`Flowway`,Date) %>%
summarise(n= n()) %>%
filter(n==24)

#calculate hourly TP load and flow under different scenarios
Outflow_TP_Load_Scenarios_wide<- Outflow_TP_Load_Scenarios_wide1  %>%  
semi_join(Complete_days,by=c("Date","Flowway")) %>%  #filter
group_by(Flowway,Date) %>%
#mutate(`Outflow 50% Day`=mean(Outflow,na.rm=TRUE)) %>%            #Distribute cumulative outflow evenly through day and night
mutate(`Outflow 66% between 8pm-8am`=case_when(between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*.6666666666,!between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*1.333333333)) %>%
mutate(`Outflow 100% between 8am-8pm`=case_when(between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*2,!between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*0)) %>% 
mutate(`Outflow 75% between 12-4AM`=case_when(between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*18/4,!between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*6/20)) %>%
mutate(`Outflow 66% Day`=case_when(between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*1.333333333,!between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*.666666666)) %>%  #Distribute cumulative outflow 2/3 during day and 1/3 at night  
#mutate(`Outflow 100% Night`=case_when(between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*0,!between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*2)) %>%
#mutate(`Outflow 50% between 12-4AM`=case_when(between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*3,!between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*12/20)) %>%
#mutate(`Outflow 100% between 12-4AM`=case_when(between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*6,!between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*0)) %>%
#mutate(`Outflow Inverse Diel P Pattern`=case_when(between(Hour,0,1)~mean(Outflow,na.rm=TRUE)*16/10,
#                                           between(Hour,2,3)~mean(Outflow,na.rm=TRUE)*14/10,
#                                           between(Hour,4,5)~mean(Outflow,na.rm=TRUE)*12/10,
#                                           between(Hour,6,7)~mean(Outflow,na.rm=TRUE)*10/10,
#                                           between(Hour,8,9)~mean(Outflow,na.rm=TRUE)*8/10,
#                                           between(Hour,10,11)~mean(Outflow,na.rm=TRUE)*6/10,
#                                           between(Hour,12,13)~mean(Outflow,na.rm=TRUE)*4/10,
#                                           between(Hour,14,15)~mean(Outflow,na.rm=TRUE)*6/10,
#                                           between(Hour,16,17)~mean(Outflow,na.rm=TRUE)*8/10,
#                                           between(Hour,18,19)~mean(Outflow,na.rm=TRUE)*10/10,
#                                           between(Hour,20,21)~mean(Outflow,na.rm=TRUE)*12/10,
#                                           between(Hour,22,23)~mean(Outflow,na.rm=TRUE)*14/10)) %>%
mutate(`P Load`=if_else(is.finite(Outflow),`TP interpolated`*Outflow*3600*28.3168,0)) %>%  #ppb/1000mg/l*28.3168L/cf*60sec/min*60min/hour*1kg/1000g*1g/1000mg
mutate(`Outflow 66% Night Load`=`TP interpolated`*`Outflow 66% between 8pm-8am`*3600*28.3168) %>%
mutate(`Outflow 100% Day Load`=`TP interpolated`*`Outflow 100% between 8am-8pm`*3600*28.3168) %>%
mutate(`Outflow 75% between 12-4AM Load`=`TP interpolated`*`Outflow 75% between 12-4AM`*3600*28.3168) %>%
mutate(`Outflow 66% Day Load`=`TP interpolated`*`Outflow 66% Day`*3600*28.3168) %>% 
#mutate(`Outflow 100% Night Load`=`TP interpolated`*`Outflow 100% Night`*3600*28.3168) %>%
#mutate(`Outflow 50% Day Load`=`TP interpolated`*`Outflow 50% Day`*3600*28.3168) %>%                             #Unused scenarios
#mutate(`Outflow 50% between 12-4AM Load`=`TP interpolated`*`Outflow 50% between 12-4AM`*3600*28.3168) %>%
#mutate(`Outflow 100% between 12-4AM Load`=`TP interpolated`*`Outflow 100% between 12-4AM`*3600*28.3168) %>%
#mutate(`Outflow Opposite Diel P Pattern Load`=`TP interpolated`*`Outflow Inverse Diel P Pattern`*3600*28.3168) %>%
group_by(Flowway) %>%
mutate(`Cumulative Flow`=cumsum(`Outflow`*3600*28.3168)) %>% #Cumulative outflows- Check if cumulative outflows are the same for every scenario 
mutate(`Cumulative Flow 100% 8am to 8pm`=cumsum(`Outflow 100% between 8am-8pm`*3600*28.3168)) %>% 
mutate(`Cumulative Flow 8pm-8am`=cumsum(`Outflow 66% between 8pm-8am`*3600*28.3168)) %>%  
mutate(`Cumulative Flow Outflow 75% between 12-4AM`=cumsum(`Outflow 75% between 12-4AM`*3600*28.3168)) %>%   
mutate(`Cumulative Flow 66% day`=cumsum(`Outflow 66% Day`*3600*28.3168)) %>%  
#mutate(`Cumulative Flow 50% day`=cumsum(`Outflow 50% Day`)) %>%                                   
#mutate(`Cumulative Flow 100% night`=cumsum(`Outflow 100% Night`)) %>%
#mutate(`Cumulative Flow Opposite Diel P Pattern`=cumsum(`Outflow Inverse Diel P Pattern`)) %>% 
mutate(`Cumulative P Load (Baseline)`=cumsum(`P Load`)) %>%  
mutate(`Cumulative P Load 8pm-8am`=cumsum(`Outflow 66% Night Load`)) %>%
mutate(`Cumulative P Load 8am-8pm`=cumsum(`Outflow 100% Day Load`))  %>%  
mutate(`Cumulative P Load between 12-4AM`=cumsum(`Outflow 75% between 12-4AM Load`)) %>% 
mutate(`Cumulative P Load 66% Day`=cumsum(`Outflow 66% Day Load`)) %>%
#mutate(`Cumulative P Load 50% Day`=cumsum(`Outflow 50% Day Load`)) %>%
#mutate(`Cumulative P Load 100% Night`=cumsum(`Outflow 100% Night Load`)) %>%  
#mutate(`Cumulative P 50% between 12-4AM`=cumsum(`Outflow 50% between 12-4AM Load`)) %>%  
#mutate(`Cumulative P 100% between 12-4AM`=cumsum(`Outflow 100% between 12-4AM Load`)) %>%  
#mutate(`Inverse Diel P Pattern Flow`=cumsum(`Outflow Opposite Diel P Pattern Load`))  %>% 
select(-`Outflow HLR`, -Inflow,-`Inflow HLR`,-TRP,-Month,-Day,-Time,-Year,-Minute,-date) 

#Make sure cumulative flow is the same for each scenario
Cumulative_flow_check <- Outflow_TP_Load_Scenarios_wide %>%
group_by(Flowway) %>%
summarise(`baseline flow`=sum(Outflow*3600*28.3168),`Outflow 100% between 8am-8pm`=sum(`Outflow 100% between 8am-8pm`*3600*28.3168),`Outflow 66% between 8pm-8am`=sum(`Outflow 66% between 8pm-8am`*3600*28.3168),`Outflow 75% between 12-4AM`=sum(`Outflow 75% between 12-4AM`*3600*28.3168),`Outflow 66% Day`=sum(`Outflow 66% Day`*3600*28.3168)) %>%
mutate(across(where(is.numeric),~.x/1233000))  

#Count number of days in scenario
number_of_days <- Outflow_TP_Load_Scenarios_wide %>% group_by(Flowway) %>% distinct(Date) %>% summarise(n())

# Estimated FWM TP using cumulative TP load and cumulative flow ------------

#Calculate FWM using the cumulative sum of rowwise calculations
Calculated_FWM <-Outflow_TP_Load_Scenarios_wide %>%
mutate(`Baseline FWM`=`Cumulative P Load (Baseline)`/`Cumulative Flow`,`FWM Cumulative Flow 100% 8am to 8pm`=`Cumulative P Load 8am-8pm`/`Cumulative Flow 100% 8am to 8pm`,
`FWM 66% flow between 8pm-8am`=`Cumulative P Load 8pm-8am`/`Cumulative Flow 8pm-8am`,`FWM 75% flow between 12am-4am`=`Cumulative P Load between 12-4AM`/`Cumulative Flow Outflow 75% between 12-4AM`,
`FWM 66% flow between 8am-8pm`=`Cumulative P Load 66% Day`/`Cumulative Flow 66% day`)

#Cumulative TP load by scenario long format for plots
Outflow_TP_Load_Scenarios_long <- Outflow_TP_Load_Scenarios_wide %>%
pivot_longer(`Cumulative P Load (Baseline)`:`Cumulative P Load 66% Day`,names_to = "Scenario", values_to = "Value") %>%
mutate(`Scenario`=factor(`Scenario`,levels = c("Cumulative P Load 8am-8pm","Cumulative P Load 66% Day","Cumulative P Load (Baseline)", "Cumulative P Load 8pm-8am","Cumulative P Load between 12-4AM")))

#Calculated FWM from rowwise calculations
flow_weighted_mean <- Outflow_TP_Load_Scenarios_long %>%
group_by(Flowway,Scenario) %>%
summarise(`Cumulative P Load (kg)`=max(Value,na.rm=TRUE)/1000000000,`Cumulative Flow (L)`=max(`Cumulative Flow`,na.rm=TRUE),`Cumulative Flow (acre-ft)`=`Cumulative Flow (L)`/1233000,`FWM Concentration (ug/L)`=max(Value,na.rm=TRUE)/`Cumulative Flow (L)`,n=sum(!is.na(Value)),sd=sd(`Value`/`Cumulative Flow (L)`,na.rm = TRUE),se=sd/sqrt(n))

write.csv(flow_weighted_mean,"./Data/FWM_Rowwise_Estimates.csv")

# bootstrap CI and SE of Flow Weighted Mean -------------------------------

#Function to calculate FWM using base weighted mean function
samplewmean <- function(data, d) {
  return(weighted.mean(x=data[d,2], w=data[d,1],normwt = TRUE)) 
}

#Function to calculate FWM using my own calculations of weighted mean
Flow_Weighted_Mean <- function(data, d) {
n <-nrow(data)
Flow <- data[d,1]
TP_interpolated <- data[d,2]
TP_Load <- sum(Flow*TP_interpolated)
Flow_total <- sum(Flow)
FWM<- TP_Load/Flow_total
return(FWM)
}

#set seed for reproducibility 
set.seed(1000)

#STA2 
STA2_DF <- Outflow_TP_Load_Scenarios_wide %>% filter(Flowway=="STA-2 Central")  #create DF for STA2 FW3
  
#Bootstrap SE and CI 
STA2_Baseline <- mutate(tidy(boot(data=STA2_DF[,c(4,9)], statistic = samplewmean, R=1000),conf.int = TRUE),Scenario="Baseline")
STA2_66_night  <-mutate(tidy(boot(data=STA2_DF[,c(10,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="66% Flow between 8pm-8am ")
STA2_100_day  <-mutate(tidy(boot(data=STA2_DF[,c(11,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="100% Flow between 8am-8pm")
STA2_75_12am_4am <-mutate(tidy(boot(data=STA2_DF[,c(12,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="75% Flow between 12am-4am")
STA2_66_day <-mutate(tidy(boot(data=STA2_DF[,c(13,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="66% Flow between 8am-8pm")

#combine parameter estimates in DF
STA2_Stats <-mutate(rbind(STA2_Baseline,STA2_66_night,STA2_100_day,STA2_75_12am_4am,STA2_66_day),Flowway="STA-2 Central")

#STA34 Central
STA34C_DF <- Outflow_TP_Load_Scenarios_wide %>%  #create DF for STA34 FW central
filter(Flowway=="STA-3/4 Central")

#Bootstrap SE and CI 
STA34C_Baseline <- mutate(tidy(boot(data=STA34C_DF[,c(4,9)], statistic = samplewmean, R=1000),conf.int = TRUE),Scenario="Baseline")
STA34C_66_night  <-mutate(tidy(boot(data=STA34C_DF[,c(10,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="66% Flow between 8pm-8am ")
STA34C_100_day  <-mutate(tidy(boot(data=STA34C_DF[,c(11,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="100% Flow between 8am-8pm")
STA34C_75_12am_4am <-mutate(tidy(boot(data=STA34C_DF[,c(12,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="75% Flow between 12am-4am")
STA34C_66_day <-mutate(tidy(boot(data=STA34C_DF[,c(13,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="66% Flow between 8am-8pm")

#combine parameter estimates in DF
STA34C_Stats <-mutate(rbind(STA34C_Baseline,STA34C_66_night,STA34C_100_day,STA34C_75_12am_4am,STA34C_66_day),Flowway="STA-34 Central")

#STA34 Western
STA34W_DF <- Outflow_TP_Load_Scenarios_wide %>%  #create DF for STA34 FW Western
filter(Flowway=="STA-3/4 Western")

#Bootstrap SE and CI 
STA34W_Baseline <- mutate(tidy(boot(data=STA34W_DF[,c(4,9)], statistic = samplewmean, R=1000),conf.int = TRUE),Scenario="Baseline")
STA34W_66_night  <-mutate(tidy(boot(data=STA34W_DF[,c(10,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="66% Flow between 8pm-8am ")
STA34W_100_day  <-mutate(tidy(boot(data=STA34W_DF[,c(11,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="100% Flow between 8am-8pm")
STA34W_75_12am_4am <-mutate(tidy(boot(data=STA34W_DF[,c(12,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="75% Flow between 12am-4am")
STA34W_66_day <-mutate(tidy(boot(data=STA34W_DF[,c(13,9)], statistic = samplewmean, R=1000),conf.int = TRUE,digits = 6),Scenario="66% Flow between 8am-8pm")

#combine parameter estimates in DF
STA34W_Stats <-mutate(rbind(STA34W_Baseline,STA34W_66_night,STA34W_100_day,STA34W_75_12am_4am,STA34W_66_day),Flowway="STA-34 Western")

#Combine bootstrap estimates from all flowways
All_FWs_Stats <- rbind(STA34W_Stats,STA2_Stats,STA34C_Stats)

#Save data from bootstrap estimates
write.csv(All_FWs_Stats,"./Data/FWM estimates from bootstrap.csv")


# Estimated SE using the HMISC package ------------------------------------

#Function for calculated weighted SE from https://stats.stackexchange.com/questions/25895/computing-standard-error-in-weighted-mean-estimation
weighted.var.se <- function(x, w, na.rm=FALSE)
  #  Computes the variance of a weighted mean following Cochran 1977 definition
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  return(out)
}

#Estimates of variance using Hmisc package
weighted_mean_baseline <- Outflow_TP_Load_Scenarios_wide %>%
group_by(Flowway) %>%
summarise(`Baseline WM`=weighted.mean(`TP interpolated`,`Outflow`,na.rm=TRUE),`Baseline WVar`=wtd.var(`TP interpolated`,`Outflow`,na.rm=TRUE),`Baseline WSTdev`=sqrt(`Baseline WVar`),`Baseline WSE calc`= `Baseline WSTdev`/sqrt(n()),`Baseline WSE function`=weighted.var.se(`TP interpolated`,`Outflow`),
            `100% Day WM`=weighted.mean(`TP interpolated`,`Outflow 100% between 8am-8pm`,na.rm=TRUE),`100% Day WVar`=wtd.var(`TP interpolated`,`Outflow 100% between 8am-8pm`,na.rm=TRUE),`100% Day WSTdev`=sqrt(`100% Day WVar`),`100% Day WSE calc`= `100% Day WSTdev`/sqrt(n()),`100% Day WSE function`=weighted.var.se(`TP interpolated`,`Outflow 100% between 8am-8pm`),
            `66% Night WM`=weighted.mean(`TP interpolated`,`Outflow 66% between 8pm-8am`,na.rm=TRUE),`66% Night WVar`=wtd.var(`TP interpolated`,`Outflow 66% between 8pm-8am`,na.rm=TRUE),`66% Night WSTdev`=sqrt(`66% Night WVar`),`66% Night WSE calc`= `66% Night WSTdev`/sqrt(n()),`66% Night WSE function`=weighted.var.se(`TP interpolated`,`Outflow 66% between 8pm-8am`),
            `75% 12-4 WM`=weighted.mean(`TP interpolated`,`Outflow 75% between 12-4AM`,na.rm=TRUE),`75% 12-4 WVar`=wtd.var(`TP interpolated`,`Outflow 75% between 12-4AM`,na.rm=TRUE),`75% 12-4 WSTdev`=sqrt(`75% 12-4 WVar`),`75% 12-4 WSE calc`= `75% 12-4 WSTdev`/sqrt(n()),`75% 12-4 WSE function`=weighted.var.se(`TP interpolated`,`Outflow 75% between 12-4AM`))
            


`Transposed Weighted Mean Table` <- as.data.frame(t(head(weighted_mean_baseline)))

#Save data from HMISC estimates
write.csv(`Transposed Weighted Mean Table`,"./Data/FWM estimates using HMISC.csv")





  


# Hypothesis tests of FWM  --------------------------------------------
#Bootstrap method used for hypothesis test because daily or instant FWM values do not equal the FWM when calculated using cumulative values

#Table showing average of daily FWM does not equal cumulative flow weighted mean
Instantaneous_FWM <- Outflow_TP_Load_Scenarios_wide %>%
group_by(Flowway,Date) %>%
summarise(`Instant FWM Baseline`=sum(`TP interpolated`*Outflow)/sum(`Outflow`),`Instant FWM 100% 8am to 8pm`=sum(`TP interpolated`*`Outflow 100% between 8am-8pm`)/sum(`Outflow 100% between 8am-8pm`)) %>%
pivot_longer(`Instant FWM Baseline`:`Instant FWM 100% 8am to 8pm`,names_to = "Scenario", values_to = "Value") %>%
group_by(Flowway,Scenario) %>%
summarise(n=n(),`is finite`=sum(is.finite(Value)),`FWM mean of Instant`=mean(Value,na.rm=TRUE),`FWM median of Instant`=median(Value,na.rm=TRUE))  
  
#Create STA2 Bootstrap objects
STA2_boot_Baseline <-boot(data=STA2_DF[,c(4,9)], statistic = samplewmean, R=1000)
STA2_boot_66_night <-boot(data=STA2_DF[,c(10,9)], statistic = samplewmean, R=1000)
STA2_boot_100_day  <-boot(data=STA2_DF[,c(11,9)], statistic = samplewmean, R=1000)
STA2_boot_75_12am_4am <- boot(data=STA2_DF[,c(12,9)], statistic = samplewmean, R=1000)
STA2_boot_66_day <- boot(data=STA2_DF[,c(13,9)], statistic = samplewmean, R=1000)

#Use confidence inversion to test for statistical difference vs baseline using method  https://modernstatisticswithr.com/modchapter.html#intervalinversion
boot.pval(STA2_boot_66_night, type = "perc", theta_null = STA2_boot_Baseline$t0)   #test baseline vs 66% night
boot.pval(STA2_boot_100_day, type = "perc", theta_null = STA2_boot_Baseline$t0)   #test baseline vs 100% day
boot.pval(STA2_boot_75_12am_4am , type = "perc", theta_null = STA2_boot_Baseline$t0)   #test baseline vs 75% 12-4am
boot.pval(STA2_boot_66_day , type = "perc", theta_null = STA2_boot_Baseline$t0)   #test baseline vs 66% day


#Create STA34C Bootstrap objects
STA34C_boot_Baseline <-boot(data=STA34C_DF[,c(4,9)], statistic = samplewmean, R=1000)
STA34C_boot_66_night <-boot(data=STA34C_DF[,c(10,9)], statistic = samplewmean, R=1000)
STA34C_boot_100_day  <-boot(data=STA34C_DF[,c(11,9)], statistic = samplewmean, R=1000)
STA34C_boot_75_12am_4am <- boot(data=STA34C_DF[,c(12,9)], statistic = samplewmean, R=1000)
STA34C_boot_66_day <- boot(data=STA34C_DF[,c(13,9)], statistic = samplewmean, R=1000)

#CI inversion for statistical difference
boot.pval(STA34C_boot_66_night, type = "perc", theta_null = STA34C_boot_Baseline$t0)   #test baseline vs 66% night
boot.pval(STA34C_boot_100_day, type = "perc", theta_null = STA34C_boot_Baseline$t0)   #test baseline vs 100% day
boot.pval(STA34C_boot_75_12am_4am  , type = "perc", theta_null = STA34C_boot_Baseline$t0)   #test baseline vs 75% 12-4am
boot.pval(STA34C_boot_66_day  , type = "perc", theta_null = STA34C_boot_Baseline$t0)   #test baseline vs 66% day

#Create STA34W Bootstrap objects
STA34W_boot_Baseline <-boot(data=STA34W_DF[,c(4,9)], statistic = samplewmean, R=1000)
STA34W_boot_66_night <-boot(data=STA34W_DF[,c(10,9)], statistic = samplewmean, R=1000)
STA34W_boot_100_day  <-boot(data=STA34W_DF[,c(11,9)], statistic = samplewmean, R=1000)
STA34W_boot_75_12am_4am <- boot(data=STA34W_DF[,c(12,9)], statistic = samplewmean, R=1000)
STA34W_boot_66_day <- boot(data=STA34W_DF[,c(13,9)], statistic = samplewmean, R=1000)

#CI inversion for statistical difference
boot.pval(STA34W_boot_66_night, type = "perc", theta_null = STA34W_boot_Baseline$t0)   #test baseline vs 66% night
boot.pval(STA34W_boot_100_day, type = "perc", theta_null = STA34W_boot_Baseline$t0)   #test baseline vs 100% day
boot.pval(STA34W_boot_75_12am_4am  , type = "perc", theta_null = STA34W_boot_Baseline$t0)   #test baseline vs 75% 12-4am
boot.pval(STA34W_boot_66_day  , type = "perc", theta_null = STA34W_boot_Baseline$t0)   #test baseline vs 75% 12-4am


# Figures -----------------------------------------------------------------
ggplot(Outflow_TP_Load_Scenarios,aes(Date_Time,`Value`,color=Scenario,label=Scenario))+geom_text(label="-")+theme_bw()+facet_wrap(vars(Flowway),nrow=1,scales = "free")+
scale_y_continuous(breaks=pretty_breaks(n=8),label=comma)+scale_x_datetime(breaks = "4 month", date_labels = "%b %y")+scale_color_brewer(palette = "Set2")+#scale_color_viridis( discrete = TRUE,option="D")+
labs(title = "P Load Scenarios at STA Outflows",y="Total Phosphorus (kg)",x="Date")+guides(color = guide_legend(override.aes = list(size = 10)))+
theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("Figures/Outflow P Load Scenarios.jpeg", plot = last_plot(), width = 8, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Flow Weighted mean 
ggplot(flow_weighted_mean,aes(reorder(`Scenario`,-`FWM Concentration (ug/L)`),`FWM Concentration (ug/L)`,fill=Scenario,label=round(`FWM Concentration (ug/L)`,2)))+geom_col(color="black")+geom_text(vjust = -0.5)+
geom_errorbar(aes(ymin=`FWM Concentration (ug/L)`-se, ymax=`FWM Concentration (ug/L)`+se), width=.2) +  
theme_bw()+facet_wrap(vars(Flowway),nrow=1)+scale_fill_brewer(palette = "Set3")+scale_y_continuous(breaks=seq(0,25,1),label=comma)+guides(color = guide_legend(override.aes = list(size = 10)))+
theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+labs(x="")

ggsave("Figures/Flow Weighted Mean of Flow Scenarios.jpeg", plot = last_plot(), width = 8, height = 6.5, units = "in", dpi = 300, limitsize = TRUE)


#Flow Weighted mean by date
ggplot(flow_weighted_mean_date,aes(`Scenario`,`FWM Concentration (ug/L)`,fill=Scenario))+geom_boxplot()+
theme_bw()+facet_wrap(vars(Flowway),nrow=1,scales = "free")+scale_fill_brewer(palette = "Set3")+scale_y_continuous(breaks=pretty_breaks(n=25),label=comma,limits = c(0,25))+guides(color = guide_legend(override.aes = list(size = 10)))+
theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Example flow scenario calculation
Outflow_TP_Load_Scenarios2 <- Outflow_TP_Load_Scenarios %>%
filter(Date=="2014-10-14") %>%
select(Date_Time,Flowway,`TPO4`,`TP interpolated`,`Outflow 100% between 8am-8pm`,`Outflow 66% between 8pm-8am`,`Outflow 75% between 12-4AM`,`Outflow Inverse Diel P Pattern`,`Outflow`) %>% 
rename(`Measured Outflow`="Outflow") %>%
pivot_longer(`Outflow 100% between 8am-8pm`:`Measured Outflow`,names_to = "Flow Scenarios", values_to = "CFS") 
  

ggplot(filter(Outflow_TP_Load_Scenarios2,`Flow Scenarios` %in% c("Outflow Inverse Diel P Pattern","Measured Outflow","Outflow 66% between 8pm-8am")),aes(Date_Time, ymin = 0,ymax=CFS,fill=`Flow Scenarios`))+geom_ribbon(alpha=0.4,color="black")+
facet_wrap(vars(Flowway),nrow=2,scales = "free")+geom_line(aes(Date_Time,`TP interpolated`),linetype = "dashed")+ geom_point(aes(Date_Time,TPO4),fill="#2b8cbe",size=3,shape=21,show_legend = TRUE) +
scale_y_continuous(sec.axis = sec_axis(~ . /.5, name = "Mean Hourly CFS",breaks = pretty_breaks(5)))+
theme_bw()+scale_x_datetime(breaks = "1 hour", date_labels = "%H:%M")+labs(y=expression(P~mu~L^-1))+
theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
guides(shape = guide_legend(override.aes = list(color = "black")))

ggsave("Figures/Example flow scenarios.jpeg", plot = last_plot(), width = 8, height = 6, units = "in", dpi = 300, limitsize = TRUE)


#Histogram of STA34 Central FW
hist(boot(data=STA34C_DF[,c(4,9)], statistic = samplewmean, R=1000)$t, xlab = "Statistic", main = "Bootstrap Distribution") 
box()
abline(v =boot(data=STA34C_DF[,c(4,9)], statistic = samplewmean, R=1000)$t0, lty = 2, col = "red")

#Histogram of STA2 Central FW
hist(boot(data=STA2_DF[,c(4,9)], statistic = samplewmean, R=1000)$t, xlab = "Statistic", main = "Bootstrap Distribution") 
box()
abline(v = boot(data=STA2_DF[,c(4,9)], statistic = samplewmean, R=1000)$t0, lty = 2, col = "red")

#Histogram of STA34 western FW
hist(Bootstrap_FWM_STA34W$t, xlab = "Statistic", main = "Bootstrap Distribution") 
box()
abline(v = Bootstrap_FWM_STA34W$t0, lty = 2, col = "red")

#distribution of outflow data. Not normal
ggplot(Outflow_TP_Load_Scenarios_wide,aes(`Outflow`,fill=Flowway))+geom_histogram()+facet_wrap(~Flowway)
  


