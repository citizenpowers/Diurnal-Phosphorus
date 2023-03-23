#Objective of this script is to see if daily mean TP from grabs is higher than daily mean TP from RPA samples 

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
library(zoo)
library(ggeffects)
library(stats)
library(dbhydroR)
library(stringr)


# Import Data -------------------------------------------------------------
#flow and compliance data
Compliance_data <- get_wq(station_id= c("G334","G381B","G379D","G377D","G378B","G380B","G384B","G333C"), date_min = "2012-07-01",date_max="2017-10-01",test_name ="PHOSPHATE, TOTAL AS P",raw=TRUE) #DBHYDRO WQ at compliance site
Combined_BK_Flow <-  read.csv( "Data/Combined_BK_Flow.csv") #Import flow data

#RPA tidy data 
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") %>%
rename(Wind="WIND BELLEGLADE",HLRin="Inflow HLR",HLRout="Outflow HLR",Flowpath="Flowpath Region",TEMP="Temp S7",Rain="Rain S7") %>% #models will not accept variables with blank spaces as input 
mutate(Station_ID=as.factor(Station_ID),Flowpath=as.factor(Flowpath)) %>%
mutate(Month=as.factor(month(Date, label=TRUE, abbr=TRUE))) %>%
mutate(Station=str_sub(Station_ID,1,4)) %>%   #Compliance samples not located at same station as RPA data. Renamed stations to match nearest station
mutate(Day=yday(Date)) %>% select(`Flowpath`,Flowway,Station,Date,Year,Day,Time,Hour,HLRout,HLRin,TPO4) 

# daily mean tp vs daily mean RPA data ---------------------------------------------------------------

#Tidy Compliance data
Compliance_data_tidy <- Compliance_data %>%
filter(Sample.Type.New=="SAMP")  %>%   # remove FCEBs and autosampler samples
mutate(Hour=hour(dmy_hm(Collection_Date))) %>%                # Add hour to DF
mutate(Date=as.Date(dmy_hm(Collection_Date))) %>%             # Format date
rename(Station_ID="Station.ID") %>%                           # Rename Station_ID to standard format
mutate(`Value`=Value*1000) %>%                                  # convert to ug/l  
mutate(Station=str_sub(Station_ID,1,4)) %>%     #Compliance samples not located at same station as RPA data. Renamed stations to match nearest station
select(Station,Date,Hour,Collection.Method,Value)  %>%
mutate(`Flowway` = case_when(`Station`=="G334"~"STA-2 Central",`Station`=="G379"~"STA-3/4 Central",`Station`=="G377"~"STA-3/4 Central",`Station`=="G381"~"STA-3/4 Western",`Station`=="G380"~"STA-3/4 Western",`Station`=="G333"~"STA-2 Central")) %>%        #Add flowway info to RPA data
pivot_wider(names_from=Collection.Method,values_from=Value) %>% select(-ACT) 

#figure time of day grab samples were collected
ggplot(Compliance_data_tidy,aes(Hour))+geom_histogram()


#Create DF to evaluate relationship between compliance dataset and RPA dataset
Compliance_vs_RPA_Data <- RPAs_with_Flow_Stage_Weather_Sonde %>%
full_join(Compliance_data_tidy,by=c("Flowway","Station","Date","Hour")) %>%
rename(RPA="TPO4")

#create summary of grab samples collected from days RPA samples were collected
Day_with_grab_and_RPA <- RPAs_with_Flow_Stage_Weather_Sonde %>%
full_join(Compliance_data_tidy,by=c("Flowway","Station","Date")) %>% 
select(Station,Date,G,TPO4) %>%
filter(!is.na(G)) %>%
group_by(Station,Date)  %>%
summarise(n=n())  %>%
group_by(Station)  %>%
summarise(n=n())

 #create DF of samples collected in the same hour 
Summary_Compliance_vs_RPA_Data <-Compliance_vs_RPA_Data %>%
mutate(`All Samples Same Time`=ifelse(is.finite(G)+is.finite(ACF)+is.finite(RPA)==3,1,0))  %>%
mutate(`RPA and Grab Same Time`=ifelse(is.finite(G)+is.finite(RPA)==2,1,0)) %>%
group_by(Station) %>%
summarise(n(),ACF=sum(is.finite(ACF)),G=sum(is.finite(G)),RPA=sum(is.finite(RPA)),`All Samples Same Time`=sum(`All Samples Same Time`),`RPA and Grab Same Time`=sum(`RPA and Grab Same Time`))

Daily_mean_TP_G_vs_RPA <- Compliance_vs_RPA_Data %>%
group_by(Station,Date) %>%
summarise(n(),`Daily Mean Grab`=mean(G,na.rm=TRUE),`Daily Mean RPA`=mean(RPA,na.rm=TRUE),`Daily Median Grab`=median(G,na.rm=TRUE),`Daily Median RPA`=median(RPA,na.rm=TRUE))  %>%
drop_na() %>%
mutate(`Mean Grab-RPA`=`Daily Mean Grab`-`Daily Mean RPA`,`Median Grab-RPA`=`Daily Median Grab`-`Daily Median RPA`)

Summary_daily_means_by_station <- Daily_mean_TP_G_vs_RPA %>%
group_by(Station) %>%
summarise(`Mean Grab-RPA`=mean(`Mean Grab-RPA`,na.rm=TRUE),`Median Grab-RPA`=mean(`Median Grab-RPA`,na.rm=TRUE))  



# Calculate days of stable flows --------------------------------------------------

Daily_mean_flow <- Combined_BK_Flow %>%
mutate(Date=as.Date(Date))%>%  
group_by(`Flowway`,Date) %>%
summarise(`Daily_mean_outflow`=mean(Outflow.HLR,na.rm=TRUE),`Daily_mean_inflow`=mean(Inflow.HLR,na.rm=TRUE)) 

diff_daily_mean <- Combined_BK_Flow %>%
mutate(Date=as.Date(Date))%>%    
left_join(`Daily_mean_flow`,by=c("Flowway","Date")) %>%
mutate(`Diff from daily mean outflow`=if_else(is.finite((Outflow.HLR-`Daily_mean_outflow`)/`Daily_mean_outflow`),(Outflow.HLR-`Daily_mean_outflow`)/`Daily_mean_outflow`,0))%>%
mutate(`Diff from daily mean inflow`=if_else(is.finite((Inflow.HLR-`Daily_mean_inflow`)/`Daily_mean_inflow`),(Inflow.HLR-`Daily_mean_inflow`)/`Daily_mean_inflow`,0))

Max_daily_flow <- diff_daily_mean %>%
group_by(Date,Flowway) %>%
summarise(`Max daily percentage inflow`=max(abs(`Diff from daily mean inflow`)),`Max daily percentage outflow`=max(abs(`Diff from daily mean outflow`)))

ggplot(Max_daily_flow ,aes(`Max daily percentage inflow`,fill=Flowway))+geom_histogram()+theme_bw()+facet_wrap(~Flowway,nrow=3)
  
Days_of_stable_outflow <- Max_daily_flow %>%
filter(`Max daily percentage outflow`<0.1)

Days_of_stable_inflow <- Max_daily_flow %>%
filter(`Max daily percentage inflow`<0.1)

distinct(Days_of_stable_outflow,Flowway)

# Join stable flow data with RPA data -------------------------------------

Days_of_stable_outflow_with_rpa_data <- RPAs_with_Flow_Stage_Weather_Sonde %>%
inner_join(Days_of_stable_outflow, by=c("Flowway","Date"))  %>%
inner_join(Compliance_data_tidy,by=c("Station","Date"))

Days_of_stable_inflow_with_rpa_data <- RPAs_with_Flow_Stage_Weather_Sonde %>%
inner_join(Days_of_stable_inflow, by=c("Flowway","Date"))  %>%
inner_join(Compliance_data_tidy,by=c("Station","Date"))

Summary_number_of_days_of_stable_outflow_with_grab_and_RPA <- Days_of_stable_outflow_with_rpa_data %>%
distinct(Date,Station) %>%
count(Station)

Summary_number_of_days_of_stable_inflow_with_grab_and_RPA <- Days_of_stable_inflow_with_rpa_data %>%
distinct(Date,Station) %>%
count(Station)  


# Calculate daily mean FWMC TP  -------------------------------------------


Stable_outflow_with_rpa_and_grab_data <-Combined_BK_Flow  %>%
mutate(Date=as.Date(Date))%>%  
left_join(filter(RPAs_with_Flow_Stage_Weather_Sonde,Flowpath=="Outflow"),by=c("Flowway","Date","Hour")) %>%  #add RPA data
left_join( filter(Compliance_data_tidy, Station %in% c("G334","G379","G381") ),by=c("Flowway","Date","Hour")) %>%
inner_join(distinct(Days_of_stable_outflow,Flowway),by=c("Flowway","Date")) #select days of stable flow  

Stable_inflow_with_rpa_and_grab_data <-Combined_BK_Flow  %>%
mutate(Date=as.Date(Date))%>%  
left_join(filter(RPAs_with_Flow_Stage_Weather_Sonde,Flowpath=="Inflow"),by=c("Flowway","Date","Hour")) %>%  #add RPA data
left_join( filter(Compliance_data_tidy, Station %in% c("G333","G377","G380") ),by=c("Flowway","Date","Hour")) %>%
inner_join(distinct(Days_of_stable_inflow,Flowway),by=c("Flowway","Date")) #select days of stable flow  


Summary_mean_tp_stable_flow <- Stable_outflow_with_rpa_and_grab_data %>%
group_by(Flowway,Date) %>%
summarise(n(),`Grab mean TP`=mean(G,na.rm=TRUE),`RPA mean TP`=mean(TPO4,na.rm=TRUE)) %>%
drop_na() %>%
group_by(Flowway)%>%
summarise(n(),`Mean grab TP`=mean(`Grab mean TP`) ,`Mean RPA TP`=mean(`RPA mean TP`),) 
  
Daily_mean_grab_tp <- Stable_outflow_with_rpa_and_grab_data %>%
group_by(`Flowway`,Date) %>%
summarise(`Daily_mean_TP`=mean(G,na.rm=TRUE))

Daily_mean_grab_tp_in <- Stable_inflow_with_rpa_and_grab_data %>%
group_by(`Flowway`,Date) %>%
summarise(`Daily_mean_TP`=mean(G,na.rm=TRUE))


Stable_outflow_FWMC_TP  <- Stable_outflow_with_rpa_and_grab_data %>%
left_join(Daily_mean_flow, by=c("Flowway","Date"))  %>%
left_join(Daily_mean_grab_tp, by=c("Flowway","Date"))  %>%  
filter(`Daily_mean_outflow`>0) %>%
group_by(Flowway) %>%  
arrange(Date,Hour) %>%
group_by(Flowway,Date) %>%   
mutate(`RPA interpolated`=na.locf(`TPO4`, na.rm=FALSE),`G interpolated`=na.locf(`G`, na.rm=FALSE))  %>%
arrange(Flowway,Date,Hour) %>%
mutate( `RPA interpolated`=if_else(is.na(`RPA interpolated`),na.locf(`TPO4`,fromLast=TRUE, na.rm=FALSE),`RPA interpolated`),`G interpolated`=if_else(is.na(`G interpolated`),na.locf(`G`,fromLast=TRUE, na.rm=FALSE),`G interpolated`)) %>%
mutate(`Hourly RPA Outflow TP (kg)`=`Outflow.HLR`*`RPA interpolated`,`Hourly Grab Outflow TP (kg)`=`Outflow.HLR`*`G interpolated`) %>%
group_by(Flowway,Date) %>%
summarise(`Daily RPA Load (kg)`=sum(`Hourly RPA Outflow TP (kg)`),`Daily Grab Load (kg)`=sum(`Hourly Grab Outflow TP (kg)`)) %>%
drop_na() %>%
mutate(`percent differce Grab- RPA`=(`Daily Grab Load (kg)`-`Daily RPA Load (kg)`)/`Daily Grab Load (kg)`*100) %>%
summarise(`Mean Percent difference (Grab-RPA)`=mean(`percent differce Grab- RPA`),n=n(),SD=sd(`percent differce Grab- RPA`),SE=SD/n^.5)
  

Stable_inflow_FWMC_TP  <- Stable_inflow_with_rpa_and_grab_data %>%
left_join(Daily_mean_flow, by=c("Flowway","Date"))  %>%  
left_join(Daily_mean_grab_tp_in, by=c("Flowway","Date"))  %>%  
filter(`Daily_mean_inflow`>0) %>%
group_by(Flowway) %>%  
arrange(Date,Hour) %>%
group_by(Flowway,Date) %>%   
mutate(`RPA interpolated`=na.locf(`TPO4`, na.rm=FALSE),`G interpolated`=na.locf(`G`, na.rm=FALSE))  %>%
arrange(Flowway,Date,Hour) %>%
mutate( `RPA interpolated`=if_else(is.na(`RPA interpolated`),na.locf(`TPO4`,fromLast=TRUE, na.rm=FALSE),`RPA interpolated`),`G interpolated`=if_else(is.na(`G interpolated`),na.locf(`G`,fromLast=TRUE, na.rm=FALSE),`G interpolated`)) %>%
mutate(`Hourly RPA Inflow TP (kg)`=`Inflow.HLR`*`RPA interpolated`,`Hourly Grab Inflow TP (kg)`=`Inflow.HLR`*`G interpolated`) %>%
group_by(Flowway,Date) %>%
summarise(`Daily RPA Load (kg)`=sum(`Hourly RPA Inflow TP (kg)`),`Daily Grab Load (kg)`=sum(`Hourly Grab Inflow TP (kg)`)) %>%
drop_na() %>%
mutate(`percent differce Grab- RPA`=(`Daily Grab Load (kg)`-`Daily RPA Load (kg)`)/`Daily Grab Load (kg)`*100) %>%
summarise(`Mean Percent difference (Grab-RPA)`=mean(`percent differce Grab- RPA`),n=n(),SD=sd(`percent differce Grab- RPA`),SE=SD/n^.5)



