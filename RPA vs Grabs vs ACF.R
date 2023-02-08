#goal of this script is to 1) compare compliance auto and grab samples to RPA data 2) compare daily FWMC grabs from days of  

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

#RPA tidy data 
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") %>%
rename(Wind="WIND BELLEGLADE",HLRin="Inflow HLR",HLRout="Outflow HLR",Flowpath="Flowpath Region",TEMP="Temp S7",Rain="Rain S7") %>% #models will not accept variables with blank spaces as input 
mutate(Station_ID=as.factor(Station_ID),Flowpath=as.factor(Flowpath)) %>%
mutate(Month=as.factor(month(Date, label=TRUE, abbr=TRUE))) %>%
mutate(Station=str_sub(Station_ID,1,4)) %>%   #Compliance samples not located at same station as RPA data. Renamed stations to match nearest station
mutate(Day=yday(Date)) %>% select(Station,Date,Year,Day,Time,Hour,TPO4) 

# Tidy data ---------------------------------------------------------------

#Tidy Compliance data
Compliance_data_tidy <- Compliance_data %>%
filter(Sample.Type.New=="SAMP")  %>%   # remove FCEBs and autosampler samples
mutate(Hour=hour(dmy_hm(Collection_Date))) %>%                # Add hour to DF
mutate(Date=as.Date(dmy_hm(Collection_Date))) %>%             # Format date
rename(Station_ID="Station.ID") %>%                           # Rename Station_ID to standard format
mutate(`Value`=Value*1000) %>%                                  # convert to ug/l  
mutate(Station=str_sub(Station_ID,1,4)) %>%     #Compliance samples not located at same station as RPA data. Renamed stations to match nearest station
select(Station,Date,Hour,Collection.Method,Value)  %>%
pivot_wider(names_from=Collection.Method,values_from=Value) %>% select(-ACT)

#Create DF to evaluate relationship between compliance dataset and RPA dataset
Compliance_vs_RPA_Data <- RPAs_with_Flow_Stage_Weather_Sonde %>%
full_join(Compliance_data_tidy,by=c("Station","Date","Hour")) %>%
rename(RPA="TPO4") 

Compliance_vs_RPA_Data_long <- Compliance_vs_RPA_Data %>%
pivot_longer(names_to="Sample Method",values_to="Value",7:9)

Summary_Compliance_vs_RPA_Data <-Compliance_vs_RPA_Data %>%
mutate(`All Samples Same Time`=ifelse(is.finite(G)+is.finite(ACF)+is.finite(RPA)==3,1,0))  %>%
group_by(Station) %>%
summarise(n(),ACF=sum(is.finite(ACF)),G=sum(is.finite(G)),RPA=sum(is.finite(RPA)),`All Samples Same Time`=sum(`All Samples Same Time`))

# Figures -----------------------------------------------------------------


#Grabs vs autos
ggplot(filter(Compliance_vs_RPA_Data,Station %in% c("G334","G379","G381"))  ,aes(ACF,G))+geom_point()+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq()+coord_cartesian(xlim=c(0,60),ylim=c(0,60)) #scatterplot observed vs fit
#Grabs vs autos by station
ggplot(filter(Compliance_vs_RPA_Data,Station %in% c("G334","G379","G381"))  ,aes(ACF,G))+geom_point()+facet_wrap(~Station,scales="free")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq()+coord_cartesian(xlim=c(0,60),ylim=c(0,60))+#scatterplot observed vs fit
labs(title="Grab vs Autosampler Samples",y="Grabs (ug/L)",x="Autosampler composite flow (ug/L)")
#Grabs vs RPAs
ggplot(filter(Compliance_vs_RPA_Data,Station %in% c("G334","G379","G381"))  ,aes(RPA,G))+geom_point()+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq()+coord_cartesian(xlim=c(0,60),ylim=c(0,60)) #scatterplot observed vs fit
#Grabs vs RPAs by station
ggplot(filter(Compliance_vs_RPA_Data,Station %in% c("G334","G379","G381")) ,aes(RPA,G))+geom_point()+facet_wrap(~Station,scales="free")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq()+coord_cartesian(xlim=c(0,60),ylim=c(0,60))+ #scatterplot observed vs fit
labs(title="RPA vs Grab Samples",y="Grabs (ug/L)",x="Autosampler composite flow (ug/L)")
#Autos vs RPAs
ggplot(filter(Compliance_vs_RPA_Data,Station %in% c("G334","G379","G381"))  ,aes(RPA,ACF))+geom_point()+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq()+coord_cartesian(xlim=c(0,60),ylim=c(0,60)) #scatterplot observed vs fit
#Autos vs RPAs by station
ggplot(filter(Compliance_vs_RPA_Data,Station %in% c("G334","G379","G381"))  ,aes(RPA,ACF))+geom_point()+facet_wrap(~Station,scales="free")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq()+coord_cartesian(xlim=c(0,60),ylim=c(0,60)) #scatterplot observed vs fit


#box and whisker
ggplot(Compliance_vs_RPA_Data_long  ,aes(Value,color=`Sample Method`))+geom_boxplot()+facet_wrap(~Station,scales="free")+theme_bw()




