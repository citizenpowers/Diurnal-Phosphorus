

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

# Import data for RPA analysis -------------------------------------------------------------
#RPA tidy data
RPAs_Sorted <- read_csv("Data/RPAs Sorted.csv")

#Import Flow Data
Combined_BK_Flow <- read_csv("Data/Combined_BK_Flow.csv", col_types = cols(Flow = col_number(),HLR = col_number()))

#Import RPA Flow and Stage and weather data
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv")



# G334 continouos TP Load scenarios----------------------------------------------------

G334_TP_Load_Scenarios <- Combined_BK_Flow  %>%
filter(Date > "2016-02-07", Station=="G334") %>%   #filter to days with continous RPA data at station G334 
left_join(select(RPAs_Sorted,2:10) , by= c("Station","Date", "Hour")) %>%     #join RPA data
mutate(`Date_Time`=ymd_hms(ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York")),`TP interpolated`=TPO4) %>%   #create hourly date time index
mutate(`TP interpolated`=na.approx(`TP interpolated`,along=index(`Date_Time`),na.rm=FALSE))  %>%                
mutate(`Hourly TP LOAD`=if_else(is.finite(Flow),`TP interpolated`/1000*Flow*3600*28.31/1000000,0)) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
mutate(`Flow Lagged 12 hours`=if_else(is.finite(lag(Flow,12)),lag(Flow,12),0),`Hourly TP Load Lagged 12 hours`=`TP interpolated`/1000*`Flow Lagged 12 hours`*3600*28.31/1000000) %>%
filter(is.finite(`TP interpolated`))   %>%
group_by(Date) %>%
mutate(`Flow 50% Day`=mean(Flow,na.rm=TRUE)) %>%
mutate(`Flow 66% Day`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*1.33,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*.66)) %>%
mutate(`Flow 66% Night`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*.66,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*1.33)) %>%
mutate(`Flow 100% Night`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*0,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*2)) %>%
mutate(`Flow 100% Day`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*2,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*0)) %>%
mutate(`P Load`=if_else(is.finite(Flow),`TP interpolated`/1000*Flow*3600*28.31/1000000,0)) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
mutate(`Flow 50% Day Load`=`TP interpolated`/1000*`Flow 50% Day`*3600*28.31/1000000) %>%  
mutate(`Flow 66% Day Load`=`TP interpolated`/1000*`Flow 66% Day`*3600*28.31/1000000) %>% 
mutate(`Flow 66% Night Load`=`TP interpolated`/1000*`Flow 66% Night`*3600*28.31/1000000) %>%
mutate(`Flow 100% Night Load`=`TP interpolated`/1000*`Flow 100% Night`*3600*28.31/1000000) %>%
mutate(`Flow 100% Day Load`=`TP interpolated`/1000*`Flow 100% Day`*3600*28.31/1000000) %>%
ungroup() %>%  
mutate(`Cumulative P Load`=cumsum(`P Load`)) %>%
mutate(`Cumulative P Load 50% Day`=cumsum(`Flow 50% Day Load`)) %>%
mutate(`Cumulative P Load 66% Day`=cumsum(`Flow 66% Day Load`)) %>%
mutate(`Cumulative P Load 66% Night`=cumsum(`Flow 66% Night Load`)) %>%
mutate(`Cumulative P Load 100% Night`=cumsum(`Flow 100% Night Load`)) %>%
mutate(`Cumulative P Load 100% Day`=cumsum(`Flow 100% Day Load`))  %>%
pivot_longer(29:34,names_to = "Scenario", values_to = "Value")

G379_TP_Load_Scenarios <- Combined_BK_Flow  %>%
filter(Date > "2014-02-23", Station=="G379") %>%   #filter to days with continous RPA data at station G379 
left_join(select(RPAs_Sorted,2:10) , by= c("Station","Date", "Hour")) %>%     #join RPA data
mutate(`Date_Time`=ymd_hms(ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York")),`TP interpolated`=TPO4) %>%   #create hourly date time index
mutate(`TP interpolated`=na.approx(`TP interpolated`,along=index(`Date_Time`),na.rm=FALSE))  %>%                
mutate(`Hourly TP LOAD`=if_else(is.finite(Flow),`TP interpolated`/1000*Flow*3600*28.31/1000000,0)) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
mutate(`Flow Lagged 12 hours`=if_else(is.finite(lag(Flow,12)),lag(Flow,12),0),`Hourly TP Load Lagged 12 hours`=`TP interpolated`/1000*`Flow Lagged 12 hours`*3600*28.31/1000000) %>%
filter(is.finite(`TP interpolated`))   %>%
group_by(Date) %>%
mutate(`Flow 50% Day`=mean(Flow,na.rm=TRUE)) %>%
mutate(`Flow 66% Day`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*1.33,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*.66)) %>%
mutate(`Flow 66% Night`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*.66,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*1.33)) %>%
mutate(`Flow 100% Night`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*0,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*2)) %>%
mutate(`Flow 100% Day`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*2,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*0)) %>%
mutate(`P Load`=if_else(is.finite(Flow),`TP interpolated`/1000*Flow*3600*28.31/1000000,0)) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
mutate(`Flow 50% Day Load`=`TP interpolated`/1000*`Flow 50% Day`*3600*28.31/1000000) %>%
mutate(`Flow 66% Day Load`=`TP interpolated`/1000*`Flow 66% Day`*3600*28.31/1000000) %>% 
mutate(`Flow 66% Night Load`=`TP interpolated`/1000*`Flow 66% Night`*3600*28.31/1000000) %>%
mutate(`Flow 100% Night Load`=`TP interpolated`/1000*`Flow 100% Night`*3600*28.31/1000000) %>%
mutate(`Flow 100% Day Load`=`TP interpolated`/1000*`Flow 100% Day`*3600*28.31/1000000) %>%
ungroup() %>%  
mutate(`Cumulative P Load`=cumsum(`P Load`)) %>%
mutate(`Cumulative P Load 50% Day`=cumsum(`Flow 50% Day Load`)) %>%
mutate(`Cumulative P Load 66% Day`=cumsum(`Flow 66% Day Load`)) %>%
mutate(`Cumulative P Load 66% Night`=cumsum(`Flow 66% Night Load`)) %>%
mutate(`Cumulative P Load 100% Night`=cumsum(`Flow 100% Night Load`)) %>%
mutate(`Cumulative P Load 100% Day`=cumsum(`Flow 100% Day Load`))  %>%
pivot_longer(29:34,names_to = "Scenario", values_to = "Value")

G381_TP_Load_Scenarios <- Combined_BK_Flow  %>%
filter(Date > "2014-02-23", Station=="G381") %>%   #filter to days with continous RPA data at station G381 
left_join(select(RPAs_Sorted,2:10) , by= c("Station","Date", "Hour")) %>%     #join RPA data
mutate(`Date_Time`=ymd_hms(ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York")),`TP interpolated`=TPO4) %>%   #create hourly date time index
mutate(`TP interpolated`=na.approx(`TP interpolated`,along=index(`Date_Time`),na.rm=FALSE))  %>%                
mutate(`Hourly TP LOAD`=if_else(is.finite(Flow),`TP interpolated`/1000*Flow*3600*28.31/1000000,0)) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
mutate(`Flow Lagged 12 hours`=if_else(is.finite(lag(Flow,12)),lag(Flow,12),0),`Hourly TP Load Lagged 12 hours`=`TP interpolated`/1000*`Flow Lagged 12 hours`*3600*28.31/1000000) %>%
filter(is.finite(`TP interpolated`))   %>%
group_by(Date) %>%
mutate(`Flow 50% Day`=mean(Flow,na.rm=TRUE)) %>%
mutate(`Flow 66% Day`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*1.33,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*.66)) %>%
mutate(`Flow 66% Night`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*.66,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*1.33)) %>%
mutate(`Flow 100% Night`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*0,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*2)) %>%
mutate(`Flow 100% Day`=case_when(between(Hour,8,20)~mean(Flow,na.rm=TRUE)*2,!between(Hour,8,20)~mean(Flow,na.rm=TRUE)*0)) %>%
mutate(`P Load`=if_else(is.finite(Flow),`TP interpolated`/1000*Flow*3600*28.31/1000000,0)) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
mutate(`Flow 50% Day Load`=`TP interpolated`/1000*`Flow 50% Day`*3600*28.31/1000000) %>%
mutate(`Flow 66% Day Load`=`TP interpolated`/1000*`Flow 66% Day`*3600*28.31/1000000) %>% 
mutate(`Flow 66% Night Load`=`TP interpolated`/1000*`Flow 66% Night`*3600*28.31/1000000) %>%
mutate(`Flow 100% Night Load`=`TP interpolated`/1000*`Flow 100% Night`*3600*28.31/1000000) %>%
mutate(`Flow 100% Day Load`=`TP interpolated`/1000*`Flow 100% Day`*3600*28.31/1000000) %>%
ungroup() %>%  
mutate(`Cumulative P Load`=cumsum(`P Load`)) %>%
mutate(`Cumulative P Load 50% Day`=cumsum(`Flow 50% Day Load`)) %>%
mutate(`Cumulative P Load 66% Day`=cumsum(`Flow 66% Day Load`)) %>%
mutate(`Cumulative P Load 66% Night`=cumsum(`Flow 66% Night Load`)) %>%
mutate(`Cumulative P Load 100% Night`=cumsum(`Flow 100% Night Load`)) %>%
mutate(`Cumulative P Load 100% Day`=cumsum(`Flow 100% Day Load`))  %>%
pivot_longer(29:34,names_to = "Scenario", values_to = "Value")



# Figures -----------------------------------------------------------------
ggplot(G381_TP_Load_Scenarios,aes(Date_Time,`Value`,color=Scenario,linetype=Scenario))+geom_line(size=.75,alpha=.8)+theme_bw()+
scale_y_continuous(breaks=pretty_breaks(n=10),label=comma)+scale_x_datetime(breaks = "3 month", date_labels = "%b %y")+scale_color_viridis( discrete = TRUE,option="D")+
labs(title = "G381 P Load Scenarios",y="Total Phosphorus (kg)",x="Date",caption = "")+theme(legend.position = "bottom")

ggsave("Figures/G381 P Load Scenarios.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


ggplot(G334_TP_Load_Scenarios,aes(Date_Time,`Value`,color=Scenario,linetype=Scenario))+geom_line(size=.75,alpha=.8)+theme_bw()+
scale_y_continuous(breaks=pretty_breaks(n=10),label=comma)+scale_x_datetime(breaks = "3 month", date_labels = "%b %y")+scale_color_viridis( discrete = TRUE,option="D")+
labs(title = "G334 P Load Scenarios",y="Total Phosphorus (kg)",x="Date",caption = "")+theme(legend.position = "bottom")

ggsave("Figures/G334 P Load Scenarios.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

ggplot(G379_TP_Load_Scenarios,aes(Date_Time,`Value`,color=Scenario,linetype=Scenario))+geom_line(size=.75,alpha=.8)+theme_bw()+
scale_y_continuous(breaks=pretty_breaks(n=10),label=comma)+scale_x_datetime(breaks = "3 month", date_labels = "%b %y")+scale_color_viridis( discrete = TRUE,option="D")+
labs(title = "G379 P Load Scenarios",y="Total Phosphorus (kg)",x="Date",caption = "")+theme(legend.position = "bottom")

ggsave("Figures/G379 P Load Scenarios.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


