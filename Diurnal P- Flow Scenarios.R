

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

# Import data for RPA analysis -------------------------------------------------------------
#RPA raw data
RPAs <-  read_excel("Data/Outflows.xlsx", col_types = c("text", "date", "numeric",  "numeric")) 

#RPA tidy data
RPAs_Sorted <- read_csv("Data/RPAs Sorted.csv")

#Import Flow Data
Combined_BK_Flow <- read_csv("Data/Combined_BK_Flow.csv", col_types = cols(Flow = col_number(),HLR = col_number()))

#RPA and flow DF. 
RPAs_with_Flow <- read_csv("Data/RPA and Flow.csv") %>%
mutate(`Flow Category`=factor(`Flow Category`,levels = c("Reverse Flow", "0-1 (cfs)", "1-100 (cfs)","100-250 (cfs)","250-500 (cfs)","500-1000 (cfs)","1000+ (cfs)"))) %>%
mutate(`Month`=factor(`Month`,levels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))

#RPA Flow and Stage Data
RPAs_with_Flow_Stage  <- read_csv("Data/RPA and Flow and Stage.csv")

#RPA Flow and Stage and weather data
RPAs_with_Flow_Stage_Weather <- read_csv("Data/RPA and Flow Stage Weather.csv")

#Import RPA Flow and Stage and weather data
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv")



# G334 TP Load scenarios----------------------------------------------------

G334_TP_Load_Scenarios <- Combined_BK_Flow  %>%
filter(Date > "2016-02-07", Station=="G334") %>%   #filter to days with continous RPA data at station G334 
left_join(select(RPAs_Sorted,2:10) , by= c("Station","Date", "Hour")) %>%     #join RPA data
mutate(`Date_Time`=ymd_hms(ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York")),`TP interpolated`=TPO4) %>%   #create hourly date time index
mutate(`TP interpolated`=na.approx(`TP interpolated`,along=index(`Date_Time`),na.rm=FALSE))  %>%                
mutate(`Hourly TP LOAD`=`TP interpolated`/1000*Flow*3600*28.31/1000000) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
mutate(`Flow Lagged 12 hours`=lag(Flow,12),`Hourly TP Load Lagged 12 hours`=`TP interpolated`/1000*`Flow Lagged 12 hours`*3600*28.31/1000000) %>%
filter(is.finite(`TP interpolated`))   %>%
mutate(`100% day Flow`=case_when(between(Hour,8,20)~152.33*2, !between(Hour,8,20)~0)) %>%
mutate(`100% night Flow`=case_when(!between(Hour,8,20)~152.33*2, between(Hour,8,20)~0)) %>%
mutate(`66% day Flow`=case_when(between(Hour,8,20)~152.33*1.33, !between(Hour,8,20)~152.33*.66)) %>%
mutate(`66% night Flow`=case_when(!between(Hour,8,20)~152.33*1.33, between(Hour,8,20)~152.33*.66)) %>%  
mutate(`Day TP LOAD`=`TP interpolated`/1000*`100% day Flow`*3600*28.31/1000000,`Night TP LOAD`=`TP interpolated`/1000*`100% night Flow`*3600*28.31/1000000) %>% 
mutate(`66% Day TP LOAD`=`TP interpolated`/1000*`66% day Flow`*3600*28.31/1000000,`66% Night TP LOAD`=`TP interpolated`/1000*`66% night Flow`*3600*28.31/1000000) %>% 
mutate(`Cumulative TP Load 100% Day Flow`=cumsum(`Day TP LOAD`),`Cumulative TP Load 100% Night Flow`=cumsum(`Night TP LOAD`)) %>%
mutate(`Cumulative TP Load 66% Day Flow`=cumsum(`66% Day TP LOAD`),`Cumulative TP Load 66% Night Flow`=cumsum(`66% Night TP LOAD`)) %>%
mutate(`Cumulative TP Load`=cumsum(`Hourly TP LOAD`),`Cumulative TP Load Lagged 12 Hours`=cumsum(`Hourly TP Load Lagged 12 hours`)) %>%
pivot_longer(26:31,names_to = "Scenario", values_to = "Value")

G334_summary <-G334_TP_Load_Scenarios%>%
summarise(`Total flow`=sum(Flow,na.rm=TRUE),n=n(), `mean cfs`=mean(Flow,na.rm=TRUE))  

G379_TP_Load_Scenarios <- Combined_BK_Flow  %>%
filter(Date > "2014-02-23", Station=="G379") %>%   #filter to days with continous RPA data at station G334 
left_join(select(RPAs_Sorted,2:10) , by= c("Station","Date", "Hour")) %>%     #join RPA data
mutate(`Date_Time`=ymd_hms(ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York")),`TP interpolated`=TPO4) %>%   #create hourly date time index
mutate(`TP interpolated`=na.approx(`TP interpolated`,along=index(`Date_Time`),na.rm=FALSE))  %>%                
mutate(`Hourly TP LOAD`=if_else(is.finite(Flow),`TP interpolated`/1000*Flow*3600*28.31/1000000,0)) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
mutate(`Flow Lagged 12 hours`=if_else(is.finite(lag(Flow,12)),lag(Flow,12),0),`Hourly TP Load Lagged 12 hours`=`TP interpolated`/1000*`Flow Lagged 12 hours`*3600*28.31/1000000) %>%
filter(is.finite(`TP interpolated`))   %>%
mutate(`100% day Flow`=case_when(between(Hour,8,20)~116.63*2, !between(Hour,8,20)~0)) %>%
mutate(`100% night Flow`=case_when(!between(Hour,8,20)~116.63*2, between(Hour,8,20)~0)) %>%
mutate(`66% day Flow`=case_when(between(Hour,8,20)~116.63*1.33, !between(Hour,8,20)~116.63*.66)) %>%
mutate(`66% night Flow`=case_when(!between(Hour,8,20)~116.63*1.33, between(Hour,8,20)~116.63*.66)) %>%  
mutate(`Day TP LOAD`=`TP interpolated`/1000*`100% day Flow`*3600*28.31/1000000,`Night TP LOAD`=`TP interpolated`/1000*`100% night Flow`*3600*28.31/1000000) %>% 
mutate(`66% Day TP LOAD`=`TP interpolated`/1000*`66% day Flow`*3600*28.31/1000000,`66% Night TP LOAD`=`TP interpolated`/1000*`66% night Flow`*3600*28.31/1000000) %>% 
mutate(`Cumulative TP Load 100% Day Flow`=cumsum(`Day TP LOAD`),`Cumulative TP Load 100% Night Flow`=cumsum(`Night TP LOAD`)) %>%
mutate(`Cumulative TP Load 66% Day Flow`=cumsum(`66% Day TP LOAD`),`Cumulative TP Load 66% Night Flow`=cumsum(`66% Night TP LOAD`)) %>%
mutate(`Cumulative TP Load`=cumsum(`Hourly TP LOAD`),`Cumulative TP Load Lagged 12 Hours`=cumsum(`Hourly TP Load Lagged 12 hours`)) %>%
pivot_longer(26:31,names_to = "Scenario", values_to = "Value")

G379_summary <-G379_TP_Load_Scenarios%>%
summarise(`Total flow`=sum(Flow,na.rm=TRUE),n=n(), `mean cfs`=mean(Flow,na.rm=TRUE))  

G381_TP_Load_Scenarios <- Combined_BK_Flow  %>%
filter(Date > "2014-02-23", Station=="G381") %>%   #filter to days with continous RPA data at station G334 
left_join(select(RPAs_Sorted,2:10) , by= c("Station","Date", "Hour")) %>%     #join RPA data
mutate(`Date_Time`=ymd_hms(ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York")),`TP interpolated`=TPO4) %>%   #create hourly date time index
mutate(`TP interpolated`=na.approx(`TP interpolated`,along=index(`Date_Time`),na.rm=FALSE))  %>%                
mutate(`Hourly TP LOAD`=if_else(is.finite(Flow),`TP interpolated`/1000*Flow*3600*28.31/1000000,0)) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
mutate(`Flow Lagged 12 hours`=if_else(is.finite(lag(Flow,12)),lag(Flow,12),0),`Hourly TP Load Lagged 12 hours`=`TP interpolated`/1000*`Flow Lagged 12 hours`*3600*28.31/1000000) %>%
filter(is.finite(`TP interpolated`))   %>%
mutate(`100% day Flow`=case_when(between(Hour,8,20)~203.12*2, !between(Hour,8,20)~0)) %>%
mutate(`100% night Flow`=case_when(!between(Hour,8,20)~203.12*2, between(Hour,8,20)~0)) %>%
mutate(`66% day Flow`=case_when(between(Hour,8,20)~203.12*1.33, !between(Hour,8,20)~203.12*.66)) %>%
mutate(`66% night Flow`=case_when(!between(Hour,8,20)~203.12*1.33, between(Hour,8,20)~203.12*.66)) %>%  
mutate(`Day TP LOAD`=`TP interpolated`/1000*`100% day Flow`*3600*28.31/1000000,`Night TP LOAD`=`TP interpolated`/1000*`100% night Flow`*3600*28.31/1000000) %>% 
mutate(`66% Day TP LOAD`=`TP interpolated`/1000*`66% day Flow`*3600*28.31/1000000,`66% Night TP LOAD`=`TP interpolated`/1000*`66% night Flow`*3600*28.31/1000000) %>% 
mutate(`Cumulative TP Load 100% Day Flow`=cumsum(`Day TP LOAD`),`Cumulative TP Load 100% Night Flow`=cumsum(`Night TP LOAD`)) %>%
mutate(`Cumulative TP Load 66% Day Flow`=cumsum(`66% Day TP LOAD`),`Cumulative TP Load 66% Night Flow`=cumsum(`66% Night TP LOAD`)) %>%
mutate(`Cumulative TP Load`=cumsum(`Hourly TP LOAD`),`Cumulative TP Load Lagged 12 Hours`=cumsum(`Hourly TP Load Lagged 12 hours`)) %>%
pivot_longer(26:31,names_to = "Scenario", values_to = "Value")

G381_summary <-G381_TP_Load_Scenarios %>%
summarise(`Total flow`=sum(Flow,na.rm=TRUE),n=n(), `mean cfs`=mean(Flow,na.rm=TRUE))  

ggplot(G334_TP_Load_Scenarios,aes(`Date_Time`,`Value`,color=Scenario))+geom_line()+theme_bw()+
scale_y_continuous(breaks=pretty_breaks(n=10),label=comma)+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title = "P Load Under Different Flow Scenarios at G334",y="Total Phosphorus (kg)",x="Date",caption = "")+theme(legend.position = "bottom")

ggplot(G379_TP_Load_Scenarios,aes(`Date_Time`,`Value`,color=Scenario))+geom_line()+theme_bw()+
scale_y_continuous(breaks=pretty_breaks(n=10),label=comma)+
labs(title = "P Load Under Different Flow Scenarios at G379",y="Total Phosphorus (kg)",x="Date",caption = "")+theme(legend.position = "bottom")

ggplot(G381_TP_Load_Scenarios,aes(`Date_Time`,`Value`,color=Scenario))+geom_point(size=1.5)+theme_bw()+
scale_y_continuous(breaks=pretty_breaks(n=10),label=comma)+
labs(title = "P Load Under Different Flow Scenarios at G381",y="Total Phosphorus (kg)",x="Date",caption = "")+theme(legend.position = "bottom")




  
