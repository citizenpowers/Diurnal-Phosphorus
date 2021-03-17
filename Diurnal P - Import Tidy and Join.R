rm(list=ls())


library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(maptools)


#Steps (Only Run if data need update. Skip to create fig script otherwise) 
# Step 1: Import and Tidy Flow  Data from CSV --------

#G381 Stations - STA-3/4 western flowway
G381_C_BK <- select(read_csv("Data/G381_C_BK part 1.csv"),date,STATION,VALUE) %>%
bind_rows(select(read_csv("Data/G381_C_BK part 2.csv"),date,STATION,VALUE)) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%
fill(`G381A-C-Q`,`G381B-C-Q`,`G381C-C-Q`,`G381D-C-Q`,`G381E-C-Q`,`G381F-C-Q`) %>%
mutate(G381=rowSums(.[2:7],na.rm=TRUE))          

#G379 Stations - STA-3/4 western flowway
G379_C_BK <- select(read_csv("Data/G379_C_BK part 1.csv"),date,STATION,VALUE) %>%
bind_rows(select(read_csv("Data/G379_C_BK part 2.csv"),date,STATION,VALUE)) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%
fill(`G379A-C-Q`,`G379B-C-Q`,`G379C-C-Q`,`G379D-C-Q`,`G379E-C-Q`) %>%
mutate(G379=rowSums(.[2:6],na.rm=TRUE))          

#G334
G334_S_BK <- select(read_csv("Data/G334_S_BK.csv"),date,STATION,VALUE) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
arrange(date) %>%
mutate(G334=VALUE) %>%
select(-STATION,-VALUE)

#G333 stations  -STA-2 central flowway
G333_C_BK <- read_csv("Data/G333_C_BK.csv") %>%
select(date,STATION,VALUE) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%
fill(`G333A-C-Q`,`G333B-C-Q`,`G333C-C-Q`,`G333D-C-Q`,`G333E-C-Q`) %>%
mutate(G333=rowSums(.[2:6],na.rm=TRUE))

#G380 Stations - STA-3/4 western flowway
G380_C_BK <- select(read_csv("Data/G380_C_BK part 1.csv"),date,STATION,VALUE) %>%
bind_rows(select(read_csv("Data/G380_C_BK part 2.csv"),date,STATION,VALUE)) %>%
bind_rows(select(read_csv("Data/G380_C_BK part 3.csv"),date,STATION,VALUE)) %>%  
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%
fill(`G380A-C-Q`,`G380B-C-Q`,`G380C-C-Q`,`G380D-C-Q`,`G380E-C-Q`,`G380F-C-Q`) %>%
mutate(G380=rowSums(.[2:7],na.rm=TRUE))          
         
#G377 Stations  - STA-3/4 central flowway
G377_C_BK <- select(read_csv("Data/G377_C_BK part 1.csv"),date,STATION,VALUE) %>%
bind_rows(select(read_csv("Data/G377_C_BK part 2.csv"),date,STATION,VALUE)) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%
fill(`G377A-C-Q`,`G377B-C-Q`,`G377C-C-Q`,`G377D-C-Q`,`G377E-C-Q`) %>%
mutate(G377=rowSums(.[2:6],na.rm=TRUE))          

#Combined Outflow over entire flowway
Combined_BK_Flow <-  setNames(as.data.frame(seq(from=ISOdate(2012,7,01,0,0,0,tz = "America/New_York"), to=ISOdate(2017,10,01,0,0,0,tz = "America/New_York"),by = "min")),"date") %>%
left_join(G381_C_BK,by="date") %>%  #combine data from G381, G334, G379D
left_join(G379_C_BK,by="date") %>%
left_join(G334_S_BK,by="date")  %>%
left_join(G333_C_BK,by="date") %>% 
left_join(G380_C_BK,by="date") %>%
left_join(G377_C_BK,by="date") %>%
select(date,G381,G379,G334,G333,G380,G377) %>%
arrange(date)  %>%
fill(G381,G379,G334,G333,G380,G377) %>% 
distinct() %>%
gather("Station","Flow",G381,G379,G334,G333,G380,G377) %>%
mutate(`Flowway` = case_when(`Station`=="G334"~"STA-2 Central",`Station`=="G379"~"STA-3/4 Central",`Station`=="G377"~"STA-3/4 Central",`Station`=="G381"~"STA-3/4 Western",`Station`=="G380"~"STA-3/4 Western",`Station`=="G333"~"STA-2 Central")) %>%        #Add flowway info to RPA data
mutate(`Flowpath Region` = case_when(`Station`=="G334"~"Outflow",`Station`=="G379"~"Outflow",`Station`=="G377"~"Inflow",`Station`=="G381"~"Outflow",`Station`=="G333"~"Inflow",`Station`=="G380"~"Inflow"))  %>%     #Add flowpath position
mutate(`Outflow` = case_when(`Flowway` == "STA-2 Central" & `Flowpath Region`=="Outflow" ~Flow,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Outflow"~Flow,`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Outflow"~Flow)) %>% 
mutate(`Outflow HLR` = case_when(`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Outflow"~Flow/2087,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Outflow"~Flow/2375,`Flowway` == "STA-2 Central" & `Flowpath Region`=="Outflow" ~Flow/2400)) %>%
mutate(`Inflow` = case_when(`Flowway` == "STA-2 Central"  & `Flowpath Region`=="Inflow" ~Flow,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Inflow"~Flow,`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Inflow"~Flow))  %>%
mutate(`Inflow HLR` = case_when(`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Inflow"~Flow/2087,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Inflow"~Flow/2375,`Flowway` == "STA-2 Central" & `Flowpath Region`=="Inflow" ~Flow/2400)) %>%
mutate(Date=as.Date(date)) %>%
mutate(Hour=hour(round_date(date, unit = "hour"))) %>%
group_by(`Flowway`,Date,Hour) %>%
summarise(Outflow=mean(Outflow,na.rm = TRUE),`Outflow HLR`=mean(`Outflow HLR`,na.rm=TRUE),Inflow=mean(Inflow,na.rm = TRUE),`Inflow HLR`=mean(`Inflow HLR`,na.rm=TRUE))

write.csv(Combined_BK_Flow, "Data/Combined_BK_Flow.csv",row.names=FALSE)

# Step 2: Import and Tidy RPA data  --------------------------------------

#RPA data from outflows 
RPAs_outflows <-  read_excel("Data/Outflows.xlsx", col_types = c("text", "date", "numeric",  "numeric")) 

#RPA data from inflows and midflows
RPAs_midflow <- read_csv("Data/G384C_TP.csv") %>%
bind_rows(read_csv("Data/G380C_TP.csv"))   %>%
bind_rows(read_excel("Data/G378C_Midflow_TP.xlsx")) %>%
bind_rows(read_excel("Data/G377C_Inflow_TP.xlsx",col_types = c("text", "date", "numeric","numeric"))) %>%  
bind_rows(select(mutate(read_csv("Data/G333C_Inflow_TP.csv",col_types = cols(BATTERY_VOLTAGE = col_datetime(format = "%m/%d/%Y %H:%M"))),COLLECT_DATE=mdy_hm(COLLECT_DATE)),SITE_NAME,COLLECT_DATE,TRP,TP)) %>%
select(SITE_NAME,COLLECT_DATE,TRP,TP)  %>%
rename(Station="SITE_NAME",Date="COLLECT_DATE",TPO4="TP")

#All RPA data untidied and uncensored except negative values removed and substituted with MDL
RPAs_Raw<- RPAs_outflows %>%
bind_rows(RPAs_midflow) %>%
mutate(TPO4=if_else(TPO4<1,1,TPO4),TRP=if_else(TRP<1,1,TRP))   #TPO4 and TRP values less than MDL replaced with the MDL

write.csv(RPAs_Raw,"Data/RPAs_Raw.csv",row.names=FALSE)

#Detect Outliers limits per station
Outliers <- RPAs_outflows %>%  
bind_rows(RPAs_midflow) %>%
mutate(`Station` = case_when(`Station`=="G379D"~ "G379",`Station`=="G381B" ~ "G381",`Station`=="G334" ~ "G334",`Station`=="G384C" ~ "G384",`Station`=="G380C" ~ "G380",`Station`=="G378C" ~ "G378",`Station`=="G377C" ~ "G377",`Station`=="G333C" ~ "G333")) %>%
group_by(Station) %>%
summarise(n=n(),`min TPO4`=min(TPO4,na.rm=TRUE),`max TPO4`=max(TPO4,na.rm=TRUE),mean=mean(TPO4,na.rm=TRUE),median=median(TPO4,na.rm=TRUE),`Q1`=quantile(TPO4,.25,na.rm = TRUE),`Q3`=quantile(TPO4,.75,na.rm = TRUE),IQR=Q3-Q1,`Mild Outliers`=Q3+1.5*IQR,`Extreme Outliers`=Q3+3*IQR,
`TPO4 Observations`=sum(is.finite(TPO4)),`NAs Observed`=sum(is.na(TPO4)),`Below detection`=sum(if_else(TPO4<1,1,0),na.rm=TRUE),`above outlier`=sum(if_else(TPO4>`Mild Outliers`,1,0),na.rm=TRUE),
`above extreme outlier`=sum(if_else(TPO4>`Extreme Outliers`,1,0),na.rm=TRUE),`% mild outliers`=percent(`above outlier`/n()),`% extreme mild outliers`=percent(`above extreme outlier`/n()))

write.csv(Outliers, "Data/Outliers.csv",row.names=FALSE)

#remove outliers   
RPAs_outliers_removed <- RPAs_Raw %>%
mutate(Month=month(Date,label=TRUE)) %>%
mutate(Day=day(Date)) %>%
mutate(Time=hour(Date)+ minute(Date)/60) %>%
mutate(Year=year(Date)) %>%
mutate(Hour=hour(Date)) %>%
mutate(Minute=minute(Date)) %>%  
mutate(Date=as.Date(Date)) %>%
mutate(`Station` = case_when(`Station`=="G379D"~ "G379",`Station`=="G381B" ~ "G381",`Station`=="G334" ~ "G334",`Station`=="G384C" ~ "G384",`Station`=="G380C" ~ "G380",`Station`=="G378C" ~ "G378",`Station`=="G377C" ~ "G377",`Station`=="G333C" ~ "G333")) %>%
mutate(`Flowway` = case_when(`Station`=="G334"~"STA-2 Central",`Station`=="G379"~"STA-3/4 Central",`Station`=="G381"~"STA-3/4 Western",`Station`=="G380"~"STA-3/4 Western",`Station`=="G384"~"STA-3/4 Western",`Station`=="G378" ~ "STA-3/4 Central",`Station`=="G377" ~ "STA-3/4 Central",`Station`=="G333" ~ "STA-2 Central")) %>%        #Add flowway info to RPA data
mutate(`Flowpath Region` = case_when(`Station`=="G334"~"Outflow",`Station`=="G379"~"Outflow",`Station`=="G381"~"Outflow",`Station`=="G380"~"Inflow",`Station`=="G384"~"Midflow",`Station`=="G380" ~ "Inflow",`Station`=="G378" ~ "Midflow",`Station`=="G377" ~ "Inflow",`Station`=="G333" ~ "Inflow"))   %>%     #Add flowpath position
mutate(TPO4=case_when(Station=="G333" & TPO4 >193.25 ~ 193.25,
                      Station=="G334" & TPO4 >35.28 ~ 35.28,
                      Station=="G377" & TPO4 >87.50 ~ 87.50,
                      Station=="G378" & TPO4 >24.00 ~ 24.00,
                      Station=="G379" & TPO4 >53.50 ~ 53.50,
                      Station=="G380" & TPO4 >120.50 ~ 120.50,
                      Station=="G381" & TPO4 >33.50 ~ 33.50,
                      Station=="G384" & TPO4 >39.00 ~ 39.00,
                      TRUE~as.numeric(as.character(TPO4))))     #Replace outliers with quantile(.75)+1.5*IQR

#Keep Outliers
RPAs_tidy <- RPAs_Raw %>%
mutate(Month=month(Date,label=TRUE),Day=day(Date),Time=hour(Date)+ minute(Date)/60,Year=year(Date),Hour=hour(Date),Minute=minute(Date),Date=as.Date(Date)) %>%
mutate(Station_ID=Station) %>%
mutate(`Station` = case_when(`Station`=="G379D"~ "G379",`Station`=="G381B" ~ "G381",`Station`=="G334" ~ "G334",`Station`=="G384C" ~ "G384",`Station`=="G380C" ~ "G380",`Station`=="G378C" ~ "G378",`Station`=="G377C" ~ "G377",`Station`=="G333C" ~ "G333")) %>%
mutate(`Flowway` = case_when(`Station`=="G334"~"STA-2 Central",`Station`=="G379"~"STA-3/4 Central",`Station`=="G381"~"STA-3/4 Western",`Station`=="G380"~"STA-3/4 Western",`Station`=="G384"~"STA-3/4 Western",`Station`=="G378" ~ "STA-3/4 Central",`Station`=="G377" ~ "STA-3/4 Central",`Station`=="G333" ~ "STA-2 Central")) %>%        #Add flowway info to RPA data
mutate(`Flowpath Region` = case_when(`Station`=="G334"~"Outflow",`Station`=="G379"~"Outflow",`Station`=="G381"~"Outflow",`Station`=="G380"~"Inflow",`Station`=="G384"~"Midflow",`Station`=="G380" ~ "Inflow",`Station`=="G378" ~ "Midflow",`Station`=="G377" ~ "Inflow",`Station`=="G333" ~ "Inflow"))      #Add flowpath position
  
RPAs_Sorted <- RPAs_tidy %>%
group_by(Station,Year,Day,Month) %>%
mutate(RANK=row_number(TPO4),`TRP Rank`=row_number(TRP))  %>%
mutate(PERCENT_RANK=cume_dist(TPO4)) %>% 
mutate(Scaled_Value=TPO4/max(TPO4)) %>%
mutate(`24_hour_mean`=mean(TPO4,na.rm=TRUE),`24_hour_median`=median(TPO4,na.rm=TRUE),`log mean`=mean(log10(TPO4),na.rm = TRUE),`Cube root mean`=mean((TPO4)^(1/3),na.rm = TRUE)) %>%
mutate(Diff_24_hour_mean=TPO4-`24_hour_mean`,Diff_24_hour_median=TPO4-`24_hour_median`,Diff_24_hour_log_trans=log(TPO4)-`log mean`,Diff_24_hour_cube_root=(TPO4^(1/3)-`Cube root mean`)^3) %>%
mutate(`Percent difference from daily mean`=(Diff_24_hour_mean/`24_hour_mean`)*100,`Percent difference from daily median`=(Diff_24_hour_median/`24_hour_median`)*100) 


write.csv(RPAs_Sorted, "Data/RPAs Sorted.csv",row.names=FALSE)

RPAs_Sorted_outliers_removed <- RPAs_outliers_removed %>%
group_by(Station,Year,Day,Month) %>%
mutate(RANK=row_number(TPO4),`TRP Rank`=row_number(TRP))  %>%
mutate(PERCENT_RANK=cume_dist(TPO4)) %>% 
mutate(Scaled_Value=TPO4/max(TPO4)) %>%
mutate(`24_hour_mean`=mean(TPO4,na.rm=TRUE),`TRP Daily Mean`=mean(TRP,na.rm = TRUE)) %>%
mutate(Diff_24_hour_mean=TPO4-`24_hour_mean`,`TRP Diff from daily mean`=TRP-`TRP Daily Mean`) %>%
mutate(`Percent difference from daily mean`=(Diff_24_hour_mean/`24_hour_mean`)*100) %>%
mutate(`24_hour_median`=median(TPO4,na.rm=TRUE)) %>%
mutate(Diff_24_hour_median=TPO4-`24_hour_median`) %>%
mutate(`Percent difference from daily median`=(Diff_24_hour_median/`24_hour_median`)*100) 

write.csv(RPAs_Sorted_outliers_removed, "Data/RPAs Sorted outliers removed.csv",row.names=FALSE)

# Step 2: Create categories for TP concentration --------------------------
RPAs_Sorted_concentration <- RPAs_Sorted %>%
mutate(`Concentration Range` = case_when(between(`24_hour_median`,0,10)~"0-10",
                                         between(`24_hour_median`,10,20)~"10-20",
                                         between(`24_hour_median`,20,30)~"20-30",
                                         between(`24_hour_median`,30,40)~"30-40",
                                         between(`24_hour_median`,40,50)~"40-50",
                                         between(`24_hour_median`,50,100)~"50-100",
                                         `24_hour_median`>100~"100+"))  %>%     #add concentration range
mutate(`Concentration Range` = factor(`Concentration Range`, levels = c("0-10", "10-20","20-30","30-40","40-50","50-100","100+")))
  
RPAs_Sorted_concentration %>% group_by(`Concentration Range`) %>% summarise(n=n())
  

write.csv(RPAs_Sorted_concentration, "Data/RPAs Sorted concentration.csv",row.names=FALSE)
# Step 4: Import and Tidy Stage from flowway inflows and ouflows---------------------------

STA2C3_Stage <- select(read_csv("Data/STA2C3_Stage.csv"),date,STATION,VALUE) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%  
fill(`G333C-T`,`G334-H`) 

STA34_central_flowway_Stage <- select(read_csv("Data/STA34 central flowway Stage.csv"),date,STATION,VALUE) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%  
fill(`G379D-H`,`G377D-T`) 

STA34_western_flowway_Stage <- select(read_csv("Data/STA34 western flowway Stage.csv"),date,STATION,VALUE) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%  
fill(`G381B-H`,`G380B-T`) 

Combined_Stage <- setNames(as.data.frame(seq(from=ISOdate(2012,7,01,0,0,0,tz = "America/New_York"), to=ISOdate(2017,10,01,0,0,0,tz = "America/New_York"),by = "min")),"date") %>%
full_join(STA2C3_Stage,by="date") %>%  
full_join(STA34_central_flowway_Stage ,by="date") %>%
full_join(STA34_western_flowway_Stage ,by="date") %>%  
arrange(date) %>%
fill(`G333C-T`,`G334-H`,`G379D-H`,`G377D-T`,`G381B-H`,`G380B-T`) %>%
mutate(Date=as.Date(date)) %>%
mutate(Hour=hour(round_date(date, unit = "hour"))) %>%
group_by(Date,Hour) %>%  
summarise(`G333C-T`=mean(`G333C-T`,na.rm = TRUE),`G334-H`=mean(`G334-H`,na.rm=TRUE),`G379D-H`=mean(`G379D-H`,na.rm = TRUE),`G377D-T`=mean(`G377D-T`,na.rm=TRUE),`G381B-H`=mean(`G381B-H`,na.rm = TRUE),`G380B-T`=mean(`G380B-T`,na.rm=TRUE)) %>% 
gather("Station","Stage",`G333C-T`,`G334-H`,`G379D-H`,`G377D-T`,`G381B-H`,`G380B-T`) %>% 
mutate(`Flowway` = case_when(`Station`=="G333C-T"~"STA-2 Central",`Station`=="G379D-H"~"STA-3/4 Central",`Station`=="G377D-T"~"STA-3/4 Central",`Station`=="G381B-H"~"STA-3/4 Western",`Station`=="G380B-T"~"STA-3/4 Western",`Station`=="G334-H"~"STA-2 Central")) %>%        #Add flowway info to RPA data
mutate(`Flowpath Region` = case_when(`Station` %in% c("G333C-T","G377D-T","G380B-T")~"Inflow", Station %in% c("G334-H","G381B-H","G379D-H")~"Outflow"))  %>%     #Add flowpath position
mutate(`Outflow Stage` = case_when(`Flowway` == "STA-2 Central" & `Flowpath Region`=="Outflow" ~Stage,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Outflow"~Stage,`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Outflow"~Stage)) %>% 
mutate(`Inflow Stage` = case_when(`Flowway` == "STA-2 Central"  & `Flowpath Region`=="Inflow" ~Stage,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Inflow"~Stage,`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Inflow"~Stage))  %>%
group_by(`Flowway`,Date,Hour) %>%
summarise(`Outflow Stage`=mean(`Outflow Stage`,na.rm = TRUE),`Inflow Stage`=mean(`Inflow Stage`,na.rm=TRUE)) %>%
group_by(`Flowway`,Date) %>%
mutate(`Max Daily Outflow Stage` = case_when(max(`Outflow Stage`,na.rm=TRUE)<10.5~ " < 10.5 Max Daily Stage ft",between(max(`Outflow Stage`,na.rm=TRUE),10.5,11)~ "10.5-11 Max Daily Stage ft",between(max(`Outflow Stage`,na.rm=TRUE),11,12)~ "11-12 Max Daily Stage ft",between(max(`Outflow Stage`,na.rm=TRUE),12,13)~ "12-13 Max Daily Stage ft",max(`Outflow Stage`,na.rm=TRUE)>13~ "13+ Max Daily Stage ft")) %>%
mutate(`Max Daily Outflow Stage` = factor(`Max Daily Outflow Stage`, levels = c(" < 10.5 Max Daily Stage ft", "10.5-11 Max Daily Stage ft", "11-12 Max Daily Stage ft","12-13 Max Daily Stage ft","13+ Max Daily Stage ft"))) %>%
mutate(`Max Daily Inflow Stage` = case_when(max(`Inflow Stage`,na.rm=TRUE)<10.5~ " < 10.5 Max Daily Stage ft",between(max(`Inflow Stage`,na.rm=TRUE),10.5,11)~ "10.5-11 Max Daily Stage ft",between(max(`Inflow Stage`,na.rm=TRUE),11,12)~ "11-12 Max Daily Stage ft",between(max(`Inflow Stage`,na.rm=TRUE),12,13)~ "12-13 Max Daily Stage ft",max(`Inflow Stage`,na.rm=TRUE)>13~ "13+ Max Daily Stage ft")) %>%
mutate(`Max Daily Inflow Stage` = factor(`Max Daily Inflow Stage`, levels = c(" < 10.5 Max Daily Stage ft", "10.5-11 Max Daily Stage ft", "11-12 Max Daily Stage ft","12-13 Max Daily Stage ft","13+ Max Daily Stage ft")))

write.csv(Combined_Stage, "Data/Stage All Flowways.csv",row.names=FALSE)
  
# Step 5: Import and Tidy Sonde Data ----------------------------------------------
SONDE_DATA <- read_csv("Data/SONDE_DATA.csv") 

Sonde_Tidy <- SONDE_DATA  %>%
mutate(date=mdy_hm(`Date/Time`),Date=as.Date(date),Hour=hour(date),Year=year(date),Minute=minute(date),Day=day(date)) %>%
group_by(Date,Hour,Station) %>%
summarise(`Avg Hourly Temp`=mean(Temp,na.rm = TRUE),`Avg Hourly SpCond`=mean(`SpCond`,na.rm = TRUE),`Avg Hourly DO`=mean(`DO Conc`,na.rm = TRUE),`Avg Hourly pH`=mean(pH,na.rm = TRUE)) %>%
group_by(Date,Station) %>%
mutate(Temp_Diff_24_hour_mean=`Avg Hourly Temp`-mean(`Avg Hourly Temp`),SpCond_Diff_24_hour_mean=`Avg Hourly SpCond`-mean(`Avg Hourly SpCond`),
DO_Diff_24_hour_mean=`Avg Hourly DO`-mean(`Avg Hourly DO`),pH_Diff_24_hour_mean=`Avg Hourly pH`-mean(`Avg Hourly pH`)) 
  
  
# Step 6: Import and Tidy Weather Data ------------------------------------

S7_R_BK <- select(rename(read_csv("Data/S7_R_BK.csv",  skip = 2),date = 1,"Rain S7"=4),1,4)  #Rain data at S7

S7_E_BK <- select(rename(read_csv("Data/S7_E_BK.csv",  skip = 2),date = 1,"EVAP S7"=4),1,4)  #Evaporation data at S7

BELLW_WNVS_BK <- select(rename(read_csv("Data/BELLW_WNVS_BK.csv",  skip = 2),date = 1,"WIND BELLEGLADE"=4),1,4)  #Evaporation data at S7


Combined_Weather <- setNames(as.data.frame(seq(from=ISOdate(2012,7,01,0,0,0,tz = "America/New_York"), to=ISOdate(2017,9,04,0,0,0,tz = "America/New_York"),by = "min")),"date") %>%
full_join(mutate(S7_R_BK,date=dmy_hms(date)),by="date") %>%  
full_join(mutate(S7_E_BK,date=dmy_hms(date)),by="date") %>%   
full_join(mutate(BELLW_WNVS_BK,date=dmy_hms(date)),by="date") %>%   
arrange(date) %>%
fill(`Rain S7`,`WIND BELLEGLADE`) %>%       #This fills in chronologically with last known value for RAIN and WIND. Not sure that fill down is good idea for EVAP data
mutate(Date=as.Date(date),Hour=hour(date),Minute=minute(date)) %>%
group_by(Date) %>%
mutate(`Rainy Day` = case_when(max(`Rain S7`)==0~ "Dry Day",
                     between(max(`Rain S7`),0,0.015)~ "0-.01 Rain Day",
                     between(max(`Rain S7`),.015,0.045)~ "0.01-.03 Rain Day",
                     between(max(`Rain S7`),.045,0.095)~ "0.04-.09 Rain Day",
                     max(`Rain S7`)>.095~ "0.10+ Rain Day")) %>%
mutate(`Rainy Day` = factor(`Rainy Day`, levels = c("Dry Day", "0-.01 Rain Day", "0.01-.03 Rain Day","0.04-.09 Rain Day","0.10+ Rain Day"))) %>% 
mutate(`Max Daily Evap` = case_when(between(max(`EVAP S7`,na.rm=TRUE),0,0.249)~ "0-.25 EVAP Day",
                                    between(max(`EVAP S7`,na.rm=TRUE),0.25,0.499)~ ".25-.5 EVAP Day",
                                    between(max(`EVAP S7`,na.rm=TRUE),.5,0.749)~ "0.5-.75 EVAP Day",
                                    between(max(`EVAP S7`,na.rm=TRUE),.75,.999)~ "0.75-1.0 EVAP Day",
                                    max(`EVAP S7`,na.rm=TRUE)>1~ "1.0+ EVAP Day")) %>%
mutate(`Max Daily Evap` = factor(`Max Daily Evap`, levels = c("0-.25 EVAP Day", ".25-.5 EVAP Day", "0.5-.75 EVAP Day","0.75-1.0 EVAP Day","1.0+ EVAP Day"))) %>% 
mutate(`Max Daily Wind` = case_when(between(max(`WIND BELLEGLADE`,na.rm=TRUE),0,4.999)~ "0-5 Max Daily Wind mph",
                                    between(max(`WIND BELLEGLADE`,na.rm=TRUE),5,9.99)~ "5-10 Max Daily Wind mph",
                                    between(max(`WIND BELLEGLADE`,na.rm=TRUE),10,14.99)~ "10-15 Max Daily Wind mph",
                                    between(max(`WIND BELLEGLADE`,na.rm=TRUE),15,19.99)~ "15-20 Max Daily Wind mph",
                                    max(`WIND BELLEGLADE`,na.rm=TRUE)>20~ "20+ Max Daily Wind mph")) %>%
mutate(`Max Daily Wind` = factor(`Max Daily Wind`, levels = c("0-5 Max Daily Wind mph", "5-10 Max Daily Wind mph", "10-15 Max Daily Wind mph","15-20 Max Daily Wind mph","20+ Max Daily Wind mph"))) %>% 
select(-date) 

# Step 7: Import and Tidy Inflow P Data from compliance sites ------------------------------------

G378B_Midflow_TP <- read_csv("Data/G378B_Midflow_TP.csv")
G384B_Midflow <- read_csv("Data/G384B Midflow.csv")
G333C_Inflow <- read_csv("Data/G333C Inflow.csv")

G378B_tidy <- G378B_Midflow_TP %>%
  mutate(date=dmy_hm(`Collection_Date`)) %>%
  mutate(`Storet Code`=as.character(`Storet Code`))

G384B_tidy <- G384B_Midflow %>%
  mutate(date=mdy_hm(`Collection_Date`)) %>%
  mutate(`Storet Code`=as.character(`Storet Code`))

G333C_tidy <- G333C_Inflow %>%
  mutate(date=mdy_hm(`Collection_Date`)) %>%
  mutate(`Storet Code`=as.character(`Storet Code`))

Inflow_TP_Data <-G378B_tidy %>%
  bind_rows(G333C_tidy) %>%
  bind_rows(G384B_tidy ) %>%
  filter(`Collection Method`=="G",`Sample Type New`=="SAMP") %>%
  mutate(Date=as.Date(date),Year=year(date),Hour=hour(date),Month=month(Date,label=TRUE),Minute=minute(Date),`Inflow TP`=Value*1000) %>%
  mutate(`Flowway` = case_when(`Station ID`=="G333C"~"STA-2C3",`Station ID`=="G378B"~"STA-3/4C2",`Station ID`=="G384B"~"STA-3/4C3")) %>%
  select(Date,date,Year,Month,Hour,Minute,`Station ID`,Flowway,`Inflow TP`)


# Step 8: Join Flow and RPA data and save DF --------------------------------------
RPAs_with_Flow <-  RPAs_Sorted %>%
left_join(Combined_BK_Flow ,by=c("Date","Hour","Flowway")) %>%
filter(is.finite(Outflow) || is.finite(Inflow)) %>%     #need inflow data
mutate(Outflow=as.numeric(Outflow)) %>%
mutate(Season=if_else(between(month(Date),5,11),"Wet Season","Dry Season")) %>%
mutate(`Outflow Category` = as.factor(case_when( between(Outflow,0,1) ~ "0-1 (cfs)",between(Outflow,1,100) ~ "1-100 (cfs)",between(Outflow,100,250) ~ "100-250 (cfs)",between(Outflow,250,500) ~ "250-500 (cfs)",between(Outflow,500,1000) ~ "500-1000 (cfs)", Outflow>1000 ~ "1000+ (cfs)", Outflow<0 ~ "Reverse Flow"))) %>%
mutate(`Inflow Category` = as.factor(case_when( between(Inflow,0,1) ~ "0-1 (cfs)",between(Inflow,1,100) ~ "1-100 (cfs)",between(Inflow,100,250) ~ "100-250 (cfs)",between(Inflow,250,500) ~ "250-500 (cfs)",between(Inflow,500,1000) ~ "500-1000 (cfs)", Inflow>1000 ~ "1000+ (cfs)", Inflow < 0 ~ "Reverse Flow"))) %>%
mutate(`Outflow Category`=factor(`Outflow Category`,levels = c("Reverse Flow","0-1 (cfs)","1-100 (cfs)","100-250 (cfs)","250-500 (cfs)","500-1000 (cfs)","1000+ (cfs)"))) %>%
mutate(`Inflow Category`=factor(`Inflow Category`,levels = c("Reverse Flow","0-1 (cfs)","1-100 (cfs)","100-250 (cfs)","250-500 (cfs)","500-1000 (cfs)","1000+ (cfs)"))) 

write.csv(RPAs_with_Flow, "Data/RPA and Flow.csv",row.names=FALSE)

# Step 9: Join with Stage Data and save DF ----------------------------------------------------

RPAs_with_Flow_Stage <- RPAs_with_Flow %>%
left_join(Combined_Stage ,by=c("Flowway","Date","Hour")) %>%
group_by(Flowway,Date) %>%
mutate(`Outflow_Stage_24_hour_mean`=mean(`Outflow Stage`),`Inflow_Stage_24_hour_mean`=mean(`Inflow Stage`)) %>%       #calculate daily mean stage for inflow and outflow
mutate(`Outflow Stage Diff 24 hour mean`=`Outflow Stage`-`Outflow_Stage_24_hour_mean`,`Inflow Stage Diff 24 hour mean`=`Inflow Stage`-`Inflow_Stage_24_hour_mean`)  #calculate hourly deviation from daily mean for inflow and outflow


write.csv(RPAs_with_Flow_Stage, "Data/RPA and Flow and Stage.csv",row.names=FALSE)


# Step 10: Join with Weather data ------------------------------------------

RPAs_with_Flow_Stage_Weather <- RPAs_with_Flow_Stage %>%
left_join(Combined_Weather ,by=c("Date","Hour","Minute"))

write.csv(RPAs_with_Flow_Stage_Weather, "Data/RPA and Flow Stage Weather.csv",row.names=FALSE)


# Step 11: Join with Sonde Data --------------------------------------------

RPAs_with_Flow_Stage_Weather_Sonde <- RPAs_with_Flow_Stage_Weather %>%
left_join(Sonde_Tidy ,by=c("Date","Hour","Station"))

write.csv(RPAs_with_Flow_Stage_Weather_Sonde, "Data/RPA and Flow Stage Weather Sonde.csv",row.names=FALSE)

# Step 11: Join with Inflow compliance TP Data ------------------------------------------
RPAs_with_Flow_Stage_Weather_Sonde_Inflow_TP <- RPAs_with_Flow_Stage_Weather_Sonde %>%
mutate(`Flowway` = case_when(`Station`=="G334"~"STA-2C3",`Station`=="G379"~"STA-3/4C2",`Station`=="G381"~"STA-3/4C3",`Station`=="G380"~"STA-3/4C3",`Station`=="G384"~"STA-3/4C3")) %>%        #Add flowway info to RPA data
mutate(`Flowpath Region` = case_when(`Station`=="G334"~"Outflow",`Station`=="G379"~"Outflow",`Station`=="G381"~"Outflow",`Station`=="G380"~"Inflow",`Station`=="G384"~"Midflow")) %>%        #Add flowpath position
left_join(Inflow_TP_Data ,by=c("Date","Month","Year","Flowway"))   %>%  #When joining this data the compliance sample is joined to every RPA sample collected on the date. 
mutate(`Closest Hour`=(Hour.x+Minute.x/60)-(Hour.y+Minute.y/60)) %>%    #calculate time difference between compliance sample and RPA samples
group_by(Station,Date) %>%                                              #group by date
mutate(`Closest time rank`=row_number(abs(`Closest Hour`)))  %>%        #Rank the time differences 
mutate(`Inflow TP`=ifelse(`Closest time rank`==1,`Inflow TP`, NA)) %>%  #Keep compliance sample at closest time diffrence only. 
ungroup()

write.csv(RPAs_with_Flow_Stage_Weather_Sonde_Inflow_TP, "Data/RPA and Flow Stage Weather Sonde Inflow TP.csv",row.names=FALSE)


