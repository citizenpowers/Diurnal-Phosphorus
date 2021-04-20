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
Combined_BK_Flow_step1 <-  setNames(as.data.frame(seq(from=ISOdate(2012,7,01,0,0,0,tz = "UTC"), to=ISOdate(2017,10,01,0,0,0,tz = "UTC"),by = "min")),"date") %>%
left_join(G381_C_BK,by="date") %>%  #combine data from G381, G334, G379D
left_join(G379_C_BK,by="date") %>%
left_join(G334_S_BK,by="date")  %>%
left_join(G333_C_BK,by="date") %>% 
left_join(G380_C_BK,by="date") %>%
left_join(G377_C_BK,by="date") %>%
select(date,G381,G379,G334,G333,G380,G377) %>%
arrange(date)  %>%
fill(G381,G379,G334,G333,G380,G377) 

Combined_BK_Flow <-Combined_BK_Flow_step1 %>% 
gather("Station","Flow",G381,G379,G334,G333,G380,G377) %>%
mutate(`Flowway` = case_when(`Station`=="G334"~"STA-2 Central",`Station`=="G379"~"STA-3/4 Central",`Station`=="G377"~"STA-3/4 Central",`Station`=="G381"~"STA-3/4 Western",`Station`=="G380"~"STA-3/4 Western",`Station`=="G333"~"STA-2 Central")) %>%        #Add flowway info to RPA data
mutate(`Flowpath Region` = case_when(`Station`=="G334"~"Outflow",`Station`=="G379"~"Outflow",`Station`=="G377"~"Inflow",`Station`=="G381"~"Outflow",`Station`=="G333"~"Inflow",`Station`=="G380"~"Inflow"))  %>%     #Add flowpath position
mutate(`Outflow` = case_when(`Flowway` == "STA-2 Central" & `Flowpath Region`=="Outflow" ~Flow,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Outflow"~Flow,`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Outflow"~Flow)) %>% 
mutate(`Outflow HLR` = case_when(`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Outflow"~Flow/4558,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Outflow"~Flow/5421,`Flowway` == "STA-2 Central" & `Flowpath Region`=="Outflow" ~Flow/2296)) %>%
mutate(`Inflow` = case_when(`Flowway` == "STA-2 Central"  & `Flowpath Region`=="Inflow" ~Flow,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Inflow"~Flow,`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Inflow"~Flow))  %>%
mutate(`Inflow HLR` = case_when(`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Inflow"~Flow/4558,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Inflow"~Flow/5421,`Flowway` == "STA-2 Central" & `Flowpath Region`=="Inflow" ~Flow/2296)) %>%
mutate(Date=as.Date(date)) %>%
mutate(Hour=hour(round_date(date, unit = "hour"))) %>%
group_by(`Flowway`,Date,Hour) %>%
summarise(Outflow=mean(Outflow,na.rm = TRUE),`Outflow HLR`=mean(`Outflow HLR`,na.rm=TRUE)*60.4556,Inflow=mean(Inflow,na.rm = TRUE),`Inflow HLR`=mean(`Inflow HLR`,na.rm=TRUE)*60.4556) #convert to HLR (24hours *60 min * 60 Sec) /day  * (1 FT^3/sec) *(30.48 cm/1ft) * (1 acre/43560 ft^2) 

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
mutate(Month = factor(Month, levels=month.abb)) %>%
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

# Step 3: Create categories for TP concentration --------------------------
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

Combined_Stage <- setNames(as.data.frame(seq(from=ISOdate(2012,7,01,0,0,0,tz = "UTC"), to=ISOdate(2017,10,01,0,0,0,tz = "UTC"),by = "min")),"date") %>%
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
DO_Diff_24_hour_mean=`Avg Hourly DO`-mean(`Avg Hourly DO`),pH_Diff_24_hour_mean=`Avg Hourly pH`-mean(`Avg Hourly pH`),
`DO Percent Diff from 24 mean`=DO_Diff_24_hour_mean/mean(`Avg Hourly DO`)*100,`pH Percent Diff from 24 mean`=pH_Diff_24_hour_mean/mean(`Avg Hourly pH`)*100,
`SpCond Percent Diff from 24 mean`=SpCond_Diff_24_hour_mean/mean(`Avg Hourly SpCond`)*100,`Temp Percent Diff from 24 mean`=Temp_Diff_24_hour_mean/mean(`Avg Hourly Temp`)*100)

Sonde_Tidy_medians <- SONDE_DATA  %>%
mutate(date=mdy_hm(`Date/Time`),Date=as.Date(date),Hour=hour(date),Year=year(date),Minute=minute(date),Day=day(date)) %>%
group_by(Date,Hour,Station) %>%
summarise(`Avg Hourly Temp`=median(Temp,na.rm = TRUE),`Avg Hourly SpCond`=median(`SpCond`,na.rm = TRUE),`Avg Hourly DO`=median(`DO Conc`,na.rm = TRUE),`Avg Hourly pH`=median(pH,na.rm = TRUE)) %>%
group_by(Date,Station) %>%
mutate(Temp_Diff_24_hour_median=`Avg Hourly Temp`-median(`Avg Hourly Temp`),SpCond_Diff_24_hour_median=`Avg Hourly SpCond`-median(`Avg Hourly SpCond`),
        DO_Diff_24_hour_median=`Avg Hourly DO`-median(`Avg Hourly DO`),pH_Diff_24_hour_median=`Avg Hourly pH`-median(`Avg Hourly pH`),
        `DO Percent Diff from 24 median`=DO_Diff_24_hour_median/median(`Avg Hourly DO`)*100,`pH Percent Diff from 24 median`=pH_Diff_24_hour_median/median(`Avg Hourly pH`)*100,
        `SpCond Percent Diff from 24 median`=SpCond_Diff_24_hour_median/median(`Avg Hourly SpCond`)*100,`Temp Percent Diff from 24 median`=Temp_Diff_24_hour_median/median(`Avg Hourly Temp`)*100)



  
# Step 6: Import and Tidy Weather Data ------------------------------------

S7_R_BK <- mutate(select(rename(read_csv("Data/S7_R_BK.csv",  skip = 2),date = 1,"Rain S7"=4),1,4),date=dmy_hms(date))  #Rain data at S7

S7_R_DA <- mutate(select(rename(read_csv("Data/S7_R_DA.csv",  skip = 3),Date = 3,"Rain S7 DA"=4),3,4),Date=dmy(Date))  #Rain data at S7

S7_E_BK <- mutate(select(rename(read_csv("Data/S7_E_BK.csv",  skip = 2),date = 1,"EVAP S7"=4),1,4),date=dmy_hms(date))  #Evaporation data at S7

BELLW_WNVS_BK <- mutate(select(rename(read_csv("Data/BELLW_WNVS_BK.csv",  skip = 2),date = 1,"WIND BELLEGLADE"=4),1,4),date=mdy_hm(date)) #max daily windspeed in at belleglade weather station

S7_R_BK_tidy <- S7_R_BK %>%
mutate(Date=as.Date(date),Hour=hour(date),Minute=minute(date)) %>%  
group_by(Date,Hour,Minute) %>%
summarise(`Rain S7`=mean(`Rain S7`,na.rm=TRUE))

S7_E_BK_tidy <- S7_E_BK %>%
mutate(Date=as.Date(date),Hour=hour(date),Minute=minute(date)) %>%
select(-date)

BELLW_WNVS_BK_tidy <- BELLW_WNVS_BK %>%
mutate(Date=as.Date(date),Hour=hour(date),Minute=minute(date)) %>%
select(-date)

Combined_Weather <- setNames(as.data.frame(seq(from=ISOdate(2012,7,01,0,0,0,tz = "UTC"), to=ISOdate(2017,9,14,0,0,0,tz = "UTC"),by = "min")),"date") %>%
mutate(Date=as.Date(date),Hour=hour(date),Minute=minute(date)) %>% 
select(-date) %>%
left_join(S7_R_DA,by="Date")%>%  
full_join(S7_R_BK_tidy,by=c("Date","Hour","Minute")) %>%  
full_join(S7_E_BK_tidy,by=c("Date","Hour","Minute")) %>%    
full_join(BELLW_WNVS_BK_tidy,by=c("Date","Hour","Minute")) %>%   
arrange(Date) %>%
fill(`Rain S7`,`WIND BELLEGLADE`) %>%       #This fills in chronologically with last known value for RAIN and WIND. Not sure that fill down is good idea for EVAP data
group_by(Date) %>%
mutate(`Daily Rainfall Range` = case_when(max(`Rain S7 DA`)==0~ "No Rainfall",
                                 between(max(`Rain S7 DA`),0.001,0.1)~ "0-.1 (in) Rainfall",
                                 between(max(`Rain S7 DA`),.1,.25)~ "0.1-.25 (in) Rainfall",
                                 between(max(`Rain S7 DA`),.25,0.5)~ "0.25-.5 (in) Rainfall",
                                 between(max(`Rain S7 DA`),.5,0.75)~ "0.5-.75 (in) Rainfall",
                                 between(max(`Rain S7 DA`),.75,1)~ "0.75-1.0 (in) Rainfall",
                                 max(`Rain S7 DA`)>1~ "1.0+ (in) Rainfall")) %>%  
mutate(`Daily Rainfall Range` = factor(`Daily Rainfall Range`, levels = c("No Rainfall", "0-.1 (in) Rainfall","0.1-.25 (in) Rainfall","0.25-.5 (in) Rainfall","0.5-.75 (in) Rainfall","0.75-1.0 (in) Rainfall","1.0+ (in) Rainfall"))) %>% 
mutate(`Rainy Day` = case_when(max(`Rain S7`)==0~ "Dry Day",
                     between(max(`Rain S7`),0,0.015)~ "0-.01 Rain Day",
                     between(max(`Rain S7`),.015,0.045)~ "0.01-.03 Rain Day",
                     between(max(`Rain S7`),.045,0.095)~ "0.04-.09 Rain Day",
                     max(`Rain S7`)>.095~ "0.10+ Rain Day")) %>%
mutate(`Rainy Day` = factor(`Rainy Day`, levels = c("Dry Day", "0-.01 Rain Day", "0.01-.03 Rain Day","0.04-.09 Rain Day","0.10+ Rain Day"))) %>% 
mutate(`Max Daily Evap` = case_when(between(max(`EVAP S7`,na.rm=TRUE),0,.1)~ "0-.1 EVAP Day",
                                    between(max(`EVAP S7`,na.rm=TRUE),.1,0.249)~ "0.1-.25 EVAP Day",
                                    between(max(`EVAP S7`,na.rm=TRUE),0.25,0.499)~ "0.25-.5 EVAP Day",
                                    between(max(`EVAP S7`,na.rm=TRUE),.5,0.749)~ "0.5-.75 EVAP Day",
                                    between(max(`EVAP S7`,na.rm=TRUE),.75,.999)~ "0.75-1.0 EVAP Day",
                                    max(`EVAP S7`,na.rm=TRUE)>1~ "1.0+ EVAP Day")) %>%
mutate(`Max Daily Evap` = factor(`Max Daily Evap`, levels = c("0-.1 EVAP Day", "0.1-.25 EVAP Day","0.25-.5 EVAP Day", "0.5-.75 EVAP Day","0.75-1.0 EVAP Day","1.0+ EVAP Day"))) %>% 
mutate(`Max Daily Wind` = case_when(between(max(`WIND BELLEGLADE`,na.rm=TRUE),0,9.99)~ "0-10 Max Daily Wind (mph)",
                                    between(max(`WIND BELLEGLADE`,na.rm=TRUE),10,14.99)~ "10-15 Max Daily Wind (mph)",
                                    between(max(`WIND BELLEGLADE`,na.rm=TRUE),15,19.99)~ "15-20 Max Daily Wind (mph)",
                                    max(`WIND BELLEGLADE`,na.rm=TRUE)>20~ "20+ Max Daily Wind (mph)")) %>%
mutate(`Max Daily Wind` = factor(`Max Daily Wind`, levels = c("0-10 Max Daily Wind (mph)", "10-15 Max Daily Wind (mph)","15-20 Max Daily Wind (mph)","20+ Max Daily Wind (mph)")))  

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
mutate(`Outflow Category` = as.factor(case_when( between(Outflow,0,1) ~ "0-1 (cfs)",between(Outflow,1,500) ~ "1-500 (cfs)",between(Outflow,500,1000) ~ "500-1000 (cfs)", Outflow>1000 ~ "1000+ (cfs)", Outflow<0 ~ "Reverse Flow"))) %>%
mutate(`Inflow Category` = as.factor(case_when( between(Inflow,0,1) ~ "0-1 (cfs)",between(Inflow,1,500) ~ "1-500 (cfs)",between(Inflow,500,1000) ~ "500-1000 (cfs)", Inflow>1000 ~ "1000+ (cfs)", Inflow < 0 ~ "Reverse Flow"))) %>%
mutate(`Inflow Category`=factor(`Inflow Category`,levels = c("Reverse Flow","0-1 (cfs)","1-500 (cfs)","500-1000 (cfs)","1000+ (cfs)"))) %>%
mutate(`Outflow HLR Category` = as.factor(case_when( between(`Outflow HLR`,0,.1) ~ "0-.1 (cm/day)",between(`Outflow HLR`,.1,10) ~ "0.1-10 (cm/day)",between(`Outflow HLR`,10,20) ~ "10-20 (cm/day)",`Outflow HLR`>20 ~ "20+ (cm/day)", `Outflow HLR`<0 ~ "Reverse Flow"))) %>%
mutate(`Outflow HLR Category`=factor(`Outflow HLR Category`,levels = c("Reverse Flow","0-.1 (cm/day)","0.1-10 (cm/day)","10-20 (cm/day)","20+ (cm/day)"))) %>%
mutate(`Inflow HLR Category` = as.factor(case_when( between(`Inflow HLR`,0,.1) ~ "0-.1 (cm/day)",between(`Inflow HLR`,.1,10) ~ "0.1-10 (cm/day)",between(`Inflow HLR`,10,20) ~ "10-20 (cm/day)",`Inflow HLR`>20 ~ "20+ (cm/day)", `Inflow HLR`<0 ~ "Reverse Flow"))) %>%
mutate(`Inflow HLR Category`=factor(`Inflow HLR Category`,levels = c("Reverse Flow","0-.1 (cm/day)","0.1-10 (cm/day)","10-20 (cm/day)","20+ (cm/day)"))) 

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

RPAs_with_Flow_Stage_Weather_Sonde_medians <- RPAs_with_Flow_Stage_Weather %>%   #calculates physicochemical paramters as medians instaed of means
left_join(Sonde_Tidy_medians ,by=c("Date","Hour","Station"))

write.csv(RPAs_with_Flow_Stage_Weather_Sonde_medians, "Data/RPA and Flow Stage Weather Sonde medians.csv",row.names=FALSE)


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


