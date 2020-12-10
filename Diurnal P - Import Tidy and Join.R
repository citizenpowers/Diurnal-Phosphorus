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
#G381
G381A_C_BK <- select(rename(read_csv("Data/G381A_C_BK.csv",  skip = 2),date = 1,G381A=4),1,4)
G381B_C_BK <- select(rename(read_csv("Data/G381B_C_BK.csv",  skip = 2),date = 1,G381B=4),1,4)
G381C_C_BK <- select(rename(read_csv("Data/G381C_C_BK.csv",  skip = 2),date = 1,G381C=4),1,4)
G381D_C_BK <- select(rename(read_csv("Data/G381D_C_BK.csv",  skip = 2),date = 1,G381D=4),1,4)
G381E_C_BK <- select(rename(read_csv("Data/G381E_C_BK.csv",  skip = 2),date = 1,G381E=4),1,4)
G381F_C_BK <- select(rename(read_csv("Data/G381F_C_BK.csv",  skip = 2),date = 1,G381F=4),1,4)

#G379 
G379A_C_BK <- select(rename(read_csv("Data/G379A_C_BK.csv",  skip = 2),date = 1,G379A=4),1,4)
G379B_C_BK <- select(rename(read_csv("Data/G379B_C_BK.csv",  skip = 2),date = 1,G379B=4),1,4)
G379C_C_BK <- select(rename(read_csv("Data/G379C_C_BK.csv",  skip = 2),date = 1,G379C=4),1,4)
G379D_C_BK <- select(rename(read_csv("Data/G379D_C_BK.csv",  skip = 2),date = 1,G379D=4),1,4)

#G334
G334_S_BK_1 <- select(rename(read_csv("Data/G334_S_BK.csv",skip = 2),date = 1,G334=4),1,4)

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


#G381 Combine individual flowway stations
G381_BK <- mutate(G381A_C_BK,date=dmy_hms(date)) %>%  #sum flow from all gates in STA34 cell 3B
full_join(mutate(G381B_C_BK,date=dmy_hms(date)),by="date") %>%
full_join(mutate(G381C_C_BK,date=dmy_hms(date)),by="date") %>%
full_join(mutate(G381D_C_BK,date=dmy_hms(date)),by="date") %>%
full_join(mutate(G381E_C_BK,date=dmy_hms(date)),by="date") %>%
full_join(mutate(G381F_C_BK,date=mdy_hm(date)),by="date") %>%
arrange(date) %>%
fill(G381A,G381B,G381C,G381D,G381E,G381F) %>%
mutate(G381=rowSums(.[2:7],na.rm=TRUE))

G379_BK <- mutate(G379A_C_BK,date=dmy_hms(date))  %>%   #sum flow from all gates in STA34 cell 2B
full_join(mutate(G379B_C_BK,date=dmy_hms(date)),by="date") %>%
full_join(mutate(G379C_C_BK,date=dmy_hms(date)),by="date") %>%
full_join(mutate(G379D_C_BK,date=mdy_hm(date)),by="date") %>%
arrange(date) %>%
fill(G379A,G379B,G379C,G379D) %>%
mutate(G379=rowSums(.[2:5],na.rm=TRUE))

G334_BK <-mutate(G334_S_BK_1,date=mdy_hm(date))

#Combined Outflow over entire flowway
Combined_BK_Flow <- G381_BK %>%  #combine data from G381, G334, G379D
bind_rows(G379_BK) %>%
bind_rows(G334_BK)  %>%
bind_rows(G333_C_BK) %>% 
bind_rows(G380_C_BK) %>%
bind_rows(G377_C_BK) %>%
select(date,G381,G379,G334,G333,G380,G377) %>%
fill(G381,G379,G334,G333,G380,G377) %>%  
gather("Station","Flow",G381,G379,G334,G333,G380,G377) %>%
mutate(`Outflow` = case_when(`Station` %in% c("G381","G379","G334")~Flow)) %>% 
mutate(`Outflow HLR` = case_when(`Station`=="G381" ~ Flow/2087,`Station`=="G379" ~ Flow/2375,`Station`=="G334" ~ Flow/2400)) %>%
mutate(`Inflow` = case_when(`Station` %in% c("G333","G380","G377")~Flow)) %>% 
mutate(`Inflow HLR` = case_when(`Station`=="G333" ~ Flow/2400,`Station`=="G380" ~ Flow/2087,`Station`=="G377" ~ Flow/2375)) %>%
mutate(Date=as.Date(date)) %>%
mutate(Hour=hour(round_date(date, unit = "hour"))) %>%
mutate(`Flowway` = case_when(`Station`=="G334"~"STA-2 Central",`Station`=="G379"~"STA-3/4 Central",`Station`=="G377"~"STA-3/4 Central",`Station`=="G381"~"STA-3/4 Western",`Station`=="G380"~"STA-3/4 Western",`Station`=="G333"~"STA-2 Central")) %>%        #Add flowway info to RPA data
mutate(`Flowpath Region` = case_when(`Station`=="G334"~"Outflow",`Station`=="G379"~"Outflow",`Station`=="G377"~"Inflow",`Station`=="G381"~"Outflow",`Station`=="G333"~"Inflow",`Station`=="G380"~"Inflow")) %>%        #Add flowpath position
group_by(`Flowway`,`Flowpath Region`,Station,Date,Hour) %>%
summarise(Outflow=mean(Outflow,na.rm = TRUE),`Outflow HLR`=mean(`Outflow HLR`,na.rm=TRUE),Inflow=mean(Inflow,na.rm = TRUE),`Inflow HLR`=mean(`Inflow HLR`,na.rm=TRUE))

write.csv(Combined_BK_Flow, "Data/Combined_BK_Flow.csv")

# Step 2: Import and Tidy RPA data  --------------------------------------

#RPA data from outflows 
RPAs <-  read_excel("Data/Outflows.xlsx", col_types = c("text", "date", "numeric",  "numeric")) 

#RPA data from inflows and midflows
RPAs_midflow <- read_csv("Data/G384C_TP.csv") %>%
bind_rows(read_csv("Data/G380C_TP.csv"))   %>%
bind_rows(read_excel("Data/G378C_Midflow_TP.xlsx")) %>%
bind_rows(read_excel("Data/G377C_Inflow_TP.xlsx",col_types = c("text", "date", "numeric","numeric"))) %>%  
bind_rows(select(mutate(read_csv("Data/G333C_Inflow_TP.csv",col_types = cols(BATTERY_VOLTAGE = col_datetime(format = "%m/%d/%Y %H:%M"))),COLLECT_DATE=mdy_hm(COLLECT_DATE)),SITE_NAME,COLLECT_DATE,TRP,TP)) %>%
select(SITE_NAME,COLLECT_DATE,TRP,TP)  %>%
rename(Station="SITE_NAME",Date="COLLECT_DATE",TPO4="TP")
  
 
RPAs_Sorted <- RPAs %>%
bind_rows(RPAs_midflow)  %>%
filter(!is.na(TPO4),TPO4>=0) %>%
mutate(Month=month(Date,label=TRUE)) %>%
mutate(Day=day(Date)) %>%
mutate(Time=hour(Date)+ minute(Date)/60) %>%
mutate(Year=year(Date)) %>%
mutate(Hour=hour(Date)) %>%
mutate(Minute=minute(Date)) %>%  
mutate(Date=as.Date(Date)) %>%
mutate(`Station` = case_when(`Station`=="G379D"~ "G379",`Station`=="G381B" ~ "G381",`Station`=="G334" ~ "G334",`Station`=="G384C" ~ "G384",`Station`=="G380C" ~ "G380",`Station`=="G378C" ~ "G378",`Station`=="G377C" ~ "G377",`Station`=="G333C" ~ "G333")) %>%
mutate(`Flowway` = case_when(`Station`=="G334"~"STA-2 Central",`Station`=="G379"~"STA-3/4 Central",`Station`=="G381"~"STA-3/4 Western",`Station`=="G380"~"STA-3/4 Western",`Station`=="G384"~"STA-3/4 Western",`Station`=="G378" ~ "STA-3/4 Central",`Station`=="G377" ~ "STA-3/4 Central",`Station`=="G333" ~ "STA-2 Central")) %>%        #Add flowway info to RPA data
mutate(`Flowpath Region` = case_when(`Station`=="G334"~"Outflow",`Station`=="G379"~"Outflow",`Station`=="G381"~"Outflow",`Station`=="G380"~"Inflow",`Station`=="G384"~"Midflow",`Station`=="G380" ~ "Inflow",`Station`=="G378" ~ "Midflow",`Station`=="G377" ~ "Inflow",`Station`=="G333" ~ "Inflow")) %>%        #Add flowpath position
group_by(Station,Year,Day,Month) %>%
mutate(RANK=row_number(TPO4))  %>%
mutate(PERCENT_RANK=cume_dist(TPO4)) %>% 
mutate(Scaled_Value=TPO4/max(TPO4)) %>%
mutate(`24_hour_mean`=mean(TPO4)) %>%
mutate(Diff_24_hour_mean=TPO4-`24_hour_mean`) %>%
mutate(`Percent difference from daily mean`=(Diff_24_hour_mean/`24_hour_mean`)*100)

write.csv(RPAs_Sorted, "Data/RPAs Sorted.csv",row.names=FALSE)

# Step 3: Import and Tidy Stage from G334_H,G379B_H, G381B_H---------------------------

G334_H_BK <- select(rename(read_csv("Data/G334_H_BK.csv",  skip = 2),date = 1,G334=4),1,4)

G379B_H_BK <- select(rename(read_csv("Data/G379B_H_BK.csv",  skip = 2),date = 1,G379=4),1,4)

G381B_H_BK <- select(rename(read_csv("Data/G381B_H_BK.csv",  skip = 2),date = 1,G381=4),1,4)

Combined_Stage <- setNames(as.data.frame(seq(from=ISOdate(2012,7,01,0,0,0,tz = "America/New_York"), to=ISOdate(2017,9,04,0,0,0,tz = "America/New_York"),by = "min")),"date") %>%
full_join(mutate(G334_H_BK,date=dmy_hms(date)),by="date") %>%  #sum flow from all gates in STA34 cell 3B
full_join(mutate(G379B_H_BK,date=dmy_hms(date)),by="date") %>%
full_join(mutate(G381B_H_BK,date=mdy_hm(date)),by="date") %>%  
arrange(date) %>%
fill(G381,G379,G334) %>% 
pivot_longer(2:4,names_to="Station",values_to="Stage") %>%
mutate(Date=as.Date(date),Hour=hour(date),Minute=minute(date)) %>%
group_by(Date) %>%
mutate(`Max Daily Stage` = case_when(max(`Stage`,na.rm=TRUE)<10.5~ " < 10.5 Max Daily Stage ft",
                                     between(max(`Stage`,na.rm=TRUE),10.5,11)~ "10.5-11 Max Daily Stage ft",
                                     between(max(`Stage`,na.rm=TRUE),11,12)~ "11-12 Max Daily Stage ft",
                                     max(`Stage`,na.rm=TRUE)>12~ "12+ Max Daily Stage ft")) %>%
mutate(`Max Daily Stage` = factor(`Max Daily Stage`, levels = c(" < 10.5 Max Daily Stage ft", "10.5-11 Max Daily Stage ft", "11-12 Max Daily Stage ft","12+ Max Daily Stage ft"))) %>% 
select(-date)
  
# Step 4: Import and Tidy Sonde Data ----------------------------------------------
SONDE_DATA <- read_csv("Data/SONDE_DATA.csv") 

Sonde_Tidy <- SONDE_DATA  %>%
mutate(date=mdy_hm(`Date/Time`),Date=as.Date(date),Hour=hour(date),Year=year(date),Minute=minute(date),Day=day(date)) %>%
group_by(Date,Hour,Station) %>%
summarise(`Avg Hourly Temp`=mean(Temp,na.rm = TRUE),`Avg Hourly SpCond`=mean(`SpCond`,na.rm = TRUE),`Avg Hourly DO`=mean(`DO Conc`,na.rm = TRUE),`Avg Hourly pH`=mean(pH,na.rm = TRUE)) %>%
group_by(Date,Station) %>%
mutate(Temp_Diff_24_hour_mean=`Avg Hourly Temp`-mean(`Avg Hourly Temp`),SpCond_Diff_24_hour_mean=`Avg Hourly SpCond`-mean(`Avg Hourly SpCond`),
DO_Diff_24_hour_mean=`Avg Hourly DO`-mean(`Avg Hourly DO`),pH_Diff_24_hour_mean=`Avg Hourly pH`-mean(`Avg Hourly pH`)) 
  
  
# Step 5: Import and Tidy Weather Data ------------------------------------

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

# Step 6: Import and Tidy Inflow P Data from compliance sites ------------------------------------

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

# Step 7: Join Flow and RPA data and save DF --------------------------------------
RPAs_with_Flow <-  RPAs_Sorted %>%
left_join(Combined_BK_Flow ,by=c("Station","Date","Hour","Flowway","Flowpath Region")) %>%
filter(is.finite(Outflow) || is.finite(Inflow)) %>%     #need inflow data
mutate(Outflow=as.numeric(Outflow)) %>%
mutate(Season=if_else(between(month(Date),5,11),"Wet Season","Dry Season")) %>%
mutate(`Outflow Category` = as.factor(case_when( between(Outflow,0,1) ~ "0-1 (cfs)",between(Outflow,1,100) ~ "1-100 (cfs)",between(Outflow,100,250) ~ "100-250 (cfs)",between(Outflow,250,500) ~ "250-500 (cfs)",between(Outflow,500,1000) ~ "500-1000 (cfs)", Outflow>1000 ~ "1000+ (cfs)", Outflow<0 ~ "Reverse Flow"))) %>%
mutate(`Inflow Category` = as.factor(case_when( between(Inflow,0,1) ~ "0-1 (cfs)",between(Inflow,1,100) ~ "1-100 (cfs)",between(Inflow,100,250) ~ "100-250 (cfs)",between(Inflow,250,500) ~ "250-500 (cfs)",between(Inflow,500,1000) ~ "500-1000 (cfs)", Inflow>1000 ~ "1000+ (cfs)", Inflow < 0 ~ "Reverse Flow"))) %>%
mutate(`Outflow Category`=factor(`Outflow Category`,levels = c("Reverse Flow","0-1 (cfs)","1-100 (cfs)","100-250 (cfs)","250-500 (cfs)","500-1000 (cfs)","1000+ (cfs)"))) %>%
mutate(`Inflow Category`=factor(`Inflow Category`,levels = c("Reverse Flow","0-1 (cfs)","1-100 (cfs)","100-250 (cfs)","250-500 (cfs)","500-1000 (cfs)","1000+ (cfs)"))) 

write.csv(RPAs_with_Flow, "Data/RPA and Flow.csv",row.names=FALSE)

# Step 8: Join with Stage Data and save DF ----------------------------------------------------

RPAs_with_Flow_Stage <- RPAs_with_Flow %>%
left_join(Combined_Stage ,by=c("Station","Date","Hour","Minute")) %>%
group_by(Station,Date) %>%
mutate(`Stage_24_hour_mean`=mean(Stage)) %>%  
mutate(Stage_Diff_24_hour_mean=Stage-`Stage_24_hour_mean`) 


write.csv(RPAs_with_Flow_Stage, "Data/RPA and Flow and Stage.csv",row.names=FALSE)


# Step 9: Join with Weather data ------------------------------------------

RPAs_with_Flow_Stage_Weather <- RPAs_with_Flow_Stage %>%
left_join(Combined_Weather ,by=c("Date","Hour","Minute"))

write.csv(RPAs_with_Flow_Stage_Weather, "Data/RPA and Flow Stage Weather.csv",row.names=FALSE)


# Step 10: Join with Sonde Data --------------------------------------------

RPAs_with_Flow_Stage_Weather_Sonde <- RPAs_with_Flow_Stage_Weather %>%
left_join(Sonde_Tidy ,by=c("Date","Hour","Station"))

write.csv(RPAs_with_Flow_Stage_Weather_Sonde, "Data/RPA and Flow Stage Weather Sonde.csv",row.names=FALSE)

# Step 11: Join with Inflow Data ------------------------------------------
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


