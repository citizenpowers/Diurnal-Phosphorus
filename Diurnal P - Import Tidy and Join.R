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


#Steps
# Step 1: Import Flow  Data from CSV (Only Run if data need update. Skip to create fig script otherwise) --------
#read from CSV
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

# Step 2: Tidy Flow Data ------------------------------------------------

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

#Combined flow over entire flowway
Combined_BK_Flow <- G381_BK %>%  #combine data from G381, G334, G379D
bind_rows(G379_BK) %>%
bind_rows(G334_BK)  %>%
gather("Station","Flow",G381,G379,G334) %>%
select(date,Station,Flow) %>%
mutate(`HLR` = case_when(`Station`=="G381" ~ Flow/2087,`Station`=="G379" ~ Flow/2375,`Station`=="G334" ~ Flow/2400)) %>%
mutate(Date=as.Date(date)) %>%
mutate(Hour=hour(round_date(date, unit = "hour"))) %>%
group_by(Station,Date,Hour) %>%
summarise(Flow=mean(Flow,na.rm = TRUE),HLR=mean(HLR,na.rm=TRUE))

#Combined flow at closest gate only
Combined_BK_Flow_closest_gate <- mutate(G381B_C_BK,date=dmy_hms(date)) %>%
bind_rows(mutate(G379D_C_BK,date=mdy_hm(date))) %>%
bind_rows(mutate(G334_S_BK_1,date=mdy_hm(date)))  %>%
gather("Station","Flow",G381B,G379D,G334) %>%
mutate(Date=as.Date(date)) %>%
#mutate(Flow=if_else(Station=="G334",Flow,Flow)) %>%  #G334 flow/5 since it is larger structure representing larger area
mutate(Hour=hour(round_date(date, unit = "hour"))) %>%
group_by(Station,Date,Hour) %>%
summarise(Flow=mean(Flow,na.rm = TRUE))

# Step 3: Import and Tidy RPA data  --------------------------------------

#RPA data
RPAs <-  read_excel("Data/Outflows.xlsx", col_types = c("text", "date", "numeric",  "numeric")) 


RPAs_Sorted <- RPAs %>%
filter(!is.na(TPO4)) %>%
mutate(Month=month(Date,label=TRUE)) %>%
mutate(Day=day(Date)) %>%
mutate(Time=hour(Date)+ minute(Date)/60) %>%
mutate(Year=year(Date)) %>%
mutate(Hour=hour(Date)) %>%
mutate(Minute=minute(Date)) %>%  
mutate(Date=as.Date(Date)) %>%
mutate(`Station` = case_when(`Station`=="G379D"~ "G379",`Station`=="G381B" ~ "G381",`Station`=="G334" ~ "G334")) %>%
group_by(Station,Year,Day,Month) %>%
mutate(RANK=row_number(TPO4))  %>%
mutate(PERCENT_RANK=cume_dist(TPO4)) %>% 
mutate(Scaled_Value=TPO4/max(TPO4)) %>%
mutate(`24_hour_mean`=mean(TPO4)) %>%
mutate(Diff_24_hour_mean=TPO4-`24_hour_mean`) %>%
mutate(`Percent difference from daily mean`=(Diff_24_hour_mean/`24_hour_mean`)*100)

write.csv(RPAs_Sorted, "Data/RPAs Sorted.csv")

# Step 4: Import and Tidy Stage from G334_H,G379B_H, G381B_H---------------------------

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
  
# Step 5: Import and Tidy Sonde Data ----------------------------------------------



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

# Step 7: Join Flow and RPA data and save DF --------------------------------------
RPAs_with_Flow <-  RPAs_Sorted %>%
left_join(Combined_BK_Flow ,by=c("Station","Date","Hour")) %>%
filter(is.finite(Flow)) %>%
mutate(Flow=as.numeric(Flow)) %>%
mutate(Season=if_else(between(month(Date),5,11),"Wet Season","Dry Season")) %>%
mutate(`Flow Category` = as.factor(case_when( 
    between(Flow,0,1) ~ "0-1 (cfs)",
    between(Flow,1,100) ~ "1-100 (cfs)",
    between(Flow,100,250) ~ "100-250 (cfs)",
    between(Flow,250,500) ~ "250-500 (cfs)",
    between(Flow,500,1000) ~ "500-1000 (cfs)",
    Flow>1000 ~ "1000+ (cfs)",
    Flow<0 ~ "Reverse Flow"))) %>%
mutate(`Flow Category`=factor(`Flow Category`,levels = c("Reverse Flow", "0-1 (cfs)", "1-100 (cfs)","100-250 (cfs)","250-500 (cfs)","500-1000 (cfs)","1000+ (cfs)")))

write.csv(RPAs_with_Flow, "Data/RPA and Flow.csv")



# Step 8: Join with Stage Data and save DF ----------------------------------------------------

RPAs_with_Flow_Stage <- RPAs_with_Flow %>%
left_join(Combined_Stage ,by=c("Station","Date","Hour","Minute")) %>%
group_by(Station,Date) %>%
mutate(`Stage_24_hour_mean`=mean(Stage)) %>%  
mutate(Stage_Diff_24_hour_mean=Stage-`Stage_24_hour_mean`) 


write.csv(RPAs_with_Flow_Stage, "Data/RPA and Flow and Stage.csv")


# Step 9: Join with Weather data ------------------------------------------

RPAs_with_Flow_Stage_Weather <- RPAs_with_Flow_Stage %>%
left_join(Combined_Weather ,by=c("Date","Hour","Minute"))

write.csv(RPAs_with_Flow_Stage_Weather, "Data/RPA and Flow Stage Weather.csv")
