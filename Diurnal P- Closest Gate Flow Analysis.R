
#closest gate analysis- Goal: analyze flow effect from closest gate on Diel P ---------------------------------------------------

#closest gates: STA34 Central flow-way Outflow G379D Inflow G377C. STA34 Western Flow-way Outflow G381B Inflow G380C. STA-2 C3 G334 Inflow G333C


# Import and tidy Flow data ----------------------------------------------------
#G379 Stations - STA-3/4 western flowway
G379D_C_BK <- select(read_csv("Data/G379_C_BK part 1.csv"),date,STATION,VALUE) %>%
bind_rows(select(read_csv("Data/G379_C_BK part 2.csv"),date,STATION,VALUE)) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%
fill(`G379A-C-Q`,`G379B-C-Q`,`G379C-C-Q`,`G379D-C-Q`,`G379E-C-Q`) %>%
select(date,`G379D-C-Q`) %>%
rename(`G379D`="G379D-C-Q")

#G377 Stations  - STA-3/4 central flowway
G377C_C_BK <- select(read_csv("Data/G377_C_BK part 1.csv"),date,STATION,VALUE) %>%
bind_rows(select(read_csv("Data/G377_C_BK part 2.csv"),date,STATION,VALUE)) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%
fill(`G377A-C-Q`,`G377B-C-Q`,`G377C-C-Q`,`G377D-C-Q`,`G377E-C-Q`) %>%
select(date,`G377C-C-Q`) %>%
rename(`G377C`="G377C-C-Q")   

#G381 Stations - STA-3/4 western flowway
G381B_C_BK <- select(read_csv("Data/G381_C_BK part 1.csv"),date,STATION,VALUE) %>%
bind_rows(select(read_csv("Data/G381_C_BK part 2.csv"),date,STATION,VALUE)) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%
fill(`G381A-C-Q`,`G381B-C-Q`,`G381C-C-Q`,`G381D-C-Q`,`G381E-C-Q`,`G381F-C-Q`) %>%
select(date,`G381B-C-Q`) %>%
rename(`G381B`="G381B-C-Q")

#G380 Stations - STA-3/4 western flowway
G380C_C_BK <- select(read_csv("Data/G380_C_BK part 1.csv"),date,STATION,VALUE) %>%
bind_rows(select(read_csv("Data/G380_C_BK part 2.csv"),date,STATION,VALUE)) %>%
bind_rows(select(read_csv("Data/G380_C_BK part 3.csv"),date,STATION,VALUE)) %>%  
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%
fill(`G380A-C-Q`,`G380B-C-Q`,`G380C-C-Q`,`G380D-C-Q`,`G380E-C-Q`,`G380F-C-Q`) %>%
select(date,`G380C-C-Q`) %>%
rename(`G380C`="G380C-C-Q")  

#G334
G334_S_BK <- select(read_csv("Data/G334_S_BK.csv"),date,STATION,VALUE) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
arrange(date) %>%
mutate(G334=VALUE) %>%
select(-STATION,-VALUE)

#G333 stations  -STA-2 central flowway
G333C_C_BK <- read_csv("Data/G333_C_BK.csv") %>%
select(date,STATION,VALUE) %>%
filter(!is.na(STATION)) %>%
mutate(date=mdy_hm(date)) %>%  
distinct(date,STATION,.keep_all = TRUE) %>%     #Required to remove intances where there are multiple values in a single minute
pivot_wider(names_from = STATION, values_from = VALUE) %>%
arrange(date) %>%
fill(`G333A-C-Q`,`G333B-C-Q`,`G333C-C-Q`,`G333D-C-Q`,`G333E-C-Q`) %>%
select(date,`G333C-C-Q`) %>%
rename(`G333C`="G333C-C-Q")  

Closest_Gate_Combined_BK_Flow_step1 <-  setNames(as.data.frame(seq(from=ISOdate(2012,7,01,0,0,0,tz = "US/Eastern"), to=ISOdate(2017,10,01,0,0,0,tz = "US/Eastern"),by = "min")),"date") %>%
left_join(G379D_C_BK,by="date") %>%  #combine data from G381, G334, G379D
left_join(G381B_C_BK,by="date") %>%
left_join(G334_S_BK,by="date")  %>%
left_join(G333C_C_BK,by="date")  %>%
left_join(G380C_C_BK,by="date")  %>%
left_join(G377C_C_BK,by="date")  %>%
select(date,G381B,G379D,G334,G333C,G380C,G377C) %>%
arrange(date)  %>%
fill(G381B,G379D,G334,G333C,G380C,G377C) 

Closest_Gate_Combined_BK_Flow <-Closest_Gate_Combined_BK_Flow_step1 %>% 
gather("Station","Flow",G381B,G379D,G334,G333C,G380C,G377C) %>%
mutate(`Flowway` = case_when(`Station`=="G334"~"STA-2 Central",`Station`=="G379D"~"STA-3/4 Central",`Station`=="G377C"~"STA-3/4 Central",`Station`=="G381B"~"STA-3/4 Western",`Station`=="G380C"~"STA-3/4 Western",`Station`=="G333C"~"STA-2 Central")) %>%        #Add flowway info to RPA data
mutate(`Flowpath Region` = case_when(`Station`=="G334"~"Outflow",`Station`=="G379D"~"Outflow",`Station`=="G377C"~"Inflow",`Station`=="G381B"~"Outflow",`Station`=="G333C"~"Inflow",`Station`=="G380C"~"Inflow"))  %>%     #Add flowpath position
mutate(`Outflow` = case_when(`Flowway` == "STA-2 Central" & `Flowpath Region`=="Outflow" ~Flow,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Outflow"~Flow,`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Outflow"~Flow)) %>% 
mutate(`Inflow` = case_when(`Flowway` == "STA-2 Central"  & `Flowpath Region`=="Inflow" ~Flow,`Flowway` == "STA-3/4 Central" & `Flowpath Region`=="Inflow"~Flow,`Flowway` == "STA-3/4 Western" & `Flowpath Region`=="Inflow"~Flow))  %>%
mutate(Date=as.Date(date)) %>%
mutate(Hour=hour(round_date(date, unit = "hour"))) %>%
group_by(`Flowway`,Date,Hour) %>%
summarise(Outflow=mean(Outflow,na.rm = TRUE),Inflow=mean(Inflow,na.rm = TRUE)) #

write.csv(Closest_Gate_Combined_BK_Flow, "Data/Closeset_gate_Combined_BK_Flow.csv",row.names=FALSE)



# Import and Tidy RPA data ------------------------------------------------
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

# Join Flow data with RPA TP Data -----------------------------------------

RPAs_with_Flow_Closest_Gate <-  RPAs_Sorted %>%
left_join(Closest_Gate_Combined_BK_Flow ,by=c("Date","Hour","Flowway")) %>%
filter(is.finite(Outflow) || is.finite(Inflow)) %>%     #need inflow data
mutate(Outflow=as.numeric(Outflow)) %>%
mutate(Season=if_else(between(month(Date),5,11),"Wet Season","Dry Season")) %>%
mutate(`Outflow Category` = as.factor(case_when( between(Outflow,0,1) ~ "0-1 (cfs)",between(Outflow,1,100) ~ "1-100 (cfs)",between(Outflow,100,200) ~ "100-200 (cfs)", Outflow>200 ~ "200+ (cfs)", Outflow<0 ~ "Reverse Flow"))) %>%
mutate(`Inflow Category` = as.factor(case_when( between(Inflow,0,1) ~ "0-1 (cfs)",between(Inflow,1,100) ~ "1-100 (cfs)",between(Inflow,100,200) ~ "100-200 (cfs)", Inflow>200 ~ "200+ (cfs)", Inflow < 0 ~ "Reverse Flow"))) %>%
mutate(`Inflow Category`=factor(`Inflow Category`,levels = c("Reverse Flow","0-1 (cfs)","1-100 (cfs)","100-200 (cfs)","200+ (cfs)"))) %>%
mutate(`Outflow Category`=factor(`Outflow Category`,levels = c("Reverse Flow","0-1 (cfs)","1-100 (cfs)","100-200 (cfs)","200+ (cfs)"))) 

RPAs_with_Flow_Closest_Gate <-  RPAs_Sorted %>%
  left_join(Closest_Gate_Combined_BK_Flow ,by=c("Date","Hour","Flowway")) %>%
  filter(is.finite(Outflow) || is.finite(Inflow)) %>%     #need inflow data
  mutate(Outflow=as.numeric(Outflow)) %>%
  mutate(Season=if_else(between(month(Date),5,11),"Wet Season","Dry Season")) %>%
  mutate(`Outflow Category` = as.factor(case_when( between(Outflow,0,1) ~ "0-1 (cfs)",between(Outflow,1,75) ~ "1-75 (cfs)",between(Outflow,75,150) ~ "75-150 (cfs)", Outflow>150 ~ "150+ (cfs)", Outflow<0 ~ "Reverse Flow"))) %>%
  mutate(`Inflow Category` = as.factor(case_when( between(Inflow,0,1) ~ "0-1 (cfs)",between(Inflow,1,75) ~ "1-75 (cfs)",between(Inflow,75,150) ~ "75-150 (cfs)", Inflow>150 ~ "150+ (cfs)", Inflow < 0 ~ "Reverse Flow"))) %>%
  mutate(`Inflow Category`=factor(`Inflow Category`,levels = c("Reverse Flow","0-1 (cfs)","1-75 (cfs)","75-150 (cfs)","150+ (cfs)"))) %>%
  mutate(`Outflow Category`=factor(`Outflow Category`,levels = c("Reverse Flow","0-1 (cfs)","1-75 (cfs)","75-150 (cfs)","150+ (cfs)"))) 


# Visualize Data ----------------------------------------------------------
#histogram of Inflow categories
ggplot(RPAs_with_Flow_Closest_Gate,aes(`Inflow`))+geom_histogram()+facet_wrap(~Station,scales="free")+theme_bw()

Closest_Gate_Days_with_continual_flow <- Closest_Gate_Combined_BK_Flow %>%
group_by(`Flowway`,Date) %>%
summarize(`Min Outflow`=min(Outflow,na.rm=TRUE),`Mean Outflow`=mean(Outflow,na.rm=TRUE),`Max Outflow`=max(Outflow,na.rm=TRUE),`Min Inflow`=min(Inflow,na.rm=TRUE),`Mean Inflow`=mean(Inflow,na.rm=TRUE),`Max Inflow`=max(Inflow,na.rm=TRUE)) %>% 
mutate(`Continuous OutFlow`=ifelse(`Min Outflow`>=`Mean Outflow`*.66 &`Max Outflow` <=`Mean Outflow`*1.33,TRUE,FALSE)) %>% #days with all outflow within 33% of mean
mutate(`Continuous InFlow`=ifelse(`Min Inflow`>=`Mean Inflow`*.66 &`Max Inflow` <=`Mean Inflow`*1.33,TRUE,FALSE)) %>%   #days with all inflow within 33% of mean
select(`Flowway`,Date,`Continuous OutFlow`,`Continuous InFlow`)

#RPAS with flow data from days of continuous flow only
Closest_Gate_RPAs_with_Flow_Complete_Days <-  RPAs_with_Flow_Closest_Gate %>%
left_join(Closest_Gate_Days_with_continual_flow)


Closest_Gate_RPAs_with_Flow_Complete_Days %>%
filter(!is.na(TPO4)) %>%
summarise(`Day of continuous inflow`=sum(`Continuous InFlow`),`Day of continuous outflow`=sum(`Continuous OutFlow`),
          `Day of continuous inflow and outflow`=sum(ifelse(`Continuous OutFlow`== TRUE & `Continuous InFlow`==TRUE,T,F)))

#Hourly TP Variation from the Daily Median by inflow category 
ggplot(filter(Closest_Gate_RPAs_with_Flow_Complete_Days ,`Inflow Category`!="Reverse Flow",Station_ID %in% c("G333C","G377C","G380C"),`Continuous InFlow`==TRUE),aes(Time,Diff_24_hour_median,color=Station_ID,fill=Station_ID))+geom_point(shape=21)+geom_smooth(method="loess",color="black",fill="grey",method.args = list(family = "symmetric",degree=2))+
theme_bw()+facet_grid(`Inflow Category`~Station_ID)+scale_colour_brewer( type = "qual", palette = "Set2",guide = 'none')+scale_fill_brewer( type = "qual", palette = "Set2",name="Station")+
geom_hline(yintercept=0)+scale_y_continuous(breaks = seq(-10,10,1))+theme(legend.position="bottom")+coord_cartesian(ylim = c(-10,10))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Hourly Deviation from Daily Median by Inflow Strength at Closest Gate",y="TPO4 Deviation from daily median (ug/L)",x="Hour",color=NULL)

ggsave("Figures/Hourly Deviation from Daily Median by Inflow Strength at Closest Gate.jpeg", plot = last_plot(), width = 10, height = 11, units = "in", dpi = 300, limitsize = TRUE)

#Hourly TP Variation from the Daily Median by Outflow category 
ggplot(filter(Closest_Gate_RPAs_with_Flow_Complete_Days ,`Outflow Category`!="Reverse Flow",Station_ID %in% c("G334","G379D","G381B")) ,aes(Time,Diff_24_hour_median,color=Station_ID,fill=Station_ID))+geom_point(shape=21)+geom_smooth(method="loess",color="black",fill="grey",method.args = list(family = "symmetric",degree=2))+
theme_bw()+facet_grid(`Outflow Category`~Station_ID)+scale_colour_brewer( type = "qual", palette = "Set2",guide = 'none')+scale_fill_brewer( type = "qual", palette = "Set2",name="Station")+
geom_hline(yintercept=0)+scale_y_continuous(breaks = seq(-10,10,1))+theme(legend.position="bottom")+coord_cartesian(ylim = c(-10,10))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Hourly Deviation from Daily Median by Outflow Strength at Closest Gate",y="TPO4 Deviation from daily median (ug/L)",x="Hour",color=NULL)

ggsave("Figures/Hourly Deviation from Daily Median by Outflow Strength at Closest Gate.jpeg", plot = last_plot(), width = 10, height = 11, units = "in", dpi = 300, limitsize = TRUE)




