

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


# G334 continuous TP Load scenarios----------------------------------------------------

Days_with_TPO4 <- RPAs_Sorted %>%                  #DF with days of TP sample collection. 
filter(`Flowpath Region`=="Outflow") %>%  
group_by(`Flowway`,Date) %>%
summarize(`Has Sample`=sum(!is.na(TPO4))) %>%
filter(`Has Sample`>0)

Outflow_TP_Load_Scenarios_1 <- Combined_BK_Flow  %>%
left_join(filter(select(RPAs_Sorted,2:14),`Flowpath Region`=="Outflow") ,by=c("Date","Hour","Flowway")) %>% 
group_by(Flowway)  %>%
mutate(`Date_Time`=ymd_hms(ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York")),`TP interpolated`=TPO4) %>%   #create hourly date time index
mutate(`TP interpolated`=na.approx(`TP interpolated`,along=index(`Date_Time`),na.rm=FALSE))  %>%                
mutate(`Hourly TP LOAD`=if_else(is.finite(Outflow),`TP interpolated`/1000*Outflow*3600*28.31/1000000,0)) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
mutate(`Outflow Lagged 12 hours`=if_else(is.finite(lag(Outflow,12)),lag(Outflow,12),0),`Hourly TP Load Lagged 12 hours`=`TP interpolated`/1000*`Outflow Lagged 12 hours`*3600*28.31/1000000) %>%
filter(is.finite(`TP interpolated`))   %>%
semi_join(Days_with_TPO4,by=c("Date","Flowway")) %>%             #elimiante days from which no TP sample was collected
group_by(Flowway,Date) %>%
mutate(`Outflow 50% Day`=mean(Outflow,na.rm=TRUE)) %>%
mutate(`Outflow 66% Day`=case_when(between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*1.333333333,!between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*.666666666)) %>%  
mutate(`Outflow 66% between 8pm-8am`=case_when(between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*.66666666,!between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*1.333333333)) %>%
mutate(`Outflow 100% Night`=case_when(between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*0,!between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*2)) %>%
mutate(`Outflow 100% between 8am-8pm`=case_when(between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*2,!between(Hour,8,19)~mean(Outflow,na.rm=TRUE)*0)) %>% 
mutate(`Outflow 75% between 12-4AM`=case_when(between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*18/4,!between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*6/20)) %>%
mutate(`Outflow 50% between 12-4AM`=case_when(between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*3,!between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*12/20)) %>%
mutate(`Outflow 100% between 12-4AM`=case_when(between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*6,!between(Hour,1,4)~mean(Outflow,na.rm=TRUE)*0)) %>%
mutate(`Outflow Inverse Diel P Pattern`=case_when(between(Hour,0,1)~mean(Outflow,na.rm=TRUE)*16/10,
                                           between(Hour,2,3)~mean(Outflow,na.rm=TRUE)*14/10,
                                           between(Hour,4,5)~mean(Outflow,na.rm=TRUE)*12/10,
                                           between(Hour,6,7)~mean(Outflow,na.rm=TRUE)*10/10,
                                           between(Hour,8,9)~mean(Outflow,na.rm=TRUE)*8/10,
                                           between(Hour,10,11)~mean(Outflow,na.rm=TRUE)*6/10,
                                           between(Hour,12,13)~mean(Outflow,na.rm=TRUE)*4/10,
                                           between(Hour,14,15)~mean(Outflow,na.rm=TRUE)*6/10,
                                           between(Hour,16,17)~mean(Outflow,na.rm=TRUE)*8/10,
                                           between(Hour,18,19)~mean(Outflow,na.rm=TRUE)*10/10,
                                           between(Hour,20,21)~mean(Outflow,na.rm=TRUE)*12/10,
                                           between(Hour,22,23)~mean(Outflow,na.rm=TRUE)*14/10)) %>%
mutate(`P Load`=if_else(is.finite(Outflow),`TP interpolated`/1000*Outflow*3600*28.31/1000000,0)) %>%  #ppb/1000mg/l*28.31L/cc*60sec/min*60min/hour*1kg/1000g*1g/1000mg  
mutate(`Outflow 50% Day Load`=`TP interpolated`/1000*`Outflow 50% Day`*3600*28.31/1000000) %>%  
mutate(`Outflow 66% Day Load`=`TP interpolated`/1000*`Outflow 66% Day`*3600*28.31/1000000) %>% 
mutate(`Outflow 66% Night Load`=`TP interpolated`/1000*`Outflow 66% between 8pm-8am`*3600*28.31/1000000) %>%
mutate(`Outflow 100% Night Load`=`TP interpolated`/1000*`Outflow 100% Night`*3600*28.31/1000000) %>%
mutate(`Outflow 100% Day Load`=`TP interpolated`/1000*`Outflow 100% between 8am-8pm`*3600*28.31/1000000) %>%
mutate(`Outflow 75% between 12-4AM Load`=`TP interpolated`/1000*`Outflow 75% between 12-4AM`*3600*28.31/1000000) %>%
mutate(`Outflow 50% between 12-4AM Load`=`TP interpolated`/1000*`Outflow 50% between 12-4AM`*3600*28.31/1000000) %>%
mutate(`Outflow 100% between 12-4AM Load`=`TP interpolated`/1000*`Outflow 100% between 12-4AM`*3600*28.31/1000000) %>%
mutate(`Outflow Opposite Diel P Pattern Load`=`TP interpolated`/1000*`Outflow Inverse Diel P Pattern`*3600*28.31/1000000) %>%
group_by(Flowway) %>%
mutate(`Cumulative Flow`=cumsum(`Outflow`)) %>%
mutate(`Cumulative Flow Outflow 75% between 12-4AM`=cumsum(`Outflow 75% between 12-4AM`)) %>%  
mutate(`Cumulative Flow Opposite Diel P Pattern`=cumsum(`Outflow Inverse Diel P Pattern`)) %>%  
mutate(`Measured flow (Baseline)`=cumsum(`P Load`)) %>%
#mutate(`Cumulative P Load 50% Day`=cumsum(`Outflow 50% Day Load`)) %>%
#mutate(`Cumulative P Load 66% Day`=cumsum(`Outflow 66% Day Load`)) %>%
mutate(`66% flow between 10pm-7am`=cumsum(`Outflow 66% Night Load`)) %>%
#mutate(`Cumulative P Load 100% Night`=cumsum(`Outflow 100% Night Load`)) %>%
mutate(`75% flow between 12-4AM`=cumsum(`Outflow 75% between 12-4AM Load`)) %>%  
#mutate(`Cumulative P 50% between 12-4AM`=cumsum(`Outflow 50% between 12-4AM Load`)) %>%  
#mutate(`Cumulative P 100% between 12-4AM`=cumsum(`Outflow 100% between 12-4AM Load`)) %>%  
mutate(`Inverse Diel P Pattern Flow`=cumsum(`Outflow Opposite Diel P Pattern Load`))  %>%  
mutate(`100% flow between 8am-9pm`=cumsum(`Outflow 100% Day Load`))  %>%
pivot_longer(`Measured flow (Baseline)`:`100% flow between 8am-9pm`,names_to = "Scenario", values_to = "Value") %>%
mutate(`Scenario`=factor(`Scenario`,levels = c("100% flow between 8am-9pm", "Measured flow (Baseline)", "66% flow between 10pm-7am","Inverse Diel P Pattern Flow","75% flow between 12-4AM")))


Mean_Flow_by_hour <-Outflow_TP_Load_Scenarios_1 %>%
group_by(Flowway,Hour) %>%
summarise(n=n(),`Hourly Flow 100% night`=mean(`Outflow 100% Night`,na.rm=TRUE),
`Hourly Flow 66% day`=mean(`Outflow 66% Day Load`,na.rm=TRUE),
`Hourly Flow 66% night`=mean(`Outflow 66% between 8pm-8am`,na.rm=TRUE))


Complete_days <- Outflow_TP_Load_Scenarios_1 %>%                  #find complete days
group_by(`Flowway`,Date,Scenario) %>%
summarize(`Total Hours`=sum(!is.na(Hour))) %>%
filter(`Total Hours`==24)

Outflow_TP_Load_Scenarios <- Outflow_TP_Load_Scenarios_1 %>%   #remove incomplete days
semi_join(Complete_days ,by=c("Date","Flowway","Scenario")) 

write.csv(Outflow_TP_Load_Scenarios,"./Data/Outflow_TP_Load_Scenarios.csv")

days_in_scenario <- Outflow_TP_Load_Scenarios %>%
group_by(Flowway,Date) %>%
summarise(`has TP sample`=sum(!is.na(TPO4))) %>%
group_by(Flowway) %>%
summarise(n=n()) 

flow_weighted_mean <- Outflow_TP_Load_Scenarios %>%
group_by(Flowway,Scenario) %>%
summarise(`Cumulative P Load (kg)`=max(Value,na.rm=TRUE),`Cumulative Flow (L)`=max(`Cumulative Flow`,na.rm=TRUE)*27.31*60*60,`Cumulative Flow (acre-ft)`=`Cumulative Flow (L)`/1233000,`FWM Concentration (ug/L)`=`Cumulative P Load (kg)`/`Cumulative Flow (L)`*1000*1000*1000,n=sum(!is.na(Value)),sd=sd(`Value`/`Cumulative Flow (L)`*1000*1000*1000,na.rm = TRUE),se=sd/sqrt(n))

write.csv(flow_weighted_mean,"./Data/Outflow_FWM_TP_Load_Scenarios_Summary.csv")


flow_weighted_mean_table <- flow_weighted_mean%>%
pivot_wider(names_from = Scenario,values_from=`FWM Concentration (ug/L)`)  

flow_weighted_mean_date <- Outflow_TP_Load_Scenarios %>%
group_by(Flowway,Scenario,Date) %>%
summarise(`Cumulative P Load (kg)`=max(Value,na.rm=TRUE),`Cumulative Flow (L)`=max(`Cumulative Flow`,na.rm=TRUE)*27.31*60*60,`FWM Concentration (ug/L)`=`Cumulative P Load (kg)`/`Cumulative Flow (L)`*1000*1000*1000) 

flow_weighted_mean_date_summary <-flow_weighted_mean_date %>%
group_by(Flowway,Scenario) %>%
summarise(`FWM Concentration (ug/L)`=mean(`FWM Concentration (ug/L)`,na.rm=TRUE)) 


  

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



  


