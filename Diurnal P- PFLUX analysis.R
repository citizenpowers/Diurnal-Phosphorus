
library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(maptools)

# Import Data -------------------------------------------------------------

PFLUX_WQ_Data <- read_excel("Data/PFLUX WQ LIMSP.xlsx")  #WQ Data

#Sonde Data 
PFLUX_Sonde_Data_STA2C1 <- read_excel("Data/All Combined Sonde Data STA2C1.xlsx",sheet = "Combined Data")
PFLUX_Sonde_Data_STA2C3 <-read_excel("Data/STA2C3 All Sonde Data.xlsx")

# Tidy WQ Data ---------------------------------------------------------------

PFLUX_WQ_Data_tidy <- PFLUX_WQ_Data %>%
filter(COLLECT_METHOD %in% c("ACT","ADT"),TEST_NAME=="TPO4" ,SAMPLE_TYPE=="SAMP") %>%
mutate(FST=mdy_hms(FIRST_TRIGGER_DATE))  %>%
mutate(Date=as.Date(FST),Year=year(FST),Month=month(FST,label=TRUE),Day=day(FST),Hour=hour(FST),Minute=minute(FST),Time=hour(FST)+ minute(FST)/60) %>%
group_by(STATION,Year,Day,Month) %>%
mutate(TPO4=VALUE*1000,Station=as.character(STATION)) %>%  
mutate(Station=if_else(Station=="ST2C1H2","ST2C1H3",Station)) %>%  #combine H2 and H3 since they are close together 
mutate(Station=if_else(Station=="ST2C1B2","ST2C1C3",Station)) %>%  #combine B2 and C3 since they are close together 
#filter(TPO4<250) %>% #remove extreme values from likely particulates
mutate(RANK=row_number(TPO4))  %>%
mutate(`24_hour_mean`=mean(TPO4,na.rm=TRUE),`24_hour_median`=median(TPO4,na.rm=TRUE)) %>%
mutate(Diff_24_hour_mean=TPO4-`24_hour_mean`,Diff_24_hour_median=TPO4-`24_hour_median`) %>%
mutate(`Percent difference from daily mean`=(Diff_24_hour_mean/`24_hour_mean`)*100,`Percent difference from daily median`=(Diff_24_hour_median/`24_hour_median`)*100) %>%   #removed G330D as it only has 3 collection times
filter(Station %in% c("ST2C1A3","ST2C1C3","ST2C1F3","ST2C1G3","ST2C1H2","ST2C1H3","ST2C1OUT","ST2C3C128","ST2C3C164","ST2C3C20","ST2C3C200","ST2C3C56","ST2C3C92")) %>%
mutate(`Flowway` = case_when(`Station` %in% c("ST2C3C128","ST2C3C164","ST2C3C20","ST2C3C200","ST2C3C56","ST2C3C92")~"STA-2 FW3",
                             `Station` %in% c("ST2C1A3","ST2C1C3","ST2C1F3","ST2C1G3","ST2C1H2","ST2C1H3","ST2C1OUT") ~ "STA-1 FW1",
                              TRUE~as.character(Station)))       #Add flowway info to RPA data
test <- select(PFLUX_WQ_Data_tidy,FST,Date,Year,Month,Day,Hour,Minute,Time)  


# Tidy Sonde Data ---------------------------------------------------------

PFLUX_Sonde_Data_tidy <-PFLUX_Sonde_Data_STA2C1 %>%
select(1:2,4,7:18) %>%
rename(Turbidity="Turbidity NTU") %>%  #Turbidy measued in NTU
bind_rows(rename(select(PFLUX_Sonde_Data_STA2C3,1:2,4,8:11,13:16,18:19,21,23),Turbidity="Turbidity FNU")) %>%  #Turbidity Measured in FNU
mutate(Date=ymd(`Date (MM/DD/YYYY)`),Year=year(`Date (MM/DD/YYYY)`),Month=month(`Date (MM/DD/YYYY)`,label=TRUE),Day=day(`Date (MM/DD/YYYY)`)) %>%
mutate(Hour=hour(`Time (HH:MM:SS)`),Minute=minute(`Time (HH:MM:SS)`),Time=hour(`Time (HH:MM:SS)`)+ minute(`Time (HH:MM:SS)`)/60) %>%
rename(Station="Site Name")  %>%
group_by(Date,Hour,Station) %>%  
summarise(`Avg Hourly Temp`=mean(`Temp °C`,na.rm = TRUE),`Avg Hourly SpCond`=mean(`SpCond µS/cm`,na.rm = TRUE),`Avg Hourly DO`=mean(`ODO mg/L`,na.rm = TRUE),`Avg Hourly pH`=mean(pH,na.rm = TRUE),`Avg Hourly Chlorophyll`=mean(`Chlorophyll µg/L`,na.rm = TRUE)) %>%  
group_by(Date,Station) %>%
mutate(Temp_Diff_24_hour_mean=`Avg Hourly Temp`-mean(`Avg Hourly Temp`),SpCond_Diff_24_hour_mean=`Avg Hourly SpCond`-mean(`Avg Hourly SpCond`),
DO_Diff_24_hour_mean=`Avg Hourly DO`-mean(`Avg Hourly DO`),pH_Diff_24_hour_mean=`Avg Hourly pH`-mean(`Avg Hourly pH`),Chlorophyll_Diff_24_hour_mean=`Avg Hourly Chlorophyll`-mean(`Avg Hourly Chlorophyll`),
`DO Percent Diff from 24 mean`=DO_Diff_24_hour_mean/mean(`Avg Hourly DO`)*100,`pH Percent Diff from 24 mean`=pH_Diff_24_hour_mean/mean(`Avg Hourly pH`)*100,
`SpCond Percent Diff from 24 mean`=SpCond_Diff_24_hour_mean/mean(`Avg Hourly SpCond`)*100,`Temp Percent Diff from 24 mean`=Temp_Diff_24_hour_mean/mean(`Avg Hourly Temp`)*100,
Temp_Diff_24_hour_median=`Avg Hourly Temp`-median(`Avg Hourly Temp`),SpCond_Diff_24_hour_median=`Avg Hourly SpCond`-median(`Avg Hourly SpCond`),
DO_Diff_24_hour_median=`Avg Hourly DO`-median(`Avg Hourly DO`),pH_Diff_24_hour_median=`Avg Hourly pH`-median(`Avg Hourly pH`),
`DO Percent Diff from 24 median`=DO_Diff_24_hour_median/median(`Avg Hourly DO`)*100,`pH Percent Diff from 24 median`=pH_Diff_24_hour_median/median(`Avg Hourly pH`)*100,
`SpCond Percent Diff from 24 median`=SpCond_Diff_24_hour_median/median(`Avg Hourly SpCond`)*100,`Temp Percent Diff from 24 median`=Temp_Diff_24_hour_median/median(`Avg Hourly Temp`)*100) %>%
mutate(`Station` = case_when(`Station` == "STA2C1F3"~"ST2C1F3", `Station` == "STA2C1A3"~"ST2C1A3",`Station` == "STA2C1Out"~"ST2C1OUT",`Station` == "STA2C1B2"~"ST2C1C3",
                             `Station` == "STA2C1G3"~"ST2C1G3",`Station` == "STA2C1H2"~"ST2C1H3",TRUE~as.character(Station)))




# Join Sonde and WQ Data --------------------------------------------------

PFLUX_WQ_Sonde_tidy <- PFLUX_WQ_Data_tidy %>%
left_join(PFLUX_Sonde_Data_tidy ,by=c("Date","Hour","Station")) %>%
mutate(`Station` = factor(Station, levels = c("ST2C1A3","ST2C1C3","ST2C1F3","ST2C1G3","ST2C1H2","ST2C1H3","ST2C1OUT","ST2C3C20","ST2C3C56","ST2C3C92","ST2C3C128","ST2C3C164","ST2C3C200"))) 

# WQ Summary Table -----------------------------------------------------------

PFLUX_WQ_Data_Summary <-PFLUX_WQ_Data_tidy %>%
group_by(Station) %>%
summarise(n=n(),`min TPO4`=min(TPO4,na.rm=TRUE),`max TPO4`=max(TPO4,na.rm=TRUE),mean=mean(TPO4,na.rm=TRUE),median=median(TPO4,na.rm=TRUE),`Q1`=quantile(TPO4,.25,na.rm = TRUE),`Q3`=quantile(TPO4,.75,na.rm = TRUE),IQR=Q3-Q1,`Mild Outliers`=Q3+1.5*IQR,`Extreme Outliers`=Q3+3*IQR,
            `TPO4 Observations`=sum(is.finite(TPO4)),`NAs Observed`=sum(is.na(TPO4)),`Below detection`=sum(if_else(TPO4<1,1,0),na.rm=TRUE),`above outlier`=sum(if_else(TPO4>`Mild Outliers`,1,0),na.rm=TRUE),
            `above extreme outlier`=sum(if_else(TPO4>`Extreme Outliers`,1,0),na.rm=TRUE),`% mild outliers`=percent(`above outlier`/n()),`% extreme mild outliers`=percent(`above extreme outlier`/n()))



# WQ Figures -----------------------------------------------------------------

Custom_theme <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),axis.text.y = element_text(size=12),axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
title =element_text(size = 15),strip.text.x = element_text(size =12),legend.text = element_text(size = 12))


#Hourly TP Variation from the Daily Mean by Station
ggplot(PFLUX_WQ_Sonde_tidy ,aes(Time,Diff_24_hour_median,color=Station,fill=Station))+geom_point(shape=21)+geom_smooth(method="loess",color="black",fill="grey",method.args = list(family = "symmetric",degree=2))+theme_bw()+
facet_wrap(~Station,nrow = 2)+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+Custom_theme+theme(legend.position="bottom")+
labs(title="Hourly Variation from Daily Median from PFLUX Autosamplers",y=bquote("Deviation from daily median TP"~~(mu*g~L^-1)),x="Hour")

ggsave("Figures/Hourly Variation from Daily Median from PFLUX Autosamplers.jpeg", plot = last_plot(), width = 8, height = 9, units = "in", dpi = 300, limitsize = TRUE)


#Hourly % TP Variation from the Daily Median by Station
ggplot(PFLUX_WQ_Data_tidy,aes(Time,`Percent difference from daily median`,color=Station,fill=Station))+geom_point(shape=21)+
geom_smooth(method="loess",color="black",fill="grey",method.args = list(family = "symmetric",degree=2))+theme_bw()+
facet_wrap(~Station,nrow = 2)+geom_hline(yintercept=0)+geom_vline(xintercept = c(4,8,12,16,20),linetype="dashed",color="grey")+
scale_y_continuous(breaks = seq(-50,50,5))+#scale_fill_brewer( type = "qual", palette = "Set2")+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+coord_cartesian(ylim = c(-50,50))+theme(legend.position="bottom")+guides(fill=guide_legend(title="Station"))+
Custom_theme+
labs(title="Hourly Percent Variation from Daily Median",y="Deviation from daily median TP (%)",x="Hour")

ggsave("Figures/Hourly Percent Variation from Daily Median from PFLUX Autosamplers.jpeg", plot = last_plot(), width = 8, height = 9, units = "in", dpi = 300, limitsize = TRUE)



# Sonde figs --------------------------------------------------------------


Sonde_only_differences_absolute <- filter(pivot_longer(PFLUX_WQ_Sonde_tidy,`Temp_Diff_24_hour_mean`:`pH_Diff_24_hour_mean`,names_to="Parameter",values_to="Value"),is.finite(Value)) %>%
mutate(`Parameter Labels` = case_when(Parameter=="Temp_Diff_24_hour_mean"~paste0("Temperature C",intToUtf8(176)),
                                        Parameter=="SpCond_Diff_24_hour_mean"~paste0("Specific Conductivity (",intToUtf8(956),"g L",intToUtf8(8315),intToUtf8(185),")"),
                                        Parameter=="DO_Diff_24_hour_mean"~paste0("Dissolved Oxygen (mg L",intToUtf8(8315),intToUtf8(185),")"),
                                        Parameter=="pH_Diff_24_hour_mean"~"pH"))  

Sonde_only_percent_diff <- filter(pivot_longer(PFLUX_WQ_Sonde_tidy,`DO Percent Diff from 24 median`:`Temp Percent Diff from 24 median`,names_to="Parameter",values_to="Value"),is.finite(Value)) %>%
mutate(`Parameter Labels` = case_when(Parameter=="Temp Percent Diff from 24 median"~paste0("Temperature C",intToUtf8(176)),
                                        Parameter=="SpCond Percent Diff from 24 median"~paste0("Specific Conductivity (",intToUtf8(956),"g L",intToUtf8(8315),intToUtf8(185),")"),
                                        Parameter=="DO Percent Diff from 24 median"~paste0("Dissolved Oxygen (mg L",intToUtf8(8315),intToUtf8(185),")"),
                                        Parameter=="pH Percent Diff from 24 median"~"pH"))        

#Sonde parameters over time
ggplot(Sonde_only_differences_absolute,aes(Date,Value,color=Station))+
geom_point()+theme_bw()+facet_wrap(~Parameter,scales = "free")

Sonde_only_differences_absolute %>% distinct(Station,Date) %>% summarise(n=n())


#Diel varaiation Sonde and TPO4 natural units
ggplot(Sonde_only_differences_absolute,aes(Time,Value))+geom_point(shape=21,fill="#fdb462",color="#fdb462",size=2,alpha=.5)+
geom_point(aes(Time,Diff_24_hour_median),fill="#bebada",color="#bebada",shape=21,size=2,alpha=.5)+
geom_smooth(aes(Time,Diff_24_hour_median), method="loess",color="purple",fill="grey",method.args = list(family = "symmetric",degree=2))+theme_bw()+
geom_smooth(method="loess",color="dark orange",fill="grey",method.args = list(family = "symmetric",degree=2))+
facet_wrap(~`Parameter Labels`,scales="free",nrow = 1)+
scale_colour_brewer( type = "qual", palette = "Set2",guide = 'none')+
scale_fill_brewer( type = "qual", palette = "Set2",name="Station")+
geom_hline(yintercept=0)+theme(legend.position="bottom")+scale_y_continuous(breaks = pretty_breaks(n=5))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Physicochemical and Diel Phosphorus Patterns",x="Hour",y="TPO4 Deviation from daily median (ug/L)",color=NULL)

ggsave("Figures/Physicochemical Patterns compared to Diel Phosphorus Pattern.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Sonde 
ggplot(Sonde_only_differences_absolute,aes(Value,Diff_24_hour_mean,color=Station))+geom_point()+theme_bw()+facet_wrap(~Parameter,scales = "free")+geom_smooth(method="lm")+
scale_y_continuous(limits = c(-5,5),breaks = seq(-5,5,1))+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="Sonde Parameters vs Deviation in Daily P",y="TPO4 Deviation from daily median (ug/L)",x="Value")

ggsave("Figures/Sonde Parameters vs Deviation in Daily P.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


#Diel varaiation Sonde and TPO4 percent difference pH only
ggplot(filter(Sonde_only_percent_diff,Parameter=="pH Percent Diff from 24 median"),aes(Time,Value/100, color=Station))+geom_point(shape=21,fill="#fdb462",color="#fdb462",size=2,alpha=.5)+
geom_point(aes(Time,`Percent difference from daily median`/100),fill="#bebada",color="#bebada",shape=21,size=2,alpha=.5)+facet_wrap(Flowway~Station, labeller = label_wrap_gen(multi_line=FALSE),nrow=2)+
geom_smooth(aes(Time,`Percent difference from daily median`/100), method="loess",color="purple",fill="grey",method.args = list(family = "symmetric",degree=2))+theme_bw()+
geom_smooth(method="loess",color="dark orange",fill="grey",method.args = list(family = "symmetric",degree=2))+
geom_hline(yintercept=0)+theme(legend.position="bottom")+scale_y_continuous(breaks = pretty_breaks(n=5),labels = scales::percent)+coord_cartesian(ylim = c(-.15,.15))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+Custom_theme+
labs(x="Hour",y="Deviation from Daily Median (%)",title = "Diel P and pH patterns from PFLUX data")

ggsave("Figures/Diel P and pH patterns from PFLUX data.jpeg", plot = last_plot(), width = 14, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Diel variation Sonde and TPO4 percent difference SPCOnd only
ggplot(filter(Sonde_only_percent_diff,Parameter=="SpCond Percent Diff from 24 median"),aes(Time,Value/100, color=Station))+geom_point(shape=21,fill="#fdb462",color="#fdb462",size=2,alpha=.5)+
geom_point(aes(Time,`Percent difference from daily median`/100),fill="#bebada",color="#bebada",shape=21,size=2,alpha=.5)+facet_wrap(Flowway~Station, labeller = label_wrap_gen(multi_line=FALSE),nrow=2)+
geom_smooth(aes(Time,`Percent difference from daily median`/100), method="loess",color="purple",fill="grey",method.args = list(family = "symmetric",degree=2))+theme_bw()+
geom_smooth(method="loess",color="dark orange",fill="grey",method.args = list(family = "symmetric",degree=2))+
geom_hline(yintercept=0)+theme(legend.position="bottom")+scale_y_continuous(breaks = pretty_breaks(n=5),labels = scales::percent)+coord_cartesian(ylim = c(-.15,.15))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+Custom_theme+
labs(x="Hour",y="Deviation from Daily Median (%)",title = "Diel P and Specific Conductivity patterns from PFLUX data")

ggsave("Figures/Diel P and Specific Conductivity patterns from PFLUX data.jpeg", plot = last_plot(), width = 14, height = 8, units = "in", dpi = 300, limitsize = TRUE)


#Diel varaiation Sonde and TPO4 percent difference TEMP only
ggplot(filter(Sonde_only_percent_diff,Parameter=="Temp Percent Diff from 24 median"),aes(Time,Value/100, color=Station))+geom_point(shape=21,fill="#fdb462",color="#fdb462",size=2,alpha=.5)+
geom_point(aes(Time,`Percent difference from daily median`/100),fill="#bebada",color="#bebada",shape=21,size=2,alpha=.5)+facet_wrap(Flowway~Station, labeller = label_wrap_gen(multi_line=FALSE),nrow=2)+
geom_smooth(aes(Time,`Percent difference from daily median`/100), method="loess",color="purple",fill="grey",method.args = list(family = "symmetric",degree=2))+theme_bw()+
geom_smooth(method="loess",color="dark orange",fill="grey",method.args = list(family = "symmetric",degree=2))+Custom_theme+
geom_hline(yintercept=0)+theme(legend.position="bottom")+scale_y_continuous(breaks = pretty_breaks(n=5),labels = scales::percent)+coord_cartesian(ylim = c(-.15,.15))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(x="Hour",y="Deviation from Daily Median (%)",title = "Diel P and Teperature Patterns from PFLUX data")

ggsave("Figures/Diel P and Teperature Patterns from PFLUX data.jpeg", plot = last_plot(), width = 14, height = 8, units = "in", dpi = 300, limitsize = TRUE)


#Diel varaiation Sonde and TPO4 percent difference DO only
ggplot(filter(Sonde_only_percent_diff,Parameter=="DO Percent Diff from 24 median"),aes(Time,Value/100, color=Station))+geom_point(shape=21,fill="#fdb462",color="#fdb462",size=2,alpha=.5)+
geom_point(aes(Time,`Percent difference from daily median`/100),fill="#bebada",color="#bebada",shape=21,size=2,alpha=.5)+facet_wrap(Flowway~Station, labeller = label_wrap_gen(multi_line=FALSE),nrow=2)+
geom_smooth(aes(Time,`Percent difference from daily median`/100), method="loess",color="purple",fill="grey",method.args = list(family = "symmetric",degree=2))+theme_bw()+
geom_smooth(method="loess",color="dark orange",fill="grey",method.args = list(family = "symmetric",degree=2))+Custom_theme+
geom_hline(yintercept=0)+theme(legend.position="bottom")+scale_y_continuous(breaks = pretty_breaks(n=5),labels = scales::percent)+coord_cartesian(ylim = c(-1,1.5))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(x="Hour",y="Deviation from Daily Median (%)",title = "Diel P and Dissolved Oxygen Patterns from PFLUX data")

ggsave("Figures/Diel P and Dissolved Oxygen Patterns from PFLUX data.jpeg", plot = last_plot(), width = 14, height = 8, units = "in", dpi = 300, limitsize = TRUE)

