
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

PFLUX_WQ_Data <- read_excel("Data/PFLUX WQ LIMSP.xlsx")

distinct(PFLUX_WQ_Data_tidy,Station)

# Tidy Data ---------------------------------------------------------------

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
mutate(`Station` = factor(Station, levels = c("ST2C1A3","ST2C1C3","ST2C1F3","ST2C1G3","ST2C1H2","ST2C1H3","ST2C1OUT","ST2C3C20","ST2C3C56","ST2C3C92","ST2C3C128","ST2C3C164","ST2C3C200"))) %>%
mutate(`Flowway` = case_when(`Station` %in% c("ST2C3C128","ST2C3C164","ST2C3C20","ST2C3C200","ST2C3C56","ST2C3C92")~"STA-2 Central",
                             `Station` %in% c("ST2C1A3","ST2C1C3","ST2C1F3","ST2C1G3","ST2C1H2","ST2C1H3","ST2C1OUT") ~ "STA-1 Eastern",
                              TRUE~as.character(Station)))       #Add flowway info to RPA data
  

  



# Summary Table -----------------------------------------------------------

PFLUX_WQ_Data_Summary <-PFLUX_WQ_Data_tidy %>%
group_by(Station) %>%
summarise(n=n(),`min TPO4`=min(TPO4,na.rm=TRUE),`max TPO4`=max(TPO4,na.rm=TRUE),mean=mean(TPO4,na.rm=TRUE),median=median(TPO4,na.rm=TRUE),`Q1`=quantile(TPO4,.25,na.rm = TRUE),`Q3`=quantile(TPO4,.75,na.rm = TRUE),IQR=Q3-Q1,`Mild Outliers`=Q3+1.5*IQR,`Extreme Outliers`=Q3+3*IQR,
            `TPO4 Observations`=sum(is.finite(TPO4)),`NAs Observed`=sum(is.na(TPO4)),`Below detection`=sum(if_else(TPO4<1,1,0),na.rm=TRUE),`above outlier`=sum(if_else(TPO4>`Mild Outliers`,1,0),na.rm=TRUE),
            `above extreme outlier`=sum(if_else(TPO4>`Extreme Outliers`,1,0),na.rm=TRUE),`% mild outliers`=percent(`above outlier`/n()),`% extreme mild outliers`=percent(`above extreme outlier`/n()))

Hourly_Median_Summary <-PFLUX_WQ_Data_tidy %>%
group_by(Hour,Station) %>%
summarise(median=median(TPO4,na.rm=TRUE)) %>%
pivot_wider(names_from = "Station",values_from=median)

# Figures -----------------------------------------------------------------

Custom_theme <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),axis.text.y = element_text(size=12),axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),
title =element_text(size = 15),strip.text.x = element_text(size =12),legend.text = element_text(size = 12))


#Hourly TP Variation from the Daily Mean by Station
ggplot(PFLUX_WQ_Data_tidy ,aes(Time,Diff_24_hour_median,color=Station,fill=Station))+geom_point(shape=21)+geom_smooth(method="loess",color="black",fill="grey",method.args = list(family = "symmetric",degree=2))+theme_bw()+
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
