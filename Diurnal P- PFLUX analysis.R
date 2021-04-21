
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

distinct(PFLUX_WQ_Data ,SAMPLE_TYPE)

# Tidy Data ---------------------------------------------------------------

PFLUX_WQ_Data_tidy <- PFLUX_WQ_Data %>%
filter(COLLECT_METHOD %in% c("ACT","ADT"),TEST_NAME=="TPO4" ,SAMPLE_TYPE=="SAMP") %>%
mutate(FIRST_TRIGGER_DATE=mdy_hms(FIRST_TRIGGER_DATE))  %>%
mutate(Date=as.Date(FIRST_TRIGGER_DATE),Year=year(FIRST_TRIGGER_DATE),Month=month(FIRST_TRIGGER_DATE,label=TRUE),Day=day(FIRST_TRIGGER_DATE),Hour=hour(FIRST_TRIGGER_DATE),Minute=minute(FIRST_TRIGGER_DATE),Time=hour(FIRST_TRIGGER_DATE)+ minute(FIRST_TRIGGER_DATE)/60) %>%
group_by(STATION,Year,Day,Month) %>%
mutate(TPO4=VALUE*1000,Station=STATION) %>%  
mutate(RANK=row_number(TPO4))  %>%
mutate(`24_hour_mean`=mean(TPO4,na.rm=TRUE),`24_hour_median`=median(TPO4,na.rm=TRUE)) %>%
mutate(Diff_24_hour_mean=TPO4-`24_hour_mean`,Diff_24_hour_median=TPO4-`24_hour_median`) %>%
mutate(`Percent difference from daily mean`=(Diff_24_hour_mean/`24_hour_mean`)*100,`Percent difference from daily median`=(Diff_24_hour_median/`24_hour_median`)*100) 





# Figures -----------------------------------------------------------------

#Hourly TP Variation from the Daily Mean by Station
ggplot(PFLUX_WQ_Data_tidy ,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-100,100),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")
