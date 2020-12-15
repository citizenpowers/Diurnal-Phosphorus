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
library(ggpmisc)


# Import data for RPA analysis -------------------------------------------------------------
#RPA raw data
RPAs <-  read_excel("Data/Outflows.xlsx", col_types = c("text", "date", "numeric",  "numeric")) 

#RPA tidy data
RPAs_Sorted <- read_csv("Data/RPAs Sorted.csv")

#Import Flow Data
Combined_BK_Flow <- read_csv("Data/Combined_BK_Flow.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d"),  Inflow = col_number(), `Inflow HLR` = col_number(), Outflow = col_number(), `Outflow HLR` = col_number()))

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

#Import RPA Flow and Stage and weather ands Inflow TP data
RPAs_with_Flow_Stage_Weather_Sonde_Inflow_TP <- read_csv("Data/RPA and Flow Stage Weather Sonde Inflow TP.csv")

#sta2 c3 =2400 acres
#STA34 C3B 2087 acres
#STA34 C2B 2375 acres

# Date Range of RPA Data ------------------------------------------------------
ggplot(pivot_longer(RPAs_Sorted,3:4,names_to="Analyte",values_to="Value") ,aes(Date,Value,color=Analyte))+geom_point()+facet_wrap(~Station,ncol = 1)+theme_bw()

ggsave("Figures/Date Range of RPA Data.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Hour at which samples were collected over time 
ggplot(RPAs_Sorted,aes(Date,Hour,color=Station))+geom_point()+facet_wrap(~Station,ncol = 3)+theme_bw()+scale_y_continuous(limits = c(0,24),breaks = seq(0,24,1))

RPA_summary <-RPAs_Sorted %>%
mutate(date=ymd_hms(ISOdate(year(Date),month(Date),1,1,0,0,tz = "America/New_York")))  %>%
group_by(Hour,Station,date) %>%
summarise(`Number of Observations`=sum(!is.na(TPO4)))

ggplot(RPA_summary,aes(date,Hour,fill=-`Number of Observations`))+geom_raster()+facet_wrap(~Station,ncol = 3)+theme_bw()+scale_y_continuous(limits = c(0,24),breaks = seq(0,24,1))



# RPA TP Variation figures ------------------------------------------------------------

#Hourly TP Variation from the Daily Mean by Station
ggplot(RPAs_Sorted,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Hourly TP Variation from the Daily Mean by Station.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Hourly TP Variation from the Daily Mean by Flowpath and POsition
ggplot(RPAs_Sorted,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(Flowway~`Flowpath Region`)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Hourly TP Variation from the Daily Mean by Flowway and Region.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


#Hourly % TP Variation from the Daily Mean by Station
ggplot(RPAs_Sorted,aes(Time,`Percent difference from daily mean`,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_wrap(~Station,nrow=1)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits=c(-50,50),breaks = seq(-50,50,5))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Percent Variation from Daily Mean by Hour",y="TPO4 Percent Deviation from Daily (%)",x="Hour")

ggsave("Figures/Hourly Percent Variation in TP from the Daily Mean by Station.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Hourly TP Variation from the Daily Mean by Station and month
ggplot(RPAs_Sorted,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(Station~Month)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="TP Variation from Daily by Station and Month",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Hourly TP Variation from the Daily Mean by Station and Month.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure flow category and difference from the daily mean OutFLOW stations only
ggplot(filter(RPAs_with_Flow,!is.na(`Outflow Category`)),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+geom_hline(yintercept=0)+
facet_grid(Station~`Outflow Category`,scales = "free_x")+scale_colour_brewer( type = "qual", palette = "Set2")+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,2))+theme_bw()+
labs(title="TPO4 Deviation from Daily Mean by Station and Outflow Category",y="Deviation from Daily Mean TPO4 (ug/L)",x="Hour")

ggsave("Figures/TPO4 Deviation from Daily Mean by Station and Outflow Category.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure flow category and difference from the daily mean Inflow stations only
ggplot(filter(RPAs_with_Flow,!is.na(`Inflow Category`)),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+geom_hline(yintercept=0)+
facet_grid(Station~`Inflow Category`,scales = "free_x")+scale_colour_brewer( type = "qual", palette = "Set2")+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,2))+theme_bw()+
labs(title="TPO4 Deviation from Daily Mean by Station and Inflow Category",y="Deviation from Daily Mean TPO4 (ug/L)",x="Hour")

ggsave("Figures/TPO4 Deviation from Daily Mean by Station and Inflow Category.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure RPA and flow by Season
ggplot(RPAs_with_Flow,aes(Outflow,TPO4,color=Season))+geom_point(shape=1,alpha=.3)+geom_smooth(se=FALSE)+theme_bw()+
scale_y_continuous(limits=c(0,80),breaks =seq(0,80,10))+facet_wrap(~`Station`,nrow=3)+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="TPO4 vs Flow by Station and Season",y="TPO4 (ug/L)",x="Flow (cfs)")

ggsave("Figures/TPO4 vs Flow by Station and Season.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)



# TRP diel trend? ---------------------------------------------------------
#Hourly TP Variation from the Daily Mean by Station
ggplot(RPAs_Sorted,aes(Time,`TRP Diff from daily mean`,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Hourly TRP Variation from the Daily Mean by Station.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


# RPA TP Variation figures- Days of continuous Only ------------------------------------------------


#create DF of days where hourly average flow does not deviate by more than 25% from daily mean
Days_with_continual_flow <- Combined_BK_Flow %>%
group_by(`Flowway`,Date) %>%
summarize(`Min Outflow`=min(Outflow,na.rm=TRUE),`Mean Outflow`=mean(Outflow,na.rm=TRUE),`Max Outflow`=max(Outflow,na.rm=TRUE),`Min Inflow`=min(Inflow,na.rm=TRUE),`Mean Inflow`=mean(Inflow,na.rm=TRUE),`Max Inflow`=max(Inflow,na.rm=TRUE)) %>% 
mutate(`Continuous OutFlow`=ifelse(`Min Outflow`>=`Mean Outflow`*.75 &`Max Outflow` <=`Mean Outflow`*1.25,TRUE,FALSE)) %>% #days with all outflow within 25% of mean
mutate(`Continuous InFlow`=ifelse(`Min Inflow`>=`Mean Inflow`*.75 &`Max Inflow` <=`Mean Inflow`*1.25,TRUE,FALSE)) %>%   #days with all inflow within 25% of mean
select(`Flowway`,Date,`Continuous OutFlow`,`Continuous InFlow`)

#RPAS with flow data from days of continuous flow only
RPAs_with_Flow_Complete_Days <-  RPAs_with_Flow %>%
left_join(Days_with_continual_flow)

#Hourly TP Variation from the Daily Mean by Station and outflow category
ggplot(filter(RPAs_with_Flow_Complete_Days,`Continuous OutFlow`==TRUE,!is.na(`Outflow Category`)),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(Station~`Outflow Category`)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Variation from Daily Mean by Hour from Days with Steady outflow",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Hourly TP Variation from the Daily Mean by Station from days of steady Outflow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Hourly TP Variation from the Daily Mean by Station and Inflow category
ggplot(filter(RPAs_with_Flow_Complete_Days,`Continuous InFlow`==TRUE,!is.na(`Inflow Category`)),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(Station~`Inflow Category`)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Variation from Daily Mean by Hour from Days with Steady Inflow",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Hourly TP Variation from the Daily Mean by Station from days of steady Inflow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure RPA and OUtflow Continous Days
ggplot(RPAs_with_Flow_Complete_Days,aes(Outflow,TPO4,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_wrap(~Station,nrow=1,scales = "free_x")+scale_y_continuous(limits=c(0,200),breaks =seq(0,200,20))+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="TPO4 vs Flow by Station from Days of Continuous Outflow",y="TPO4 (ug/L)",x="Flow (cfs)")

ggsave("Figures/PO4 vs Flow by Station from Days of Continuous Outflow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure RPA and Intflow Continous Days
ggplot(RPAs_with_Flow_Complete_Days,aes(Inflow,TPO4,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_wrap(~Station,nrow=1,scales = "free_x")+scale_y_continuous(limits=c(0,200),breaks =seq(0,200,10))+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="TPO4 vs Flow by Station from Days of Continuous Inflow",y="TPO4 (ug/L)",x="Flow (cfs)")

ggsave("Figures/PO4 vs Flow by Station from Days of Continuous Inflow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure RPA and flow and month from days with continuous outflow
ggplot(filter(RPAs_with_Flow_Complete_Days,`Continuous OutFlow`==TRUE,!is.na(`Outflow Category`)),aes(Hour,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(color="black")+theme_bw()+
facet_grid(Station~Month,scales = "free_x")+scale_y_continuous(limits=c(-10,10),breaks =seq(-10,10,2))+scale_colour_brewer( type = "qual", palette = "Set2")+
geom_hline(yintercept=0)+labs(title="TPO4 vs Flow by Station from Days with Continuous Outflow",y="TPO4 (ug/L)",x="Flow (cfs)")

ggsave("Figures/TPO4 vs Flow by Station and Month from days with Continuous Outlow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Need figure of HLR categories vs diel P tend


# Daylight analysis -------------------------------------------------------
Sunrise <-sunriset(matrix(c(-80.6676, 26.6845), nrow=1),seq(from=as.POSIXct("2012-01-01"), to=as.POSIXct("2018-01-01") , by="days"), direction="sunrise", POSIXct.out=TRUE)
Sunset  <-sunriset(matrix(c(-80.6676, 26.6845), nrow=1),seq(from=as.POSIXct("2012-01-01"), to=as.POSIXct("2018-01-01") , by="days"), direction="sunset", POSIXct.out=TRUE)

Sunrise_clean <- Sunrise %>%
  mutate(Date=as.Date(time))  %>%
  rename(Sunrise=`time`) %>%
  mutate(Sunrise=hour(Sunrise)+minute(Sunrise)/60)

Sunset_clean <- Sunset %>%
  mutate(Date=as.Date(time)) %>%
  rename(Sunset=`time`) %>%
  mutate(Sunset=hour(Sunset)+minute(Sunset)/60)

Sunrise_Sunset <- Sunset_clean %>%
  left_join(Sunrise_clean, by="Date") %>%
  select(Date,Sunrise,Sunset) %>%
  mutate(Month=month(Date,label=TRUE))  %>%
  group_by(Month) %>%
  summarise(`Avg Sunset`=mean(Sunset),`Avg Sunrise`=mean(Sunrise))

RPA_Sorted_Daylight <- RPAs_Sorted %>%
  left_join(Sunrise_Sunset,by="Month") %>%
  group_by(Station,Hour,Month) %>%
  summarise(mean_rank=mean(RANK),percent_rank_mean=mean(PERCENT_RANK),mean_Diff_24_hour_mean=mean(Diff_24_hour_mean),n=n(),`Avg Sunrise`=mean(`Avg Sunrise`,na.rm=TRUE),`Avg Sunset`=mean(`Avg Sunset`,na.rm=TRUE)) %>%
  mutate(Month = factor(Month,levels = month.abb)) 
  

#figure- RPA Hourly TP Variation from the Daily Mean by Month
ggplot(RPA_Sorted_Daylight,aes(Hour,mean_Diff_24_hour_mean,color=Station))+facet_grid(Month~.)+
  geom_rect(aes(xmin=0,xmax =`Avg Sunrise`,ymin=-3,ymax=3),fill="light grey")+geom_rect(aes(xmin =`Avg Sunset`,xmax=24,ymin=-3,ymax=3),fill="light grey")+
  geom_rect(aes(xmin =`Avg Sunrise`,xmax=`Avg Sunset`,ymin=-3,ymax=3),fill="light yellow")+
  geom_hline(yintercept=0)+geom_line()+scale_y_continuous(limits=c(-3,3))+scale_x_continuous(breaks = seq(0,24,4))+coord_cartesian(xlim = c(1, 23))+
  labs(title="RPA Hourly TP Variation from the Daily Mean by Month with Daylight Hours",y="Deviation From Daily Mean (ug/L)")+theme_bw()

ggsave("Figures/TP Variation from the Daily Mean Month with Daylight hours.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


# Weather effects on TP ---------------------------------------------------

#Instantanious Rain effect on TP Variation from the Daily Mean by Station
ggplot(RPAs_with_Flow_Stage_Weather,aes(`Rain S7`,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+theme_bw()+geom_smooth(na.rm=TRUE)+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_wrap(~Station,ncol=1)+
geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,.12),breaks = seq(0,.12,.01))+
labs(title="Rain Effects Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Rain")

ggsave("Figures/Rain Effects Variation from Daily Mean by Hour.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Instantainous Rain effect on TP Variation from the Daily Mean by Station boxplots
ggplot(RPAs_with_Flow_Stage_Weather,aes(as.factor(`Rain S7`),Diff_24_hour_mean,color=Station))+geom_boxplot()+theme_bw()+
scale_colour_brewer( type = "qual", palette = "Set2")+
geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
labs(title="Rain Effects Variation from Daily Mean by Hour boxplots",y="TPO4 Deviation from daily mean (ug/L)",x="Rain")

ggsave("Figures/Rain Effects Variation from Daily Mean by Hour boxplots.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Effect of rainy days on diel P 
ggplot(filter(RPAs_with_Flow_Stage_Weather,!is.na(`Rainy Day`)),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(`Rainy Day`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Rainy Days Affect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Rainy Days Affect on TP Variation.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Evaporation effect on TP Variation from the Daily Mean by Station boxplots(Needs work.Not enough data or data needs interpolation )
ggplot(RPAs_with_Flow_Stage_Weather,aes(as.factor(`EVAP S7`),Diff_24_hour_mean,color=Station))+geom_point()+theme_bw()+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_wrap(~Station,ncol=1)+
geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
labs(title="Evaporation Effects Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Rain")

#Effect of max daily evaporation on diel P 
ggplot(RPAs_with_Flow_Stage_Weather,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(`Max Daily Evap`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Max Daily Evaporation Effect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Max Daily Evaporation Effect on Diel P.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Wind effect on TP Variation from the Daily Mean by Station 
ggplot(RPAs_with_Flow_Stage_Weather,aes(`WIND BELLEGLADE`,Diff_24_hour_mean,color=Station))+geom_point()+theme_bw()+geom_smooth(color="black")+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_wrap(~Station,ncol=1)+
geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
labs(title="Wind Effect on P Variation from Daily Mean",y="TPO4 Deviation from daily mean (ug/L)",x="Wind (MPH)")

ggsave("Figures/Wind Effect on P Variation from Daily Mean.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Effect of max daily Wind on diel P 
ggplot(filter(RPAs_with_Flow_Stage_Weather,!is.na(`Max Daily Wind`)),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(`Max Daily Wind`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Max Daily Wind Effect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Max Daily Wind Effect on Diel P.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


# Stage Analysis ----------------------------------------------------------
#Stage over time
ggplot(gather(RPAs_with_Flow_Stage_Weather,"Stage Position","Stage",`Inflow Stage`,`Outflow Stage`),aes(Date,Stage,fill=`Stage Position`))+geom_point(size=1.5,shape=21,alpha=.5)+theme_bw()+facet_wrap(~Flowway,ncol=1)+
scale_fill_brewer( type = "qual", palette = "Set2")

#Stage histogram
ggplot(gather(RPAs_with_Flow_Stage_Weather,"Stage Position","Stage",`Inflow Stage`,`Outflow Stage`),aes(Stage,fill=Station))+geom_histogram(alpha=.5)+theme_bw()+facet_wrap(~Station)

#Stage effect on TP Variation from the Daily Mean by Station 
ggplot(RPAs_with_Flow_Stage_Weather,aes(`Outflow Stage`,Diff_24_hour_mean,color=Station))+geom_point()+theme_bw()+geom_smooth(color="black")+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_wrap(~Station,ncol=1)+
geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
labs(title="Outflow Stage Effect on Variation from Daily Mean",y="TPO4 Deviation from daily mean (ug/L)",x="Stage (ft)")

ggsave("Figures/Outflow Stage Effect on Variation from Daily Mean.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Effect of max daily Outflow Stage on diel P 
ggplot(filter(RPAs_with_Flow_Stage_Weather,is.finite(`Max Daily Outflow Stage`)),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(`Max Daily Outflow Stage`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Max Daily Outflow Stage Effect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Max Daily Outflow Stage Effect on Diel P.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Effect of max daily Inflow Stage on diel P 
ggplot(filter(RPAs_with_Flow_Stage_Weather,is.finite(`Max Daily Inflow Stage`)),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(`Max Daily Inflow Stage`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Max Daily Inflow Stage Effect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Max Daily Inflow Stage Effect on Diel P.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


#Deviation from daily mean Outflow stage vs TP deviation from 24 hour mean 
ggplot(RPAs_with_Flow_Stage_Weather,aes(`Outflow Stage Diff 24 hour mean`,Diff_24_hour_mean,color=Station))+geom_point(size=.5,alpha=.5)+theme_bw()+
scale_y_continuous(limits = c(-25,25),breaks = seq(-25,25,5))+geom_smooth(color="black",method="lm")+
facet_wrap(~Station)+geom_point(aes(mean(`Outflow Stage Diff 24 hour mean`,na.rm=TRUE),mean(`Diff_24_hour_mean`,na.rm=TRUE)),color="black",size=2,shape=3)+
geom_hline(aes(yintercept = 0))+labs(title="Change in Outflow Stage Effect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Outflow Stage Deviation from 24 hour mean (ft)")

ggsave("Figures/Change in Outflow Stage Effect on Diel P.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Deviation from daily mean Outflow stage vs TP deviation from 24 hour mean. Filter only days of stage change greater than 1 inch
ggplot(filter(RPAs_with_Flow_Stage_Weather,abs(`Outflow Stage Diff 24 hour mean`)>.083),aes(`Outflow Stage Diff 24 hour mean`,Diff_24_hour_mean,color=Station))+geom_point(size=.5,alpha=.5)+theme_bw()+
scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+geom_smooth(method="lm",color="black")+
facet_wrap(~Station)+geom_hline(aes(yintercept = 0))+labs(title="Change in Outflow Stage Effect on P Deviation from Daily Mean",y="TPO4 Deviation from daily mean (ug/L)",x="Outflow Stage Deviation from 24 hour mean (ft)")+
stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "right", label.y.npc = 0.15,formula = y~x, parse = TRUE, size = 3)

ggsave("Figures/Change in Outflow Stage Effect on Diel P from days of over 1 inch Outflow stage change.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Deviation from daily mean Inflow stage vs TP deviation from 24 hour mean 
ggplot(RPAs_with_Flow_Stage_Weather,aes(`Inflow Stage Diff 24 hour mean`,Diff_24_hour_mean,color=Station))+geom_point(size=.5,alpha=.5)+theme_bw()+
scale_y_continuous(limits = c(-25,25),breaks = seq(-25,25,5))+geom_smooth(color="black",method="lm")+
facet_wrap(~Station)+geom_point(aes(mean(`Inflow Stage Diff 24 hour mean`,na.rm=TRUE),mean(`Diff_24_hour_mean`,na.rm=TRUE)),color="black",size=2,shape=3)+
geom_hline(aes(yintercept = 0))+labs(title="Change in Inflow Stage Effect on P Deviation from Daily Mean",y="TPO4 Deviation from daily mean (ug/L)",x="Inflow Stage Deviation from 24 hour mean (ft)")

ggsave("Figures/Change in Inflow Stage Effect on Diel P.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Deviation from daily mean Inflow stage vs TP deviation from 24 hour mean. Filter only days of stage change greater than 1 inch
ggplot(filter(RPAs_with_Flow_Stage_Weather,abs(`Inflow Stage Diff 24 hour mean`)>.083),aes(`Inflow Stage Diff 24 hour mean`,Diff_24_hour_mean,color=Station))+geom_point(size=.5,alpha=.5)+theme_bw()+
scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+geom_smooth(method="lm",color="black")+
facet_wrap(~Station)+geom_hline(aes(yintercept = 0))+labs(title="Change in Inflow Stage Effect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Inflow Stage Deviation from 24 hour mean (ft)")+
stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "right", label.y.npc = 0.15,formula =y~x, parse = TRUE, size = 3)

ggsave("Figures/Change in Inflow Stage Effect on Diel P from days of over 1 inch Inflow stage change.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)



# Sonde Analysis ----------------------------------------------------------

Sonde_only <- filter(pivot_longer(RPAs_with_Flow_Stage_Weather_Sonde,31:38,names_to="Parameter",values_to="Value"),is.finite(Value))

#Sonde parameters over time
ggplot(Sonde_only,aes(Date,Value,color=Station))+
geom_point()+theme_bw()+facet_wrap(~Parameter,scales = "free")

#Sonde 
ggplot(Sonde_only,aes(Value,Diff_24_hour_mean,color=Station))+geom_point()+theme_bw()+facet_wrap(~Parameter,scales = "free")+geom_smooth(method="lm")+
scale_y_continuous(limits = c(-5,5),breaks = seq(-5,5,1))+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="Sonde Parameters vs Deviation in Daily P",y="TPO4 Deviation from daily mean (ug/L)",x="Value")

ggsave("Figures/Sonde Parameters vs Deviation in Daily P.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)



# Percent Flow by hour ----------------------------------------------------


#percentage of flow by hour
Total_Flow_Hour<- Combined_BK_Flow %>%
mutate(Year=year(Date)) %>%
group_by(Station,Hour,Year) %>%
summarise(n=n(),`Total Cubic ft`=sum(Flow*60,na.rm=TRUE),`Average cfs`=mean(Flow,na.rm=TRUE),`Total Acre ft`=sum(`Flow`*60*2.2957e-5,na.rm=TRUE),`Average Acre ft`=mean(`Flow`*60*2.2957e-5,na.rm=TRUE))

#percentage of flow by hour
Percent_flow  <- Combined_BK_Flow %>%
mutate(Year=year(Date)) %>%
group_by(Station,Year) %>%
summarise(n=n(),`Cumulative Cubic ft`=sum(Flow*60,na.rm=TRUE)) %>%
right_join(Total_Flow_Hour,by =c("Station","Year")) %>%
mutate(`Day or Night`=if_else(between(as.numeric(Hour),9,21),"Day","Night")) %>%
mutate(`Percent Flow by Hour`=`Total Cubic ft`/`Cumulative Cubic ft`)

#percentage of flow by hour
ggplot(Percent_flow,aes(as.factor(Hour),`Percent Flow by Hour`,fill=Station,color=Station))+geom_point()+theme_bw()+facet_wrap(~Year,nrow = 1)+
scale_color_brewer( type = "qual", palette = "Set2")+scale_x_discrete(limits=paste("",seq(0,24,1),sep=""),breaks =seq(0,24,4))+
theme(axis.text.x=element_text(angle=90,hjust=1,vjust = .5))+scale_y_continuous(labels = percent,limits = c(0,.1))+
labs(title="Percent Flow by Hour",y="Percent %",x="Hour")

ggsave("Figures/Percent Flow by Hour.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Percent flow by day and night
Percent_flow_day  <- Percent_flow  %>% 
group_by(Station, Year,`Day or Night` ) %>%
summarise(n=n(),`Percent Flow`=sum(`Percent Flow by Hour`,na.rm=TRUE)) 

ggplot(filter(Percent_flow_day,is.finite(`Year`)),aes(Station,`Percent Flow`,fill=`Day or Night`))+geom_col(position="dodge")+theme_bw()+facet_grid(~Year)+
scale_fill_brewer( type = "qual", palette = "Set2")+geom_text(aes(label = percent(`Percent Flow`,accuracy = 0.1)),position = position_dodge(1),vjust = -.5)+
theme(axis.text.x=element_text(angle=90,hjust=1,vjust = .5))+scale_y_continuous(labels = scales::percent_format(accuracy = 3L))+
labs(title="Percentage Flow during Day and Night ",y="Percent Flow %",x="")

ggsave("Figures/Percent Flow day or night.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


# Compare inflow data to outflow data -------------------------------------

#Pretty good coverage of RPA data with compliance data
ggplot(RPAs_with_Flow_Stage_Weather_Sonde_Inflow_TP,aes(Date,TPO4))+geom_point(shape=1)+theme_bw()+geom_point(aes(Date,`Inflow TP`),color="red")+
facet_wrap(~Flowway,ncol=1)+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="Variation from Daily Mean by Hour",y="TPO4 (ug/L)",x="Ddate")

#RPA data from inflow and RPA data from outflow
RPAs_Sorted_flowpath <- RPAs_Sorted %>%
mutate(`Flowway` = case_when(`Station`=="G334"~"STA-2C3",`Station`=="G379"~"STA-3/4C2",`Station`=="G381"~"STA-3/4C3",`Station`=="G380"~"STA-3/4C3",`Station`=="G384"~"STA-3/4C3")) %>%        #Add flowway info to RPA data
mutate(`Flowpath Region` = case_when(`Station`=="G334"~"Outflow",`Station`=="G379"~"Outflow",`Station`=="G381"~"Outflow",`Station`=="G380"~"Inflow",`Station`=="G384"~"Midflow"))        #Add flowpath position

ggplot(RPAs_Sorted_flowpath ,aes(Date,TPO4,color=`Flowpath Region`))+geom_point(shape=1)+theme_bw()+geom_point()+
facet_wrap(~Flowway,ncol=1)+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="Variation from Daily Mean by Hour",y="TPO4 (ug/L)",x="Ddate")

#inflow TP is < than outlow TP! what is happening in STA-3/4 central flowway?
ggplot(filter(RPAs_with_Flow_Stage_Weather_Sonde_Inflow_TP,Flowway=="STA-3/4C2"),aes(Date,TPO4))+geom_point(shape=1)+theme_bw()+geom_point(aes(Date,`Inflow TP`),color="red")+
facet_wrap(~Flowway,ncol=1)+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="Variation from Daily Mean by Hour",y="TPO4 (ug/L)",x="Date")

#Strong linear realtionships fror the flowways in STA-3/4
ggplot(filter(RPAs_with_Flow_Stage_Weather_Sonde_Inflow_TP,!is.na(`Inflow TP`)),aes(`Inflow TP`,TPO4,color=Flowway))+geom_point(shape=1)+geom_smooth(method="lm")+
stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "right",formula =y~x, parse = TRUE, size = 3)+theme_bw()+labs(y="Outflow TP (ug/L)",x="Inflow TP (ug/L)")


# closest gate analysis (needs work) ---------------------------------------------------


#analysis of flow at closest gate only
Combined_BK_Flow_closest_gate <- mutate(G381B_C_BK,date=dmy_hms(date)) %>%
full_join(mutate(G379D_C_BK,date=mdy_hm(date))) %>%
full_join(mutate(G334_S_BK_1,date=mdy_hm(date)))  %>%
arrange(date) %>%  
gather("Station","Flow",G381B,G379D,G334) %>%
mutate(Date=as.Date(date)) %>%
#mutate(Flow=if_else(Station=="G334",Flow,Flow)) %>%  #G334 flow/5 since it is larger structure representing larger area
mutate(Hour=hour(round_date(date, unit = "hour"))) %>%
group_by(Station,Date,Hour) %>%
summarise(Flow=mean(Flow,na.rm = TRUE))







