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
Combined_BK_Flow <- read_csv("Data/Combined_BK_Flow.csv", col_types = cols(Flow = col_number(),HLR = col_number()))

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

#sta2 c3 =2400 acres
#STA34 C3B 2087 acres
#STA34 C2B 2375 acres

# Date Range of RPA Data ------------------------------------------------------
ggplot(pivot_longer(RPAs_Sorted,4:5,names_to="Analyte",values_to="Value") ,aes(Date,Value,color=Analyte))+geom_point()+facet_wrap(~Station,ncol = 1)+theme_bw()

ggsave("Figures/Date Range of RPA Data.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


# RPA TP Variation figures ------------------------------------------------------------

#Hourly TP Variation from the Daily Mean by Station
ggplot(RPAs_Sorted,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Hourly TP Variation from the Daily Mean by Station.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

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

#Figure flow category and difference from the daily mean
ggplot(RPAs_with_Flow,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+geom_hline(yintercept=0)+
facet_grid(Station~`Flow Category`,scales = "free_x")+scale_colour_brewer( type = "qual", palette = "Set2")+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,2))+theme_bw()+
labs(title="TPO4 Deviation from Daily Mean by Station and Flow Category",y="Deviation from Daily Mean TPO4 (ug/L)",x="Hour")

ggsave("Figures/TPO4 Deviation from Daily Mean by Station and Flow Category.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure RPA and flow by Season
ggplot(RPAs_with_Flow,aes(Flow,TPO4,color=Season))+geom_point(shape=1,alpha=.3)+geom_smooth(se=FALSE)+theme_bw()+
scale_y_continuous(limits=c(0,80),breaks =seq(0,80,10))+facet_wrap(~`Station`,nrow=3)+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="TPO4 vs Flow by Station and Season",y="TPO4 (ug/L)",x="Flow (cfs)")

ggsave("Figures/TPO4 vs Flow by Station and Season.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


# RPA TP Variation figures- Days of continuous Only ------------------------------------------------


#create DF of continual flow dates
Days_with_continual_flow <- Combined_BK_Flow %>%
group_by(Station,Date) %>%
summarize(n=n(),`Min Flow`=min(Flow)) %>%
drop_na() %>%
filter(`Min Flow`!=0) %>%
select(Station,Date)

#RPAS with flow data from days of continuous flow only
RPAs_with_Flow_Complete_Days <-  RPAs_Sorted %>%
left_join(Combined_BK_Flow ,by=c("Station","Date","Hour")) %>%
inner_join(Days_with_continual_flow) %>%
filter(is.finite(Flow)) %>%
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


#Hourly TP Variation from the Daily Mean by Station from days of continuous flow
ggplot(RPAs_with_Flow_Complete_Days,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Variation from Daily Mean by Hour from Days with Continuous Flow",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Hourly TP Variation from the Daily Mean by Station from days with continuous flow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure RPA and flow Continous Days
ggplot(RPAs_with_Flow_Complete_Days,aes(Flow,TPO4,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_wrap(~Station,nrow=1,scales = "free_x")+scale_y_continuous(limits=c(0,80),breaks =seq(0,80,10))+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="TPO4 vs Flow by Station from Days of Continuous Flow",y="TPO4 (ug/L)",x="Flow (cfs)")

ggsave("Figures/PO4 vs Flow by Station from Days of Continuous Flow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure flow category and difference from the daily mean
ggplot(RPAs_with_Flow_Complete_Days ,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+geom_hline(yintercept=0)+
facet_grid(Station~`Flow Category`,scales = "free_x")+scale_colour_brewer( type = "qual", palette = "Set2")+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,2))+theme_bw()+
labs(title="TPO4 Deviation from Daily Mean by Station and Flow Category from Days of Continuous Flow",y="Deviation from Daily Mean TPO4 (ug/L)",x="Hour")

ggsave("Figures/TPO4 Deviation from Daily Mean by Station and Flow Category from days of Continuous Flow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure RPA and flow and month from days with continuous flow
ggplot(RPAs_with_Flow_Complete_Days,aes(Hour,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(color="black")+theme_bw()+
facet_grid(Station~Month,scales = "free_x")+scale_y_continuous(limits=c(-10,10),breaks =seq(-10,10,2))+scale_colour_brewer( type = "qual", palette = "Set2")+
geom_hline(yintercept=0)+labs(title="TPO4 vs Flow by Station from Days with Continuous Flow",y="TPO4 (ug/L)",x="Flow (cfs)")

ggsave("Figures/TPO4 vs Flow by Station and Month from days with Continuous Flow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

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
ggplot(RPAs_with_Flow_Stage_Weather,aes(Date,Stage,color=Station))+geom_point(size=.5)+theme_bw()

#Stage histogram
ggplot(RPAs_with_Flow_Stage_Weather,aes(Stage,fill=Station))+geom_histogram(alpha=.5)+theme_bw()+facet_wrap(~Station)

#Stage effect on TP Variation from the Daily Mean by Station 
ggplot(RPAs_with_Flow_Stage_Weather,aes(`Stage`,Diff_24_hour_mean,color=Station))+geom_point()+theme_bw()+geom_smooth(color="black")+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_wrap(~Station,ncol=1)+
geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
labs(title="Stage Effect on Variation from Daily Mean",y="TPO4 Deviation from daily mean (ug/L)",x="Stage (ft)")

ggsave("Figures/Stage Effect on Variation from Daily Mean.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Effect of max daily Stage on diel P 
ggplot(filter(RPAs_with_Flow_Stage_Weather,is.finite(`Max Daily Stage`)),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(`Max Daily Stage`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Max Daily Stage Effect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Figures/Max Daily Stage Effect on Diel P.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Deviation from daily mean stage vs TP deviation from 24 hour mean 
ggplot(RPAs_with_Flow_Stage_Weather,aes(`Stage_Diff_24_hour_mean`,Diff_24_hour_mean,color=Station))+geom_point(size=.5,alpha=.5)+theme_bw()+
scale_y_continuous(limits = c(-25,25),breaks = seq(-25,25,5))+geom_smooth(color="black")+
facet_wrap(~Station)+geom_point(aes(mean(`Stage_Diff_24_hour_mean`,na.rm=TRUE),mean(Diff_24_hour_mean,na.rm=TRUE)),color="black",size=2,shape=3)+
geom_hline(aes(yintercept = 0))+labs(title="Change in Stage Effect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Stage Deviation from 24 hour mean (ft)")

ggsave("Figures/Change in Stage Effect on Diel P.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Deviation from daily mean stage vs TP deviation from 24 hour mean. Filter only days of stage change greater than 1 inch
formula <- y ~ x
ggplot(filter(RPAs_with_Flow_Stage_Weather,abs(`Stage_Diff_24_hour_mean`)>.083),aes(`Stage_Diff_24_hour_mean`,Diff_24_hour_mean,color=Station))+geom_point(size=.5,alpha=.5)+theme_bw()+
scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+geom_smooth(method="lm",color="black")+
facet_wrap(~Station)+geom_hline(aes(yintercept = 0))+labs(title="Change in Stage Effect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Stage Deviation from 24 hour mean (ft)")+
stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), label.x.npc = "right", label.y.npc = 0.15,formula = formula, parse = TRUE, size = 3)

ggsave("Figures/Change in Stage Effect on Diel P from days of over 1 inch stage change.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)



# Sonde Analysis ----------------------------------------------------------

Sonde_only <- filter(pivot_longer(RPAs_with_Flow_Stage_Weather_Sonde,32:39,names_to="Parameter",values_to="Value"),is.finite(Value))

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







