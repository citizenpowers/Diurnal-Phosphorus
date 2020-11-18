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



# Import data for RPA analysis -------------------------------------------------------------
#RPA raw data
RPAs <-  read_excel("Data/Outflows.xlsx", col_types = c("text", "date", "numeric",  "numeric")) 

#RPA tidy data
RPAs_Sorted <- read_csv("Data/RPAs Sorted.csv")

#RPA and flow DF. 
RPAs_with_Flow <- read_csv("Data/RPA and Flow.csv") %>%
mutate(`Flow Category`=factor(`Flow Category`,levels = c("Reverse Flow", "0-1 (cfs)", "1-100 (cfs)","100-250 (cfs)","250-500 (cfs)","500-1000 (cfs)","1000+ (cfs)"))) %>%
mutate(`Month`=factor(`Month`,levels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))

#RPA Flow and Stage Data
RPA_and_Flow_and_Stage <- read_csv("Data/RPA and Flow and Stage.csv")

#RPA Flow and Stage and weather data
RPAs_with_Flow_Stage_Weather <- read_csv("Data/RPA and Flow Stage Weather.csv")

#sta2 c3 =2400 acres
#STA34 C3B 2087 acres
#STA34 C2B 2375 acres

# Date Range of RPA Data ------------------------------------------------------
ggplot(pivot_longer(RPAs_Sorted,4:5,names_to="Analyte",values_to="Value") ,aes(Date,Value,color=Analyte))+geom_point()+facet_wrap(~Station,ncol = 1)+theme_bw()

# RPA TP Variation figures ------------------------------------------------------------

#Hourly TP Variation from the Daily Mean by Station
ggplot(RPAs_Sorted,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Hourly TP Variation from the Daily Mean by Station.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Hourly % TP Variation from the Daily Mean by Station
ggplot(RPAs_Sorted,aes(Time,`Percent difference from daily mean`,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_wrap(~Station,nrow=1)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits=c(-50,50),breaks = seq(-50,50,5))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Percent Variation from Daily Mean by Hour",y="TPO4 Percent Deviation from Daily (%)",x="Hour")

ggsave("Hourly Percent Variation in TP from the Daily Mean by Station.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Hourly TP Variation from the Daily Mean by Station and month
ggplot(RPAs_Sorted,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(Station~Month)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="TP Variation from Daily by Station and Month",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Hourly TP Variation from the Daily Mean by Station and Month.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure flow category and difference from the daily mean
ggplot(RPAs_with_Flow,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+geom_hline(yintercept=0)+
facet_grid(Station~`Flow Category`,scales = "free_x")+scale_colour_brewer( type = "qual", palette = "Set2")+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,2))+theme_bw()+
labs(title="TPO4 Deviation from Daily Mean by Station and Flow Category",y="Deviation from Daily Mean TPO4 (ug/L)",x="Hour")

ggsave("TPO4 Deviation from Daily Mean by Station and Flow Category.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure RPA and flow by Season
ggplot(RPAs_with_Flow,aes(Flow,TPO4,color=Season))+geom_point(shape=1,alpha=.3)+geom_smooth(se=FALSE)+theme_bw()+
scale_y_continuous(limits=c(0,80),breaks =seq(0,80,10))+facet_wrap(~`Station`,nrow=3)+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="TPO4 vs Flow by Station and Season",y="TPO4 (ug/L)",x="Flow (cfs)")

ggsave("TPO4 vs Flow by Station and Season.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

RPAs_Sorted_Summary_hour <- RPAs_Sorted %>%
group_by(Station,Hour) %>%
summarise(mean_rank=mean(RANK),percent_rank_mean=mean(PERCENT_RANK),mean_Diff_24_hour_mean=mean(Diff_24_hour_mean),`Percent difference from daily mean`=mean(`Percent difference from daily mean`,na.rm=TRUE),n=n())

#Figure-RPA Hourly TP Variation from the Daily Mean 
ggplot(RPAs_Sorted_Summary_hour,aes(Hour,mean_Diff_24_hour_mean,color=Station))+geom_line(size=2)+geom_hline(yintercept=0)+theme_bw()+labs(title="RPA Hourly TP Variation from the Daily Mean", y="Deviation From Daily Mean (ug/L)")+
scale_colour_brewer( type = "qual", palette = "Set2")

#Figure-RPA Hourly TP Variation from the Daily Mean (Percent)
ggplot(RPAs_Sorted_Summary_hour,aes(Hour,`Percent difference from daily mean`,color=Station))+geom_line(size=2)+geom_hline(yintercept=0)+theme_bw()+
labs(title="RPA Hourly TP Percent Variation from the Daily Mean", y="Deviation From Daily Mean %")+scale_colour_brewer( type = "qual", palette = "Set2")

RPAs_Sorted_Summary_month <- RPAs_Sorted %>%
group_by(Station,Hour,Month) %>%
summarise(mean_rank=mean(RANK),percent_rank_mean=mean(PERCENT_RANK),mean_Diff_24_hour_mean=mean(Diff_24_hour_mean),n=n())

#Figures- RPA Hourly TP Variation from the Daily Mean by Month
ggplot(RPAs_Sorted_Summary_month,aes(Hour,mean_Diff_24_hour_mean,color=Month))+geom_hline(yintercept=0)+geom_smooth(se = FALSE,na.rm = TRUE)+facet_grid(Station~.)+theme_bw()+
labs(title="Hourly TP Variation from the Daily Mean by Station and Month",y="Deviation From Daily Mean (ug/L)")+scale_y_continuous(limits=c(-3,3))

ggsave("Hourly TP Variation from the Daily Mean by Station and Month 2.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

ggplot(RPAs_Sorted_Summary_month,aes(Hour,mean_Diff_24_hour_mean,color=Month))+geom_hline(yintercept=0)+geom_smooth(se = FALSE,na.rm = TRUE)+theme_bw()+
labs(title="RPA Hourly TP Variation from the Daily Mean by Month",y="Deviation From Daily Mean (ug/L)")+scale_y_continuous(limits=c(-3,3))

ggsave("TP Variation from the Daily Mean by Month.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


# RPA TP Variation figures- Days of continuous Only ------------------------------------------------

#create DF of every minute RPAs were running. 2 steps since it is long process. (Only run if updated needed, otherwise upload from csv)
Total_Flow_1 <- setNames(as.data.frame(seq(from=ISOdate(2012,7,01,0,0,0,tz = "America/New_York"), to=ISOdate(2017,9,04,0,0,0,tz = "America/New_York"),by = "min")),"date") %>%
full_join(select(G381_BK,date,G381),by="date") %>%
full_join(select(G379_BK,date,G379),by="date") %>%
full_join(select(G334_BK,date,G334),by="date") %>%
mutate(Date=as.Date(date, tz='America/New_York'))     #Make sure to specify Time zone. Unspecified TZ will assume UTC

Total_Flow <-Total_Flow_1  %>%
fill(G381,G379,G334) %>%  
mutate(Year=year(Date)) %>%
mutate(Hour=hour(round_date(date, unit = "hour"))) %>%
mutate(Minute=minute(round_date(date, unit = "minute"))) %>%
mutate(Month=month(date,label=TRUE)) %>%
gather("Station","Flow",G381,G379,G334) 

#create DF of continual flow dates
Days_with_continual_flow <- Total_Flow %>%
group_by(Station,Date) %>%
summarize(n=n(),`Min Flow`=min(Flow)) %>%
drop_na() %>%
filter(`Min Flow`!=0) %>%
select(Station,Date)

#RPAS with flow data from days of continuous flow only
RPAs_with_Flow_Complete_Days <-  RPAs_Sorted %>%
left_join(Total_Flow ,by=c("Station","Date","Year","Month","Hour","Minute")) %>%
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

#save RPA and flow so entire DF does not have to be recreated 
write.csv(RPAs_with_Flow_Complete_Days ,"RPA and Flow from continous flow day.csv")

#upload csv
RPAs_with_Flow_Complete_Days  <- read_csv("RPA and Flow from continous flow day.csv") 

#Hourly TP Variation from the Daily Mean by Station from days of continuous flow
ggplot(RPAs_with_Flow_Complete_Days,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Variation from Daily Mean by Hour from Days with Continuous Flow",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Hourly TP Variation from the Daily Mean by Station from days with continuous flow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure RPA and flow Continous Days
ggplot(RPAs_with_Flow_Complete_Days,aes(Flow,TPO4,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_wrap(~Station,nrow=1,scales = "free_x")+scale_y_continuous(limits=c(0,80),breaks =seq(0,80,10))+scale_colour_brewer( type = "qual", palette = "Set2")+
labs(title="TPO4 vs Flow by Station from Days of Continuous Flow",y="TPO4 (ug/L)",x="Flow (cfs)")

ggsave("TPO4 vs Flow by Station from Days of Continuous Flow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure flow category and difference from the daily mean
ggplot(RPAs_with_Flow_Complete_Days ,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+geom_hline(yintercept=0)+
facet_grid(Station~`Flow Category`,scales = "free_x")+scale_colour_brewer( type = "qual", palette = "Set2")+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,2))+theme_bw()+
labs(title="TPO4 Deviation from Daily Mean by Station and Flow Category from Days of Continuous Flow",y="Deviation from Daily Mean TPO4 (ug/L)",x="Hour")

ggsave("TPO4 Deviation from Daily Mean by Station and Flow Category from days of Continuous Flow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#flow by hour for year and station
ggplot(Total_Flow_Hour,aes(as.factor(Hour),`Average cfs`,fill=Station,color=Station))+geom_point()+theme_bw()+facet_wrap(~Year,ncol=4)+
scale_color_brewer( type = "qual", palette = "Set2")+scale_x_discrete(limits=paste("",seq(0,24,1),sep=""),breaks =seq(0,24,4))+
theme(axis.text.x=element_text(angle=90,hjust=1,vjust = .5))+
labs(title="Hourly Average Flow by Year and Station ",y="Average Flow (cfs)",x="Hour")

ggsave("Average Flow by Hour and year.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Figure RPA and flow and month from days with continuous flow
ggplot(RPAs_with_Flow_Complete_Days,aes(Hour,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(color="black")+theme_bw()+
facet_grid(Station~Month,scales = "free_x")+scale_y_continuous(limits=c(-10,10),breaks =seq(-10,10,2))+scale_colour_brewer( type = "qual", palette = "Set2")+
geom_hline(yintercept=0)+labs(title="TPO4 vs Flow by Station from Days with Continuous Flow",y="TPO4 (ug/L)",x="Flow (cfs)")

ggsave("TPO4 vs Flow by Station and Month from days with Continuous Flow.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#calculate flow by  Year,Month, and hour for each station
Total_Flow_Hour_Month<- Total_Flow %>%
group_by(Station,Hour,Month,Year) %>%
summarise(n=n(),`Total Cubic ft`=sum(Flow*60,na.rm=TRUE),`Average cfs`=mean(Flow,na.rm=TRUE),`Total Acre ft`=sum(`Flow`*60*2.2957e-5,na.rm=TRUE),`Average Acre ft`=mean(`Flow`*60*2.2957e-5,na.rm=TRUE))

#Average Flow by Hour Year and Month
ggplot(Total_Flow_Hour_Month,aes(as.factor(Hour),`Average cfs`,fill=Station,color=Station))+geom_point()+theme_bw()+facet_grid(Year~Month)+
scale_color_brewer( type = "qual", palette = "Set2")+scale_x_discrete(limits=paste("",seq(0,24,1),sep=""),breaks =seq(0,24,4))+
theme(axis.text.x=element_text(angle=90,hjust=1,vjust = .5))+
labs(title="Hourly Average Flow by Year, Month, and Station",y="Average Flow (cfs)",x="Hour")

ggsave("Average Flow by Hour month and year.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


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
  summarise(mean_rank=mean(RANK),percent_rank_mean=mean(PERCENT_RANK),mean_Diff_24_hour_mean=mean(Diff_24_hour_mean),n=n(),`Avg Sunrise`=mean(`Avg Sunrise`,na.rm=TRUE),`Avg Sunset`=mean(`Avg Sunset`,na.rm=TRUE))

#figure- RPA Hourly TP Variation from the Daily Mean by Month
ggplot(RPA_Sorted_Daylight,aes(Hour,mean_Diff_24_hour_mean,color=Station))+facet_grid(Month~.)+
  geom_rect(aes(xmin=0,xmax =`Avg Sunrise`,ymin=-3,ymax=3),fill="light grey")+geom_rect(aes(xmin =`Avg Sunset`,xmax=24,ymin=-3,ymax=3),fill="light grey")+
  geom_rect(aes(xmin =`Avg Sunrise`,xmax=`Avg Sunset`,ymin=-3,ymax=3),fill="light yellow")+
  geom_hline(yintercept=0)+geom_line()+scale_y_continuous(limits=c(-3,3))+scale_x_continuous(breaks = seq(0,24,4))+coord_cartesian(xlim = c(1, 23))+
  labs(title="RPA Hourly TP Variation from the Daily Mean by Month with Daylight Hours",y="Deviation From Daily Mean (ug/L)")+theme_bw()

ggsave("TP Variation from the Daily Mean Month with Daylight hours.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


# Weather effects on TP ---------------------------------------------------

#Instantaious Rain effect on TP Variation from the Daily Mean by Station
ggplot(RPAs_with_Flow_Stage_Weather,aes(`Rain S7`,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+theme_bw()+geom_smooth(na.rm=TRUE)+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_wrap(~Station,ncol=1)+
geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,.12),breaks = seq(0,.12,.01))+
labs(title="Rain Effects Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Rain")

#Instantainous Rain effect on TP Variation from the Daily Mean by Station boxplots
ggplot(RPAs_with_Flow_Stage_Weather,aes(as.factor(`Rain S7`),Diff_24_hour_mean,color=Station))+geom_boxplot()+theme_bw()+
scale_colour_brewer( type = "qual", palette = "Set2")+
geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
labs(title="Rain Effects Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Rain")

#Effect of rainy days on diel P 
ggplot(na.omit(RPAs_with_Flow_Stage_Weather),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(`Rainy Day`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Rainy Days Affect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

ggsave("Rainy Days Affect on TP Variation.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Evaporation effect on TP Variation from the Daily Mean by Station boxplots
ggplot(RPAs_with_Flow_Stage_Weather,aes(as.factor(`EVAP S7`),Diff_24_hour_mean,color=Station))+geom_point()+theme_bw()+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_wrap(~Station,ncol=1)+
geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
labs(title="Rain Effects Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Rain")

#Effect of max daily evaporation on diel P 
ggplot(RPAs_with_Flow_Stage_Weather,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+geom_smooth(method="loess",color="black")+theme_bw()+
facet_grid(`Max Daily Evap`~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
labs(title="Max Daily Evaporation Effect on Diel P",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

# Percent Flow by hour ----------------------------------------------------


#percentage of flow by hour
Percent_flow  <- Total_Flow %>%
group_by(Station,Year) %>%
summarise(n=n(),`Cumulative Cubic ft`=sum(Flow*60,na.rm=TRUE)) %>%
right_join(Total_Flow_Hour,by =c("Station","Year")) %>%
mutate(`Day or Night`=if_else(between(as.numeric(Hour),9,21),"Day","Night")) %>%
mutate(`Percent Flow by Hour`=`Total Cubic ft`/`Cumulative Cubic ft`)

#percentage of flow by hour
ggplot(Percent_flow,aes(as.factor(Hour),`Percent Flow by Hour`,fill=Station,color=Station))+geom_point()+theme_bw()+facet_wrap(~Year,nrow = 1)+
scale_color_brewer( type = "qual", palette = "Set2")+scale_x_discrete(limits=paste("",seq(0,24,1),sep=""),breaks =seq(0,24,4))+
theme(axis.text.x=element_text(angle=90,hjust=1,vjust = .5))+scale_y_continuous(labels = percent)+
labs(title="Percent Flow by Hour",y="Percent %",x="Hour")

ggsave("Percent Flow by Hour.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Percent flow by day and night
Percent_flow_day  <- Percent_flow  %>% 
group_by(Station, Year,`Day or Night` ) %>%
summarise(n=n(),`Percent Flow`=sum(`Percent Flow by Hour`,na.rm=TRUE)) 

ggplot(Percent_flow_day ,aes(`Day or Night`,`Percent Flow`,fill=Station))+geom_col(position="dodge")+theme_bw()+facet_grid(~Year)+
scale_fill_brewer( type = "qual", palette = "Set2")+geom_text(aes(label = percent(`Percent Flow`)),position = position_dodge(1),vjust = -.5)+
theme(axis.text.x=element_text(angle=90,hjust=1,vjust = .5))+scale_y_continuous(labels = percent)+
labs(title="Percentage Flow during Day and Night ",y="Percent Flow %",x="")

ggsave("Percent Flow day or night.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


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







