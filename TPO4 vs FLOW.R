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
library(e1071)
library(ggrepel)


# Import data -------------------------------------------------------------

#RPA tidy data
RPAs_Sorted <- read_csv("Data/RPAs Sorted.csv")

#Import Flow Data
Combined_BK_Flow <- read_csv("Data/Combined_BK_Flow.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d"),  Inflow = col_number(), `Inflow HLR` = col_number(), Outflow = col_number(), `Outflow HLR` = col_number()))

#RPA and flow DF. 
RPAs_with_Flow <- read_csv("Data/RPA and Flow.csv") 

#Hours since flow DF 
All_flow_RPA1 <- read_csv("Data/Hours Since Flow.csv") 


# TPO4 vs FLOW at inflow sites --------------------------------------------

RPAs_with_Flow_inflow <- RPAs_with_Flow %>%
filter(`Flowpath Region`=="Inflow") %>%
filter(Outflow>2) %>%
mutate(`Month`=factor(`Month`,levels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))

ggplot(RPAs_with_Flow_inflow ,aes(Inflow,TPO4,color=Station))+geom_point()+theme_bw()+geom_smooth(color="black")+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_grid(Station~Month)+
labs(title="Inflow effect on Phosphorus",y="TPO4 (ug/L)",x="Flow (cfs)")

RPAs_with_Flow_outflows <- RPAs_with_Flow %>%
#filter(`Flowpath Region`=="Outflow") %>%
#filter(Outflow>2) %>%
mutate(`Month`=factor(`Month`,levels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) %>%
mutate(Flag=if_else(Station == "G334" & Date >"2017-01-01",TRUE,FALSE)  ) %>%
filter(Flag ==TRUE)

ggplot(RPAs_with_Flow_outflows ,aes(`Outflow HLR`,(TPO4),color=Station))+geom_point()+theme_bw()+geom_smooth(color="black")+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_grid(Station~Month)+coord_cartesian(ylim = c(0,100),xlim = c(0,35))+
labs(title="",y="TPO4 (ug/L)",x="Flow (cm/day)")

ggplot(RPAs_with_Flow_outflows ,aes(Outflow,TPO4,color=Station))+geom_point()+theme_bw()+geom_smooth(color="black")+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_wrap(~Station,nrow=3)+coord_cartesian(ylim = c(0,100))+
labs(title="Outflow effect on Phosphorus",y="TPO4 (ug/L)",x="Flow (cfs)")

ggplot(Combined_BK_Flow ,aes(Date+Hour/24,Outflow,color=Station))+geom_point()+theme_bw()+geom_smooth(color="black")+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_grid(Station~Month)+coord_cartesian(ylim = c(0,100))+
labs(title="Outflows",y="Flow (cfs)",x="Date")



#find outlying values
RPAs_with_Flow_outflows_outlying <- RPAs_with_Flow_outflows%>%
filter(Station=="G334",Month=="Jun",Date <"2017-06-01")

ggplot(RPAs_with_Flow_outflows_outlying ,aes(`Outflow HLR`,(TPO4)))+geom_point(shape=21,size=3)+geom_smooth(color="black")+
scale_colour_brewer( type = "qual", palette = "Set2")+coord_cartesian(ylim = c(0,100),xlim = c(0,35))+theme_bw()+
labs(title="Outflow effect on Phosphorus",y="TPO4 (ug/L)",x="HLR (cm/day)")


ggplot(RPAs_with_Flow_outflows_outlying ,aes(Date,(TPO4)))+geom_point(shape=21,size=3)+
scale_colour_brewer( type = "qual", palette = "Set2")+theme_bw()+
labs(title="Outflow effect on Phosphorus",y="TPO4 (ug/L)",x="HLR (cm/day)")





# First flush theory ------------------------------------------------------


All_flow_RPA <- RPAs_Sorted  %>%           #create DF with flow for everyhour for the POR
filter(`Flowpath Region`=="Inflow") %>%
full_join(Combined_BK_Flow ,by=c("Date","Hour","Flowway")) %>%
group_by(Flowway) %>%
mutate(`Date_Time`=ymd_hms(ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York"))) %>%   #create hourly date time index
arrange(Flowway,`Date_Time`)               #sort DF by flowway and time so that it can be looped through later 


# Takes long time to run load from data unless needs update
# Counts the number of hours since last flow. 5 categories of flow. 
All_flow_RPA1<-mutate(All_flow_RPA,`Days Since Flow >0`=0,`Days Since Flow >100`=0,`Days Since Flow >250`=0,`Days Since Flow >500`=0,`Days Since Flow >1000`=0)
hours_0 <- 0
hours_100 <- 0
hours_250 <- 0
hours_500 <- 0
hours_1000 <- 0
for(i in 1:nrow(All_flow_RPA1)) 
{           
ifelse(All_flow_RPA1$Inflow[i]>0.5,hours_0 <-0,hours_0  <-hours_0 +1)
ifelse(All_flow_RPA1$Inflow[i]>100,hours_100 <-0,hours_100  <-hours_100 +1)
ifelse(All_flow_RPA1$Inflow[i]>250,hours_250 <-0,hours_250 <-hours_250 +1)
ifelse(All_flow_RPA1$Inflow[i]>500,hours_500 <-0,hours_500  <-hours_500 +1)
ifelse(All_flow_RPA1$Inflow[i]>1000,hours_1000 <-0,hours_1000  <-hours_1000 +1)
All_flow_RPA1$`Days Since Flow >0`[i] <- hours_0/24 
All_flow_RPA1$`Days Since Flow >100`[i] <- hours_100/24 
All_flow_RPA1$`Days Since Flow >250`[i] <- hours_250/24
All_flow_RPA1$`Days Since Flow >500`[i] <- hours_500/24 
All_flow_RPA1$`Days Since Flow >1000`[i] <- hours_1000/24 
}

write.csv(All_flow_RPA1, "Data/Hours Since Flow.csv",row.names=FALSE)

All_flow_RPA2<-filter(gather(All_flow_RPA1,"Flow Category","Value",33:37),!is.na(TPO4)) %>%
mutate(`Flow Category` = factor(`Flow Category`, levels = c("Days Since Flow >0", "Days Since Flow >100","Days Since Flow >250","Days Since Flow >500","Days Since Flow >1000")))  


ggplot(All_flow_RPA2,aes(Value,TPO4,color=Station))+geom_point()+facet_grid(`Station`~`Flow Category`)+theme_bw()+scale_x_log10()

ggplot(All_flow_RPA2,aes(`Value`,TPO4,color=Station))+geom_point()+facet_grid(`Flow Category`~Flowway)+theme_bw()+
coord_cartesian(ylim = c(0,100))+geom_smooth(method="loess",color="black",fill="grey",method.args = list(family = "symmetric",degree=2))





# Does difference from daily mean flow correlate with deviation from daily mean TP?-----------------------------------

Flow_diff_24_mean <- Combined_BK_Flow %>%
group_by(`Flowway`,Date) %>%
mutate(`24_hour_mean_outflow`=mean(Outflow,na.rm=TRUE)) %>%
mutate(Diff_24_hour_mean_outflow=Outflow-`24_hour_mean_outflow`) %>%
mutate(`Percent difference from daily mean outflow`=(Diff_24_hour_mean_outflow/`24_hour_mean_outflow`)*100)

#Join with RPA data
Flow_diff_24_mean_w_RPA <-RPAs_Sorted %>%
left_join(Flow_diff_24_mean,by=c("Flowway","Date","Hour"))

#Figures
ggplot(Flow_diff_24_mean_w_RPA ,aes(`Diff_24_hour_mean_outflow`,Diff_24_hour_median,color=Station_ID,fill=Station_ID))+geom_point(shape=21)+geom_smooth(method="loess",color="black",fill="grey",method.args = list(family = "symmetric",degree=2))+
theme_bw()+facet_grid(`Flowpath Region`~Flowway,scales="free")+scale_colour_brewer( type = "qual", palette = "Set2",guide = 'none')+scale_fill_brewer( type = "qual", palette = "Set2",name="Station")+
geom_hline(yintercept=0)+theme(legend.position="bottom")+coord_cartesian(ylim = c(-25,25))+
labs(title="TPO4 vs outflow  ",y="percent diff TP",x="diff flow",color=NULL)




