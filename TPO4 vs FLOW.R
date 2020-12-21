library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(maptools)



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

RPAs_with_Flow_outflows_censored <- RPAs_with_Flow %>%
filter(`Flowpath Region`=="Inflow")

ggplot(RPAs_with_Flow_outflows_censored  ,aes(Inflow,TPO4,color=Station))+geom_point()+theme_bw()+geom_smooth(color="black")+
scale_colour_brewer( type = "qual", palette = "Set2")+facet_wrap(~Station,ncol=3)+
labs(title="Inflow effect on Phosphorus",y="TPO4 (ug/L)",x="Flow (cfs)")


# First flush theory ------------------------------------------------------



All_flow_RPA <- RPAs_Sorted  %>%           #create DF with flow for everyhour for the POR
filter(`Flowpath Region`=="Inflow") %>%
full_join(Combined_BK_Flow ,by=c("Date","Hour","Flowway")) %>%
group_by(Flowway) %>%
mutate(`Date_Time`=ymd_hms(ISOdate(year(Date),month(Date),day(Date),Hour,0,0,tz = "America/New_York"))) %>%   #create hourly date time index
arrange(Flowway,`Date_Time`)               #sort DF by flowway and time so that it can be looped through later 


# Takes long time to run load from data unless needs update
# Counts the number of hours since last flow. 5 categories of flow. 
All_flow_RPA1<-mutate(All_flow_RPA,`Hours Since Flow >0`=0,`Hours Since Flow >100`=0,`Hours Since Flow >250`=0,`Hours Since Flow >500`=0,`Hours Since Flow >1000`=0)
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
All_flow_RPA1$`Hours Since Flow >0`[i] <- hours_0 
All_flow_RPA1$`Hours Since Flow >100`[i] <- hours_100 
All_flow_RPA1$`Hours Since Flow >250`[i] <- hours_250 
All_flow_RPA1$`Hours Since Flow >500`[i] <- hours_500 
All_flow_RPA1$`Hours Since Flow >1000`[i] <- hours_1000 
}

write.csv(All_flow_RPA1, "Data/Hours Since Flow.csv",row.names=FALSE)

test <-gather(All_flow_RPA1,"Flow Category","Value",27:31)

ggplot(gather(All_flow_RPA1,"Flow Category","Hours Since Flow",27:31),aes(`Hours Since Flow`,TPO4,color=Flowway))+geom_point()+facet_grid(`Flow Category`~Flowway)+theme_bw()+scale_x_log10()




