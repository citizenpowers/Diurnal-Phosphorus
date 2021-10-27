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


# Import data for RPA analysis -------------------------------------------------------------

#RPA and flow DF. 
RPAs_with_Flow <- read_csv("Data/RPA and Flow.csv") %>%
mutate(`Month`=factor(`Month`,levels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))


#RPA tidy data
RPAs_Sorted <- read_csv("Data/RPAs Sorted.csv")


# Difference from daily mean flow ----------------------------------------------------

Flow_diff_24_mean <- Combined_BK_Flow %>%
group_by(`Flowway`,Date) %>%
mutate(`24_hour_mean_outflow`=mean(Outflow,na.rm=TRUE)) %>%
mutate(Diff_24_hour_mean_outflow=Outflow-`24_hour_mean_outflow`) %>%
mutate(`Percent difference from daily mean outflow`=(Diff_24_hour_mean_outflow/`24_hour_mean_outflow`)*100)



# Join with RPA Data -----------------------------------------------------------------

Flow_diff_24_mean_w_RPA <-RPAs_Sorted %>%
left_join(Flow_diff_24_mean,by=c("Flowway","Date","Hour"))



# Figures -----------------------------------------------------------------

ggplot(Flow_diff_24_mean_w_RPA ,aes(`Diff_24_hour_mean_outflow`,Diff_24_hour_median,color=Station_ID,fill=Station_ID))+geom_point(shape=21)+geom_smooth(method="loess",color="black",fill="grey",method.args = list(family = "symmetric",degree=2))+
theme_bw()+facet_grid(`Flowpath Region`~Flowway,scales="free")+scale_colour_brewer( type = "qual", palette = "Set2",guide = 'none')+scale_fill_brewer( type = "qual", palette = "Set2",name="Station")+
geom_hline(yintercept=0)+theme(legend.position="bottom")+coord_cartesian(ylim = c(-25,25))+
labs(title="TPO4 vs outflow  ",y="percent diff TP",x="diff flow",color=NULL)




