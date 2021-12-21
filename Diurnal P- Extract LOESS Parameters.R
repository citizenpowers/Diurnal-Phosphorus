
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
library(purrr)
library(broom)
library(mgcv)


# Import Data -------------------------------------------------------------


#RPA tidy data
RPAs_Sorted <- read_csv("Data/RPAs Sorted.csv") %>%
mutate(Flag=if_else(Station == "G334" & Date >"2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed
filter(Flag ==FALSE)  




# Add data at the boundaries ----------------------------------------------

RPAs_Sorted_negative_time <-RPAs_Sorted %>%
mutate(Time=Time-24)  

RPAs_Sorted_added_time <-RPAs_Sorted %>%
mutate(Time=Time+24)  

RPAS_extra_time <- RPAs_Sorted %>%
rbind(RPAs_Sorted_negative_time,RPAs_Sorted_added_time)


# Predict LOESS----------------------------------------------------------------

span_width <-.5

 
G333_model <- loess(Diff_24_hour_mean ~ Time, filter(RPAS_extra_time,Station=="G333"),span = span_width,method.args = list(family = "symmetric",degree=2))
G334_model <- loess(Diff_24_hour_mean ~ Time, filter(RPAS_extra_time,Station=="G334"),span = span_width,method.args = list(family = "symmetric",degree=2)) 
G381_model <- loess(Diff_24_hour_mean ~ Time, filter(RPAS_extra_time,Station=="G381"),span = span_width,method.args = list(family = "symmetric",degree=2))
G384_model <- loess(Diff_24_hour_mean ~ Time, filter(RPAS_extra_time,Station=="G384"),span = span_width,method.args = list(family = "symmetric",degree=2),control = loess.control(surface = "direct"))
G379_model <- loess(Diff_24_hour_mean ~ Time, filter(RPAS_extra_time,Station=="G379"),span = span_width,method.args = list(family = "symmetric",degree=2))
G380_model <- loess(Diff_24_hour_mean ~ Time, filter(RPAS_extra_time,Station=="G380"),span = span_width,method.args = list(family = "symmetric",degree=2))
G378_model <- loess(Diff_24_hour_mean ~ Time, filter(RPAS_extra_time,Station=="G378"),span = span_width,method.args = list(family = "symmetric",degree=2))
G377_model <- loess(Diff_24_hour_mean ~ Time, filter(RPAS_extra_time,Station=="G377"),span = span_width,method.args = list(family = "symmetric",degree=2))

G377_Predicted<- data.frame(Time= seq(-24, 48,.1)) %>% mutate(Predicted=predict(data=.,G377_model, Time, se = FALSE)) %>% rename(`G377`="Predicted")
G333_Predicted<- data.frame(Time= seq(-24, 48,.1)) %>% mutate(Predicted=predict(data=.,G333_model, Time, se = FALSE)) %>% rename(`G333`="Predicted")
G334_Predicted<- data.frame(Time= seq(-24, 48,.1)) %>% mutate(Predicted=predict(data=.,G334_model, Time, se = FALSE)) %>% rename(`G334`="Predicted")
G381_Predicted<- data.frame(Time= seq(-24, 48,.1)) %>% mutate(Predicted=predict(data=.,G381_model, Time, se = FALSE)) %>% rename(`G381`="Predicted")
G384_Predicted<- data.frame(Time= seq(-24, 48,.1)) %>% mutate(Predicted=predict(data=.,G384_model, Time, se = FALSE)) %>% rename(`G384`="Predicted")
G379_Predicted<- data.frame(Time= seq(-24, 48,.1)) %>% mutate(Predicted=predict(data=.,G379_model, Time, se = FALSE)) %>% rename(`G379`="Predicted")
G380_Predicted<- data.frame(Time= seq(-24, 48,.1)) %>% mutate(Predicted=predict(data=.,G380_model, Time, se = FALSE)) %>% rename(`G380`="Predicted")
G378_Predicted<- data.frame(Time= seq(-24, 48,.1)) %>% mutate(Predicted=predict(data=.,G378_model, Time, se = FALSE)) %>% rename(`G378`="Predicted")

Model_predictions <- left_join(G377_Predicted,G333_Predicted,by="Time") %>%
left_join(G384_Predicted,by="Time") %>%
left_join(G381_Predicted,by="Time") %>%
left_join(G379_Predicted,by="Time") %>%
left_join(G380_Predicted,by="Time") %>% 
left_join(G378_Predicted,by="Time") %>%
left_join(G334_Predicted,by="Time") %>%
pivot_longer(names_to = "Station",values_to="LOESS Prediction",2:9)  

predict(data=.,G377_model, 12, se = TRUE)
gam.check(G377_Predicted)

summary(G384_model )
tail(warnings(), 5)

# Extract LOESS parameters ------------------------------------------------


Model_parameters<- Model_predictions %>%
group_by(Station) %>%
filter(between(Time,0,24)) %>%
mutate(max=max(`LOESS Prediction`,na.rm = TRUE),min=min(`LOESS Prediction`,na.rm = TRUE),`Max Time`=ifelse(`LOESS Prediction`==max,Time,NA)) %>%
summarise(n=n(),Height=max(max,na.rm =TRUE),`Max Time`=max(`Max Time`,na.rm = TRUE))


str(Model_predictions)

# Visualize ---------------------------------------------------------------

ggplot(filter(RPAS_extra_time,Station=="G377"),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
geom_smooth(method = "gam", formula = y ~s(x),color="red")+
geom_smooth(method="loess",color="black",span=span_width,method.args = list(family = "symmetric",degree=2),se=FALSE)+
geom_smooth(data=slice_sample(filter(RPAS_extra_time,Station=="G377"),n=900),method="loess",color="green",span=span_width,method.args = list(family = "symmetric",degree=2),se=FALSE)+
theme_bw()+
geom_line(data =filter(Model_predictions,Station=="G384"),aes(Time,`LOESS Prediction`) ,color="blue")+coord_cartesian(ylim = c(-10,10),xlim = c(0,24))+
scale_y_continuous(breaks = seq(-10,10,1))



#Hourly TP Variation from the Daily Mean by Station
ggplot(RPAS_extra_time,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
geom_smooth(method="loess",color="black",span=span_width,method.args = list(family = "symmetric",degree=2),normalize=FALSE)+theme_bw()+
geom_line(data =Model_predictions,aes(Time,`LOESS Prediction`) ,color="blue")+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
#scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
#coord_cartesian(xlim = c(0, 24))+  
labs(title="Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")
