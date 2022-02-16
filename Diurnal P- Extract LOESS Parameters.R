#this scriptis test code. do not use for production


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
library(gratia)
library(zoo)

# Import Data -------------------------------------------------------------


#RPA tidy data
RPAs_Sorted <- read_csv("Data/RPAs Sorted.csv") %>%
mutate(Flag=if_else(Station == "G334" & Date >"2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed
mutate(Flag=if_else(Station == "G333" & Date =="2017-05-25",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed  
filter(Flag ==FALSE)  


# Add data at the boundaries ----------------------------------------------

RPAs_Sorted_negative_time <-RPAs_Sorted %>%
mutate(Time=Time-24)  

RPAs_Sorted_added_time <-RPAs_Sorted %>%
mutate(Time=Time+24)  

RPAS_extra_time <- RPAs_Sorted %>%
rbind(RPAs_Sorted_negative_time,RPAs_Sorted_added_time)

write.csv(RPAS_extra_time, "Data/RPAS_extra_time.csv",row.names=FALSE)

# Add Rolling Mean to DF --------------------------------------------------

Rolling_Mean_RPAs <-  RPAS_extra_time %>%
group_by(Station) %>%
arrange(Time) %>%
mutate(roll_mean=rollmean(Diff_24_hour_mean,12,fill = NA))



# Predict LOESS----------------------------------------------------------------

span_width <-.2

Model_predictions_LOESS <- LOESS_MODEL(RPAS_extra_time,span_width) 

Model_Parameters_LOESS <-LOESS_Extract_Parameters(Model_predictions_LOESS )

Model_predictions_LOESS_Symetric <- LOESS_MODEL_Symetric(RPAS_extra_time,span_width) 

Model_Parameters_LOESS_Symetric <-LOESS_Extract_Parameters(Model_predictions_LOESS_Symetric)

# Predict GAM -------------------------------------------------------------

Model_predictions_GAM <-GAM_MODEL(RPAS_extra_time) 

Model_Parameters_GAM <-GAM_Extract_Parameters(Model_predictions_GAM)

# Visualize ---------------------------------------------------------------


span_width_2 <-LOESS_MODEL(RPAS_extra_time,.2,50) 
span_width_25 <-LOESS_MODEL(RPAS_extra_time,.25,50)
span_width_3 <-LOESS_MODEL(RPAS_extra_time,.3,50)
span_width_35 <-LOESS_MODEL(RPAS_extra_time,.35,50)
span_width_4 <-LOESS_MODEL(RPAS_extra_time,.4,50)
span_width_45 <-LOESS_MODEL(RPAS_extra_time,.45,50)
span_width_5 <-LOESS_MODEL(RPAS_extra_time,.5,50)

#test span 
#LOESS Visualized All Stations
ggplot(RPAS_extra_time,aes(Time,Diff_24_hour_mean,color=Station))+#geom_point(shape=1)+
geom_point(data =span_width_2,aes(Time,`LOESS Prediction`) ,color="blue",size=1)+
geom_point(data =span_width_25,aes(Time,`LOESS Prediction`) ,color="red",size=1)+
geom_point(data =span_width_3,aes(Time,`LOESS Prediction`) ,color="green",size=1)+
geom_point(data =span_width_35,aes(Time,`LOESS Prediction`) ,color="yellow",size=1)+
geom_point(data =span_width_4,aes(Time,`LOESS Prediction`) ,color="pink",size=1)+
geom_point(data =span_width_45,aes(Time,`LOESS Prediction`) ,color="orange",size=1)+
geom_point(data =span_width_5,aes(Time,`LOESS Prediction`) ,color="black",size=1)+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+#scale_y_continuous(breaks = seq(-10,10,1))+
coord_cartesian(ylim = c(-10,10),xlim = c(0,24))+theme_bw()+
labs(title="Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

G333_data <- filter(RPAs_Sorted,Station=="G333")
ggplot(G333_data,aes(Time,Diff_24_hour_median))+geom_point()+theme_bw()
ggplot(G333_data,aes(Hour,mean(Diff_24_hour_mean)))+geom_point()+theme_bw()

ggplot(G333_data,aes(Diff_24_hour_median))+geom_histogram()+theme_bw()




ggplot(G333_data_Rolling_Mean,aes(Time,roll_mean))+geom_point()+theme_bw()
G333_data_Rolling_Mean <- filter(Rolling_Mean_RPAs,Station=="G333")



#LOESS Visualized Single Station
ggplot(filter(RPAS_extra_time,Station=="G334"),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
geom_smooth(method = "gam", formula = y ~s(x),color="red")+
geom_smooth(method="loess",color="black",span=span_width,method.args = list(family = "symmetric",degree=2),se=FALSE)+
#geom_smooth(data=slice_sample(filter(RPAS_extra_time,Station=="G377"),n=900),method="loess",color="green",span=span_width,method.args = list(family = "symmetric",degree=2),se=FALSE)+
geom_line(data=filter(RPAS_extra_time,Station=="G334"),aes(Time,rollmean(Diff_24_hour_mean,4)),color="yellow")+  
theme_bw()+
geom_line(data =filter(Model_predictions_LOESS,Station=="G334"),aes(Time,`LOESS Prediction`) ,color="blue")+coord_cartesian(ylim = c(-10,10),xlim = c(0,24))+
scale_y_continuous(breaks = seq(-10,10,1))

#LOESS Visualized All Stations
ggplot(RPAS_extra_time,aes(Time,Diff_24_hour_median,color=Station))+geom_point(shape=1)+
#geom_smooth(method="loess",color="black",formula =y ~ x,span=span_width,method.args = list(family = "symmetric",degree=2))+theme_bw()+
#geom_smooth(method = "gam", formula = y ~s(x),color="blue")+
#geom_smooth(method="loess",color="orange",formula =y ~ x,span=.25,method.args = list(family = "gaussian",degree=2),se=FALSE) +  
geom_point(data =Model_predictions_LOESS,aes(Time,`LOESS Prediction`) ,color="blue",size=1)+
#geom_line(data =Model_predictions_LOESS_Symetric,aes(Time,`LOESS Prediction`) ,color="green")+
facet_grid(~Station)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(breaks = seq(-10,10,1))+
#scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
coord_cartesian(ylim = c(-10,10),xlim = c(0,24))+theme_bw()+
labs(title="Variation from Daily Mean by Hour",y="TPO4 Deviation from daily mean (ug/L)",x="Hour")

#GAM Visualized Single Station
ggplot(filter(Model_predictions_GAM,Station=="G334"),aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
geom_smooth(method = "gam", formula = y ~s(x),color="black")+
geom_line(data =filter(Model_predictions_GAM,Station=="G334"),aes(Time,`Predicted`) ,color="blue")  +
scale_y_continuous(breaks = seq(-10,10,1))  +theme_bw()+coord_cartesian(ylim = c(-10,10),xlim = c(0,24))

#GAM Visualized All Stations
ggplot(Model_predictions_GAM,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
geom_smooth(method = "gam", formula = y ~s(x),color="black")+
geom_smooth(method = "loess",span=0.5,color="red")+
#geom_line(data =Model_predictions_GAM,aes(Time,`Predicted`) ,color="blue")  +
facet_wrap(~Station)+
scale_y_continuous(breaks = seq(-10,10,1))  +theme_bw()+coord_cartesian(ylim = c(-10,10),xlim = c(0,24))+
geom_smooth(method="loess",color="green",span=0.5,method.args = list(family = "symmetric",degree=2),se=FALSE)

#Rolling Mean Visualized all Stations
ggplot(Rolling_Mean_RPAs,aes(Time,Diff_24_hour_mean,color=Station))+geom_point(shape=1)+
geom_smooth(method = "gam", formula = y ~s(x),color="black")+
#geom_smooth(method = "loess",span=0.5,color="red")+
geom_point(aes(Time,`roll_mean`) ,color="red",fill="red")  +
facet_wrap(~Station)+
scale_y_continuous(breaks = seq(-10,10,1))  +theme_bw()+coord_cartesian(ylim = c(-10,10),xlim = c(0,24))



# Functions   (rename so they are not confused with functions on other script) ---------------------------------------------------------------

GAM_MODEL <- function(df)
{
  
  G333_model_GAM <- gam(data=filter(df,Station=="G333"),Diff_24_hour_median ~ s(Time, bs="cs") )
  G334_model_GAM <- gam(data=filter(df,Station=="G334"),Diff_24_hour_median ~ s(Time, bs="cs") )
  G381_model_GAM <- gam(data=filter(df,Station=="G381"),Diff_24_hour_median ~ s(Time, bs="cs") )
  G384_model_GAM <- gam(data=filter(df,Station=="G384"),Diff_24_hour_median ~ s(Time, bs="cs") )
  G379_model_GAM <- gam(data=filter(df,Station=="G379"),Diff_24_hour_median ~ s(Time, bs="cs") )
  G380_model_GAM <- gam(data=filter(df,Station=="G380"),Diff_24_hour_median ~ s(Time, bs="cs") )
  G378_model_GAM <- gam(data=filter(df,Station=="G378"),Diff_24_hour_median ~ s(Time, bs="cs") )
  G377_model_GAM <- gam(data=filter(df,Station=="G377"),Diff_24_hour_median ~ s(Time, bs="cs") )
  
  G333_Predicted_GAM<- filter(df,Station=="G333")  %>% add_fitted(G333_model_GAM,value="Predicted") 
  G377_Predicted_GAM<- filter(df,Station=="G377")  %>% add_fitted(G377_model_GAM,value="Predicted")
  G334_Predicted_GAM<- filter(df,Station=="G334")  %>% add_fitted(G334_model_GAM,value="Predicted")
  G381_Predicted_GAM<- filter(df,Station=="G381")  %>% add_fitted(G381_model_GAM,value="Predicted")
  G384_Predicted_GAM<- filter(df,Station=="G384")  %>% add_fitted(G384_model_GAM,value="Predicted")
  G379_Predicted_GAM<- filter(df,Station=="G379")  %>% add_fitted(G379_model_GAM,value="Predicted")
  G380_Predicted_GAM<- filter(df,Station=="G380")  %>% add_fitted(G380_model_GAM,value="Predicted")
  G378_Predicted_GAM<- filter(df,Station=="G378")  %>% add_fitted(G378_model_GAM,value="Predicted")
  
  Model_predictions_GAM <- rbind(G377_Predicted_GAM,G333_Predicted_GAM,by="Time") %>%
  rbind(G384_Predicted_GAM,by="Time") %>%
  rbind(G381_Predicted_GAM,by="Time") %>%
  rbind(G379_Predicted_GAM,by="Time") %>%
  rbind(G380_Predicted_GAM,by="Time") %>% 
  rbind(G378_Predicted_GAM,by="Time") %>%
  rbind(G334_Predicted_GAM,by="Time") %>%
  mutate(Time=as.numeric(Time),Diff_24_hour_median=as.numeric(Diff_24_hour_median),Predicted=as.numeric(Predicted)) %>%
  filter(Station!="Time")
  
return (Model_predictions_GAM)
}

GAM_Extract_Parameters <- function(df) 
{
Model_parameters_GAM<- df %>%
group_by(Station) %>%
filter(between(Time,0,24)) %>%
mutate(max=max(`Predicted`,na.rm = TRUE),min=min(`Predicted`,na.rm = TRUE)) %>%
mutate(`Max Time`=ifelse(`Predicted`==max,Time,NA),`Min Time`=ifelse(`Predicted`==min,Time,NA)) %>%
summarise(n=n(),
          Max=as.numeric(format(max(max,na.rm =TRUE),scientific=FALSE)),
          `Min`=as.numeric(format(min(min,na.rm=TRUE),scientific = FALSE)),
          `Max Time`=paste(sep = "",as.character(trunc(max(`Max Time`,na.rm = TRUE))),":",as.character(formatC(max(`Max Time`,na.rm=TRUE)%%1*60,width = 2, format = "d", flag = "0"))),
          `Min Time`=paste(sep = "",as.character(trunc(min(`Min Time`,na.rm = TRUE))),":",as.character(formatC(min(`Min Time`,na.rm = TRUE)%%1*60,width = 2, format = "d", flag = "0")))) %>%
mutate(Amplitude=as.numeric(format(as.numeric(Max)-as.numeric(`Min`),scientific = FALSE))) %>%
mutate(across(where(is.numeric),~round(.,3)))
}

LOESS_MODEL <-function(df,span_width)
{
  
G333_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G333"),span = span_width,method.args = list(family = "gaussian",degree=2))
G334_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G334"),span = span_width,method.args = list(family = "gaussian",degree=2)) 
G381_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G381"),span = span_width,method.args = list(family = "gaussian",degree=2))
G384_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G384"),span = span_width,method.args = list(family = "gaussian",degree=2))
G379_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G379"),span = span_width,method.args = list(family = "gaussian",degree=2))
G380_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G380"),span = span_width,method.args = list(family = "gaussian",degree=2))
G378_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G378"),span = span_width,method.args = list(family = "gaussian",degree=2))
G377_model <- loess(Diff_24_hour_mean ~ Time, filter(df,Station=="G377"),span = span_width,method.args = list(family = "gaussian",degree=2))

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
}


LOESS_Extract_Parameters <- function(df) 
{  
Model_parameters<- df %>%
group_by(Station) %>%
filter(between(Time,0,24)) %>%
mutate(max=max(`LOESS Prediction`,na.rm = TRUE),min=min(`LOESS Prediction`,na.rm = TRUE)) %>%
mutate(`Max Time`=ifelse(`LOESS Prediction`==max,Time,NA),`Min Time`=ifelse(`LOESS Prediction`==min,Time,NA)) %>%
summarise(n=n(),
          Max=as.numeric(format(max(max,na.rm =TRUE),scientific=FALSE)),
          `Min`=as.numeric(format(min(min,na.rm=TRUE),scientific = FALSE)),
          `Max Time`=paste(sep = "",as.character(trunc(max(`Max Time`,na.rm = TRUE))),":",as.character(formatC(max(`Max Time`,na.rm=TRUE)%%1*60,width = 2, format = "d", flag = "0"))),
          `Min Time`=paste(sep = "",as.character(trunc(min(`Min Time`,na.rm = TRUE))),":",as.character(formatC(min(`Min Time`,na.rm = TRUE)%%1*60,width = 2, format = "d", flag = "0")))) %>%
mutate(Amplitude=as.numeric(format(as.numeric(Max)-as.numeric(`Min`),scientific = FALSE))) %>%
mutate(across(where(is.numeric),~round(.,3)))
}

LOESS_MODEL_Symetric <-function(df,span_width)
{
  G333_model <- loess(mean_median ~ Time, filter(df,Station=="G333"),span = span_width,method.args = list(family = "symmetric",degree=2),normalize=FALSE)
  G334_model <- loess(mean_median ~ Time, filter(df,Station=="G334"),span = span_width,method.args = list(family = "symmetric",degree=2),normalize=FALSE) 
  G381_model <- loess(mean_median ~ Time, filter(df,Station=="G381"),span = span_width,method.args = list(family = "symmetric",degree=2),normalize=FALSE)
  G384_model <- loess(mean_median ~ Time, filter(df,Station=="G384"),span = span_width,method.args = list(family = "symmetric",degree=2),normalize=FALSE)
  G379_model <- loess(mean_median ~ Time, filter(df,Station=="G379"),span = span_width,method.args = list(family = "symmetric",degree=2),normalize=FALSE)
  G380_model <- loess(mean_median ~ Time, filter(df,Station=="G380"),span = span_width,method.args = list(family = "symmetric",degree=2),normalize=FALSE)
  G378_model <- loess(mean_median ~ Time, filter(df,Station=="G378"),span = span_width,method.args = list(family = "symmetric",degree=2),normalize=FALSE)
  G377_model <- loess(mean_median ~ Time, filter(df,Station=="G377"),span = span_width,method.args = list(family = "symmetric",degree=2),normalize=FALSE)
  
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
}