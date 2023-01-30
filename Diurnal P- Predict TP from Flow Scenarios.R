#Objective- Predict TP for various flow scenarios 




library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(ggpmisc)
library(purrr)
library(broom)
library(mgcv)
library(gratia)
library(zoo)
library(ggeffects)
library(mgcViz)
library(stats)
library(dbhydroR)
library(cowplot)
library(gridExtra)
library(ggplotify)
library(ggpubr)
library(tidymv)
library(viridis)


# Import Data -------------------------------------------------------------



#flow scenario data
Flow_Scenarios <- read_csv("Data/Flow Scenarios.csv") %>%mutate(Year=2014, Day=280)
  

#Load Models
Mod_4 <- readRDS(file="./Data/Model/Mod_4.rda") #Import Model
Mod_4.5 <- readRDS(file="./Data/Model/Mod_4.5.rda") #Import Model
Mod_4.9 <- readRDS(file="./Data/Model/Mod_4.9.rda") #Import Model



# Create low and high flow scenarios --------------------------------------

#Sin approximation of flow HLRout=A*sin(w-theta)+A
#A=Average hourly flow in cm/day    w=frequency   theta=phase shift
Flow_Scenarios_high <- read_csv("Data/Flow Scenarios.csv") %>%  #High flow scenario 
mutate(Year=2015, Day=280, `Flow Type`="High Flow", Mean_Depth=2.5 , HLRout=(Scenario/100-.5)*-14*sin(Time*pi/12-pi/3*2)+7)   #Flow approximated with by sin wave HLRout=sin(Frequency+phase shift)+average hourly flow

Flow_Scenarios_Moderate <- read_csv("Data/Flow Scenarios.csv") %>%
mutate(Year=2015, Day=200, `Flow Type`="Moderate Flow", Mean_Depth=1.75 ,HLRout=(Scenario/100-.5)*-12*sin(Time*pi/12-pi/3*2)+6)  #Moderate flow Scenario

Flow_Scenarios_low <- read_csv("Data/Flow Scenarios.csv") %>%
mutate(Year=2015, Day=30, `Flow Type`="Low Flow", Mean_Depth=1,HLRout=(Scenario/100-.5)*-1*sin(Time*pi/12-pi/3*2)+.5)  #Low flow Scenario


# Predict flow ------------------------------------------------------------


Predicted_TP_Mod_4.7.1_high <- as.data.frame(predict.gam(Mod_4.7.1,se.fit=TRUE, newdata = Flow_Scenarios_high,type="response")) %>% bind_cols(Flow_Scenarios_high)
Predicted_TP_Mod_4.7.1._Moderate <- as.data.frame(predict.gam(Mod_4.7.1,se.fit=TRUE, newdata = Flow_Scenarios_Moderate ,type="response")) %>% bind_cols(Flow_Scenarios_Moderate)
Predicted_TP_Mod_4.7.1_low <- as.data.frame(predict.gam(Mod_4.7.1,se.fit=TRUE, newdata = Flow_Scenarios_low,type="response")) %>% bind_cols(Flow_Scenarios_low)
Predicted_TP_Mod_4 <- bind_rows(Predicted_TP_Mod_4.7.1_low,Predicted_TP_Mod_4.7.1._Moderate,Predicted_TP_Mod_4.7.1_high)

Predicted_TP_Mod_4.5_high <- as.data.frame(predict.gam(Mod_4.5,se.fit=TRUE, newdata = Flow_Scenarios_high,type="response")) %>% bind_cols(Flow_Scenarios_high)
Predicted_TP_Mod_4.5_Moderate <- as.data.frame(predict.gam(Mod_4.5,se.fit=TRUE, newdata = Flow_Scenarios_Moderate ,type="response")) %>% bind_cols(Flow_Scenarios_Moderate)
Predicted_TP_Mod_4.5_low <- as.data.frame(predict.gam(Mod_4.5,se.fit=TRUE, newdata = Flow_Scenarios_low,type="response")) %>% bind_cols(Flow_Scenarios_low)
Predicted_TP_Mod_4 <- bind_rows(Predicted_TP_Mod_4.5_low,Predicted_TP_Mod_4.5_Moderate,Predicted_TP_Mod_4.5_high)

Predicted_TP_Mod_4.6_high <- as.data.frame(predict.gam(Mod_4.6,se.fit=TRUE, newdata = Flow_Scenarios_high,type="response")) %>% bind_cols(Flow_Scenarios_high)
Predicted_TP_Mod_4.6_Moderate <- as.data.frame(predict.gam(Mod_4.6,se.fit=TRUE, newdata = Flow_Scenarios_Moderate ,type="response")) %>% bind_cols(Flow_Scenarios_Moderate)
Predicted_TP_Mod_4.6_low <- as.data.frame(predict.gam(Mod_4.6,se.fit=TRUE, newdata = Flow_Scenarios_low,type="response")) %>% bind_cols(Flow_Scenarios_low)
Predicted_TP_Mod_4 <- bind_rows(Predicted_TP_Mod_4.6_low,Predicted_TP_Mod_4.6_Moderate,Predicted_TP_Mod_4.6_high)

Predicted_TP_Mod_4.9_high <- as.data.frame(predict.gam(Mod_4.9,se.fit=TRUE, newdata = Flow_Scenarios_high,type="response")) %>% bind_cols(Flow_Scenarios_high)
Predicted_TP_Mod_4.9_Moderate <- as.data.frame(predict.gam(Mod_4.9,se.fit=TRUE, newdata = Flow_Scenarios_Moderate ,type="response")) %>% bind_cols(Flow_Scenarios_Moderate)
Predicted_TP_Mod_4.9_low <- as.data.frame(predict.gam(Mod_4.9,se.fit=TRUE, newdata = Flow_Scenarios_low,type="response")) %>% bind_cols(Flow_Scenarios_low)
Predicted_TP_Mod_4 <- bind_rows(Predicted_TP_Mod_4.9_low,Predicted_TP_Mod_4.9_Moderate,Predicted_TP_Mod_4.9_high)

Predicted_TP_Mod_4.1_high <- as.data.frame(predict.gam(Mod_4.1,se.fit=TRUE, newdata = Flow_Scenarios_high,type="response")) %>% bind_cols(Flow_Scenarios_high)
Predicted_TP_Mod_4.1_Moderate <- as.data.frame(predict.gam(Mod_4.1,se.fit=TRUE, newdata = Flow_Scenarios_Moderate ,type="response")) %>% bind_cols(Flow_Scenarios_Moderate)
Predicted_TP_Mod_4.1_low <- as.data.frame(predict.gam(Mod_4.1,se.fit=TRUE, newdata = Flow_Scenarios_low,type="response")) %>% bind_cols(Flow_Scenarios_low)
Predicted_TP_Mod_4 <- bind_rows(Predicted_TP_Mod_4.1_low,Predicted_TP_Mod_4.1_Moderate,Predicted_TP_Mod_4.1_high)


Predicted_TP_Mod_4_high <- as.data.frame(predict.gam(Mod_4,se.fit=TRUE, newdata = Flow_Scenarios_high,type="response")) %>% bind_cols(Flow_Scenarios_high)
Predicted_TP_Mod_4_Moderate <- as.data.frame(predict.gam(Mod_4,se.fit=TRUE, newdata = Flow_Scenarios_Moderate ,type="response")) %>% bind_cols(Flow_Scenarios_Moderate)
Predicted_TP_Mod_4_low <- as.data.frame(predict.gam(Mod_4,se.fit=TRUE, newdata = Flow_Scenarios_low,type="response")) %>% bind_cols(Flow_Scenarios_low)
Predicted_TP_Mod_4 <- bind_rows(Predicted_TP_Mod_4_low,Predicted_TP_Mod_4_Moderate,Predicted_TP_Mod_4_high)

Predicted_TP_Mod_1.3_high <- as.data.frame(predict.gam(Mod_1.3,se.fit=TRUE, newdata = Flow_Scenarios_high,type="response")) %>% bind_cols(Flow_Scenarios_high)
Predicted_TP_Mod_1.3_Moderate <- as.data.frame(predict.gam(Mod_1.3,se.fit=TRUE, newdata = Flow_Scenarios_Moderate ,type="response")) %>% bind_cols(Flow_Scenarios_Moderate)
Predicted_TP_Mod_1.3_low <- as.data.frame(predict.gam(Mod_1.3,se.fit=TRUE, newdata = Flow_Scenarios_low,type="response")) %>% bind_cols(Flow_Scenarios_low)
Predicted_TP_Mod_1.3 <- bind_rows(Predicted_TP_Mod_1.3_low,Predicted_TP_Mod_1.3_Moderate,Predicted_TP_Mod_1.3_high)


# Calculate percent flow at night -----------------------------------------


Total_flow <- Predicted_TP_Mod_1.3 %>%
group_by(Station_ID,Scenario,`Flow Type`) %>%
summarise(`Total flow`=sum(HLRout))

percent_night <- Predicted_TP_Mod_1.3 %>%
left_join(Total_flow, by=c("Station_ID","Scenario","Flow Type")) %>%
group_by(Station_ID,Scenario,`Flow Type`,Time) %>%
summarise(`Hourly flow`=sum(HLRout),`Percent Daily Flow`=`Hourly flow`/`Total flow`*100) 

Sum_check <-percent_night %>%
group_by(Station_ID,Scenario,`Flow Type`) %>%
summarise(`Day %`=sum(if_else(between(Time,8,19),`Percent Daily Flow`,0)))
                         
Predicted_TP_Mod_1.3_percent_night <- Predicted_TP_Mod_1.3 %>%
left_join(Sum_check,by=c("Station_ID","Scenario","Flow Type"))  


# Calculate TP load -------------------------------------------------------

FWM_TP <- Predicted_TP_Mod_1.3_percent_night  %>%
mutate(`P load`=`fit`/24*HLRout)  %>%   #convert to hourly load.   
group_by(Station_ID,`Day %`,`Flow Type`) %>%
summarise(n(),`TP load`=sum(`P load`,na.rm=TRUE),`Total Flow`=sum(`HLRout`,na.rm=TRUE),`FWM-TP`=`TP load`/`Total Flow`*24,`Mean se`=mean(se.fit,na.rm=TRUE)) %>%
mutate(`Flow Type`=factor(`Flow Type`, levels = c("Low Flow", "Moderate Flow", "High Flow")))



# Plot FWMTP of different scenarios------------------------------------------------------------

fig_1 <-ggplot(FWM_TP,aes(`Day %`,`FWM-TP`,color=Station_ID,fill=Station_ID,linetype=`Flow Type`,shape=Station_ID))+geom_line()+geom_point(color="black")+geom_ribbon(aes(ymin=`FWM-TP`-`Mean se`,ymax=`FWM-TP`+`Mean se`),colour = NA,alpha=.4)+theme_bw()+
scale_x_continuous(breaks = seq(0,100,10))+scale_y_continuous(breaks = seq(8,40,1))+facet_wrap(~`Flow Type`)+scale_color_manual(values=c("#7fc97f","#beaed4","#fdc086"))+scale_fill_manual(values=c("#7fc97f","#beaed4","#fdc086"))+
theme(legend.position="bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.text.y.right = element_blank(),axis.ticks.y.right = element_blank())+
labs(title="",y=expression(paste("FWM-TP ( ",mu~g~L^-1,")")),x="Discharge During Day (%)")+guides(fill=guide_legend(title="Station"),color="none",linetype="none",shape="none")

ggsave("Figures/FWM-TP by percent flow at night.jpeg", plot = last_plot(), width = 8, height = 5, units = "in", dpi = 300, limitsize = TRUE)


# Plot HLR scenarios ------------------------------------------------------

fig_2 <- ggplot(filter(Predicted_TP_Mod_1.3_percent_night,`Flow Type`=="Moderate Flow" ),aes(Time,HLRout,color=as.factor(round(`Day %`,0))))+geom_line()+geom_point()+
scale_color_viridis( discrete = TRUE,option="D")+theme(axis.text.x=element_text(angle=90,hjust=1))+theme_bw()+scale_x_continuous(breaks = seq(0,24,4))+
labs(y=expression(paste("Discharge (",cm," ",day^-1,")")),x="Time")+theme(legend.position = "bottom" )+labs(color = "Discharge During Day (%)") 

ggsave("Figures/Flow scenarios.jpeg", plot = last_plot(), width = 8, height = 4, units = "in", dpi = 300, limitsize = TRUE)

#cowplot
HLR_and_FWMTP <-plot_grid(fig_1, fig_2, rel_heights = c(1, .4),ncol = 1,align = "v")

ggsave("FLow scenarios with HLR by hour.jpg", plot = HLR_and_FWMTP, path ="./Figures/",width = 10.666, height = 8, units = "in", dpi = 300, limitsize = TRUE)




# test --------------------------------------------------------------------


test_2012 <- head(read_csv("Data/Flow Scenarios.csv")) %>%
  mutate(Year=2012, Day=30,Time=12, `Flow Type`="Low Flow", Mean_Depth=1,HLRout=5)

test_2013 <- head(read_csv("Data/Flow Scenarios.csv")) %>%
mutate(Year=2013, Day=30,Time=12, `Flow Type`="Low Flow", Mean_Depth=1,HLRout=5)

test_2014 <- head(read_csv("Data/Flow Scenarios.csv")) %>%
mutate(Year=2014, Day=30,Time=12, `Flow Type`="Low Flow", Mean_Depth=1,HLRout=5) 

test_2015 <- head(read_csv("Data/Flow Scenarios.csv")) %>%
  mutate(Year=2015, Day=30,Time=12, `Flow Type`="Low Flow", Mean_Depth=1,HLRout=5) 

test_2016 <- head(read_csv("Data/Flow Scenarios.csv")) %>%
  mutate(Year=2016, Day=30,Time=12, `Flow Type`="Low Flow", Mean_Depth=1,HLRout=5) 

predict.gam(Mod_1,se.fit=FALSE, newdata = test_2012  ,type="response")
predict.gam(Mod_1,se.fit=FALSE, newdata = test_2013  ,type="response")
predict.gam(Mod_1,se.fit=FALSE, newdata = test_2014  ,type="response")
predict.gam(Mod_1,se.fit=FALSE, newdata = test_2015  ,type="response")
predict.gam(Mod_1,se.fit=FALSE, newdata = test_2016  ,type="response")

test_g379D <- head(read_csv("Data/Flow Scenarios.csv")) %>%
mutate(Year=2012, Station_ID="G379D",Day=30,Time=12, `Flow Type`="Low Flow", Mean_Depth=1,HLRout=5)

test_g381B <- head(read_csv("Data/Flow Scenarios.csv")) %>%
  mutate(Year=2012, Station_ID="G381B",Day=30,Time=12, `Flow Type`="Low Flow", Mean_Depth=1,HLRout=5)

test_g334 <- head(read_csv("Data/Flow Scenarios.csv")) %>%
  mutate(Year=2012, Station_ID="G334",Day=30,Time=12, `Flow Type`="Low Flow", Mean_Depth=1,HLRout=5)

predict.gam(Mod_1,se.fit=FALSE, newdata = test_g379D  ,type="response")
predict.gam(Mod_1,se.fit=FALSE, newdata = test_g381B  ,type="response")
predict.gam(Mod_1,se.fit=FALSE, newdata = test_g334  ,type="response")

