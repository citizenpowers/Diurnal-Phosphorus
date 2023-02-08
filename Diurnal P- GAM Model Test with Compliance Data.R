#goal of this script is to test model with compliance data. 

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
library(itsadug)


# Import Data -------------------------------------------------------------
#flow and compliance data
Combined_BK_Flow <-  read.csv( "Data/Combined_BK_Flow.csv") #Import flow data
Compliance_data <- get_wq(station_id= c("G334","G381B","G379D"), date_min = "2012-07-01",date_max="2017-10-01",test_name ="PHOSPHATE, TOTAL AS P",raw=TRUE) #DBHYDRO WQ at compliance site
#RPA tidy data 
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") %>%
mutate(Flag=if_else(Station == "G334" & Date >="2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed  
filter(Flag ==FALSE)  %>% select(-Flag) %>%
rename(Wind="WIND BELLEGLADE",HLRin="Inflow HLR",HLRout="Outflow HLR",Flowpath="Flowpath Region",TEMP="Temp S7",Rain="Rain S7") %>% #models will not accept variables with blank spaces as input 
mutate(Station_ID=as.factor(Station_ID),Flowpath=as.factor(Flowpath)) %>%
mutate(Month=as.factor(month(Date, label=TRUE, abbr=TRUE))) %>%
mutate(Day=yday(Date)) %>%
filter(TPO4>0)

#Load Models
Mod_1 <- readRDS(file="./Data/Model/Mod_1.rda") #Import Model
Mod_2 <- readRDS(file="./Data/Model/Mod_2.rda") #Import Model
Mod_3 <- readRDS(file="./Data/Model/Mod_3.rda") #Import Model
Mod_4 <- readRDS(file="./Data/Model/Mod_4.rda") #Import Model
Mod_4.5 <- readRDS(file="./Data/Model/Mod_4.5.rda") #Import Model
Mod_4.6 <- readRDS(file="./Data/Model/Mod_4.6.rda") #Import Model
Mod_4.7 <- readRDS(file="./Data/Model/Mod_4.7.rda") #Import Model
Mod_4.8 <- readRDS(file="./Data/Model/Mod_4.8.rda") #Import Model
Mod_4.9 <- readRDS(file="./Data/Model/Mod_4.9.rda") #Import Model
Mod_5 <- readRDS(file="./Data/Model/Mod_5.rda") #Import Model
Mod_6 <- readRDS(file="./Data/Model/Mod_6.rda") #Import Model
Mod_7 <- readRDS(file="./Data/Model/Mod_7.rda") #Import Model
Mod_8 <- readRDS(file="./Data/Model/Mod_8.rda") #Import Model
Mod_9 <- readRDS(file="./Data/Model/Mod_9.rda") #Import Model
Mod_10 <- readRDS(file="./Data/Model/Mod_10.rda") #Import Model

# Tidy data ---------------------------------------------------------------

Stage_discharge_data <- filter(RPAs_with_Flow_Stage_Weather_Sonde,Flowpath=="Outflow") %>%  #filter just the discharge data
mutate(`Mean_Depth` = case_when(Flowway=="STA-3/4 Central"~`Outflow Stage`-9.4,
                                Flowway=="STA-3/4 Western"~`Outflow Stage`-9.7,
                                Flowway=="STA-2 Central"~`Outflow Stage`-9.5)) %>%  #Calcaulate mean depth using average ground stage
rename(Stage_Out="Outflow Stage") %>% filter(TPO4<2000) %>% mutate(Year=as.factor(Year)) %>%
select(TPO4,Station_ID,Year,Day,Time,TPO4,HLRout,Mean_Depth,Wind,Rain) %>%
filter(HLRout<18,HLRout>0) #filter out high HLRs that only exist in the STA-2 Central flow-way and reverse flow condtions

#Create training and test data sets
set.seed(100)

#create ID column
Stage_discharge_data$id <- 1:nrow(Stage_discharge_data)

#use 70% of dataset as training set and 30% as test set 
Stage_discharge_data_train <- Stage_discharge_data %>% dplyr::sample_frac(0.70)
Stage_discharge_data_test  <- dplyr::anti_join(Stage_discharge_data, Stage_discharge_data_train, by = 'id')


#Tidy Compliance data
Compliance_data_tidy <- Compliance_data %>%
filter(Collection.Method=="G",Sample.Type.New=="SAMP")  %>%   # remove FCEBs and autosampler samples
mutate(Hour=hour(dmy_hm(Collection_Date))) %>%                # Add hour to DF
mutate(Date=as.Date(dmy_hm(Collection_Date))) %>%             # Format date
rename(Station_ID="Station.ID") %>%                           # Rename Station_ID to standard format
mutate(Flag=if_else(Station_ID == "G334" & Date >"2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed    
filter(Flag ==FALSE)  %>% 
select(-Flag) %>%   
mutate(Value=Value*1000) %>%                                  # convert to ug/l  
select(Station_ID,Date,Hour,Value)                         

#create DF to test models on
Tidy_test_data <- Combined_BK_Flow %>%                        # Starts with DF of HLR for each flow-way
mutate(Date=as.Date(Date)) %>%
mutate(Day=yday(Date))  %>%
mutate(Time=Hour)  %>%
mutate(Year=year(Date))%>%  
mutate(`Station_ID` = case_when(`Flowway` == "STA-2 Central"~"G334",                       #Add station names to flowways
                                `Flowway` == "STA-3/4 Central"~"G381B",
                                `Flowway` == "STA-3/4 Western"~"G379D")) %>%
rename(HLRout="Outflow.HLR",HLRin="Inflow.HLR")  %>%                                                          #Standardize names
left_join(Compliance_data_tidy,by=c("Station_ID","Date","Hour")) %>%                       #join with compliance DF
drop_na(Value) #%>%                                                                         #Remove rows with NA in Value column
filter(Value<50)                                                                           #Drop outlying values? 

#Create DF to evaluate relationship between compliance dataset and RPA dataset
Compliance_vs_RPA_Data <- RPAs_with_Flow_Stage_Weather_Sonde %>%
left_join(select(Tidy_test_data,Station_ID,Date,Hour,Value,Day,Year),by=c("Station_ID","Date","Hour","Day","Year")) %>%
drop_na(Value) %>%
select(Station_ID,Date,Year,Day,Hour,TPO4,Value) %>%
mutate(Residual=TPO4-Value) 

#Tidy Compliance data
Compliance_data_tidy <- Compliance_data %>%
  filter(Collection.Method=="G",Sample.Type.New=="SAMP")  %>%   # remove FCEBs and autosampler samples
  mutate(Hour=hour(dmy_hm(Collection_Date))) %>%                # Add hour to DF
  mutate(Date=as.Date(dmy_hm(Collection_Date))) %>%             # Format date
  rename(Station_ID="Station.ID") %>%                           # Rename Station_ID to standard format
  mutate(Flag=if_else(Station_ID == "G334" & Date >"2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed    
  filter(Flag ==FALSE)  %>% 
  select(-Flag) %>%   
  mutate(Value=Value*1000) %>%                                  # convert to ug/l  
  select(Station_ID,Date,Hour,Value)                         



# Calculate percent flow by hour -----------------------------------------

#flow by day
Flow_by_day<- Combined_BK_Flow %>%                        # Starts with DF of HLR for each flow-way
mutate(Date=as.Date(Date)) %>%
mutate(Day=yday(Date))  %>%
mutate(Time=Hour)  %>%
mutate(Year=year(Date))%>%  
mutate(`Station_ID` = case_when(`Flowway` == "STA-2 Central"~"G334",                       #Add station names to flowways
                                  `Flowway` == "STA-3/4 Central"~"G381B",
                                  `Flowway` == "STA-3/4 Western"~"G379D")) %>%
rename(HLRout="Outflow.HLR",HLRin="Inflow.HLR") %>%
group_by(Flowway,Date) %>%
summarise(n=n(),`sum HLR`=sum(abs(HLRout),na.rm=TRUE)) %>%
filter(n==24)

#histogram of daily total flow
ggplot(Flow_by_day,aes(`sum HLR`/24,fill=Flowway))+geom_histogram(color="black")+facet_wrap(~Flowway,scales="free")+theme_bw()+labs(title="",x="Daily Mean HLR")

#histogram of daily total flow removing no to little flow days
ggplot(filter(Flow_by_day,`sum HLR`/24 > .5),aes(`sum HLR`/24,fill=Flowway))+geom_histogram(color="black")+facet_wrap(~Flowway,scales="free")+theme_bw()+labs(title="",x="Daily Mean HLR")

#Percent flow by hour
Daily_percent_flow_hour <- Combined_BK_Flow %>% mutate(Date=as.Date(Date)) %>% rename(HLRout="Outflow.HLR") %>% select(-Inflow, -Inflow.HLR) %>%
left_join(Flow_by_day,by=c("Flowway","Date")) %>%
mutate(`Percent flow by Hour`=HLRout/`sum HLR`*100)
    
ggplot(Daily_percent_flow_hour ,aes(as.factor(Hour),`Percent flow by Hour`,fill=Flowway))+geom_boxplot(color="black")+facet_wrap(~Flowway)+coord_cartesian(ylim = c(0, 10))+
labs(title="",y="Percent of Daily Discharge (%)",x="Hour")+guides(fill=guide_legend(title="Flow-way"))+theme_bw()+ theme(legend.position="bottom")

ggsave("Figures/Percent of Daily Discharge by Hour.jpeg", plot = last_plot(), width = 8, height = 5, units = "in", dpi = 300, limitsize = TRUE)






# Compare compliance data to RPA data -------------------------------------

ggplot(Compliance_vs_RPA_Data ,aes(TPO4,Value))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq() #scatterplot observed vs fit

ggplot(Compliance_vs_RPA_Data ,aes(TPO4,Residual))+geom_point()+facet_wrap(~Station_ID,scales="free",ncol=1)+theme_bw()  #residuals by concentration of observed

ggplot(Compliance_vs_RPA_Data ,aes(Day,Residual))+geom_point()+facet_grid(Year~Station_ID,scales="free")+geom_smooth(se=FALSE)+theme_bw()  #residuals by season


# Create Models -----------------------------------------------------------



pred <- predict(Mod_1, newdata = Stage_discharge_data_test, se.fit = TRUE, type = "iterms")
pred <- data.frame(id=Stage_discharge_data_test$id, fit = pred$fit[,1], se.fit = pred$se.fit[,1])
pred <-left_join(Stage_discharge_data_test,pred,by="id")


## compute CI on the logit (log-odds) scale
pred <- transform(pred, upper = fit + (2 * se.fit),lower = fit - (2 * se.fit))
## transform fitted values + CI to odds scale
pred <- transform(pred,odds = exp(fit), oupper = exp(upper), olower = exp(lower))

## plot on the logit-scale
p1 <- ggplot(pred, aes(x = Time, y = fit)) +
  geom_ribbon(aes(x= Time, ymin = lower, ymax = upper),
              inherit.aes = FALSE, alpha = 0.1) +
  geom_line()


#Model with global smooth for diel trend. no group level smooths (Model G)
Mod_1 <- gam(TPO4 ~s(Day,k=15,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+ s(HLRout,k=40)+s(Mean_Depth,k=6)+s(Time,bs="cc",k=10),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary.gam(Mod_1)  #69.2
concurvity(Mod_1)
gam.check(Mod_1)
draw(Mod_1)
vis.gam(Mod_1, view = c( "HLRout","Mean_Depth"), plot.type = "contour",theta=25,too.far=.1, ticktype="detailed")  #vis.gam provides model predictions. Not PDP!
vis.gam(Mod_1, view = c("HLRout", "Mean_Depth"), plot.type = "persp",theta=35,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_1, view = c("Mean_Depth", "Time"), plot.type = "contour",theta=65,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_1, view = c("Mean_Depth", "Time"), plot.type = "persp",theta=2655,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_1, view = c("HLRout", "Time"), plot.type = "contour",theta=35,too.far=.1, ticktype="detailed")  #3D HLR and station
HLR_Time_plot <-vis.gam(Mod_1, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed",color="terrain")  #3D HLR and station
plot.gam(Mod_1, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
plot.gam(Mod_1)
acf(resid(Mod_1$lme),lag.max=36,main="ACF")  #Check for autocorrelation to be used with GAMM 
pacf(resid(Mod_1$lme),lag.max=36,main="PACF")  #Check for autocorrelation
check(getViz(Mod_1))
saveRDS(Mod_1, file="./Data/Model/Mod_1.rda")

AIC(Mod_1,Mod_4)

HLR_Time_plot <-vis.gam(Mod_1, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed",color="terrain",type="response",xlab="Discharge (cm/day)",ylab="Time (hour)",zlab=paste(expression(TP~ug/L)))  #3D HLR and station

png("Figures/HLR_Time_perperctive_plot.png", units = "mm", res = 1000, height = 280, width = 210)
vis.gam(Mod_1, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed",color="terrain",type="response",xlab="Discharge (cm/day)",ylab="Time (hour)",zlab=paste(expression(TP~ug/L)))  #3D HLR and station
dev.off()

#mod 1 with gaussian distribution
Mod_1.a <- gam(TPO4 ~s(Day,k=15,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+ s(HLRout,k=10)+s(Mean_Depth,k=6)+s(Time,bs="cc"),data =Stage_discharge_data_train ,method="REML",family="gaussian",knots=list(Day=c(0, 366),Time=c(0,24)))
summary.gam(Mod_1.a)  #63
check(getViz(Mod_1.a))


#Model to test for interaction between Time and Discharge
Mod_1.1 <- gam(TPO4 ~s(Day,k=5,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+te(HLRout,Time,bs=c("tp","cc"),m=2)+t2(HLRout,Time,Station_ID,bs=c("tp","cc","re"),m=2,full=TRUE),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
plot(Mod_1.1, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_1.1, view = c("HLRout", "Time"), plot.type = "contour",theta=35,too.far=.1, ticktype="detailed")  #3D HLR and station
pvisgam(Mod_1.1,view=c("HLRout","Time"),select=4)
AIC(Mod_1,Mod_1.1)   #very little difference in AIC between models indicating little interaction between these variables 


#allow for different HLR common time smoother
Mod_1.2 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+ s(HLRout,by = Station_ID, m = 2, bs = "tp")+s(Mean_Depth,k=5)+s(Time,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
plot(Mod_1.2, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_1.2, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed",type="response",cond=list(Station_ID="G379D"))  #3D HLR and station
gam.check(Mod_1.2)
saveRDS(Mod_1.2, file="./Data/Model/Mod_1.2.rda")

#allow for different HLR individual time 
Mod_1.3 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+ s(HLRout,by = Station_ID, m = 2, bs = "tp")+s(Mean_Depth,k=5)+s(Time,by=Station_ID,m=2,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_1.3)
concurvity(Mod_1.3)
plot(Mod_1.3, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_1.3, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed",type="response",cond=list(Station_ID="G379D"))  #3D HLR and station
gam.check(Mod_1.3)
saveRDS(Mod_1.3, file="./Data/Model/Mod_1.3.rda")

png("Figures/HLR_Time_perperctive_plot_G379D.png", units = "mm", res = 1000, height = 160, width = 140)
vis.gam(Mod_1.3, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.03,cond=list(Station_ID="G379D"), ticktype="detailed",color="terrain",type="response",xlab="Discharge (cm/day)",ylab="Time (hour)",zlab=paste(expression(TP~ug/L)))  #3D HLR and station
dev.off()

png("Figures/HLR_Time_perperctive_plot_G381B.png", units = "mm", res = 1000, height = 160, width = 140)
vis.gam(Mod_1.3, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.03,cond=list(Station_ID="G381B"), ticktype="detailed",color="terrain",type="response",xlab="Discharge (cm/day)",ylab="Time (hour)",zlab=paste(expression(TP~ug/L)))  #3D HLR and station
dev.off()

png("Figures/HLR_Time_perperctive_plot_G334.png", units = "mm", res = 1000, height = 160, width = 140)
vis.gam(Mod_1.3, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.03,cond=list(Station_ID="G334"), ticktype="detailed",color="terrain",type="response",xlab="Discharge (cm/day)",ylab="Time (hour)",zlab=paste(expression(TP~ug/L)))  #3D HLR and station
dev.off()

#allow for different seasonality
Mod_1.3 <- gam(TPO4 ~s(Day,k=10,by = Station_ID, m = 2, bs = "tp")+s(Year,bs="re")+s(Station_ID,bs="re")+ s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
plot(Mod_1.3, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_1.3, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed")  #3D HLR and station


#Model with global smooth for diel trend with group level smooths with same wiggliness (Model GS)
Mod_2 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+ s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,bs="cc",m=2)+s(Time,Station_ID,m=1,bs="fs",xt=list(bs="cc")),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_2)  #Deviance 69.3%
concurvity(Mod_2)
plot(Mod_2, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
check(getViz(Mod_2))
saveRDS(Mod_2, file="./Data/Model/Mod_2.rda")

#Model with global smooth for diel trend with group level smooths with different wiggliness (Model GI)
Mod_3 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+ s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,bs="cc")+s(Time,by=Station_ID,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_3)  #Deviance 69.3 %
check(getViz(Mod_3))
plot(Mod_3, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_3, view = c("HLRout", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_3, view = c("HLRout", "Mean_Depth"), plot.type = "persp",theta=-10,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_3, view = c("HLRout", "Mean_Depth"), plot.type = "contour",color="heat",too.far=.15, ticktype="detailed")  #3D HLR and station
concurvity(Mod_6)
saveRDS(Mod_3, file="./Data/Model/Mod_3.rda")

#Model without global smooth. Group level smooths with different wiggliness only (Model I)
Mod_4 <- gam(TPO4 ~s(Day,k=15,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+ s(HLRout,k=10)+s(Mean_Depth,k=6)+s(Time,by=Station_ID,m=2,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_4)  #Deviance 69.3 %
check(getViz(Mod_4))
plot(Mod_4, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_4, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4, view = c("HLRout", "Time"), plot.type = "contour",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4, view = c("HLRout", "Mean_Depth"), plot.type = "persp",theta=35,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_4, file="./Data/Model/Mod_4.rda")

#Model without global smooth. Group level smooths with different wiggliness only (Model I) HLR allowed different wiggliness
Mod_4.5 <- gam(TPO4~s(Day, k = 10, bs = "cc") + s(Year, bs = "re") + s(HLRout, Station_ID, m = 2, bs = "fs", xt = list(bs = "tp")) + s(Mean_Depth, k = 5) + s(Time, by = Station_ID, m = 2, bs = "cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_4.5)  #Deviance 69.3 %
check(getViz(Mod_4.5))
plot(Mod_4.5, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
draw(Mod_4.5,select=3)
vis.gam(Mod_4.5, view = c( "HLRout","Mean_Depth"), plot.type = "contour",theta=25,too.far=.2, ticktype="detailed")  #vis.gam provides model predictions. Not PDP!
vis.gam(Mod_4.5, view = c("HLRout", "Mean_Depth"), plot.type = "persp",theta=135,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4.5, view = c("Mean_Depth", "Time"), plot.type = "contour",theta=65,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4.5, view = c("Mean_Depth", "Time"), plot.type = "persp",theta=2655,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4.5, view = c("HLRout", "Time"), plot.type = "contour",theta=35,too.far=.1, ticktype="detailed",xlim=c(0,15))  #contour HLR and station
vis.gam(Mod_4.5, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.03, ticktype="detailed",xlim=c(0,15))  #3D HLR and station
saveRDS(Mod_4.5, file="./Data/Model/Mod_4.5.rda")

#Model without global smooth. Group level smooths with different wiggliness only (Model GI) HLR allowed different wiggliness and global smooth
Mod_4.6 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(HLRout,k=10,m=2)+ s(HLRout,by=Station_ID,k=5,m=1)+s(Mean_Depth,k=10)+s(Time,by=Station_ID,m=2,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_4.6)  #Deviance 69.3 %
check(getViz(Mod_4.6))
plot(Mod_4.6, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_4.6, view = c("HLRout", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4.6, view = c("HLRout", "Mean_Depth"), plot.type = "contour",theta=35,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_4.6, file="./Data/Model/Mod_4.6.rda")

#Model without global smooth. Group level smooths with different wiggliness only (Model GS) HLR allowed different wiggliness and global smooth
Mod_4.7 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+ s(HLRout,Station_ID,m=2,bs="fs",xt=list(bs="tp"))+s(Mean_Depth,k=5)+s(Time,by=Station_ID,m=2,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_4.7)  #Deviance 69.3 %
check(getViz(Mod_4.7))
plot(Mod_4.7, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_4.7, view = c("HLRout", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4.7, view = c("HLRout", "Mean_Depth"), plot.type = "persp",theta=35,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4.7, view = c("HLRout", "Mean_Depth"), plot.type = "contour",theta=35,too.far=.2, ticktype="detailed")  #
saveRDS(Mod_4.7, file="./Data/Model/Mod_4.7.rda")


#Model without global smooth. Group level smooths with different wiggliness Interaction with Day
Mod_4.8 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+te(HLRout,Day,bs=c("tp","cc"),m=2)+t2(HLRout,Day,Station_ID,bs=c("tp","cc","re"),m=2)+s(Mean_Depth,k=5)+s(Time,by=Station_ID,m=2,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_4.8)  #Deviance 69.3 %
check(getViz(Mod_4.8))
plot(Mod_4.8, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_4.8, view = c("Day", "HLRout"), plot.type = "contour",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4.8, view = c("HLRout", "Mean_Depth"), plot.type ="contour",theta=250,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_4.8, file="./Data/Model/Mod_4.8.rda")

#Model without global smooth. Group level smooths with different wiggliness Interaction with water depth
Mod_4.9 <- gam(TPO4 ~s(Day,k=5,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+te(HLRout,Mean_Depth,bs=c("tp","tp"),m=2)+t2(HLRout,Mean_Depth,Station_ID,bs=c("tp","tp","re"),m=2,full=TRUE)+s(Time,by=Station_ID,m=2,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_4.9)  #Deviance 75.8 %
check(getViz(Mod_4.9))
plot.gam(Mod_4.9, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
plot(getViz(Mod_4.9), select = 4) + l_dens(type = "cond") + l_fitLine() + l_ciLine()  #package MgcvVIz
pvisgam(Mod_4.9,view=c( "HLRout","Mean_Depth"),plot.type="contour",select=4)     #PDP from itsadug package
vis.gam(Mod_4.9, view = c( "HLRout","Time"), plot.type = "contour",theta=25,too.far=.05, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4.9, view = c( "HLRout","Time"), plot.type = "contour",theta=25,too.far=.05, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4.9, view = c( "Mean_Depth","Time"), plot.type = "contour",theta=25,too.far=.05, ticktype="detailed")  #3D Water Depth and station
vis.gam(Mod_4.9, view = c( "HLRout","Time"), plot.type = "persp",theta=-15,too.far=.05, ticktype="detailed")  #3D Water Depth and station
vis.gam(Mod_4.9, view = c( "Day","Time"), plot.type = "contour",theta=-25,too.far=.05, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4.9, view = c("HLRout", "Mean_Depth"), plot.type = "contour",theta=70,too.far=.05, ticktype="detailed")  #3D HLR and station
plot_smooth(Mod_4.9,view="Time",cond=list(Mean_Depth=2,HLRout=0,Station_ID="G381B",Year=2015,Day=280),rm.ranef=FALSE)  #how does model predict effect of time as discharge changes
plot_smooth(Mod_4.9,view="Time",cond=list(Mean_Depth=2,HLRout=.5,Station_ID="G381B",Year=2015,Day=280),rm.ranef=FALSE)  #how does model predict effect of time as discharge changes
plot_smooth(Mod_4.9,view="Time",cond=list(Mean_Depth=2,HLRout=1,Station_ID="G381B",Year=2015,Day=280),rm.ranef=FALSE)  #how does model predict effect of time as discharge changes
plot_smooth(Mod_4.9,view="Time",cond=list(Mean_Depth=2,HLRout=2,Station_ID="G381B",Year=2015,Day=280),rm.ranef=FALSE)  #how does model predict effect of time as discharge changes
plot_smooth(Mod_4.9,view="Time",cond=list(Mean_Depth=2,HLRout=4,Station_ID="G381B",Year=2015,Day=280),rm.ranef=FALSE)  #how does model predict effect of time as discharge changes
plot_smooth(Mod_4.9,view="Time",cond=list(Mean_Depth=2,HLRout=5,Station_ID="G381B",Year=2015,Day=280),rm.ranef=FALSE)  #how does model predict effect of time as discharge changes
plot_smooth(Mod_4.9,view="Time",cond=list(Mean_Depth=2,HLRout=7,Station_ID="G381B",Year=2015,Day=280),rm.ranef=FALSE)  #how does model predict effect of time as discharge changes
plot_smooth(Mod_4.9,view="Time",cond=list(Mean_Depth=2,HLRout=10,Station_ID="G381B",Year=2015,Day=280),rm.ranef=FALSE)  #how does model predict effect of time as discharge changes
plot_smooth(Mod_4.9,view="Time",cond=list(Mean_Depth=2,HLRout=13,Station_ID="G381B",Year=2015,Day=280),rm.ranef=FALSE)  #how does model predict effect of time as discharge changes
plot_smooth(Mod_4.9,view="Time",cond=list(Mean_Depth=2,HLRout=16,Station_ID="G381B",Year=2015,Day=280),rm.ranef=FALSE)  #how does model predict effect of time as discharge changes
vis.gam(Mod_4.9, view = c("HLRout", "Time"), plot.type = "persp",theta=70,too.far=.05, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_4.9, file="./Data/Model/Mod_4.9.rda")

#Test for interaction between water depth and flow. Use ANOVA to test between model with interactions and model without
anova(Mod_1,Mod_4.9,test="Chisq")

#Model without global smooth. Group level smooths with different wiggliness Interaction with Day
Mod_4.95 <- gam(TPO4 ~s(Year,bs="re")+te(HLRout,Day,bs=c("tp","cc"),m=2)+t2(HLRout,Day,Station_ID,bs=c("tp","cc","re"),m=2)+te(HLRout,Mean_Depth,bs=c("tp","cc"),m=2)+t2(HLRout,Mean_Depth,Station_ID,bs=c("tp","cc","re"),m=2)+s(Time,by=Station_ID,m=2,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_4.95)  #Deviance 78.1 %
check(getViz(Mod_4.95))
plot(Mod_4.95, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_4.95, view = c("Mean_Depth", "HLRout"), plot.type = "contour",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4.95, view = c("Day", "HLRout"), plot.type = "contour",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_4.95, file="./Data/Model/Mod_4.95.rda")

#Model without global smooth for diel trend with group level smooths with same wiggliness (Model S)
Mod_5 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re") +s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,Station_ID,m=2,bs="fs",xt=list(bs="cc")),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_5)  #Deviance 69.3 %
concurvity(Mod_5)
gam.check(Mod_5)
check(getViz(Mod_5))
plot(Mod_5, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_5, view = c("HLRout", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_5, file="./Data/Model/Mod_5.rda")
ggsave("Figures/Partial Dependence Plot Mod 5.png", plot = last_plot(), width = 8.3, height = 11.7, units = "in", dpi = 300, limitsize = TRUE)


#Model with global smooth for diel trend. no group level smooths (Model G) using thin plate splines
Mod_6 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re") +s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,bs="tp"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_6)  #69.2
gam.check(Mod_6)
plot(Mod_6, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
check(getViz(Mod_6))
saveRDS(Mod_6, file="./Data/Model/Mod_6.rda")

#Model with global smooth for diel trend with group level smooths with same wiggliness (Model GS)  using thin plate splines
Mod_7 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+ s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,bs="tp",m=2)+s(Time,Station_ID,m=2,bs="fs",xt=list(bs="tp")),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_7)  #Deviance 69.3%
concurvity(Mod_7)
plot(Mod_7, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
check(getViz(Mod_7))
saveRDS(Mod_7, file="./Data/Model/Mod_7.rda")

#Model with global smooth for diel trend with group level smooths with different wiggliness (Model GI) using thin plate splines
Mod_8 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+ s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,bs="tp")+s(Time,by=Station_ID,bs="tp"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_8)  #Deviance 69.4 %
check(getViz(Mod_8))
plot(Mod_8, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_8, view = c("HLRout", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_8, view = c("HLRout", "Mean_Depth"), plot.type = "persp",theta=-10,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_8, view = c("HLRout", "Mean_Depth"), plot.type = "contour",color="heat",too.far=.15, ticktype="detailed")  #3D HLR and station
concurvity(Mod_8)
saveRDS(Mod_8, file="./Data/Model/Mod_8.rda")

#Model without global smooth. Group level smooths with different wiggliness only (Model I) using thin plate splines
Mod_9 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re")+ s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,by=Station_ID,m=2,bs="tp"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_9)  #Deviance 69.4 %
check(getViz(Mod_9))
plot(Mod_9, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_9, view = c("HLRout", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_9, view = c("HLRout", "Mean_Depth"), plot.type = "persp",theta=35,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_9, file="./Data/Model/Mod_9.rda")

#Model without global smooth for diel trend with group level smooths with same wiggliness (Model S) using thin plate splines
Mod_10 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(Station_ID,bs="re") +s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,Station_ID,m=2,bs="fs",xt=list(bs="tp")),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_10)  #Deviance 69.4 %
concurvity(Mod_10)
gam.check(Mod_10)
check(getViz(Mod_10))
plot(Mod_10, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_10, view = c("HLRout", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed",se=2)  #3D HLR and station
saveRDS(Mod_10, file="./Data/Model/Mod_10.rda")

#Model with interaction between flow and time 
Mod_11 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(Mean_Depth,k=5)+te(Time,HLRout, bs=c("cc", "tp"),m=2)+t2(Time,HLRout,Station_ID, bs=c("cc", "tp", "re"),m=2),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_11)  #Deviance 73.2 %
concurvity(Mod_11)
gam.check(Mod_11)
check(getViz(Mod_11))
plot(Mod_11, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_11, view = c("HLRout", "Time"), plot.type = "contour",theta=210,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_11, file="./Data/Model/Mod_11.rda")
ggsave("Figures/Partial Dependence Plot Mod 11.png", plot = last_plot(), width = 8.3, height = 11.7, units = "in", dpi = 300, limitsize = TRUE)

#Model with interaction between water  depth and time 
Mod_12 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+s(HLRout,k=10)+te(Time,Mean_Depth, bs=c("cc", "tp"),m=2)+t2(Time,Mean_Depth,Station_ID, bs=c("cc", "tp", "re"),m=2),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_12)  #Deviance 70.7 %
concurvity(Mod_12)
gam.check(Mod_12)
check(getViz(Mod_12))
plot(Mod_12, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_12, view = c("Mean_Depth", "Time"), plot.type = "contour",theta=190,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_12, file="./Data/Model/Mod_12.rda")
ggsave("Figures/Partial Dependence Plot Mod 12.png", plot = last_plot(), width = 8.3, height = 11.7, units = "in", dpi = 300, limitsize = TRUE)

#Model with interaction between day and time 
Mod_13 <- gam(TPO4 ~s(Year,bs="re")+s(HLRout,k=10)+s(Mean_Depth,k=5)+te(Time,Day, bs=c("cc", "cc"),m=2)+t2(Time,Day,Station_ID, bs=c("cc", "cc", "re"),m=2),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_13)  #Deviance 73.9 %
concurvity(Mod_13)
gam.check(Mod_13)
check(getViz(Mod_13))
plot(Mod_13, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_13, view = c("Day", "Time"), plot.type = "contour",theta=190,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_13, view = c("Day", "Time"), plot.type = "persp",theta=160,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_13, file="./Data/Model/Mod_13.rda")
ggsave("Figures/Partial Dependence Plot Mod 13.png", plot = last_plot(), width = 8.3, height = 11.7, units = "in", dpi = 300, limitsize = TRUE)

#Model with interaction between water depth and time without any covariates
Mod_14 <- gam(TPO4 ~te(Time,Mean_Depth, bs=c("cc", "cc"),m=2)+t2(Time,Mean_Depth,Station_ID, bs=c("cc", "cc", "re"),m=2),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_14)  #Deviance 34 %
concurvity(Mod_14)
gam.check(Mod_14)
check(getViz(Mod_14))
plot(Mod_14, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_14, view = c("Mean_Depth", "Time"), plot.type = "contour",theta=190,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_14, view = c("Mean_Depth", "Time"), plot.type = "persp",theta=160,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_14, file="./Data/Model/Mod_14.rda")
ggsave("Figures/Partial Dependence Plot Mod 14.png", plot = last_plot(), width = 8.3, height = 11.7, units = "in", dpi = 300, limitsize = TRUE)

#Model with interaction between day and time and all covariates
Mod_15 <- gam(TPO4 ~s(Year,bs="re")+te(Time,Day, bs=c("cc", "cc"),m=2)+t2(Time,Day,Station_ID, bs=c("cc", "cc", "re"),m=2)+te(Time,Mean_Depth, bs=c("cc", "cc"),m=2)+t2(Time,Mean_Depth,Station_ID, bs=c("cc", "cc", "re"),m=2)+te(Time,HLRout, bs=c("cc", "tp"),m=2)+t2(Time,HLRout,Station_ID, bs=c("cc", "tp", "re"),m=2),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_15)  #Deviance 75.1 %
concurvity(Mod_15)
gam.check(Mod_15)
check(getViz(Mod_15))
plot(Mod_15, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_15, view = c("Mean_Depth", "Time"), plot.type = "contour",theta=190,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_15, view = c("HLRout", "Time"), plot.type = "contour",theta=190,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_15, view = c("Day", "Time"), plot.type = "contour",theta=190,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_15, view = c("Day", "Time"), plot.type = "persp",theta=160,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_15, view = c("HLRout", "Mean_Depth"), plot.type = "contour",theta=160,too.far=.25, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_15, file="./Data/Model/Mod_15.rda")
ggsave("Figures/Partial Dependence Plot Mod 15.png", plot = last_plot(), width = 8.3, height = 11.7, units = "in", dpi = 300, limitsize = TRUE)

#Model with interaction between day and time and all covariates and interaction between water depth and flow
Mod_16 <- gam(TPO4 ~s(Year,bs="re")+te(Time,Day, bs=c("cc", "cc"),m=2)+t2(Time,Day,Station_ID, bs=c("cc", "cc", "re"),m=2)+te(Time,Mean_Depth, bs=c("cc", "cc"),m=2)+t2(Time,Mean_Depth,Station_ID, bs=c("cc", "cc", "re"),m=2)+te(Time,HLRout, bs=c("cc", "tp"),m=2)+t2(Time,HLRout,Station_ID, bs=c("cc", "tp", "re"),m=2)+te(HLRout,Mean_Depth, bs=c("tp", "tp"),m=2)+t2(HLRout,Mean_Depth,Station_ID, bs=c("tp", "tp", "re"),m=2),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
summary(Mod_16)  #Deviance 77.6 %
concurvity(Mod_16)
gam.check(Mod_16)
check(getViz(Mod_16))
plot(Mod_16, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_16, view = c("Mean_Depth", "Time"), plot.type = "contour",theta=190,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_16, view = c("HLRout", "Time"), plot.type = "contour",theta=190,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_16, view = c("Day", "Time"), plot.type = "contour",theta=190,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_16, view = c("Day", "Time"), plot.type = "persp",theta=160,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_16, view = c("HLRout", "Mean_Depth"), plot.type = "contour",theta=160,too.far=.25, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_16, view = c("HLRout", "Mean_Depth"), plot.type = "persp",theta=100,too.far=.25, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_16, view = c("Day", "Mean_Depth"), plot.type = "contour",theta=100,too.far=.25, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_16, view = c("Day", "HLRout"), plot.type = "contour",theta=100,too.far=.25, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_16, file="./Data/Model/Mod_16.rda")
ggsave("Figures/Partial Dependence Plot Mod 16.png", plot = last_plot(), width = 8.3, height = 11.7, units = "in", dpi = 300, limitsize = TRUE)





# Predict Data against compliance data-------------------------------------------------------

#Make predictions with models created in  script Diurnal P- GAM TP Models
Mod_1_Predicted <- as.data.frame(predict.gam(Mod_1,se.fit=TRUE, newdata = Tidy_test_data)) %>% mutate(fit=exp(fit)) %>%rename(Mod_1="fit") %>% select(Mod_1)
Mod_2_Predicted <- as.data.frame(predict.gam(Mod_2,se.fit=TRUE, newdata = Tidy_test_data)) %>% mutate(fit=exp(fit)) %>%rename(Mod_2="fit") %>% select(Mod_2)
Mod_3_Predicted <- as.data.frame(predict.gam(Mod_3,se.fit=TRUE, newdata = Tidy_test_data)) %>% rename(Mod_3="fit") %>% select(Mod_3)
Mod_4_Predicted <- as.data.frame(predict.gam(Mod_4,se.fit=TRUE, newdata = Tidy_test_data)) %>% rename(Mod_4="fit") %>% select(Mod_4)
Mod_5_Predicted <- as.data.frame(predict.gam(Mod_5,se.fit=TRUE, newdata = Tidy_test_data)) %>% rename(Mod_5="fit") %>% select(Mod_5)
Mod_6_Predicted <- as.data.frame(predict.gam(Mod_6,se.fit=TRUE, newdata = Tidy_test_data)) %>% rename(Mod_6="fit") %>% select(Mod_6)
Mod_7_Predicted <- as.data.frame(predict.gam(Mod_7,se.fit=TRUE, newdata = Tidy_test_data)) %>% rename(Mod_7="fit") %>% select(Mod_7)
Mod_8_Predicted <- as.data.frame(predict.gam(Mod_8,se.fit=TRUE, newdata = Tidy_test_data)) %>% rename(Mod_8="fit") %>% select(Mod_8)
Mod_9_Predicted <- as.data.frame(predict.gam(Mod_9,se.fit=TRUE, newdata = Tidy_test_data)) %>% rename(Mod_9="fit") %>% select(Mod_9)

Predicted_data <- bind_cols(Tidy_test_data,Mod_1_Predicted) %>%  #join predicted data to predictors
bind_cols(Mod_2_Predicted) %>%
bind_cols(Mod_3_Predicted) %>%  
bind_cols(Mod_4_Predicted) %>%  
bind_cols(Mod_5_Predicted) %>%  
bind_cols(Mod_6_Predicted) %>%  
bind_cols(Mod_7_Predicted) %>%   
bind_cols(Mod_8_Predicted) %>%    
bind_cols(Mod_9_Predicted) %>%     
pivot_longer(names_to="Model",values_to="fit",13:21) %>%                                 #Pivot data to long format for easy visualization
mutate(Residual=Value-fit)                                                               #Calculate Residuals


# Plot predictions vs compliance  Data ----------------------------------------------
ggplot(Predicted_data ,aes(Value,fit,color=Model,fill=Model))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq() #scatterplot observed vs fit

ggplot(Predicted_data ,aes(Value,Residual,color=Model,fill=Model))+geom_point()+facet_grid(Model~Station_ID,scales="free")+geom_smooth(se=FALSE,color="black")+theme_bw()  #residuals by concentration of observed

ggplot(Predicted_data ,aes(Day,Residual,color=Model,fill=Model))+geom_point()+facet_grid(Year~Station_ID,scales="free")+geom_smooth(se=FALSE)+theme_bw()  #residuals by season

ggplot(Predicted_data ,aes(Day,Value,color=Model,fill=Model))+geom_point(color="grey")+facet_grid(Year~Station_ID,scales="free")+geom_line(aes(Day,fit))+theme_bw() #Time series of model predictions
                                  

# summary of model performance with compliance data-----------------------------------------

Predict_Summary <- Predicted_data %>%
group_by(Model,Station_ID) %>%  
summarise(n=n(),MSE=sum((Value-fit)^2)/n,`Mean Difference`=mean(Value-fit),`SD Difference`=sd(Value-fit),`R Squared`=cor(Value,fit)^2 )

AIC_Summary_compliance <- glance(Mod_1) %>% mutate(model="Mod_1") %>%
bind_rows(glance(Mod_2) %>% mutate(model="Mod_2")) %>%
bind_rows(glance(Mod_3) %>% mutate(model="Mod_3")) %>%
bind_rows(glance(Mod_4) %>% mutate(model="Mod_4")) %>%
bind_rows(glance(Mod_5) %>% mutate(model="Mod_5")) %>%
bind_rows(glance(Mod_6) %>% mutate(model="Mod_6")) %>%
bind_rows(glance(Mod_7) %>% mutate(model="Mod_7")) %>%
bind_rows(glance(Mod_8) %>% mutate(model="Mod_8")) 

# Predict Data against test data-------------------------------------------------------

#Make predictions with models 
Mod_1_test_Predicted <- as.data.frame(predict.gam(Mod_1,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% mutate(fit=exp(fit)) %>%rename(Mod_1="fit") %>% select(Mod_1)
Mod_2_test_Predicted <- as.data.frame(predict.gam(Mod_2,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% mutate(fit=exp(fit)) %>%rename(Mod_2="fit") %>% select(Mod_2)
Mod_3_test_Predicted <- as.data.frame(predict.gam(Mod_3,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% mutate(fit=exp(fit)) %>% rename(Mod_3="fit") %>% select(Mod_3)
Mod_4_test_Predicted <- as.data.frame(predict.gam(Mod_4,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% mutate(fit=exp(fit)) %>% rename(Mod_4="fit") %>% select(Mod_4)
Mod_5_test_Predicted <- as.data.frame(predict.gam(Mod_5,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% mutate(fit=exp(fit)) %>% rename(Mod_5="fit") %>% select(Mod_5)
Mod_6_test_Predicted <- as.data.frame(predict.gam(Mod_6,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% mutate(fit=exp(fit)) %>% rename(Mod_6="fit") %>% select(Mod_6)
Mod_7_test_Predicted <- as.data.frame(predict.gam(Mod_7,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% mutate(fit=exp(fit)) %>% rename(Mod_7="fit") %>% select(Mod_7)
Mod_8_test_Predicted <- as.data.frame(predict.gam(Mod_8,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% mutate(fit=exp(fit)) %>% rename(Mod_8="fit") %>% select(Mod_8)
Mod_9_test_Predicted <- as.data.frame(predict.gam(Mod_9,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% mutate(fit=exp(fit)) %>% rename(Mod_9="fit") %>% select(Mod_9)
Mod_10_test_Predicted <- as.data.frame(predict.gam(Mod_10,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% mutate(fit=exp(fit)) %>% rename(Mod_10="fit") %>% select(Mod_10)

Predicted_test_data <- bind_cols(select(Stage_discharge_data_test,Station_ID,Day,Year,Day,Time,HLRout,TPO4),Mod_1_test_Predicted) %>%  #join predicted data to predictors
  bind_cols(Mod_2_test_Predicted) %>%
  bind_cols(Mod_3_test_Predicted) %>%  
  bind_cols(Mod_4_test_Predicted) %>%  
  bind_cols(Mod_5_test_Predicted) %>%  
  bind_cols(Mod_6_test_Predicted) %>%  
  bind_cols(Mod_7_test_Predicted) %>%   
  bind_cols(Mod_8_test_Predicted) %>%
  bind_cols(Mod_9_test_Predicted) %>%
  bind_cols(Mod_10_test_Predicted) %>%
  pivot_longer(names_to="Model",values_to="fit",7:16) %>%  #Pivot data to long format for easy visualization
  rename(Value="TPO4") %>%
  mutate(Residual=Value-fit)                                 #Calculate Residuals

ggplot(Predicted_test_data ,aes(Value,fit,color=Model,fill=Model))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq() #scatterplot observed vs fit

ggplot(Predicted_test_data,aes(Value,fit,color=Model,fill=Model))+geom_point(color="grey")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq() #scatterplot of all stations together

ggplot(Predicted_test_data ,aes(Value,Residual,color=Model,fill=Model))+geom_point()+facet_grid(Model~Station_ID,scales="free")+geom_smooth(se=TRUE,color="black")+theme_bw()  #residuals by concentration of observed

ggplot(Predicted_test_data ,aes(Day,Residual,color=Model,fill=Model))+geom_point()+facet_grid(Year~Station_ID,scales="free")+geom_smooth(se=TRUE,color="black")+theme_bw()  #residuals by season

ggplot(Predicted_test_data ,aes(Day,Value,color=Model,fill=Model))+geom_point(color="grey")+facet_grid(Year~Station_ID,scales="free")+geom_line(aes(Day,fit))+theme_bw() #Time series of model predictions

ggplot(filter(Predicted_test_data,Station_ID!="G334" ),aes(Day+Time/24,Value,color=Model,fill=Model))+geom_point(color="grey")+facet_grid(Year~Station_ID,scales="free")+geom_line(aes(Day,fit))+theme_bw()+coord_cartesian(xlim = c(100,200)) #Time series of model predictions

ggplot(Stage_discharge_data_test ,aes(Mean_Depth,TPO4,color=Flowway,fill=Flowway))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq() #Residuals by water depth


Predict_Summary_test <- Predicted_test_data %>%
group_by(Model,Station_ID) %>%  
summarise(n=n(),MSE=sum((Value-fit),na.rm=TRUE)^2/n(),`Mean Difference`=mean(Value-fit,na.rm=TRUE),`SD Difference`=sd(Value-fit,na.rm=TRUE),`R Squared`=cor(Value,fit)^2 )

AIC_Summary <- glance(Mod_1) %>% mutate(model="Mod 1 (G)") %>%
  bind_rows(glance(Mod_2) %>% mutate(model="Mod_2 (GS)")) %>%
  bind_rows(glance(Mod_3) %>% mutate(model="Mod_3 (GI)")) %>%
  bind_rows(glance(Mod_4) %>% mutate(model="Mod_4 (I)")) %>%
  bind_rows(glance(Mod_5) %>% mutate(model="Mod_5 (S)")) %>%
  bind_rows(glance(Mod_6) %>% mutate(model="Mod_6 (G-tp)")) %>%
  bind_rows(glance(Mod_7) %>% mutate(model="Mod_7 (GS-tp)")) %>%
  bind_rows(glance(Mod_8) %>% mutate(model="Mod_8 (GI-tp)")) %>%
bind_rows(glance(Mod_9) %>% mutate(model="Mod_9 (I-tp)")) %>%
bind_rows(glance(Mod_10) %>% mutate(model="Mod_10 (S-tp)")) 


# Create PDP for model 1.2 ------------------------------------------------

PDP_mod_1.2 <- list()

# use c for categorical terms and s for smoothed terms
PDP_mod_1.2[[1]]<- plot(sm(getViz(Mod_1.2), 1)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Seasonality (Day)")) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("g) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_1.2[[2]] <- plot(sm(getViz(Mod_1.2), 2)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95)+l_points(size = 1, col = "red") +labs(x = expression("Gaussian quantiles (Year)")) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("h) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_1.2[[3]] <- plot(sm(getViz(Mod_1.2), 3)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95)+l_points(size = 1, col = "red") +labs(x = expression("Gaussian quantiles (Flow-way)")) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("b) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_1.2[[4]] <- plot(sm(getViz(Mod_1.2), 4))  +labs(x = expression("Discharge G334 (cm/day)")) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("d) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_1.2[[5]] <- plot(sm(getViz(Mod_1.2), 5))  +labs(x = expression("Discharge G379D (cm/day)")) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("e) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_1.2[[6]] <- plot(sm(getViz(Mod_1.2), 6))  +labs(x = expression("Discharge G381B (cm/day)")) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("f) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_1.2[[7]] <- plot(sm(getViz(Mod_1.2), 7)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Water Depth (ft)")) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("c) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_1.2[[8]] <- plot(sm(getViz(Mod_1.2), 8)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Time of Day (Hour)")) +  scale_x_continuous(breaks=seq(0,24,3))+
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("a) p-value <0.001")) +theme_classic() +themeSize


fig_layout_1.2<- matrix(c(8,7,5,1,3,4,6,2), nrow = 4)

png("Figures/Partial Dependence Plot Mod 1.2.png", units = "mm", res = 1000, height = 280, width = 210)

grid.arrange(grobs = lapply(PDP_mod_1.2, "[[", "ggObj"), layout_matrix = fig_layout_1.2)

dev.off()


# Create PDP plot mod G--------------------------------------------------------

citation(mgcViz)

test <-predict_gam(Mod_1,newdata=Stage_discharge_data_test,type="link",se=TRUE)
test2 <-predict.gam(Mod_1,newdata=Stage_discharge_data_test,type="response",se=TRUE)
test3 <-add_fitted(Stage_discharge_data_test,Mod_1,value="link predictions", type="link")
test4 <- add_fitted(Stage_discharge_data_test,Mod_1,value="Response predictions", type="response")

test <- left_join(test3,test4,by=c("id","Year","Day","Time","HLRout","TPO4","Station_ID","Mean_Depth") ,keep = FALSE)

ggplot(test,aes(Day, `Response predictions`))+geom_point()+geom_smooth()


# Set themes for each plot
themeSize <- theme(text = element_text(family = "serif"), 
axis.text = element_text(size = 11),
axis.title = element_text(size = 11), 
legend.title = element_text(size = 11), 
legend.text = element_text(size = 11))

# Create a figure list with all plots 
PDP_mod_G <- list()

# use c for categorical terms and s for smoothed terms
PDP_mod_G[[1]]<- plot(sm(getViz(Mod_1), 1)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Seasonality (Day)")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("e) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_G[[2]] <- plot(sm(getViz(Mod_1), 2)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95)+l_points(size = 1, col = "red") +labs(x = expression("Gaussian quantiles (Year)")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("f) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_G[[3]] <- plot(sm(getViz(Mod_1), 3)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95)+l_points(size = 1, col = "red") +labs(x = expression("Gaussian quantiles (Flow-way)")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("b) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_G[[4]] <- plot(sm(getViz(Mod_1), 4))  +labs(x = expression("Discharge (cm/day)")) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("d) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_G[[5]] <- plot(sm(getViz(Mod_1), 5)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Water Depth (ft)")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("c) p-value <0.001")) +theme_classic() +themeSize

PDP_mod_G[[6]] <- plot(sm(getViz(Mod_1), 6)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Time of Day (Hour)")) +  scale_x_continuous(breaks=seq(0,24,3))+
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("a) p-value <0.001")) +theme_classic() +themeSize
                       
                       

fig_layout<- matrix(c(6,5,1,3,4,2), nrow = 3)

png("Figures/Partial Dependence Plot Mod G v2.png", units = "mm", res = 1000, height = 280, width = 210)

grid.arrange(grobs = lapply(PDP_mod_G, "[[", "ggObj"), layout_matrix = fig_layout)

dev.off()


test <- plot(sm(getViz(Mod_4.9), 4))+labs(x = expression("Discharge (cm/day)",y=expression("Water Depth "))) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("d) p-value <0.001")) +theme_classic() +themeSize

ggsave("test.png", plot = test, path ="./Figures/",width = 10.666, height = 8, units = "in", dpi = 300, limitsize = TRUE)

exp(.4)
exp(-.04)

# Create PDP plot mod I--------------------------------------------------------

# Create a figure list with all plots 
PDP_mod_I <- list()

# use c for categorical terms and s for smoothed terms
PDP_mod_I[[1]] <- plot(sm(getViz(Mod_4.9), 4))  +labs(x = expression("Discharge (cm/day)"),y="Mean Water Depth (ft)") + l_fitRaster() + l_fitContour() + 
scale_x_continuous(breaks=seq(0,24,3))  +theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("a) p-value  <0.001")) +theme_bw() 

PDP_mod_I[[2]] <- plot(sm(getViz(Mod_4.9), 4)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Discharge")) +
  scale_x_continuous(breaks=seq(0,24,3))  +theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("b) p-value  <0.001")) +theme_classic() +themeSize+scale_y_continuous(limits=c(-1,1))

PDP_mod_I[[3]] <- plot(sm(getViz(Mod_4.7), 5)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Discharge")) +
  scale_x_continuous(breaks=seq(0,24,3))  +theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("c) p-value  <0.001")) +theme_classic() +themeSize+scale_y_continuous(limits=c(-1,1))

PDP_mod_I[[3]] <- plot(sm(getViz(Mod_4.7), 6)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Discharge")) +
  scale_x_continuous(breaks=seq(0,24,3))  +theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("c) p-value  <0.001")) +theme_classic() +themeSize+scale_y_continuous(limits=c(-1,1))

plot(Mod_4.9, select = 4)  

fig_layout_I <- matrix(c(1,2,3), nrow = 1)

png("Figures/Partial Dependence Plot Mod I.png", units = "mm", res = 1000, height = 90, width = 210)

grid.arrange(grobs = lapply(PDP_mod_I , "[[", "ggObj"), layout_matrix = fig_layout_I)

dev.off()

# Create PDP plot mod I with  individual wiggliness by station for HLRout--------------------------------------------------------

# Create a figure list with all plots 
PDP_mod_I <- list()

# use c for categorical terms and s for smoothed terms
PDP_mod_I[[1]] <- plot(sm(getViz(Mod_4.5), 6)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Time")) +
  scale_x_continuous(breaks=seq(0,24,3))  +theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("a) p-value  <0.001")) +theme_classic() +themeSize

PDP_mod_I[[2]] <- plot(sm(getViz(Mod_4), 7)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Time")) +
  scale_x_continuous(breaks=seq(0,24,3))  +theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("b) p-value  <0.001")) +theme_classic() +themeSize

PDP_mod_I[[3]] <- plot(sm(getViz(Mod_4), 8)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Time")) +
  scale_x_continuous(breaks=seq(0,24,3))  +theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("c) p-value  <0.001")) +theme_classic() +themeSize



fig_layout_I <- matrix(c(1,2,3), nrow = 1)

png("Figures/Partial Dependence Plot Mod I.png", units = "mm", res = 1000, height = 90, width = 210)

grid.arrange(grobs = lapply(PDP_mod_I , "[[", "ggObj"), layout_matrix = fig_layout_I)

dev.off()

# Residual plot for model G -----------------------------------------------


png("Figures/Residual Plot Mod G.png", units = "mm", res = 1000, height = 200, width = 210)

check(getViz(Mod_1))

dev.off()

# summary of data used to create models -----------------------------------

Stage_discharge_data %>%  #Percent ofobservations with HLRout greater than 12-15 
summarise(n=n(),obs=sum(is.finite(TPO4)),`HLR larger `=sum(between(HLRout,12,15),na.rm=TRUE),`percent greater than 15`=`HLR larger than 15`/obs)

Stage_discharge_data %>%  #Percent ofobservations with HLRout greater than 3-10 
  summarise(n=n(),obs=sum(is.finite(TPO4)),`HLR larger within 3-10`=sum(between(HLRout,3,10),na.rm=TRUE),`percent greater than 15`=`HLR larger than 15`/obs)






