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
# Import Data -------------------------------------------------------------

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
Mod_5 <- readRDS(file="./Data/Model/Mod_5.rda") #Import Model
Mod_6 <- readRDS(file="./Data/Model/Mod_6.rda") #Import Model
Mod_7 <- readRDS(file="./Data/Model/Mod_7.rda") #Import Model
Mod_8 <- readRDS(file="./Data/Model/Mod_8.rda") #Import Model
Mod_9 <- readRDS(file="./Data/Model/Mod_9.rda") #Import Model


# Tidy data ---------------------------------------------------------------


Stage_discharge_data <- filter(RPAs_with_Flow_Stage_Weather_Sonde,Flowpath=="Outflow") %>%  #filter just the discharge data
mutate(`Mean_Depth` = case_when(Flowway=="STA-3/4 Central"~`Outflow Stage`-9.4,
                                Flowway=="STA-3/4 Western"~`Outflow Stage`-9.7,
                                Flowway=="STA-2 Central"~`Outflow Stage`-9.5)) %>%  #Calcaulate mean depth using average ground stage
rename(Stage_Out="Outflow Stage") %>% filter(TPO4<200) %>% mutate(Year=as.factor(Year)) 


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

#Create training and test data sets
set.seed(1)

#create ID column
Stage_discharge_data$id <- 1:nrow(Stage_discharge_data)

#use 70% of dataset as training set and 30% as test set 
Stage_discharge_data_train <- Stage_discharge_data %>% dplyr::sample_frac(0.70)
Stage_discharge_data_test  <- dplyr::anti_join(Stage_discharge_data, Stage_discharge_data_train, by = 'id')



# Compare compliance data to RPA data -------------------------------------

ggplot(Compliance_vs_RPA_Data ,aes(TPO4,Value))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq() #scatterplot observed vs fit

ggplot(Compliance_vs_RPA_Data ,aes(TPO4,Residual))+geom_point()+facet_wrap(~Station_ID,scales="free",ncol=1)+theme_bw()  #residuals by concentration of observed

ggplot(Compliance_vs_RPA_Data ,aes(Day,Residual))+geom_point()+facet_grid(Year~Station_ID,scales="free")+geom_smooth(se=FALSE)+theme_bw()  #residuals by season


# Create Models -----------------------------------------------------------



Mod_1 <- gam(TPO4 ~s(Day,by=Station_ID,k=20)+s(Station_ID,Year,bs="re")+s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,by=Station_ID,bs="cc",k=10),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Mod_1)  #Deviance 79.6 %
gam.check(Mod_1)
check(getViz(Mod_1))
saveRDS(Mod_1, file="./Data/Model/Mod_1.rda")

Mod_2 <- gam(TPO4 ~s(Day,by=Station_ID)+s(Station_ID,Year,bs="re")+s(HLRout,by=Station_ID)+s(HLRin,by=Station_ID)+s(Time,by=Station_ID,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Mod_2)  #Deviance 79.8 %
plot(Mod_2, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
check(getViz(Mod_2))
saveRDS(Mod_2, file="./Data/Model/Mod_2.rda")

Mod_3 <- gam(TPO4 ~s(Day,by=Station_ID,k=20)+s(Station_ID,Year,bs="re")+s(HLRout,k=10)+s(Mean_Depth)+s(Time,by=Station_ID,bs="cc",k=10)+ti(Mean_Depth,HLRout),data =Stage_discharge_data_train ,method="REML",family="gaussian",knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Mod_3)  #Deviance 85.5 %
check(getViz(Mod_3))
plot(Mod_3, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_3, view = c("HLRout", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_3, view = c("HLRout", "Mean_Depth"), plot.type = "persp",theta=-10,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_3, view = c("HLRout", "Mean_Depth"), plot.type = "contour",color="heat",too.far=.15, ticktype="detailed")  #3D HLR and station
concurvity(Mod_6)
saveRDS(Mod_3, file="./Data/Model/Mod_3.rda")

Mod_4 <- gam(TPO4 ~s(Day,by=Station_ID,k=20)+s(Station_ID,Year,bs="re")+s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,by=Station_ID,bs="cc",k=10),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Mod_4)  #Deviance 80.4 %
check(getViz(Mod_4))
plot(Mod_4, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_4, view = c("HLRout", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_4, view = c("HLRout", "Mean_Depth"), plot.type = "persp",theta=35,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_4, file="./Data/Model/Mod_4.rda")

Mod_5 <- gam(TPO4 ~s(Day,by=Station_ID,k=20)+s(Station_ID,Year,bs="re")+s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,by=Station_ID,bs="cc",k=10),data =Stage_discharge_data_train ,method="REML",family="gaussian",knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Mod_5)  #Deviance 84.2 %
concurvity(Mod_5)
gam.check(Mod_5)
check(getViz(Mod_5))
plot(Mod_5, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_5, view = c("HLRout", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed",se=2)  #3D HLR and station
saveRDS(Mod_5, file="./Data/Model/Mod_5.rda")
ggsave("Figures/Partial Dependence Plot Mod 5.png", plot = last_plot(), width = 8.3, height = 11.7, units = "in", dpi = 300, limitsize = TRUE)


Mod_6 <- gam(TPO4 ~s(Day,by=Station_ID,k=20)+Year+Station_ID+s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,by=Station_ID,bs="cc",k=10),data =Stage_discharge_data_train ,method="REML",family="gaussian",knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Mod_6)  #Deviance 83%
concurvity(Mod_6)
plot(Mod_6, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_6, view = c("HLRout", "Time"), plot.type = "persp",theta=25,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_6, view = c("Mean_Depth", "HLRout"), plot.type = "persp",theta=220,too.far=.1, ticktype="detailed")  #3D HLR and station
check(getViz(Mod_6))
gam.check(Mod_6)
k.check(Mod_6)
saveRDS(Mod_6, file="./Data/Model/Mod_6.rda")

Mod_7 <- gam(TPO4 ~s(Day,k=50)+Year+Station_ID+s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,by=Station_ID,bs="cc",k=10),data =Stage_discharge_data_train ,method="REML",family="gaussian",knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Mod_7)  #Deviance 70.7 %
gam.check(Mod_7)
k.check(Mod_7)
check(getViz(Mod_7))
plot(Mod_7, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_7, view = c("Mean_Depth", "HLRout"), plot.type = "persp",theta=110,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_7, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_7, file="./Data/Model/Mod_7.rda")

Mod_8 <- gam(TPO4 ~s(Day,k=10,bs="cc")+Year+Station_ID+s(HLRout,k=10)+s(Mean_Depth,k=5)+s(Time,bs="cc"),data =Stage_discharge_data_train ,method="REML",family="gaussian",knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Mod_8)  #Deviance 67.5 %
summary.gam(Mod_8)
gam.check(Mod_8)
check(getViz(Mod_8))
concurvity(Mod_8)
concurvity(Mod_8,full=FALSE)
cor(filter(Stage_discharge_data_train,Station_ID=="G334")$HLRout,filter(Stage_discharge_data_train,Station_ID=="G334")$Mean_Depth,method = "spearman") #test correlation between variables. greater than 0.7 too high and remove 1 variable 
vis.gam(Mod_8, view = c("Mean_Depth", "HLRout"), plot.type = "persp",theta=110,too.far=.1, ticktype="detailed")  #3D HLR and station
vis.gam(Mod_8, view = c("HLRout", "Time"), plot.type = "persp",theta=35,too.far=.1, ticktype="detailed")  #3D HLR and station
plot(Mod_8, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(Mod_8, file="./Data/Model/Mod_8.rda")



Mod_9 <- gam(TPO4 ~s(Day,k=50)+Year+Station_ID+ti(HLRout,Mean_Depth,bs = c('tp','tp'), k = c(25, 25))+s(Time,by=Station_ID,bs="cc",k=10),data =Stage_discharge_data_train ,method="REML",family="gaussian",knots=list(Day=c(0, 365),Time=c(0,24)))
summary(Mod_9)  #Deviance 82.2 %
plot(Mod_9, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_9, view = c("HLRout", "Time"), plot.type = "persp",theta=60,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_9, file="./Data/Model/Mod_9.rda")

#test model
Mod_10 <- bam(TPO4 ~ s(Time,bs="cc") +s(Day, k = 30, bs = 'cc') + s(Station_ID,Year,bs="re")+s(HLRout,bs="tp")+
        ti(Time, Day,  bs = c('cc','cc'), k = c(20, 10)) +
        ti(Day, HLRout, bs = c('cc','tp'), k = c(25, 15)) +
        ti(Time, HLRout, bs = c('cc','tp'), k = c(25, 15))
          ,data = Stage_discharge_data_train, method = 'fREML', nthreads = 4, discrete = TRUE)
summary(Mod_10)  #Deviance 81.3 %
plot(Mod_10, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_10, view = c("HLRout", "Time"), plot.type = "persp",theta=30,too.far=.1, ticktype="detailed")  #3D HLR and station
saveRDS(Mod_10, file="./Data/Model/Mod_10.rda")

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
Mod_3_test_Predicted <- as.data.frame(predict.gam(Mod_3,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% rename(Mod_3="fit") %>% select(Mod_3)
Mod_4_test_Predicted <- as.data.frame(predict.gam(Mod_4,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% mutate(fit=exp(fit)) %>% rename(Mod_4="fit") %>% select(Mod_4)
Mod_5_test_Predicted <- as.data.frame(predict.gam(Mod_5,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% rename(Mod_5="fit") %>% select(Mod_5)
Mod_6_test_Predicted <- as.data.frame(predict.gam(Mod_6,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% rename(Mod_6="fit") %>% select(Mod_6)
Mod_7_test_Predicted <- as.data.frame(predict.gam(Mod_7,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% rename(Mod_7="fit") %>% select(Mod_7)
Mod_8_test_Predicted <- as.data.frame(predict.gam(Mod_8,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% rename(Mod_8="fit") %>% select(Mod_8)
Mod_9_test_Predicted <- as.data.frame(predict.gam(Mod_9,se.fit=TRUE, newdata = Stage_discharge_data_test)) %>% rename(Mod_9="fit") %>% select(Mod_9)

Predicted_test_data <- bind_cols(select(Stage_discharge_data_test,Station_ID,Day,Year,Day,Time,HLRin,HLRout,TPO4),Mod_1_test_Predicted) %>%  #join predicted data to predictors
  bind_cols(Mod_2_test_Predicted) %>%
  bind_cols(Mod_3_test_Predicted) %>%  
  bind_cols(Mod_4_test_Predicted) %>%  
  bind_cols(Mod_5_test_Predicted) %>%  
  bind_cols(Mod_6_test_Predicted) %>%  
  bind_cols(Mod_7_test_Predicted) %>%   
  bind_cols(Mod_8_test_Predicted) %>%
  bind_cols(Mod_9_test_Predicted) %>%    
  pivot_longer(names_to="Model",values_to="fit",8:16) %>%  #Pivot data to long format for easy visualization
  rename(Value="TPO4") %>%
  mutate(Residual=Value-fit)                                 #Calculate Residuals

ggplot(Predicted_test_data ,aes(Value,fit,color=Model,fill=Model))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq() #scatterplot observed vs fit

ggplot(Predicted_test_data,aes(Value,fit,color=Model,fill=Model))+geom_point(color="grey")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq() #scatterplot of all stations together

ggplot(Predicted_test_data ,aes(Value,Residual,color=Model,fill=Model))+geom_point()+facet_grid(Model~Station_ID,scales="free")+geom_smooth(se=FALSE,color="black")+theme_bw()  #residuals by concentration of observed

ggplot(Predicted_test_data ,aes(Day,Residual,color=Model,fill=Model))+geom_point()+facet_grid(Year~Station_ID,scales="free")+geom_smooth(se=FALSE)+theme_bw()  #residuals by season

ggplot(Predicted_test_data ,aes(Day,Value,color=Model,fill=Model))+geom_point(color="grey")+facet_grid(Year~Station_ID,scales="free")+geom_line(aes(Day,fit))+theme_bw() #Time series of model predictions

ggplot(filter(Predicted_test_data,Station_ID!="G334" ),aes(Day+Time/24,Value,color=Model,fill=Model))+geom_point(color="grey")+facet_grid(Year~Station_ID,scales="free")+geom_line(aes(Day,fit))+theme_bw()+coord_cartesian(xlim = c(100,200)) #Time series of model predictions

ggplot(Stage_discharge_data_test ,aes(TPO4,Mean_Depth,color=Flowway,fill=Flowway))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(method="lm")+theme_bw()+stat_poly_line()+stat_poly_eq() #scatterplot observed vs fit


Predict_Summary_test <- Predicted_test_data %>%
group_by(Model,Station_ID) %>%  
summarise(n=n(),MSE=sum((Value-fit),na.rm=TRUE)^2/n(),`Mean Difference`=mean(Value-fit,na.rm=TRUE),`SD Difference`=sd(Value-fit,na.rm=TRUE),`R Squared`=cor(Value,fit)^2 )

AIC_Summary <- glance(Mod_1) %>% mutate(model="Mod_1") %>%
  bind_rows(glance(Mod_2) %>% mutate(model="Mod_2")) %>%
  bind_rows(glance(Mod_3) %>% mutate(model="Mod_3")) %>%
  bind_rows(glance(Mod_4) %>% mutate(model="Mod_4")) %>%
  bind_rows(glance(Mod_5) %>% mutate(model="Mod_5")) %>%
  bind_rows(glance(Mod_6) %>% mutate(model="Mod_6")) %>%
  bind_rows(glance(Mod_7) %>% mutate(model="Mod_7")) %>%
  bind_rows(glance(Mod_8) %>% mutate(model="Mod_8")) 



# Create PDP plot mod 5--------------------------------------------------------


# Set themes for each plot
themeSize <- theme(text = element_text(family = "serif"), 
axis.text = element_text(size = 11),
axis.title = element_text(size = 11), 
legend.title = element_text(size = 11), 
legend.text = element_text(size = 11))

# Create a figure list with all plots 
PDP_mod_5 <- list()

# use c for categorical terms and s for smoothed terms
PDP_mod_5[[1]]<- plot(sm(getViz(Mod_5), 1)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Seasonality (Day)")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_5[[2]]<- plot(sm(getViz(Mod_5), 2)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Seasonality (Day)")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_5[[3]] <- plot(sm(getViz(Mod_5), 3)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Seasonality (Day)")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_5[[4]] <- plot(sm(getViz(Mod_5), 4)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95)+l_points(size = 1, col = "red") +labs(x = expression("Gaussian quantiles Year and Station")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_5[[5]] <- plot(sm(getViz(Mod_5), 5)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("HLR (cm/day)")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_5[[6]] <- plot(sm(getViz(Mod_5), 6)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Water Depth (ft)")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_5[[7]] <- plot(sm(getViz(Mod_5),7)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Time")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_5[[8]] <- plot(sm(getViz(Mod_5),8)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Time")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_5[[9]] <- plot(sm(getViz(Mod_5),9)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Time")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize



fig_layout<- matrix(c(4,1,7,5,2,8,6,3,9), nrow = 3)

png("Figures/Partial Dependence Plot Mod 5.png", units = "mm", res = 1000, height = 280, width = 210)

grid.arrange(grobs = lapply(PDP_mod_5, "[[", "ggObj"), layout_matrix = fig_layout)

dev.off()




# Create PDP plot mod 8--------------------------------------------------------


# Set themes for each plot
themeSize <- theme(text = element_text(family = "serif"), 
                   axis.text = element_text(size = 11),
                   axis.title = element_text(size = 11), 
                   legend.title = element_text(size = 11), 
                   legend.text = element_text(size = 11))

# Create a figure list with all plots 
PDP_mod_8 <- list()


# use c for categorical terms and s for smoothed terms
PDP_mod_8[[1]]<- plot(sm(getViz(Mod_8), 1)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Seasonality (Day of year)")) +
scale_x_continuous(breaks=seq(0,365,30))+ theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_8 [[2]]<- plot(sm(getViz(Mod_8), 2)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("HLR (cm/day)")) +
  scale_x_continuous(breaks=seq(0,40,5))+  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_8 [[3]] <- plot(sm(getViz(Mod_8), 3)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Water Depth (ft)")) +
scale_x_continuous(breaks=seq(0,4,.5))+  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_8 [[4]] <- plot(sm(getViz(Mod_8), 4)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +labs(x = expression("Time")) +
scale_x_continuous(breaks=seq(0,24,3))  +theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_8 [[5]] <- plot(pterm(getViz(Mod_8), 1))+ l_fitPoints(size = 1, col = "red") + l_ciBar(linetype = 3, colour = "blue", level = 0.95)  +l_rug() +labs(x = expression("Year")) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize

PDP_mod_8 [[6]] <- plot(pterm(getViz(Mod_8), 2)) + l_fitPoints(size = 1, col = "red") + l_ciBar(linetype = 3, colour = "blue", level = 0.95)  +l_rug() +labs(x = expression("Station")) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("<0.00")) +theme_classic() +themeSize



fig_layout_8<- matrix(c(5,2,1,6,3,4), nrow = 3)

png("Figures/Partial Dependence Plot Mod 8.png", units = "mm", res = 1000, height = 280, width = 210)

grid.arrange(grobs = lapply(PDP_mod_8 , "[[", "ggObj"), layout_matrix = fig_layout_8)

dev.off()
