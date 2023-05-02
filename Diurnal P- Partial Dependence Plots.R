#Goal of this script is to create partial dependence plots using different R packages


library(readr)      #import data 
library(dplyr)      #tidy data
library(lubridate)  #Date and time functions
library(mgcv)       #GAM model package
library(mgcViz)     #Visualization of GAM models
library(tidyr)      #Tidy Data
library(gridExtra)  #Needed to save partial effect plot
library(scales)     #rescaling function
library(ICEbox)
library(pdp)
library(itsadug)
library(iml)
library(cowplot)

# Import data -------------------------------------------------------------

RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") 
Mod_1.3 <- readRDS(file="./Data/Model/Mod_1.3.rda") #Import Model


# Theme for plots ---------------------------------------------------------
# Create Theme for plots
themeSize<- theme(text = element_text(family = "serif"),axis.text = element_text(size = 11),axis.title = element_text(size = 11),legend.title = element_text(size = 11),legend.text = element_text(size = 11))



# Tidy Data ---------------------------------------------------------------

#Tidy dataframe into format MGCV can use to create GAM 
Stage_discharge_data <- RPAs_with_Flow_Stage_Weather_Sonde %>%
mutate(Flag=if_else(Station == "G334" & Date >="2017-01-01",TRUE,FALSE)  ) %>%                #Flag data from SAV crash in STA-2 cell 3 2017 
filter(Flag ==FALSE)  %>% select(-Flag) %>%                                                   #Remove unrepresentative data 
filter(`Flowpath Region`=="Outflow") %>%                                                      #filter just the discharge data
rename(Wind="WIND BELLEGLADE",HLRin="Inflow HLR",HLRout="Outflow HLR",Flowpath="Flowpath Region",TEMP="Temp S7",Rain="Rain S7",Stage_Out="Outflow Stage") %>% #Rename variables. Model will not accept variables with blank spaces as input 
mutate(Station_ID=as.factor(Station_ID),Flowway=as.factor(Flowway),Year=as.factor(Year), Month=as.factor(month(Date, label=TRUE, abbr=TRUE)),Day=yday(Date)) %>%   #code categorical variables as factors
mutate(`Mean_Depth` = case_when(Flowway=="STA-3/4 Central"~`Stage_Out`-9.4,
                                Flowway=="STA-3/4 Western"~`Stage_Out`-9.7,
                                Flowway=="STA-2 Central"~`Stage_Out`-9.5)) %>%                #Calculate mean depth using average ground stage
filter(HLRout<18,HLRout>0) %>%                                                                #filter out high HLRs that only exist in the STA-2 Central flow-way and reverse flow conditions
select(TPO4,Flowway,Station_ID,Year,Day,Time,TPO4,HLRout,Mean_Depth,Wind,Rain,Diff_24_hour_mean) %>%
drop_na()

#Create training and test data sets
set.seed(100)

#create ID column
Stage_discharge_data$id <- 1:nrow(Stage_discharge_data)

#use 70% of dataset as training set and 30% as test set 
Stage_discharge_data_train <- Stage_discharge_data %>% dplyr::sample_frac(0.70)
Stage_discharge_data_test  <- dplyr::anti_join(Stage_discharge_data, Stage_discharge_data_train, by = 'id')



# Partial dependence plots using ICEbox Package ---------------------------
custom_predict <- function(object, newdata) { predict(object, newdata)$predictions[,1]}



ice <- ice(object = Mod_1.3, X = Stage_discharge_data_train, predictor = "HLRout", predictfcn = custom_predict)


iceplot = ice(object = Mod_1.3, 
              X = matrix_data, 
              y = matrix_data$TPO4, 
              predictor = "TPO4")

plot(iceplot, 
     plot_orig_pts_preds = F,
     colorvec = '000000')


# Create PDP plots using PDP package --------------------------------------

par.HLR <- partial(Mod_1.3, pred.var = c("HLRout"), chull = TRUE)
par.Time <- partial(Mod_1.3, pred.var = c("Time"), chull = TRUE)
par.Year <- partial(Mod_1.3, pred.var = c("Year"), chull = TRUE)
par.Day <- partial(Mod_1.3, pred.var = c("Day"), chull = TRUE)
par.Depth <- partial(Mod_1.3, pred.var = c("Mean_Depth"), chull = TRUE)
par.Flowway<- partial(Mod_1.3, pred.var = c("Flowway"), chull = TRUE)

autoplot(par.Flowway, contour = TRUE)
autoplot(par.Depth, contour = TRUE,,rug=TRUE,train=Stage_discharge_data_train )
autoplot(par.Time, contour = TRUE,rug=TRUE,train=Stage_discharge_data_train )+theme_bw()
autoplot(par.Day, contour = TRUE,,rug=TRUE,train=Stage_discharge_data_train )
autoplot(par.HLR, contour = TRUE,rug=TRUE,train=Stage_discharge_data_train )
autoplot(par.Year, contour = TRUE)

#Ice plots
pred.ice <- function(object, newdata) predict( Mod_1.3, head(Stage_discharge_data_test))

par.Time <- partial(Mod_1.3, pred.var = c("Time"), chull = TRUE,pred.fun = pred.ice)
par.HLR.ICE <- partial(Mod_1.3, pred.var = c("HLRout"), chull = TRUE,pred.fun = pred.ice)
autoplot(par.HLR.ICE, contour = TRUE)



# Create PDPs using Itsadug package ---------------------------------------

plot(Mod_1.3, all.terms=TRUE, rug=FALSE)

plot_smooth(Mod_1.3, view="Time",rm.ranef=FALSE)


# Create PDPs using IML package -------------------------------------------
mod <- Predictor$new(Mod_1.3, data = Stage_discharge_data_test)



# Compute the accumulated local effects for the first feature
HLRout <- FeatureEffect$new(mod, feature = "Time", grid.size = 30)
HLRout$plot()+theme_bw()


# Again, but this time with a partial dependence plot and ice curves

Time_PDP_ICE <- FeatureEffect$new(mod,feature = "Time", method = "pdp+ice", grid.size = 30)
Time_PD_plot <- plot(Time_PDP_ICE)+theme_bw()+labs(x="Time (Hour)")+scale_y_continuous(name="Predicted TP",limits = c(2,4))+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))


HLROUT_PDP_ICE <- FeatureEffect$new(mod,feature = "HLRout", method = "pdp+ice", grid.size = 30)
HLR_PD_plot <- plot(HLROUT_PDP_ICE)+theme_bw()+labs(x="Discharge (cm/day)")+scale_y_continuous(name="Predicted TP",limits = c(2,4))+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))

Depth_PDP_ICE <- FeatureEffect$new(mod,feature = "Mean_Depth", method = "pdp+ice", grid.size = 30)
Depth_PD_plot <-plot(Depth_PDP_ICE)+theme_bw()+labs(x="Water Depth (ft)")+scale_y_continuous(name="Predicted TP",limits = c(2,4))+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))

Day_PDP_ICE <- FeatureEffect$new(mod,feature = "Day", method = "pdp+ice", grid.size = 30)
Day__PD_plot <-plot(Day_PDP_ICE)+theme_bw()+labs(x="Seasonality  (Day of Year)")+scale_y_continuous(name="Predicted TP",limits = c(2,4))+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))

Year_PDP_ICE <- FeatureEffect$new(mod,feature = "Year", method = "pdp+ice", grid.size = 30)
Year_PD_plot <- plot(Year_PDP_ICE)+theme_bw()+labs(x="Year")+scale_y_continuous(name="Predicted TP",limits = c(2,4))+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))

Flowway_PDP_ICE <- FeatureEffect$new(mod,feature = "Flowway", method = "pdp+ice", grid.size = 30)
Flowway_PD_plot <-plot(Flowway_PDP_ICE)+theme_bw()+labs(x="Flowway")+scale_y_continuous(name="Predicted TP",limits = c(2,4))+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))



plot_grid(Time_PD_plot ,Depth_PD_plot, HLR_PD_plot,Day__PD_plot,Flowway_PD_plot ,Year_PD_plot, labels = c('A', 'B','C','D','E','F'), label_size = 18,nrow=3)


ggsave("Figures/Partial Dependence Plots SI4.jpeg", plot = last_plot(), width = 8, height = 11.5, units = "in", dpi = 300, limitsize = TRUE)


citation("iml")


# re-scaled predictors model -----------------------------------------------

Stage_discharge_data_train_rescale <- Stage_discharge_data_train %>%
  mutate(Day=rescale(Day),Time=rescale(Time),HLRout=rescale(HLRout),Mean_Depth=rescale(Mean_Depth))

#allow for different HLR and Time by flow-way  (Figure 4a-c)
Mod_1.3_rescale <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+Flowway+ s(HLRout,by = Flowway, m = 2, bs = "tp")+s(Mean_Depth,k=5)+s(Time,by = Flowway, m = 2, bs = "cc"),data =Stage_discharge_data_train_rescale ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
plot(Mod_1.3_rescale, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_1.3_rescale, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed",type="response",cond=list(Station_ID="G379D"))  #3D HLR and station
summary(Mod_1.3_rescale)
gam.check(Mod_1.3_rescale)
saveRDS(Mod_1.3, file="./Data/Model/Mod_1.3.rda")


#allow for different HLR and Time by flow-way  scat distribution
Mod_1.3_scat <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+Flowway+ s(HLRout,by = Flowway, m = 2, bs = "tp")+s(Mean_Depth,k=5)+s(Time,by = Flowway, m = 2, bs = "cc"),data =Stage_discharge_data_train ,method="REML",family=scat(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
plot(Mod_1.3_scat, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_1.3, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed",type="response",cond=list(Station_ID="G379D"))  #3D HLR and station
summary(Mod_1.3_scat)
gam.check(Mod_1.3_scat,rep=500)


Mod_1.3_select <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+Flowway+ s(HLRout,by = Flowway, m = 2, bs = "tp")+s(Mean_Depth,k=5)+s(Time,by = Flowway, m = 2, bs = "cc"),select=TRUE,data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
plot(Mod_1.3_select, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_1.3_select, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed",type="response",cond=list(Station_ID="G379D"))  #3D HLR and station
summary(Mod_1.3_select)
gam.check(Mod_1.3_select,rep=1000)

Mod_1.3_m_1 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+Flowway+ s(HLRout,by = Flowway, m =1, bs = "tp")+s(Mean_Depth,k=5)+s(Time,by = Flowway, m = 1, bs = "cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
plot(Mod_1.3_m_1 , shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_1.3_m_1 , view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed",type="response",cond=list(Station_ID="G379D"))  #3D HLR and station
summary(Mod_1.3_m_1 )
gam.check(Mod_1.3_m_1 ,rep=1000)

Mod_1.3_year_flowway_RI <- gam(TPO4 ~s(Day,k=10,bs="cc")+Year+Flowway+ s(HLRout,by = Flowway, m = 2, bs = "tp")+s(Mean_Depth,k=5)+s(Time,by = Flowway, m = 2, bs = "cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
plot(Mod_1.3_year_flowway_RI, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_1.3_year_flowway_RI, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed",type="response",cond=list(Station_ID="G379D"))  #3D HLR and station
summary(Mod_1.3_year_flowway_RI)
gam.check(Mod_1.3_year_flowway_RI,rep=1000)

