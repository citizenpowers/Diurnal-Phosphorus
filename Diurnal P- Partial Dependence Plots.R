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



# Create PDPs using IML package -------------------------------------------
mod <- Predictor$new(Mod_1.3, data = Stage_discharge_data_test)

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



