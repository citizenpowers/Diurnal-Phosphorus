#Goal of this script is to provide supplemental information for Diel P manuscript. Information includes method of model creation and evaluation.  


library(readr)      #import data 
library(dplyr)      #tidy data
library(lubridate)  #Date and time functions
library(mgcv)       #GAM model package
library(mgcViz)     #Visualization of GAM models
library(tidyr)      #Tidy Data
library(gridExtra)  #Needed to save partial effect plot
library(scales)     #rescaling function

# Import data -------------------------------------------------------------

RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") 



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