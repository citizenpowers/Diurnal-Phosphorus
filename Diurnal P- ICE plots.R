#create Individual Conditional Expectation plots to look for heterogeneous treatment effects 


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
#library(purrr)
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
library(ICEbox)
library(pdp)


# Import Data -------------------------------------------------------------

#RPA tidy data 
RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") %>%
  mutate(Flag=if_else(Station == "G334" & Date >="2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed  
  filter(Flag ==FALSE)  %>% select(-Flag) %>%
  rename(Wind="WIND BELLEGLADE",HLRin="Inflow HLR",HLRout="Outflow HLR",Flowpath="Flowpath Region",TEMP="Temp S7",Rain="Rain S7") %>% #models will not accept variables with blank spaces as input 
  mutate(Station_ID=as.factor(Station_ID),Flowpath=as.factor(Flowpath)) %>%
  mutate(Month=as.factor(month(Date, label=TRUE, abbr=TRUE))) %>%
  mutate(Day=yday(Date))


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
Mod_10 <- readRDS(file="./Data/Model/Mod_10.rda") #Import Model
Mod_13 <- readRDS(file="./Data/Model/Mod_13.rda") #Import Model

#Create Training and test data
Stage_discharge_data <- filter(RPAs_with_Flow_Stage_Weather_Sonde,Flowpath=="Outflow") %>%  #filter just the discharge data
mutate(`Mean_Depth` = case_when(Flowway=="STA-3/4 Central"~`Outflow Stage`-9.4,
                                  Flowway=="STA-3/4 Western"~`Outflow Stage`-9.7,
                                  Flowway=="STA-2 Central"~`Outflow Stage`-9.5)) %>%  #Calcaulate mean depth using average ground stage
rename(Stage_Out="Outflow Stage") %>% filter(TPO4<200) %>% mutate(Year=as.factor(Year)) %>%
select(TPO4,Station_ID,Year,Day,Time,TPO4,HLRout,Mean_Depth)

#Create training and test data sets
set.seed(1)

#create ID column
Stage_discharge_data$id <- 1:nrow(Stage_discharge_data)

#use 70% of dataset as training set and 30% as test set 
Stage_discharge_data_train <- Stage_discharge_data %>% dplyr::sample_frac(0.70)
Stage_discharge_data_test  <- dplyr::anti_join(Stage_discharge_data, Stage_discharge_data_train, by = 'id')

Stage_discharge_data_train_matrix <- data.matrix(Stage_discharge_data_train)


# Create ICE plots using PDP package---------------------------------------------------

set.seed(43)
Stage_discharge_data_subset_index = sample(1:nrow(Stage_discharge_data_train), size = 1000)
Stage_discharge_subset = Stage_discharge_data_train[Stage_discharge_data_subset_index,]

test <- partial(Mod_15, pred.var = "Time",ice = TRUE,pred.grid=Stage_discharge_subset,train=Stage_discharge_subset)
test1 <- partial(Mod_15, pred.var = "Time",ice = TRUE,train=Stage_discharge_subset)

ggplot(test,aes(Time,yhat, color=Station_ID))+geom_point()

autoplot(partial(Mod_1, pred.var = "Time",ice = TRUE,train=Stage_discharge_subset),center=TRUE, alpha = 0.2,plot.pdp=TRUE)+theme_bw()


plotPartial(partial(Mod_15, pred.var = "Day",ice = TRUE,train=Stage_discharge_subset),center=TRUE, alpha = 0.2)

plotPartial(partial(Mod_15, pred.var = "Station_ID",ice = TRUE,train=Stage_discharge_subset),center=TRUE, alpha = 0.2)

plotPartial(partial(Mod_4, pred.var = "Year",ice = TRUE,train=Stage_discharge_subset),center=TRUE, alpha = 0.2)

plotPartial(partial(Mod_11, pred.var = "HLRout",ice = TRUE,train=Stage_discharge_subset),center=TRUE, alpha = 0.2)

plotPartial(partial(Mod_12, pred.var = "Mean_Depth",ice = TRUE,train=Stage_discharge_subset),center=TRUE, alpha = 0.2)

plotPartial(partial(Mod_15, pred.var = "Mean_Depth",ice = TRUE,train=Stage_discharge_subset),center=TRUE, alpha = 0.2)

plotPartial(partial(Mod_15, pred.var = "Mean_Depth",ice = TRUE,train=Stage_discharge_subset),center=TRUE, alpha = 0.2)


b# ICE plots with ICEbox package -------------------------------------------
custom_predict <- function(object, newdata) { predict(object, newdata)$predictions[,1]}

ice <- ice(object = Mod_1, X = Stage_discharge_data_train, predictor = "HLRout", predictfcn = custom_predict)



# Correlation -------------------------------------------------------------

ggplot(Stage_discharge_data ,aes(`HLRout`,Mean_Depth,color=Station_ID,fill=Station_ID))+geom_point()+facet_wrap(~Station_ID,scales="free")+geom_smooth(method="lm",color="black")+stat_poly_eq() 

TP_Flow_Correlations_group <-Stage_discharge_data  %>%
group_by(Station_ID)  %>%
summarise(n=sum(!is.na(TPO4)),`Outflow pearson's Correlation`=cor(`HLRout`,Mean_Depth,use="pairwise.complete.obs", method = "pearson"),`Outflow Spearmans Correlation`=cor(`HLRout`,Mean_Depth,use="pairwise.complete.obs", method = "spearman"),`Outflow Kendall Correlation`=cor(`HLRout`,Mean_Depth,use="pairwise.complete.obs", method = "kendall"))

TP_Flow_Correlations <-Stage_discharge_data  %>%
summarise(n=sum(!is.na(TPO4)),`Outflow pearson's Correlation`=cor(`HLRout`,Mean_Depth,use="pairwise.complete.obs", method = "pearson"),`Outflow Spearmans Correlation`=cor(`HLRout`,Mean_Depth,use="pairwise.complete.obs", method = "spearman"),`Outflow Kendall Correlation`=cor(`HLRout`,Mean_Depth,use="pairwise.complete.obs", method = "kendall"))

