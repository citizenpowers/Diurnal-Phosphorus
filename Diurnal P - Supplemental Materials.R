#Goal of this script is to provide supplemental information for Diel P manuscript. Information includes method of model creation and evaluation.  


library(readr)      #import data 
library(dplyr)      #tidy data
library(lubridate)  #Date and time functions
library(mgcv)       #GAM model package
library(mgcViz)     #Visualization of GAM models
library(tidyr)      #Tidy Data
library(gridExtra)  #Needed to save partial effect plot
library(scales)     #rescaling function
library(stats)      #compare models using AIC
library(modelr)     #Calculate RMSE and R squared on test data set
library(tidygam)    #return model predictions in tibble
library(ggpmisc)    #Calculate R Squared within ggplot
citation("stats")
# Import data -------------------------------------------------------------

RPAs_with_Flow_Stage_Weather_Sonde <- read_csv("Data/RPA and Flow Stage Weather Sonde.csv") 
Combined_BK_Flow <- read_csv("Data/Combined_BK_Flow.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d"),  Inflow = col_number(), `Inflow HLR` = col_number(), Outflow = col_number(), `Outflow HLR` = col_number()))

# Theme for plots ---------------------------------------------------------
# Create Theme for plots
themeSize<- theme(text = element_text(family = "serif"),plot.title = element_text(size = 18),axis.text = element_text(size = 14),axis.title = element_text(size = 16),legend.title = element_text(size = 16),legend.text = element_text(size = 11))

# Tidy Data ---------------------------------------------------------------

#Tidy dataframe into format MGCV can use to create GAM 
Stage_discharge_data <- RPAs_with_Flow_Stage_Weather_Sonde %>%
mutate(Flag=if_else(Station == "G334" & Date >="2017-01-01",TRUE,FALSE)  ) %>%                #Flag data from SAV crash in STA-2 cell 3 2017 
filter(Flag ==FALSE)  %>% select(-Flag) %>%                                                   #Remove unrepresentative data 
filter(`Flowpath Region`=="Outflow") %>%                                                      #filter just the discharge data
rename(Wind="WIND BELLEGLADE",HLRin="Inflow HLR",HLRout="Outflow HLR",Flowpath="Flowpath Region",TEMP="Temp S7",Rain="Rain S7",Stage_Out="Outflow Stage") %>% #Rename variables. Model will not accept variables with blank spaces as input 
mutate(Station_ID=as.factor(Station_ID),Flowway=as.factor(Flowway),Year=as.factor(Year), Month=as.factor(month(Date, label=TRUE, abbr=TRUE)),Day=yday(Date)) %>%   #code categorical variables as factors
mutate(`Flowway` = if_else(Flowway=="STA-2 Central","STA-2 Flow-way 3",Flowway)) %>%          #fix flow-way name
mutate(Flowway=as.factor(Flowway)) %>%  
filter(HLRout<18,HLRout>0) %>%                                                                #filter out high HLRs that only exist in the STA-2 Central flow-way and reverse flow conditions
select(date,TPO4,Flowway,Station_ID,Year,Day,Time,TPO4,HLRout,Mean_Depth,Wind,Rain,Diff_24_hour_mean,`Percent difference from daily mean`) %>%
mutate(`Label` = case_when(Flowway=="STA-3/4 Central"~"STA-3/4 Central \n (G379D)",Flowway=="STA-3/4 Western"~"STA-3/4 Western \n (G381B)", Flowway=="STA-2 Flow-way 3"~"STA-2 Flow-way 3 \n (G334)")) %>% #Create better labels
drop_na()
  
#Create training and test data sets
set.seed(100)

#create ID column
Stage_discharge_data$id <- 1:nrow(Stage_discharge_data)

#use 70% of dataset as training set and 30% as test set 
Stage_discharge_data_train <- Stage_discharge_data %>% dplyr::sample_frac(0.70)
Stage_discharge_data_test  <- dplyr::anti_join(Stage_discharge_data, Stage_discharge_data_train, by = 'id')


# Summary Stats Table 1 ---------------------------------------------------

Summary_stats<- RPAs_with_Flow_Stage_Weather_Sonde %>%
mutate(Flag=if_else(Station == "G334" & Date >="2017-01-01",TRUE,FALSE)  ) %>%                #Flag data from SAV crash in STA-2 cell 3 2017 
filter(Flag ==FALSE)  %>% select(-Flag) %>%                                                   #Remove unrepresentative data 
filter(`Flowpath Region`=="Outflow") %>%                                                      #filter just the discharge data
mutate(`Flowway` = if_else(Flowway=="STA-2 Central","STA-2 Flow-way 3",Flowway)) %>%          #fix flow-way name
filter(`Outflow HLR`<18,`Outflow HLR`>0)    %>%                                                                #filter out high HLRs that only exist in the STA-2 Central flow-way and reverse flow conditions
select(Date,Flowway,TPO4) %>%
mutate(Deployment=case_when(Flowway=="STA-2 Flow-way 3" & Date <"2013-12-03"~"Deployment 1",
                            Flowway=="STA-2 Flow-way 3" & Date >"2015-10-21"~"Deployment 2",
                            Flowway=="STA-3/4 Western" & Date <"2013-04-20"~"Deployment 1",
                            Flowway=="STA-3/4 Western" & Date >"2014-02-12"~"Deployment 2",
                            Flowway=="STA-3/4 Central" & Date <"2012-11-17"~"Deployment 1",
                            Flowway=="STA-3/4 Central" & Date >"2014-02-23"~"Deployment 2",
                            TRUE~as.character(NA))) %>%
ungroup() %>%  
group_by(Flowway, Deployment) %>%
summarise(n=n(),samples=sum(!is.na(TPO4)),missing=n()-samples,mean=mean(TPO4,na.rm = TRUE),sd=sd(TPO4,na.rm=T),max=max(TPO4,na.rm = T),min=min(TPO4,na.rm = T)) 

missing_data_summary_stats <- Summary_stats %>%
ungroup() %>%
summarise(missing=sum(missing), `total observations`=sum(n), `total samples`=sum(samples),`missing percent`=percent(missing/`total observations`,accuracy=.01))

# Figure 2 Diel P trend  --------------------------------------------------

#Hourly TP Variation from the Daily Mean by Station Outflow stations only
ggplot(Stage_discharge_data,aes(Time,Diff_24_hour_mean,color=Label))+geom_point(shape=1)+geom_smooth(method="loess",color="black",fill="grey",se=FALSE)+theme_bw()+
geom_ribbon(stat='smooth', method = "loess", se=TRUE, alpha=.3,color="grey",level=.99) +
facet_grid(~Label)+scale_colour_brewer( type = "qual", palette = "Set2")+geom_hline(yintercept=0)+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,1))+
scale_x_continuous(limits = c(0,24),breaks = seq(0,24,4))+
theme(legend.position="none",axis.text = element_text(size = 14), axis.title=element_text(size=18),strip.text.x = element_text(size = 18))+
labs(title="",y=expression(TP~(mu~g~L^-1)),x="Time (Hour)")

ggsave("Figures/Hourly TP Variation from the Daily Mean by Station- Outflow only.jpeg", plot = last_plot(), width = 11.5, height = 8, units = "in", dpi = 300, limitsize = TRUE)


# Figure 3  ---------------------------------------------------------------

#allow for different HLR by flow-way  (Figure 3a-h)
Mod_1.2 <- gam(TPO4 ~s(Day,k=10,bs="cc")+Flowway+s(Year,bs="re")+ s(HLRout,by = Flowway, m = 2, bs = "tp")+s(Mean_Depth,k=5)+s(Time,bs="cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
plot(Mod_1.2, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
vis.gam(Mod_1.2, view = c("HLRout", "Time"), plot.type = "persp",theta=45,too.far=.05, ticktype="detailed",type="response",cond=list(Station_ID="G379D"))  #3D HLR and station
summary(Mod_1.2)
gam.check(Mod_1.2)
saveRDS(Mod_1.2, file="./Data/Model/Mod_1.2.rda")
AIC(Mod_1.2,Mod_1.2_fixed_year)


#Create Partial effect plot figure 3
PEP_mod_1.2 <- list()

# use c for categorical terms and s for smoothed terms
PEP_mod_1.2[[1]]<- plot(sm(getViz(Mod_1.2), 1)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
labs(x = expression("Seasonality (Day)"),y = expression("effects")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~"g) s(Day of Year, EDF 5.21)") +theme_classic() +themeSize

PEP_mod_1.2[[2]] <- plot(sm(getViz(Mod_1.2), 2)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95)+l_points(size = 1, col = "red") +
labs(x = expression("Gaussian quantiles (Year)",y = expression("effects"))) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~"h) f(Year) ") +theme_classic() +themeSize

PEP_mod_1.2[[3]] <- plot(pterm(getViz(Mod_1.2), 1)) + l_fitPoints(size = 1, col = "red")  +l_ciBar(linetype = 3, colour = "blue", level = 0.95)+ l_rug() + 
labs(x = expression("Flow-way"),y = expression("effects")) +scale_x_discrete(labels=c('STA-2 FW3', 'STA-34 C', 'STA-34 W'))+
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~italic("b) f(Flow-way)")) +theme_classic() +themeSize

PEP_mod_1.2[[4]] <- plot(sm(getViz(Mod_1.2), 3))  +labs(x = expression("Discharge (cm/day)"),y = expression("effects")) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~"d) s(Discharge, EDF 7.50)") +theme_classic() +themeSize

PEP_mod_1.2[[5]] <- plot(sm(getViz(Mod_1.2), 4))  +labs(x = expression("Discharge (cm/day)"),y = expression("effects")) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~"e) s(Discharge, EDF 6.34)") +theme_classic() +themeSize

PEP_mod_1.2[[6]] <- plot(sm(getViz(Mod_1.2), 5))  +labs(x = expression("Discharge (cm/day)"),y = expression("effects")) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~"f) s(Discharge, EDF 7.51)") +theme_classic() +themeSize

PEP_mod_1.2[[7]] <- plot(sm(getViz(Mod_1.2), 6)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
labs(x = expression("Water Depth (ft)"),y = expression("effects")) +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~"c) s(Mean Depth, EDF 3.81)") +theme_classic() +themeSize

PEP_mod_1.2[[8]] <- plot(sm(getViz(Mod_1.2), 7)) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
labs(x = expression("Time of Day (Hour)"),y = expression("effecst")) +  scale_x_continuous(breaks=seq(0,24,3))+
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~"a) s(Time of Day, EDF 5.21)") +theme_classic() +themeSize

fig_layout_1.2<- matrix(c(8,7,5,1,3,4,6,2), nrow = 4)

png("Figures/Partial Effects Plot Mod_1.2.png", units = "mm", res = 1000, height = 220, width = 200)

grid.arrange(grobs = lapply(PEP_mod_1.2, "[[", "ggObj"), layout_matrix = fig_layout_1.2)

dev.off()


# Figure 4 -------------------------------------------

#allow for different HLR and Time by flow-way  (Figure 4a-c)
Mod_1.3 <- gam(TPO4 ~s(Day,k=10,bs="cc")+s(Year,bs="re")+Flowway+ s(HLRout,by = Flowway, m = 2, bs = "tp")+s(Mean_Depth,k=5)+s(Time,by = Flowway, m = 2, bs = "cc"),data =Stage_discharge_data_train ,method="REML",family=Gamma(link="log"),knots=list(Day=c(0, 366),Time=c(0,24)))
plot(Mod_1.3, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE,all.terms=TRUE)
saveRDS(Mod_1.3, file="./Data/Model/Mod_1.3.rda")

#create partial effect plot for model 1.3 

PEP_mod_1.3 <- list()

PEP_mod_1.3[[1]] <- plot(sm(getViz(Mod_1.3), 7))  +labs(x = expression("STA-2 FW3   Time (Hour)"),y = expression("effects")) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~"a) s(Time, EDF 3.84) ") +theme_classic() +themeSize+ scale_x_continuous(breaks=seq(0,24,3))

PEP_mod_1.3[[2]] <- plot(sm(getViz(Mod_1.3), 8))  +labs(x = expression("STA-34 Central  Time (Hour)"),y = expression("effects")) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~"b) s(Time, EDF 4.34)") +theme_classic() +themeSize+ scale_x_continuous(breaks=seq(0,24,3))

PEP_mod_1.3[[3]] <- plot(sm(getViz(Mod_1.3), 9))  +labs(x = expression("STA-34 Western   Time (Hour)"),y = expression("effects")) + l_fitLine(linetype = 1, colour = "red") +l_ciLine(linetype = 3, colour = "blue", level = 0.95) +l_rug() +
theme(axis.text.x = element_text(angle = 15, hjust = 1)) +ggtitle(~"c)  s(Time, EDF 3.97) ") +theme_classic() +themeSize+ scale_x_continuous(breaks=seq(0,24,3))


fig_layout_1.3<- matrix(c(1,2,3), nrow = 1)

png("Figures/Partial Effects Plot Mod_1.3.png", units = "mm", res = 1000, height = 120, width = 300)

grid.arrange(grobs = lapply(PEP_mod_1.3, "[[", "ggObj"), layout_matrix = fig_layout_1.3)

dev.off()


# Create Figure 5 ---------------------------------------------------------


png("Figures/HLR_Time_perperctive_plot_G379D.png", units = "mm", res = 1000, height = 160, width = 140)
vis.gam(Mod_1.3, view = c("HLRout", "Time"), plot.type = "persp",theta=45, phi =25,too.far=.03,cond=list(Flowway="STA-3/4 Central"), ticktype="detailed",color="terrain",type="response",xlab="Discharge (cm/day)",ylab="Time (hour)",zlab=paste(expression(TP~ug/L)))  #3D HLR and station
dev.off()

png("Figures/HLR_Time_perperctive_plot_G381B.png", units = "mm", res = 1000, height = 160, width = 140)
vis.gam(Mod_1.3, view = c("HLRout", "Time"), plot.type = "persp",theta=45,phi=25,too.far=.03,cond=list(Flowway="STA-3/4 Western"), ticktype="detailed",color="terrain",type="response",xlab="Discharge (cm/day)",ylab="Time (hour)",zlab=paste(expression(TP~ug/L)))  #3D HLR and station
dev.off()

png("Figures/HLR_Time_perperctive_plot_G334.png", units = "mm", res = 1000, height = 160, width = 140)
vis.gam(Mod_1.3, view = c("HLRout", "Time"), plot.type = "persp",theta=45,phi=25,too.far=.03,cond=list(Flowway="STA-2 Central"), ticktype="detailed",color="terrain",type="response",xlab="Discharge (cm/day)",ylab="Time (hour)",zlab=paste(expression(TP~ug/L)))  #3D HLR and station
dev.off()



# Supplemental Figures 1a-d Model Diagnostics-------------------------------------------------


gam.check(Mod_1.3,rep=1000)


# Supplemental Figures 1e-f    Test for autocorrelation ------------------------------------------------
#Base method
png("Figures/Autocorrelation plot Model 1.3.png", units = "mm", res = 1000, height = 160, width = 140)
acf(residuals(Mod_1.3))
dev.off()

png("Figures/Partial Autocorrelation plot Model 1.3.png", units = "mm", res = 1000, height = 160, width = 140)
pacf(residuals(Mod_1.3))
dev.off()


# model validation --------------------------------------------------------

#predict on test data
Mod_1.2_predictions <-as.data.frame(predict.gam(Mod_1.2, newdata = Stage_discharge_data_test  ,se = F, type = 'response')) %>% setNames("Model 1.2 predictions")
Mod_1.3_predictions <-as.data.frame(predict.gam(Mod_1.3, newdata = Stage_discharge_data_test  ,se = F, type = 'response')) %>% setNames("Model 1.3 predictions")

#join predictions to original test data
Stage_discharge_data_test_predictions <-bind_cols(Stage_discharge_data_test,Mod_1.2_predictions) %>%  bind_cols(Mod_1.3_predictions)

#plot predictions
ggplot(Stage_discharge_data_test_predictions,aes(date,TPO4,fill=Label))+geom_point()+facet_grid(Year~Flowway)+geom_line(aes(date,`Model 1.2 predictions`),color="red")+
geom_line(aes(date,`Model 1.3 predictions`),color="blue") + theme_bw()

#calculate RMSE
Models_RMSE<- Stage_discharge_data_test_predictions %>%
summarise(n(),`RMSE Mod_1.2`=sqrt(sum((TPO4-`Model 1.2 predictions`)^2)/n()),`RMSE Mod_1.3`=sqrt(sum((TPO4-`Model 1.3 predictions`)^2)/n()))

#plot R squared model 1.2
ggplot(Stage_discharge_data_test_predictions,aes(`Model 1.2 predictions`,TPO4))+geom_point(shape=21)+ stat_poly_line() +
stat_poly_eq(use_label(c("eq", "R2"))) +
theme_bw()

#plot R squared model 1.3
ggplot(Stage_discharge_data_test_predictions,aes(`Model 1.3 predictions`,TPO4))+geom_point(shape=21)+ stat_poly_line() +
stat_poly_eq(use_label(c("eq", "R2"))) +
theme_bw()

# Supplemental Figure 2  --------------------------------------------------

#Figure flow category and difference from the daily mean OutFLOW stations only

ggplot(Stage_discharge_data1,aes(Time,`Percent difference from daily mean`,color=Label))+geom_point(shape=1)+geom_smooth(method="loess",color="black",level=.99)+
facet_grid(~Label)+scale_colour_brewer( type = "qual", palette = "Set2")+scale_y_continuous(limits = c(-10,10),breaks = seq(-10,10,2))+theme_bw()+
theme(legend.position="none",axis.text = element_text(size = 12), axis.title=element_text(size=18),strip.text.x = element_text(size = 18),panel.margin.x=unit(1.25, "lines"))+
scale_x_continuous(limits = c(0,24),breaks=seq(0,24,4),labels=c("12AM","4","8","12PM","4","8","12AM"))+  
labs(title="",y="Deviation from Daily Mean TP (%)",x="Hour")

ggsave("Figures/TPO4 Deviation from Daily Mean by outflow Station.jpeg", plot = last_plot(), width = 8, height = 5, units = "in", dpi = 300, limitsize = TRUE)


# Supplemental Figure 3.  Percent of daily discharge by hour in each STA flow-way----------------------------------

#Percent flow by hour
Daily_percent_flow_hour <- Combined_BK_Flow %>% mutate(Date=as.Date(Date)) %>% rename(HLRout="Outflow HLR") %>% select(-Inflow, -`Inflow HLR`) %>%
left_join(Flow_by_day,by=c("Flowway","Date")) %>%
mutate(`Percent flow by Hour`=HLRout/`sum HLR`*100) %>%
mutate(`Label` = case_when(Flowway=="STA-3/4 Central"~"STA-3/4 Central \n (G379D)",
                             Flowway=="STA-3/4 Western"~"STA-3/4 Western \n (G381B)",
                             Flowway=="STA-2 Central"~"STA-2 Flow-way 3 \n (G334)")) 
  

ggplot(Daily_percent_flow_hour ,aes(as.factor(Hour),`Percent flow by Hour`,fill=Label))+geom_boxplot(color="black")+facet_wrap(~Label)+coord_cartesian(ylim = c(0, 10))+
labs(title="",y="Percent of Daily Discharge (%)",x="Hour")+guides(fill=guide_legend(title="Flow-way"))+theme_bw()+scale_x_discrete(breaks=c("0","4","8","12","16","20","23"), labels=c("12AM","4","8","12PM","4","8","11PM")) +
theme(legend.position="none",axis.text = element_text(size = 9), axis.title=element_text(size=18),strip.text.x = element_text(size = 18),panel.margin.x=unit(1.25, "lines"))


ggsave("Figures/Percent of Daily Discharge by Hour.jpeg", plot = last_plot(), width = 8, height = 5, units = "in", dpi = 300, limitsize = TRUE)

# Supplemental Figure 4. Create ICE and PDPs using IML package -------------------------------------------
mod <- Predictor$new(Mod_1.3, data = Stage_discharge_data_test,type="response")


Time_PDP_ICE <- FeatureEffect$new(mod,feature = "Time", method = "pdp+ice", grid.size = 30)
Time_PD_plot <- plot(Time_PDP_ICE)+theme_bw()+labs(x="Time (Hour)")+scale_y_continuous(name="Predicted TP")+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))


HLROUT_PDP_ICE <- FeatureEffect$new(mod,feature = "HLRout", method = "pdp+ice", grid.size = 30)
HLR_PD_plot <- plot(HLROUT_PDP_ICE)+theme_bw()+labs(x="Discharge (cm/day)")+scale_y_continuous(name="Predicted TP")+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))

Depth_PDP_ICE <- FeatureEffect$new(mod,feature = "Mean_Depth", method = "pdp+ice", grid.size = 30)
Depth_PD_plot <-plot(Depth_PDP_ICE)+theme_bw()+labs(x="Water Depth (ft)")+scale_y_continuous(name="Predicted TP")+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))

Day_PDP_ICE <- FeatureEffect$new(mod,feature = "Day", method = "pdp+ice", grid.size = 30)
Day__PD_plot <-plot(Day_PDP_ICE)+theme_bw()+labs(x="Seasonality  (Day of Year)")+scale_y_continuous(name="Predicted TP")+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))

Year_PDP_ICE <- FeatureEffect$new(mod,feature = "Year", method = "pdp+ice", grid.size = 30)
Year_PD_plot <- plot(Year_PDP_ICE)+theme_bw()+labs(x="Year")+scale_y_continuous(name="Predicted TP")+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))

Flowway_PDP_ICE <- FeatureEffect$new(mod,feature = "Flowway", method = "pdp+ice", grid.size = 30)
Flowway_PD_plot <-plot(Flowway_PDP_ICE)+theme_bw()+labs(x="Flowway")+scale_y_continuous(name="Predicted TP")+theme(axis.text = element_text(size = 14), axis.title=element_text(size=18))+scale_x_discrete(labels=c('STA-2 FW3', 'STA-34 C', 'STA-34 W'))

plot_grid(Time_PD_plot ,Depth_PD_plot, HLR_PD_plot,Day__PD_plot,Flowway_PD_plot ,Year_PD_plot, labels = c('A', 'B','C','D','E','F'), label_size = 18,nrow=3)


ggsave("Figures/Partial Dependence Plots SI4.jpeg", plot = last_plot(), width = 8, height = 11.5, units = "in", dpi = 300, limitsize = TRUE)

