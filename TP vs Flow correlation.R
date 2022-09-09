# Correlation between TP and flow

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
library(cowplot)
library(devtools) 
library(nlcor)

# Import Data -------------------------------------------------------------

RPAs_with_Flow <- read_csv("Data/RPA and Flow.csv") %>%
mutate(`Month`=factor(`Month`,levels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) %>%
mutate(Flag=if_else(Station == "G334" & Date >"2017-01-01",TRUE,FALSE)  ) %>%  #SAV crash in cell. Unrepresentative data removed
filter(Flag ==FALSE) 

TP_Flow_Correlations <-RPAs_with_Flow  %>%
group_by(Flowway, `Flowpath Region`)  %>%
summarise(n=sum(!is.na(TPO4)),
`Inflow Spearmans Correlation`=cor(`Inflow HLR`,TPO4,use="pairwise.complete.obs", method = "spearman"),`Inflow Kendall Correlation`=cor(`Inflow HLR`,TPO4,use="pairwise.complete.obs", method = "kendall"),
`Outflow Spearmans Correlation`=cor(`Outflow HLR`,TPO4,use="pairwise.complete.obs", method = "spearman"),`Outflow Kendall Correlation`=cor(`Outflow HLR`,TPO4,use="pairwise.complete.obs", method = "kendall"))

#TP correaltions split by inflow category 
TP_Flow_Correlations_inflow_categories <-RPAs_with_Flow  %>%
mutate(`Inflow HLR Category`=if_else(`Inflow HLR`>5,"Inflow HLR > 5","Inflow HLR <5")) %>%  
group_by(Flowway, `Flowpath Region`,`Inflow HLR Category`)  %>%
summarise(n=sum(!is.na(TPO4)),
`Inflow Spearmans Correlation`=cor(`Inflow HLR`,TPO4,use="pairwise.complete.obs", method = "spearman"))

#TP correaltions split by Outflow category 
TP_Flow_Correlations_Outflow_categories <-RPAs_with_Flow  %>%
mutate(`Outflow HLR Category`=if_else(`Outflow HLR`>5,"HLR > 5","HLR <5")) %>%  
group_by(Flowway, `Flowpath Region`,`Outflow HLR Category`)  %>%
summarise(n=sum(!is.na(TPO4)),
`Inflow Spearmans Correlation`=cor(`Inflow HLR`,TPO4,use="pairwise.complete.obs", method = "spearman"))


# Nonlinear correlation with "nlcor" package ------------------------------
G380 <- filter(drop_na(RPAs_with_Flow,TPO4,`Outflow HLR`),Station=="G380",TPO4<200)
NLC_G380 <- nlcor(G380$`Outflow HLR`, G380$TPO4, plt = T,refine=.8)
NLC_G380$cor.estimate

NLC_all_data$adjusted.p.value

G334 <- filter(drop_na(RPAs_with_Flow,TPO4,`Outflow HLR`),Station=="G334",TPO4<200)
NLC_G334 <- nlcor(G334$`Outflow HLR`, G334$TPO4, plt = T,refine=.75)
NLC_G334$cor.estimate

G379 <- filter(drop_na(RPAs_with_Flow,TPO4,`Outflow HLR`),Station=="G379",TPO4<200)
NLC_G379 <- nlcor(G379$`Outflow HLR`, G379$TPO4, plt = T,refine=.8)
NLC_G379$cor.estimate


