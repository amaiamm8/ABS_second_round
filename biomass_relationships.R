#install.packages("Matrix", type = "source")
#install.packages("lme4", type = "source")
#install.packages("performance")
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(writexl)
library(openxlsx)
library(vegan)
library(Matrix)
library(lme4)
library(car)
library(emmeans)
library(performance)
library(emmeans)
biomass<- read_excel("raw/biomass.xlsx")
site_data<- read_excel("raw/alldata.xlsx")

combined_data<-left_join(biomass,site_data, by=c("Site", "Transect") ) 

#first model fire regime affecting hyphal growth rate
GR_model<- lmer(biomass_g_ha_day~Fire.Severity+fire.frequency + (1|Site/Transect), combined_data)
summary(GR_model)


#Anova_1<-round(Anova(GR_model,test='F'), 2) 
#Anova_1
Anova(GR_model)
plot(GR_model)
r2(GR_model)
#second model hyphal growth rate influenced by fire regime and nutrient contents
GR_model_full<- lmer(biomass_g_ha_day~Fire.Severity+fire.frequency +NO3+ NH4+ Ortho_P_mg_kg+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+(1|Site/Transect), combined_data)
summary(GR_model_full)


#Anova_2<-round(Anova(GR_model_full,test='F'), 2) 
#Anova_2
Anova(GR_model_full)
plot(GR_model_full)

r2(GR_model_full)
