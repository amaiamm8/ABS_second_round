library(car)
library(dplyr)
library(readxl)
library(lme4)
#loading data
data<- read_excel("raw/alldata.xlsx")
data<-data%>%mutate(Log_CVLength=log10(CV_Length))

#separating into 2 datasets the sites of high vs low fire frequency
high_freq<- data%>%
  filter(Fire.Interval %in% c("Short"))
low_freq<- data%>%
  filter(Fire.Interval %in% c("Long"))

#models for biomass~nutri
ml_high<-lmer(biomass_g_ha_day~ mean_ammonia+ mean_nitrate +Ortho_P_mg_kg+ Avg_pH + (1|Site/Transect) , 
             data=high_freq)
summary(ml_high)
Anova_high<-round(Anova(ml_high,test='F'), 2) 
Anova_high
plot(ml_high)

ml_low<-lmer(biomass_g_ha_day~ mean_ammonia+ mean_nitrate +Ortho_P_mg_kg+ Avg_pH + (1|Site/Transect) , 
              data=low_freq)
summary(ml_low)
Anova_low<-round(Anova(ml_low,test='F'), 2) 
Anova_low
plot(ml_low)

#models for biomass~veg
veg_high<-lmer(biomass_g_ha_day~Tree.Basal.Area_m2+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq + (1|Site/Transect) , 
              data=high_freq)
summary(veg_high)
Anova_high<-round(Anova(veg_high,test='F'), 2) 
Anova_high
plot(veg_high)

veg_low<-lmer(biomass_g_ha_day~Tree.Basal.Area_m2+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq + (1|Site/Transect) , 
               data=low_freq)
summary(veg_low)
Anova_low<-round(Anova(veg_low,test='F'), 2) 
Anova_low
plot(veg_low)

#models for biomass~nutri and veg
#crash when I use Tree basal area (from here onwards...)
full_high<- lmer(biomass_g_ha_day~ mean_ammonia+ mean_nitrate+Ortho_P_mg_kg+Avg_pH+  Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq+(1|Site),
                 high_freq)
summary(full_high)
Anova_high<-round(Anova(full_high,test='F'), 2) 
Anova_high
plot(full_high)

full_low<- lmer(biomass_g_ha_day~ mean_ammonia+ mean_nitrate+Ortho_P_mg_kg+Avg_pH+Herb.Cover_0.50cm_perc+Shrub.Cover_50.200cm_perc+perc_myco_host_freq+(1|Site),
                low_freq)
summary(full_low)
Anova_low<-round(Anova(full_low,test='F'), 2) 
Anova_low
plot(full_low)
vif(full_low)

#HYPHAL WIDTH
#models for width~nutri
wn_high<-lmer(Log_Length~mean_ammonia +mean_nitrate+ Ortho_P_mg_kg +Avg_pH+(1|Site/Transect/Location/Rep) , 
              data=high_freq)
summary(wn_high)
Anova_wnh<-round(Anova(wn_high,test='F'), 2) 
Anova_wnh
plot(wn_high)
qqPlot(resid(wn_high))

wn_low<-lmer(Log_Length~mean_ammonia +mean_nitrate+ Ortho_P_mg_kg +Avg_pH+(1|Site/Transect/Location/Rep) , 
              data=low_freq)
summary(wn_low)
Anova_wnl<-round(Anova(wn_low,test='F'), 2) 
Anova_wnl
plot(wn_low)
qqPlot(resid(wn_low))

#models for width~veg
#many many issues running this code- no F??? 
wv_high<-lmer(Log_Length~ Tree.Basal.Area_m2+Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+ perc_myco_host_freq+(1|Site/Transect/Location/Rep) , 
              data=high_freq)
summary(wv_high)
Anova_wvh<-round(Anova(wv_high,test='F'), 2) 
Anova_wvh
plot(wv_high)
qqPlot(resid(wv_high))


wv_low<-lmer(Log_Length~ Tree.Basal.Area_m2+Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+ perc_myco_host_freq+(1|Site/Transect/Location/Rep) , 
              data=low_freq)
summary(wv_low)
Anova_wvl<-round(Anova(wv_low,test='F'), 2) 
Anova_wvl
plot(wv_low)
qqPlot(resid(wv_low))

#to check if this was an issue but no- also tried scaling and nothing
vif(lm(Log_Length~Tree.Basal.Area_m2 + Shrub.Cover_50.200cm_perc,data))

#models for CV width~nutri
CVn_high<-lmer(Log_CVLength~ mean_ammonia+ mean_nitrate +Ortho_P_mg_kg+ Avg_pH + (1|Site/Transect/Location) , 
              data=high_freq)
summary(CVn_high)
Anova_CVnh<-round(Anova(CVn_high,test='F'), 2) 
Anova_CVnh
plot(CVn_high)


CVn_low<-lmer(Log_CVLength~ mean_ammonia+ mean_nitrate +Ortho_P_mg_kg+ Avg_pH + (1|Site/Transect) , 
             data=low_freq)
summary(CVn_low)
Anova_CVnl<-round(Anova(CVn_low,test='F'), 2) 
Anova_CVnl
plot(CVn_low)


#models for CV width~veg
CVv_high<-lmer(Log_CVLength~ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+ perc_myco_host_freq+(1|Site/Transect/Location) , 
              data=high_freq)
summary(CVv_high)
Anova_CVvh<-round(Anova(CVv_high,test='F'), 2) 
Anova_CVvh
plot(CVv_high)
qqPlot(resid(CVv_high))

CVv_low<-lmer(Log_CVLength~ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+ perc_myco_host_freq+(1|Site/Transect/Location) , 
               data=low_freq)
summary(CVv_low)
Anova_CVvl<-round(Anova(CVv_low,test='F'), 2) 
Anova_CVvl
plot(CVv_low)
qqPlot(resid(CVv_low))

