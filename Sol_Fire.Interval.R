library(car)
library(dplyr)
library(readxl)
library(lme4)
#loading data
data<- read_excel("raw/alldata.xlsx")
data<-data%>%mutate(Log_CVLength=log10(CV_Length),
                    Log_biomass_g_ha_day= log10(biomass_g_ha_day))

data_micro<-data

data<-data_micro%>%select(-c(Slide:CV_Length),-Log_Length,-Log_CVLength)%>%group_by(Site,Transect,Location,biomass_g_ha_day)%>%distinct()
sapply(data, function(x) length(unique(x)))


hist(data$biomass_g_ha_day)
hist(data$Log_biomass_g_ha_day)

#models for biomass~nutri
ml<-lmer(Log_biomass_g_ha_day~ Fire.Interval+ mean_ammonia+ mean_nitrate +Ortho_P_mg_kg+ Avg_pH + (1|Site/Transect) , 
              data=data)
summary(ml)
Anova<-round(Anova(ml,test='F'), 2) 
Anova #nitrate,PO4, pH
plot(ml)
qqPlot(resid(ml))  #  QQ plot looks strange non-transformed


#models for biomass~veg
veg<-lmer(biomass_g_ha_day~Fire.Interval+ Tree.Basal.Area_m2+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq + (1|Site/Transect) , 
               data=data)
summary(veg)
Anova<-round(Anova(veg,test='F'), 2) 
Anova
plot(veg)
qqPlot(resid(ml))  #  QQ plot

#models for biomass~nutri and veg
#crash when I use Tree basal area (from here onwards...)
full<- lmer(biomass_g_ha_day~ Fire.Interval+ mean_ammonia+ mean_nitrate+Ortho_P_mg_kg+Avg_pH+  Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq+(1|Site),
                 data)

full<- lmer(biomass_g_ha_day~ Fire.Interval+ mean_ammonia+ mean_nitrate+Ortho_P_mg_kg+Avg_pH +perc_myco_host_freq+(1|Site),   data)
summary(full)
Anova<-round(Anova(full,test='F'), 2) 
Anova
plot(full)


#HYPHAL WIDTH
#models for width~nutri
wn<-lmer(Log_Length~Fire.Interval+ mean_ammonia +mean_nitrate+ Ortho_P_mg_kg +Avg_pH+(1|Site/Transect/Location/Rep) , 
              data=data)
summary(wn)
Anova_wnh<-round(Anova(wn,test='F'), 2) 
Anova_wnh
plot(wn)
qqPlot(resid(wn))


#models for width~veg
#many many issues running this code- no F??? 
wv<-lmer(Log_Length~ Fire.Interval+ Tree.Basal.Area_m2+Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+ perc_myco_host_freq+(1|Site/Transect/Location/Rep) , 
              data=high_freq)
summary(wv)
Anova_wvh<-round(Anova(wv,test='F'), 2) 
Anova_wvh
plot(wv)
qqPlot(resid(wv))




#to check if this was an issue but no- also tried scaling and nothing
vif(lm(Log_Length~Tree.Basal.Area_m2 + Shrub.Cover_50.200cm_perc,data))

#models for CV width~nutri
CVn<-lmer(Log_CVLength~ Fire.Interval+ mean_ammonia+ mean_nitrate +Ortho_P_mg_kg+ Avg_pH + (1|Site/Transect/Location) , 
               data=data)
summary(CVn)
Anova_CVnh<-round(Anova(CVn,test='F'), 2) 
Anova_CVnh
plot(CVn)





#models for CV width~veg
CVv<-lmer(Log_CVLength~ Fire.Interval+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+ perc_myco_host_freq+(1|Site/Transect/Location) , 
               data=data)
summary(CVv)
Anova_CVvh<-round(Anova(CVv,test='F'), 2) 
Anova_CVvh
plot(CVv)
qqPlot(resid(CVv))


