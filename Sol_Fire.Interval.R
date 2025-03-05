library(car)
library(dplyr)
library(readxl)
library(lme4)
#loading data
data<- read_excel("raw/alldataforbiomass.xlsx")
data<-data%>%mutate(Log_biomass_g_ha_day= log10(biomass_g_ha_day))

hist(data$biomass_g_ha_day)
hist(data$Log_biomass_g_ha_day)

#models for biomass~nutri
ml<-lmer(Log_biomass_g_ha_day~  mean_ammonia+ mean_nitrate +Ortho_P_mg_kg+ Avg_pH + (1|Site/Transect) , 
              data=data)
summary(ml)
Anova<-round(Anova(ml,test='F'), 2) 
Anova #nitrate,PO4, pH
plot(ml)
qqPlot(resid(ml))  #  QQ plot looks strange non-transformed- looks ok now!


#models for biomass~veg
veg<-lmer(Log_biomass_g_ha_day~ Tree.Basal.Area_m2+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq + (1|Site/Transect) , 
               data=data)
summary(veg)
Anova<-round(Anova(veg,test='F'), 2) 
Anova
plot(veg)
qqPlot(resid(ml))  #  QQ plot

#models for biomass~nutri and veg
#crash when I use Tree basal area (from here onwards...)
full<- lmer(Log_biomass_g_ha_day~ Fire.Interval+ mean_ammonia+ mean_nitrate+Ortho_P_mg_kg+Avg_pH+  Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq+(1|Site/Transect),
                 data)

full<- lmer(Log_biomass_g_ha_day~ Fire.Interval+ mean_ammonia+ mean_nitrate+Ortho_P_mg_kg+Avg_pH +perc_myco_host_freq+(1|Site/Transect),   data)
summary(full)
Anova<-round(Anova(full,test='F'), 2) 
Anova
plot(full)
qqPlot(resid(full))  #  QQ plot looks strange non-transformed- looks ok now!


#just Interval
ml<-lmer(Log_biomass_g_ha_day~  Fire.Interval + (1|Site/Transect) , 
         data=data)
summary(ml)
Anova<-round(Anova(ml,test='F'), 2) 
Anova
plot(ml)
qqPlot(resid(ml))  #  QQ plot looks strange non-transformed- looks ok now!




#Ortho_Phos
m1<-lmer(log10(Ortho_P_mg_kg)~ Fire.Interval + (1|Site/Transect) , data=data)
summary(m1)
Anova(m1,test='F')
plot(m1)
qqPlot(resid(m1))

#N avail dont work, I think it is because my data doesnt fit well...another reason we need the full data here
#Ammonia
m3<-lmer(log10(mean_ammonia) ~   Fire.Interval + Fire.Severity + (1|Site/Transect) , data=data)
summary(m3)
Anova(m3,test='F')
plot(m3)
qqPlot(resid(m3))



#Nitrate
m2<-lmer(log10(mean_nitrate)~  Fire.Severity+ Fire.Interval +(1|Site/Transect) , data=data)
summary(m2)
Anova(m2,test='F')
plot(m2)
qqPlot(resid(m2))



#HYPHAL WIDTH
datalength<- read_excel("raw/alldataforlength.xlsx")
datalength<-datalength%>%mutate(Log_Length= log10(Length_mm),
                          Log_CVLength=log10(CV_Length))
#models for width~nutri
wn<-lmer(Log_Length~Fire.Interval+ mean_ammonia +mean_nitrate+ Ortho_P_mg_kg +Avg_pH+(1|Site/Transect/Location/Rep) , 
              data=datalength)
summary(wn)
Anova_wnh<-round(Anova(wn,test='F'), 2) 
Anova_wnh
plot(wn)
qqPlot(resid(wn))


#models for width~veg
#many many issues running this code- no F??? 
wv<-lmer(Log_Length~ Fire.Interval+ Tree.Basal.Area_m2+Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+ perc_myco_host_freq+(1|Site/Transect/Location/Rep) , 
              data=datalength)
summary(wv)
Anova_wvh<-round(Anova(wv,test='F'), 2) 
Anova_wvh
plot(wv)
qqPlot(resid(wv))




#to check if this was an issue but no- also tried scaling and nothing
vif(wv)

#models for CV width~nutri
CVn<-lmer(Log_CVLength~ Fire.Interval+ mean_ammonia+ mean_nitrate +Ortho_P_mg_kg+ Avg_pH + (1|Site/Transect/Location) , 
               data=datalength)
summary(CVn)
Anova_CVnh<-round(Anova(CVn,test='F'), 2) 
Anova_CVnh
plot(CVn)
qqPlot(resid(CVn))


#models for CV width~veg
CVf<-lmer(Log_CVLength~ Fire.Interval+ mean_ammonia+ mean_nitrate +Ortho_P_mg_kg+ Avg_pH +Tree.Basal.Area_m2+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+ perc_myco_host_freq+(1|Site/Transect/Location) , 
               data=datalength)
summary(CVf)
Anova_CVf<-round(Anova(CVf,test='F'), 2) 
Anova_CVf
plot(CVf)
qqPlot(resid(CVf))

#models for CV width~nutri+veg
CVv<-lmer(Log_CVLength~ Fire.Interval+ Tree.Basal.Area_m2+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+ perc_myco_host_freq+(1|Site/Transect/Location) , 
          data=datalength)
summary(CVv)
Anova_CVvh<-round(Anova(CVv,test='F'), 2) 
Anova_CVvh
plot(CVv)
qqPlot(resid(CVv))


