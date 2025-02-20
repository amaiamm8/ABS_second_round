#install.packages("Matrix", type = "source")
#install.packages("lme4", type = "source")
#install.packages("performance")
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(openxlsx)
library(vegan)
library(Matrix)
library(lme4)
library(car)
library(emmeans)
library(performance)
library(emmeans)
biomass<- read_excel("raw/biomass.xlsx")
#site_data<- read.csv("raw/site_data_Amaia.csv")[,-1]
#filtering the data to my sites
#site_data <- site_data %>%
 # filter(Site %in% c(7, 8 , 10, 12 , 26, 34))%>%
  #mutate(Site=as.character(Site),
       #  Transect=as.character(Transect))
Ortho_P<- read.csv("outputs/Ortho_P.csv")
Ortho_P <- head(Ortho_P, -2)

Ortho_P <- Ortho_P %>%
  mutate(Site = sub("S(\\d+)T.*", "\\1", Names),
         Transect = sub(".*T(\\d+)L.*", "\\1", Names),  
         Location = sub(".*L(\\d+)$", "\\1", Names))%>%
  select(Site, Transect, Location, Ortho_blanked, Ortho_P_mg_kg)

  
myco_data <- read.csv("raw/Myco_host_abundance.csv")
myco_data <- myco_data %>%
  filter(Site %in% c(7, 8 , 10, 12 , 26, 34))%>%
  mutate(Site= as.character(Site))%>%
  mutate(Transect= as.character(Transect))


combined_data<-left_join(biomass,Ortho_P%>%distinct() ) 
combined_data<-full_join(combined_data, myco_data%>%distinct() )


# Remove leading and trailing spaces

# Add this if duplicates are expected
#First model: growth influenced by fire regime
GR_model<- lmer(biomass_g_ha_day~Fire.Severity+Fire.Interval + (1|Site/Transect), combined_data)
summary(GR_model)


Anova_1<-round(Anova(GR_model,test='F'), 2) 
Anova_1

r2(GR_model)
#second model hyphal growth rate influenced by fire regime veg data, mycorrhizal hosts and nutrient contents
GR_model_full<- lmer(biomass_g_ha_day~Fire.Severity+Fire.Interval +NO3+ NH4+Tree.Basal.Area_m2+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq+(1|Site/Transect), combined_data)
summary(GR_model_full)

Anova_2<-round(Anova(GR_model_full,test='F'), 2) 
Anova_2
plot(GR_model_full)

r2(GR_model_full)

#third model using biomass
biomass_model_full<- lmer(corrected_weight~Fire.Severity+Fire.Interval +NO3+ NH4+Tree.Basal.Area_m2+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq+(1|Site/Transect), combined_data)
summary(biomass_model_full)

Anova_3<-round(Anova(biomass_model_full,test='F'), 2) 
Anova_3
plot(biomass_model_full)

r2(biomass_model_full)

