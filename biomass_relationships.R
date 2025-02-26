#install.packages("Matrix", type = "source")
#install.packages("lme4", type = "source")
#install.packages("performance")
library(readxl)
library(readr)
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

biomass<- read_excel("raw/biomass.xlsx")

biomass$Site<- as.factor(biomass$Site)
biomass<-biomass%>%
  mutate(Location = sub(".*L(\\d+)$", "\\1", Location))%>%
  group_by(Site, Transect) %>%  # Group by Site and Transect if needed
  distinct()
#site_data<- read.csv("raw/site_data_Amaia.csv")[,-1]
#filtering the data to my sites
#site_data <- site_data %>%
 # filter(Site %in% c(7, 8 , 10, 12 , 26, 34))%>%
  #mutate(Site=as.character(Site),
       #  Transect=as.character(Transect))
Ortho_P<- read.csv("outputs/Ortho_P.csv")
Ortho_P <- head(Ortho_P, -2)

Ortho_P <- Ortho_P %>%
  select(Site, Transect, Location, Ortho_blanked, Ortho_P_mg_kg)%>%
  mutate(Site= as.factor(Site))%>%
  group_by(Site, Transect) %>%  # Group by Site and Transect if needed
  distinct()

pH_data<- read_csv("outputs/pH_output.csv", col_select= c("Site","Transect", "Location", "Avg_pH"))

pH_data<- pH_data%>%
  group_by(Site, Transect)%>%
  summarise(mean_pH = mean(Avg_pH, na.rm = TRUE))%>%
  mutate(Site = sub("^(S)( *)", "\\2", Site),
         Transect = sub("^(T)( *)", "\\2", Transect))

myco_data <- read.csv("raw/Myco_host_abundance.csv")
myco_data <- myco_data %>%
  filter(Site %in% c(7, 8 , 10, 12 , 26, 34))%>%
  mutate(Site= as.character(Site))%>%
  mutate(Transect= as.character(Transect))
nutri_sol<- read_csv("outputs/Resin_Nutrients_SMM_1stRnd.csv", col_select= c("Site","Transect", "Ammonia_mg_kg", "Nitrate_mg_kg"))
nutri_sol<- nutri_sol%>%
  group_by(Site, Transect)%>%
  summarise(mean_ammonia = mean(Ammonia_mg_kg, na.rm = TRUE), 
              mean_nitrate = mean(Nitrate_mg_kg, na.rm = TRUE))%>%
  mutate(Site=as.character(Site), 
         Transect= as.character(Transect))

combined_data<-left_join(biomass,Ortho_P,by=c("Site","Transect","Location"))
combined_data<-full_join(combined_data, myco_data)
combined_data<- left_join(combined_data, nutri_sol)
combined_data<- left_join(combined_data, pH_data)
combined_data$Fire.Interval<- as.factor(combined_data$Fire.Interval)
combined_data$Fire.Severity<- as.factor(combined_data$Fire.Severity)

# Remove leading and trailing spaces

# Add this if duplicates are expected
#First model: growth influenced by fire regime
GR_model<- lmer(biomass_g_ha_day~Fire.Interval + (1|Site/Transect), combined_data)
summary(GR_model)

Anova_1<-round(Anova(GR_model,test='F'), 2) 
Anova_1

r2(GR_model)


#growth rate related to nutrients
GR_nutri<- lmer(biomass_g_ha_day~ mean_ammonia+ mean_nitrate+Ortho_P_mg_kg+mean_pH+(1|Site/Transect), combined_data)
summary(GR_nutri)
Anova_nutri<-round(Anova(GR_nutri,test='F'), 2) 
Anova_nutri
plot(GR_nutri)
qqPlot(resid(GR_nutri))
plot(resid(GR_nutri) ~ fitted(GR_nutri))

Anovanu1<-round(Anova(GR_nutri,test='F'), 2) 
Anovanu1
plot(GR_nutri)


#growth rate related to vegetation factors
GR_veg<- lmer(biomass_g_ha_day~ +Tree.Basal.Area_m2+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq+(1|Site/Transect), combined_data)
summary(GR_veg)
Anova_veg<-round(Anova(GR_veg,test='F'), 2) 
Anova_veg
plot(GR_veg)
qqPlot(resid(GR_veg))

#second model hyphal growth rate influenced by fire regime veg data, mycorrhizal hosts and nutrient contents
GR_model_full<- lmer(biomass_g_ha_day~ mean_ammonia+ mean_nitrate+Ortho_P_mg_kg+Tree.Basal.Area_m2+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq+(1|Site/Transect), combined_data)
summary(GR_model_full)

Anova_2<-round(Anova(GR_model_full,test='F'), 2) 
Anova_2
plot(GR_model_full)
qqPlot(resid(GR_model_full))
r2(GR_model_full)


#third model using biomass
biomass_model_full<- lmer(corrected_weight~Fire.Severity+Fire.Interval +NO3+ NH4+Ortho_P_mg_kg+Tree.Basal.Area_m2+ Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+perc_myco_host_freq+(1|Site/Transect), combined_data)
summary(biomass_model_full)

Anova_3<-round(Anova(biomass_model_full,test='F'), 2) 
Anova_3
plot(biomass_model_full)

r2(biomass_model_full)

library(ggplot2)
library(patchwork)

# Mean Ammonia
p1 <- ggplot(combined_data, aes(x = mean_ammonia, y = biomass_g_ha_day)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Ammonia", y = "Biomass (g/ha/day)") +
  theme_classic()

# Mean Nitrate
p2 <- ggplot(combined_data, aes(x = mean_nitrate, y = biomass_g_ha_day)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  labs(x = "Nitrate", y = "Biomass (g/ha/day)") +
  theme_classic()

# Ortho_P (Phosphorus)
p3 <- ggplot(combined_data, aes(x = Ortho_P_mg_kg, y = biomass_g_ha_day)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(x = "Ortho P (mg/kg)", y = "Biomass (g/ha/day)") +
  theme_classic()

# Mean pH
p4 <- ggplot(combined_data, aes(x = mean_pH, y = biomass_g_ha_day)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  labs(x = "pH", y = "Biomass (g/ha/day)") +
  theme_classic()

# Combine the plots into a 2x2 grid
(p1 | p2) / (p3 | p4)

# Load necessary libraries
library(ggplot2)

# Create the boxplot
ggplot(combined_data, aes(x = factor(Site), y = biomass_g_ha_day)) +
  geom_boxplot(fill = "white", color = "black", outlier.shape = 16, outlier.size = 2) +
  labs(x = "Site", y = "Biomass (g/ha/day)", title = "Biomass Variation Across Sites") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed


biomass_site<- lm(biomass_g_ha_day~Site, combined_data)  
emm<- emmeans(biomass_site, specs = "Site")
pairs(emm, specs="Site", adjust="none")
emmeans(biomass_site, specs = "Site", adjust = "none")
multcomp::cld(emm) 
site_emm <- emmeans(biomass_site, pairwise ~ Site, adjust = "tukey")

# Convert results to compact letter display (CLD)
cld <- multcomp::cld(site_emm$emmeans, Letters = letters)
cld <- as.data.frame(cld)  # Convert to dataframe


# Fit the mixed-effects model
model <- lmer(biomass_g_ha_day ~ Site + (1 | Transect), data = combined_data)

# Create a boxplot of the raw data
ggplot(combined_data, aes(x = factor(Site), y = biomass_g_ha_day)) +
  geom_boxplot() +  # Boxplot of observed biomass values per Site
  labs(x = "Site", y = "Biomass (g/ha/day)", title = "Biomass Production per Site") +
  theme_classic()


# Fit the mixed-effects model
model <- lmer(biomass_g_ha_day ~ Site + (1 | Transect), data = combined_data)

# Get estimated marginal means from the model
emm <- emmeans(model, ~ Site)
emm_df <- as.data.frame(emm)  # Convert to a dataframe

# Ensure 'Site' is a factor in both datasets
emm_df$Site <- as.factor(emm_df$Site)
combined_data$Site <- as.factor(combined_data$Site)

# Create the boxplot with model estimates as points
ggplot(combined_data, aes(x = Site, y = biomass_g_ha_day)) +
  geom_boxplot() +  # Boxplots for raw biomass data per site
  geom_point(data = emm_df, aes(x = Site, y = emmean), color = "red", size = 3) +  # Model estimates

  labs(x = "Site", y = "Biomass (g/ha/day)", title = "Biomass Production per Site (Model & Raw Data)") +
  theme_classic()
str(combined_data)

nutri<-as.data.frame(emmeans(GR_nutri, ~mean_ammonia))
nutri
plot(nutri)


plot_m3<-ggplot(nutri, aes(x = mean_ammonia, y = biomass_g_ha_day) )+
  geom_point(data=combined_data, aes(x =mean_ammonia, y = biomass_g_ha_day), size=1)+
  labs(x = "ammonia", y = "Biomass production") +
  
  annotate("text", x = 1.9, y = Inf, label = paste0("Severity (p) = ", AnovaCV1["Fire.Severity", "Pr(>F)"]),
           hjust = 2.5, vjust = 1.5, size = 3)+
  theme_classic()+
  theme(axis.text.x = element_text( hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.line = element_line(linewidth = 0.5),
        legend.position = 'none')
plot_m3
