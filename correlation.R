library(ggplot2)
data <- read_excel("raw/alldataforlength.xlsx")
data_cor<- data%>%
  group_by(Site,Transect)%>%
  mutate(avgLength_mm=mean(Length_mm))%>%
  mutate(avgbiomass=mean(biomass_g_ha_day))%>%
  select(Site, Transect,avgLength_mm,avgbiomass)%>%
  distinct()%>%
  ungroup()
cor(data_cor$avgLength_mm, data_cor$avgbiomass, method = "pearson")
cor.test(data_cor$avgLength_mm, data_cor$avgbiomass, method = "pearson")

ggplot(data_cor, aes(x = avgbiomass, y = avgLength_mm)) +
  geom_point(alpha = 0.5, color = "blue") +  # Raw data points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Overall trend line
  labs(x = "Biomass production (g/ha/day)", y = "Width (mm)", title = "Correlation between Biomass production and Hyphal width") +
  theme_classic()
data$predicted <- predict(weight_length, re.form = NA)  # Predictions without random effects
data$residuals <- resid(weight_length)  # Residuals from the model
ggplot(data, aes(x = weight, y = predicted)) +
  geom_point(alpha = 0.5, color = "blue") +  # Adjusted predictions
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Trend line for model estimates
  labs(x = "Weight", y = "Predicted Log Length", title = "Adjusted Correlation (Mixed Model)") +
  theme_classic()

# Scatter plot with regression line
ggplot(data, aes(x = Log_Length, y = weight)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear regression line
  labs(x = "Length (mm)", y = "Weight", title = "Correlation between Length and Weight") +
  theme_classic()

library(ggplot2)

# Example: Scatter plot of biomass vs. ammonia
ggplot(total, aes(x = Length_mm, y = biomass_g_ha_day)) +
  geom_point(alpha = 0.6, color = "blue") +  # Scatter points
  labs(x = "Mean Ammonia", y = "Biomass (g/ha/day)", 
       title = "Scatter Plot of Biomass vs. Ammonia") +
  theme_classic()



# Create a scatter plot to visualize the correlation
ggplot( data, aes(NH4,Length_mm)) +
  geom_point() +                      # Plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(title = "Scatter Plot with Linear Fit",
       x = "x",
       y = "y") 

#Amaia's idea
diamint<- lm(diamMax_Value~ Fire.Severity, diam_site)
diamfreq<- lm(diamMax_Value~ Fire.Interval*Fire.Severity, diam_site)
diamVeg <- lm(diamMax_Value~ Nitrogen, diam_site)


car::Anova(diamVeg)   #[instead of anova function given by WUR]
#post hoc LSD test to


