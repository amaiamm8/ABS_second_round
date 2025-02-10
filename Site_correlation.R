library(dplyr)
library(readxl)
#read the data
ph_data<- read.csv("outputs/pH_output.csv")
site_data<- read.csv("raw/site_data_Amaia.csv")[,-1] #ignoring the first column
biomass<- read_excel("raw/Labbook.xlsx", "Indiv weights")[,-4]
  biomass$Tube_ID<- as.character(biomass$Tube_ID)

  #just to add site names to tubeID
generic<- read_excel("raw/Labbook.xlsx") 
generic<- generic %>%
  distinct(Site, Transect, Location,.keep_all= TRUE) %>%
  select(Site, Transect, Location, Tube_ID)
biomass <- biomass%>%
  full_join(generic, by= "Tube_ID")

#filtering all the site data to my sites
secondround_data <- site_data %>%
  filter(Site %in% c(7, 8 , 10, 12 , 26, 34))

#diameter data but removing the 0 lengths (estimated by mosaic)
diam_data<- read_excel("raw/Updated hyphal length.xlsx")
diam_data <- diam_data %>%
  filter(Length_mm != 0)

######
#summarising diameter data
diam_summary <- diam_data %>%
  mutate(Site = sub("^(S)( *)", "\\2", Site))%>%
  mutate(Transect = sub("^(T)( *)", "\\2", Transect))%>%
  group_by(Site, Transect) %>%
  summarize(
    diamMin_Value = mean(Min_Value, na.rm = TRUE),
    diamMax_Value = mean(Max_Value, na.rm = TRUE),
    diamAvg_Value = mean(Avg, na.rm = TRUE),
    .groups = "drop" )

#ensuring right formats
secondround_data$Site <- as.factor(secondround_data$Site)
secondround_data$Transect <- as.factor(secondround_data$Transect)
diam_summary$Site <- as.factor(diam_summary$Site)
diam_summary$Transect <- as.factor(diam_summary$Transect)
diam_site$Fire.Severity <- as.factor(diam_site$Fire.Severity)

# Perform full joins on all three datasets
diam_site <- diam_summary %>%
  full_join(secondround_data, by = c("Site", "Transect"))

diamint<- lm(diamMax_Value~ Fire.Severity, diam_site)
diamfreq<- lm(diamMax_Value~ Fire.Interval*Fire.Severity, diam_site)
diamVeg <- lm(diamMax_Value~ Nitrogen, diam_site)


car::Anova(diamVeg)   #[instead of anova function given by WUR]
#post hoc LSD test to

################
#Sol script
  #"data" contains ALL data (biomass, diameter, ph, site data)
data<-diam_data%>%
  group_by(Site,Transect,Location)%>%
  #calculate Coefficient of Variation (CV= sd/mean)
  summarise(CV_Length = sd(Length_mm, na.rm = TRUE) / mean(Length_mm, na.rm = TRUE))%>%
  left_join(diam_data)%>%
  mutate(Site = sub("^(S)( *)", "\\2", Site),
         Transect = sub("^(T)( *)", "\\2", Transect))%>%
  left_join(secondround_data%>%
              mutate(Site=as.character(Site),
                     Transect=as.character(Transect)))%>%
  left_join(ph_data%>%  
              mutate(Site = sub("^(S)( *)", "\\2", Site),
                    Transect = sub("^(T)( *)", "\\2", Transect)))%>%
  left_join(biomass%>%  
              mutate(Site = sub("^(S)( *)", "\\2", Site),
                    Transect = sub("^(T)( *)", "\\2", Transect)))

#install.packages("Matrix", type = "source")
#install.packages("lme4", type = "source")
library(Matrix)
library(lme4)
library(car)
library(emmeans)

#Check the distribution of the response variable, you want this to be normalish
#trying different transformations
hist(data$Length_mm)
hist(log10(data$Length_mm))
data$Log_Length1 <- log(data$Length_mm + 0.001)
hist(data$Log_Length)
data$Sqrt_Length <- sqrt(data$Length_mm)
hist(data$Sqrt_Length)
data$Inv_Length <- 1 / data$Length_mm
hist(data$Inv_Length)
wilcox.test (data$Length_mm)
qqnorm((data$Sqrt_Length), main = "Q-Q Plot of Length_mm")
qqline((data$Sqrt_Length), col = "red")

data%>%tail() %>%  # Selects the last 6 rows of the dataframe
  arrange(Length_mm) 



data<-data%>%mutate(Log_Length=log10(Length_mm+.001/2))#adding lowest value above 0 divided by 2 (can talk about why
#BUT how did you measure 0 length?- results from Mosaic! but have deleted them now at the top

#one example of a model you could build using length as a response
model_1<-lmer(Log_Length~ Fire.Interval + Fire.Severity+ NO3+NH4+ Bray.P+ (1|Site/Transect) , 
                           data=data)
    #(1|Site/Transect) means nesting the transect within the site
# ^the model above gives: boundary (singular) fit: see help('isSingular')- deleting Transect OR Site removes error
summary(model_1)

Anova_1<-round(Anova(model_1,test='F'), 2) 
Anova_1 #only Fire.Severity significant (0.05) (in Transect)not in Site
plot(model_1)
qqPlot(resid(model_1))
install.packages("performance")
library(performance)
library(emmeans)
r2(model_1)
Log_length<-as.data.frame(emmeans(model_1, ~Fire.Severity))
Log_length
plot(Log_length)


#one example of a model you could build using the variation in length as a response
model_2<-lmer(CV_Length~ Fire.Interval + Fire.Severity+ NO3+NH4+ Bray.P+ (1|Site/Transect) , 
              data=data)

summary(model_2)
Anova_2<-round(Anova(model_2,test='F'), 2) 
Anova_2
plot(model_2)
qqPlot(resid(model_2))
install.packages("performance")
library(performance)
r2(model_2)
Log_length<-as.data.frame(emmeans(model_2, ~Fire.Severity))
Log_length
plot(Log_length)



#Attempting to build a JSDM of some sort


# Load necessary packages
install.packages("sdm")
library(sdm)
library(mgcv)  # For Generalized Additive Models (GAMs)
library(ggplot2)



# Fit a Generalized Additive Model (GAM) to predict trait distribution
# Example: Modeling the distribution of size trait with environmental predictors
model_size <- gam(Length_mm ~ Fire.Interval + Fire.Severity + Most.Recent.Fire_Year+ Avg_pH + Litter.Cover_20mm_perc + Bray.P, data = data)

# View the model summary
summary(model_size)
plot(model_size$residuals)


#ATTEMPTING A PCA
# Load necessary libraries
install.packages("brms")
library(brms)
library(tidyverse)
library(vegan)


env_data <- data.frame(
  Fire.Interval = data$Fire.Interval,
  Fire.Severity = data$Fire.Severity,
  NO3 = data$NO3,
  NH4 = data$NH4
)
env_data_numeric <- env_data %>% select_if(is.numeric)
# Assuming env_data is a data frame or matrix of environmental variables (e.g., temperature, precipitation, etc.)
 # Standardizing the environmental data
env_data_scaled <- scale(env_data_numeric)

# Perform PCA
pca_result <- prcomp(env_data_scaled, center = TRUE, scale. = TRUE)

# Check PCA summary
summary(pca_result)

# Extract PCA scores
pc_scores <- pca_result$x
# Ensure pc_scores is in a data frame format
pc_scores_df <- as.data.frame(pc_scores)  # Convert PCA scores to a data frame
data_for_model <- cbind(pc_scores_df, Length_mm = data$Length_mm)

# Include the response variable (length_mm) in the same data frame
model_size <- brm(
  Length_mm ~ PC1 + PC2,  # Use PCA component names as predictors
  data = data_for_model,        # The data frame with the response and predictors
  family = gaussian(),          # Gaussian family for continuous response
  prior = c(set_prior("normal(0, 1)", class = "b")),  # Specify priors for the model coefficients
  chains = 2,                   # Number of Markov chains
  iter = 2000                   # Number of iterations per chain
)

summary(model_size)
