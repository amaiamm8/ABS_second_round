library(dplyr)
library(readxl)
library(tidyverse)
library(readxl)

#Loading the data
site_data<- read.csv("raw/site_data_Amaia.csv")[,-1]
#filtering the data to my sites
  secondround_data <- site_data %>%
    filter(Site %in% c(7, 8 , 10, 12 , 26, 34))
diam_data<- read_excel("raw/Updated hyphal length.xlsx")
#THIS NEEDS AN EXPLINATION...I dont know how you measured 0, and maybe this is the best solution, but it makes me wonder about the rest of your
#measurements... and others will wonder why
diam_data <- diam_data %>%
  filter(Length_mm != 0)#0 measurements all taken from Mosaic!

ph_data<- read.csv("outputs/pH_output.csv")

biomass<- read_excel("raw/Labbook.xlsx", "Indiv weights")[,-4]
biomass$Tube_ID<- as.character(biomass$Tube_ID)

Ortho_P<- read.csv("outputs/Ortho_P.csv")
Ortho_P <- Ortho_P %>%
  mutate(Site = sub("^(S\\d+).*", "\\1", Names),
    Transect = sub(".*(T\\d+).*", "\\1", Names),  
    Location = sub(".*(L\\d+)$", "\\1", Names)  
  )%>%
  select(Site, Transect, Location, Ortho_blanked, Ortho_P_mg_kg)


  #just to add site names to tubeID
  generic<- read_excel("raw/Labbook.xlsx") 
  generic<- generic %>%
    distinct(Site, Transect, Location,.keep_all= TRUE) %>%
    select(Site, Transect, Location, Tube_ID)
  biomass <- biomass%>%
    full_join(generic, by= "Tube_ID")


################
#Putting all of the data together
#"data" contains ALL data (biomass, diameter, ph, site data and Ortho P)
  
  #calculate Coefficient of variation
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
                    Transect = sub("^(T)( *)", "\\2", Transect))
            )%>%
  left_join(biomass%>%  
              mutate(Site = sub("^(S)( *)", "\\2", Site),
                    Transect = sub("^(T)( *)", "\\2", Transect)))%>%
  left_join(Ortho_P%>%  
            mutate(Site = sub("^(S)( *)", "\\2", Site),
                   Transect = sub("^(T)( *)", "\\2", Transect)),
            by = c("Site", "Transect", "Location"))
      
write_xlsx(data, "raw/alldata.xlsx")

  
#Analysing the data
#install.packages("Matrix", type = "source")
#install.packages("lme4", type = "source")
#install.packages("performance")
  library(Matrix)
  library(lme4)
  library(car)
  library(emmeans)
  library(performance)
  library(emmeans)
  

#Check the distribution of the response variable, you want this to be normalish
#trying different transformations
hist(data$Length_mm)

#STOP USING AVERAGES IN YOUR MODEL!!!!!
data$Log_Length<- log10(data$Avg)
hist((data$Log_Length))

#Use QQplots to evaluate your model outputs, not the inputs, you only need to check the distribution of your response variable
#and maybe sometimes transforming your explanitory variables, but this is a story for another time 
qqnorm((data$Log_Length), main = "Q-Q Plot of Length_mm")
qqline(data$Log_Length, col = "red")

#SOL script doing same thing as above
data$Log10_Length<- log10(data$Length_mm)
#this data looks normal enough
hist((data$Log10_Length))
#this test below doesnt matter, but I am showing that how the data looks is more important than the output of a test
shapiro.test(data$Log10_Length)

#you only need to add values to your data if you have 0s otherwise there is no need
data$Log_Length1 <- log(data$Length_mm + 0.001)
hist(data$Log_Length1)

data$Sqrt_Length <- sqrt(data$Length_mm)
hist(data$Sqrt_Length)

data$Inv_Length <- 1 / data$Length_mm
hist(data$Inv_Length)

wilcox.test (data$Length_mm)





data%>%tail() %>%  # Selects the last 6 rows of the dataframe
  arrange(Length_mm) 


data<-data%>%mutate(Log_Length=log10(Length_mm))

#Log_length responding to fire regime
model_1<-lmer(Log_Length~ Fire.Interval * Fire.Severity+ (1|Site/Transect) , 
                           data=data)

summary(model_1)

Anova_1<-round(Anova(model_1,test='F'), 2) 
Anova_1
plot(model_1)

#this is the QQ plot you wan to check!
qqPlot(resid(model_1))

ggplot(data, aes(x = Fire.Interval, y = Length_mm, fill = Fire.Severity)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log-Transformed Length by Fire Intensity and Frequency",
       x = "Fire Intensity",
       y = "Log Length",
       fill="Fire Severity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(size = 9))

# Pairwise comparisons for Fire.Severity
emmeans_severity <- emmeans(model_1, pairwise ~ Fire.Interval)
summary(emmeans_severity)


library(ggpubr)

# Create the boxplot with p-values
ggplot(data, aes(x = Fire.Interval, y = Log_Length, fill = Fire.Severity)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log-Transformed Length by Fire Severity and Frequency",
       x = "Fire Interval",
       y = "Log Length",
       fill = "Fire Severity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 9)) +
  stat_compare_means(aes(group = Fire.Severity), label = "p.signif")  +# Add p-values (show significance)
  stat_compare_means(aes(group = Fire.Interval), label = "p.signif", label.x = 1.5) 

ggplot(data, aes(x = Fire.Severity, y = Log_Length, fill = Fire.Interval)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log-Transformed Length by Fire Severity and Frequency",
       x = "Fire Severity",
       y = "Log Length",
       fill = "Fire Interval") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 9)) +
  stat_compare_means(aes(group = Fire.Interval), label = "p.signif") + # Add p-values (show significance)
  stat_compare_means(aes(group = Fire.Severity), label = "p.signif", label.x = 1.5) 


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

r2(model_2)
Log_length<-as.data.frame(emmeans(model_2, ~Fire.Severity))
Log_length
plot(Log_length)


#model for log length responding to fire regime and nutrients
model_3<-lmer(Log_Length~ Fire.Interval + Fire.Severity+ NO3+NH4+ Ortho_P_mg_kg  +(1|Site/Transect) , 
              data=data)
summary(model_3)
Anova_3<-round(Anova(model_3,test='F'), 2) 
Anova_3
plot(model_3)
qqPlot(resid(model_3))

r2(model_3)
Log_length<-as.data.frame(emmeans(model_2, ~Fire.Severity))
Log_length
plot(Log_length)


##########
#something else
library(ggplot2)


cor(data$Length_mm, data$weight, method = "pearson")
cor<- data%>%
  select(weight, Length_mm,Avg, Site, Transect, Location)%>%
  distinct(Site, Transect, Location,weight, Length_mm, Avg)


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

