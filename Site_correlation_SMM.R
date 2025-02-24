library(dplyr)
library(readxl)
library(tidyverse)
library(readxl)
library(writexl)

#Loading the data
#site data from Gordon
site_data<- read.csv("raw/site_data_Amaia.csv")[,-1]
#filtering the data to my sites
secondround_data <- site_data %>%
  filter(Site %in% c(7, 8 , 10, 12 , 26, 34))
#diameter data from Mosaic
diam_data<- read_excel("raw/Updated hyphal length.xlsx")
diam_data <- diam_data %>%
  filter(Length_mm != 0) #0 measurements - issues from Mosaic!
#soil pH measurements
ph_data<- read.csv("outputs/pH_output.csv")

#biomass corrected estimates 
biomass<- read_excel("raw/Labbook.xlsx", "Indiv weights")[,-4]
biomass$Tube_ID<- as.character(biomass$Tube_ID)
#Ortho P for this study
Ortho_P<- read.csv("outputs/Ortho_P.csv")
Ortho_P <- Ortho_P %>%
  mutate(Site = sub("^(S\\d+).*", "\\1", Names),
         Transect = sub(".*(T\\d+).*", "\\1", Names),  
         Location = sub(".*(L\\d+)$", "\\1", Names)  
  )%>%
  select(Site, Transect, Location, Ortho_blanked, Ortho_P_mg_kg)
#mycorrhizal hosts (from Sol)
myco_data <- read.csv("raw/Myco_host_abundance.csv")
myco_data <- myco_data %>%
  filter(Site %in% c(7, 8 , 10, 12 , 26, 34))%>%
  mutate(Site= as.character(Site))%>%
  mutate(Transect= as.character(Transect))

#just to add site names to tubeID
generic<- read_excel("raw/Labbook.xlsx") 
generic<- generic %>%
  distinct(Site, Transect, Location,.keep_all= TRUE) %>%
  select(Site, Transect, Location, Tube_ID)
biomass <- biomass%>%
  full_join(generic, by= "Tube_ID")

#nitrogen data from Sol's first round- second round not available yet
nutri_sol<- read_csv("outputs/Resin_Nutrients_SMM_1stRnd.csv", col_select= c("Site","Transect", "Ammonia_mg_kg", "Nitrate_mg_kg"))
nutri_sol<- nutri_sol%>%
  group_by(Site, Transect)%>%
  summarise(mean_ammonia = mean(Ammonia_mg_kg, na.rm = TRUE), 
            mean_nitrate = mean(Nitrate_mg_kg, na.rm = TRUE))%>%
  mutate(Site=as.character(Site), 
         Transect= as.character(Transect))

################
#Putting all of the data together
#"data" contains ALL data (biomass, diameter, ph, site data and Ortho P and N from 1stRound)
#calculate Coefficient of variation (per Rep)
data<-diam_data%>%
  group_by(Site,Transect,Location,Rep)%>%
  #calculate Coefficient of Variation (CV= sd/mean)
  mutate(CV_Length = sd(Length_mm, na.rm = TRUE) / mean(Length_mm, na.rm = TRUE))%>%
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
            by = c("Site", "Transect", "Location"))%>%
  left_join(myco_data, by = c("Site", "Transect"))%>%
  left_join(nutri_sol, by = c("Site", "Transect"))

data$Fire.Interval<- as.factor(data$Fire.Interval)
data$Fire.Severity<- as.factor(data$Fire.Severity)
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
data$Log_Length1 <- log(data$Length_mm)
hist(data$Log_Length1)
data$Sqrt_Length <- sqrt(data$Length_mm)
hist(data$Sqrt_Length)
data$Inv_Length <- 1 / data$Length_mm
hist(data$Inv_Length)
wilcox.test (data$Length_mm)

data%>%tail() %>%  # Selects the last 6 rows of the dataframe
  arrange(Length_mm) 


data<-data%>%mutate(Log_Length=log10(Length_mm))

#CV_length responding to fire regime
modelCV<-lmer(CV_Length~ Fire.Interval + Fire.Severity+ (1|Site/Transect/Location) , 
              data=data)

summary(modelCV)
AnovaCV<-round(Anova(modelCV,test='F'), 2) 
AnovaCV
plot(modelCV)
CV_Length<-as.data.frame(emmeans(modelCV, ~Fire.Interval))
CV_Length
plot(CV_Length)
qqPlot(resid(modelCV)) #this is the QQ plot you want to check!

plot_m3<-ggplot(CV_Length, aes(x = Fire.Interval, y = CV_Length) )+
  geom_point(data=data, aes(x=Fire.Interval, y=CV_Length), size=1)+
  labs(x = "Fire Interval", y = "Coefficient of Variation of Hyphal Width") +
  
  annotate("text", x = 1.9, y = Inf, label = paste0("Interval (p) = ", AnovaCV["Fire.Interval", "Pr(>F)"]),
           hjust = 2.5, vjust = 1.5, size = 3)+
  theme_classic()+
  theme(axis.text.x = element_text( hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.line = element_line(linewidth = 0.5),
        legend.position = 'none')
plot_m3



#Log of the CV
data<-data%>%mutate(Log_CVLength=log10(CV_Length))
modelCV1<-lmer(Log_CVLength~ Fire.Interval + Fire.Severity+ (1|Site/Transect/Location) , 
               data=data)
summary(modelCV1)
AnovaCV1<-round(Anova(modelCV1,test='F'), 2) 
AnovaCV1
plot(modelCV1)
CV1_Length<-as.data.frame(emmeans(modelCV1, ~Fire.Interval))
CV1_Length
plot(CV1_Length)
qqPlot(resid(modelCV1))

plot_m3<-ggplot(CV1_Length, aes(x = Fire.Interval, y = Log_CVLength) )+
  geom_point(data=data, aes(x=Fire.Interval, y=Log_CVLength), size=1)+
  labs(x = "Fire Interval", y = "Coefficient of Variation of Hyphal Width") +
  annotate("text", x = 1.9, y = Inf, label = paste0("Interval (p) = ", AnovaCV1["Fire.Interval", "Pr(>F)"]),
           hjust = 2.5, vjust = 1.5, size = 3)+
  theme_classic()+
  theme(axis.text.x = element_text( hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.line = element_line(linewidth = 0.5),
        legend.position = 'none')
plot_m3


#Log_length responding to fire regime
model0<-lmer(Log_Length~ Fire.Interval + Fire.Severity+ (1|Site/Transect/Location) , 
              data=data)

summary(model0)
Anova0<-round(Anova(model0,test='F'), 2) 
Anova0
plot(model0)
Log_Length<-as.data.frame(emmeans(model0, ~Fire.Interval))
Log_Length
plot(model0)
qqPlot(resid(model0))

plot_log <- ggplot(data, aes(x = Fire.Interval, y = Log_Length)) +
  geom_boxplot() +  # Boxplot for raw data per Fire Interval
  geom_point(data = Log_Length, aes(x = Fire.Interval, y = emmean), color = "red", size = 3) +  # Model estimates
  geom_errorbar(data = Log_Length, aes(x = Fire.Interval, y = emmean, ymin = lower.CL, ymax = upper.CL), 
                color = "red", width = 0.2) +  # Confidence intervals
  labs(x = "Fire Interval", y = "Coefficient of Variation of Hyphal Width") +
  annotate("text", x = 1.9, y = Inf, label = paste0("Interval (p) = ", Anova0["Fire.Interval", "Pr(>F)"]),
           hjust = 2.5, vjust = 1.5, size = 3) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.line = element_line(linewidth = 0.5),
        legend.position = 'none')


plot_log




library(ggpubr)

# Create the boxplot with p-values (in log length)
ggplot(data, aes(x = Fire.Interval, y = Log_Length, fill = Fire.Severity)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log-Transformed Width by Fire Severity and Frequency",
       x = "Fire Interval",
       y = "Width (Log-transformed)",
       fill = "Fire Severity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 9)) +
  stat_compare_means(aes(group = Fire.Severity), label = "p.signif")  +# Add p-values (show significance)
  stat_compare_means(aes(group = Fire.Interval), label = "p.signif", label.x = 1.5) 

#opposite boxplot
ggplot(data, aes(x = Fire.Severity, y = Log_Length, fill = Fire.Interval)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log-Transformed Width by Fire Severity and Frequency",
       x = "Fire Severity",
       y = "Log Width",
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
model_2<-lmer(CV_Length~ Fire.Interval + Fire.Severity+ NO3+NH4+ Ortho_P_mg_kg  + Tree.Basal.Area_m2  + Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc +  All.Tree.Canopy.Cover_perc+perc_myco_host_freq+ (1|Site/Transect/Location/Rep) , 
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
model_3<-lmer(Log_Length~ Fire.Interval + Fire.Severity+ NO3+NH4+ Ortho_P_mg_kg + Tree.Basal.Area_m2  + Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+ All.Tree.Canopy.Cover_perc+ perc_myco_host_freq+(1|Site/Transect) , 
              data=data)
summary(model_3)
Anova_3<-round(Anova(model_3,test='F'), 2) 
Anova_3
plot(model_3)
qqPlot(resid(model_3))

r2(model_3)

car::Anova(single.lm)   #[instead of anova function given by WUR]
#post hoc LSD test to 

emm<- emmeans(model_1, specs = "Name")
pairs(emm, specs="Log_Length", adjust="none")
emmeans(model_1, specs = "Log_Length", adjust = "none")
multcomp::cld(emm)

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
