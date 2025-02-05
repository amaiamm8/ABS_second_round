library(dplyr)
library(readxl)
ph_data<- read.csv("outputs/pH_output.csv")
site_data<- read.csv("raw/site_data_Amaia.csv")[,-1]
secondround_data <- site_data %>%
  filter(Site %in% c(7, 8 , 10, 12 , 26, 34))
diam_data<- read_excel("raw/Updated hyphal length.xlsx")
diam_data <- diam_data %>%
  filter(Length_mm != 0)
diam_summary <- diam_data %>%
  mutate(Site = sub("^(S)( *)", "\\2", Site))%>%
  mutate(Transect = sub("^(T)( *)", "\\2", Transect))%>%
  group_by(Site, Transect) %>%
  summarize(
    diamMin_Value = mean(Min_Value, na.rm = TRUE),
    diamMax_Value = mean(Max_Value, na.rm = TRUE),
    diamAvg_Value = mean(Avg, na.rm = TRUE),
    .groups = "drop" )

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
pH_data
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
  left_join(pH_data%>%  
              mutate(Site = sub("^(S)( *)", "\\2", Site),
                    Transect = sub("^(T)( *)", "\\2", Transect)))

#Added your ph data to this [test]
install.packages("Matrix", type = "source")
install.packages("lme4", type = "source")
library(Matrix)
library(lme4)
library(car)
library(emmeans)
#Check the distribution of the response variable, you want this to be normalish
hist(data$Length_mm)
hist(log10(data$Length_mm))
data%>%tail() %>%  # Selects the last 6 rows of the dataframe
  arrange(Length_mm) 

data<-data%>%mutate(Log_Length=log10(Length_mm+.001/2))#adding lowest value above 0 divided by 2 (can talk about why
#BUT how did you measure 0 length?- results from Mosaic! but have deleted them now at the top

#one example of a model you could build using length as a response
model_1<-lmer(Log_Length~ Fire.Interval + Fire.Severity+ NO3+NH4+ Bray.P+ (1|Site/Transect) , 
                           data=data)
# ^the model above gives: boundary (singular) fit: see help('isSingular')
summary(model_1)
library(car)
Anova_1<-round(Anova(model_1,test='F'), 2) 
Anova_1
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
library(performance)
r2(model_2)
Log_length<-as.data.frame(emmeans(model_2, ~Fire.Severity))
Log_length
plot(Log_length)
