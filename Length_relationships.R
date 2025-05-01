#Analysing the data
#install.packages("Matrix", type = "source")
#install.packages("lme4", type = "source")
#install.packages("performance")
library(readxl)
library(Matrix)
library(lme4)
library(car)
library(emmeans)
library(performance)
library(emmeans)
library(ggplot2)
data<- read_excel("raw/alldataforlength.xlsx")

data$Length_um<- (data$Length_mm*1000 )
#Check the distribution of the response variable, you want this to be normalish
#trying different transformations
hist(data$Length_mm)
#CONVERT TO LOG
data$Log_Length<- log(data$Length_mm)
hist((data$Log_Length))
#Use QQplots to evaluate your model outputs, not the inputs, you only need to check the distribution of your response variable
#and maybe sometimes transforming your explanitory variables, but this is a story for another time 
qqnorm((data$Log_Length), main = "Q-Q Plot of Length_mm")
qqline(data$Log_Length, col = "red")
data$Sqrt_Length <- sqrt(data$Length_mm)
hist(data$Sqrt_Length)
data$Inv_Length <- 1 / data$Length_mm
hist(data$Inv_Length)
wilcox.test (data$Length_mm)
hist((data$CV_Length))

data%>%tail() %>%  # Selects the last 6 rows of the dataframe
  arrange(Length_mm) 

summary2<- data %>%
  group_by(Site)%>%
  summarise(
    Min = min(CV_Length, na.rm = TRUE),
    Max = max(CV_Length, na.rm = TRUE),
    Mean = mean(CV_Length, na.rm = TRUE),
    Median = median(CV_Length, na.rm = TRUE),
    SD = sd(CV_Length, na.rm = TRUE)
  )
summary2
summary3<- data %>%
  group_by(Site)%>%
  summarise(
    Min = min(Length_um, na.rm = TRUE),
    Max = max(Length_um, na.rm = TRUE),
    Mean = mean(Length_um, na.rm = TRUE),
    Median = median(Length_um, na.rm = TRUE),
    SD = sd(Length_um, na.rm = TRUE),
    CV=sd(Length_um, na.rm = TRUE)/mean(Length_um, na.rm = TRUE)
  )
summary3

#CV_length responding to fire regime
#modelCV<-lmer(CV_Length~ Fire.Interval+ (1|Site/Transect/Location) , 
#              data=data)

#summary(modelCV)
#AnovaCV<-round(Anova(modelCV,test='F'), 2) 
#AnovaCV
#plot(modelCV)
#CV_Length<-as.data.frame(emmeans(modelCV, ~Fire.Interval))
#CV_Length
#plot(CV_Length)
#qqPlot(resid(modelCV)) #this is the QQ plot you want to check!
####
#plot_m3<-ggplot(CV_Length, aes(x = Fire.Interval, y = CV_Length) )+
#  geom_point(data=data, aes(x=Fire.Interval, y=CV_Length), size=1)+
#  labs(x = "Fire Interval", y = "Coefficient of Variation of Hyphal Width") +
#  
#  annotate("text", x = 1.9, y = Inf, label = paste0("Interval (p) = ", AnovaCV["Fire.Interval", "Pr(>F)"]),
#          hjust = 1.5, vjust = 1.5, size = 3)+
# theme_classic()+
# theme(axis.text.x = element_text( hjust = 0.5, size = 12),
#       axis.text.y = element_text(size = 8),
#       axis.title.x = element_text(size = 12),
#        axis.title.y = element_text(size = 12),
#       axis.line = element_line(linewidth = 0.5),
#       legend.position = 'none')
#plot_m3




#Log of the CV
data<-data%>%mutate(Log_CVLength=log(CV_Length))
modelCV1<-lmer(Log_CVLength~ Fire.Interval + (1|Site/Transect/Location) , 
               data=data)
summary(modelCV1)
AnovaCV1<-round(Anova(modelCV1,test='F'), 2) 
AnovaCV1
plot(modelCV1)
CV1_Length<-as.data.frame(emmeans(modelCV1, ~Fire.Interval))
CV1_Length
plot(CV1_Length)
qqPlot(resid(modelCV1))

plot_CV1<-ggplot(CV1_Length, aes(x = Fire.Interval, y = Log_CVLength) )+
  geom_point(data=data, aes(x=Fire.Interval, y=Log_CVLength), size=1)+
  labs(x = "Fire Interval", y = "Coefficient of Variation of Hyphal Diameter") +
  annotate("text", x = 1.9, y = Inf, label = paste0("Interval (p) = ", AnovaCV1["Fire.Interval", "Pr(>F)"]),
           hjust = 2.5, vjust = 1.5, size = 3)+
  theme_classic()+
  theme(axis.text.x = element_text( hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.line = element_line(linewidth = 0.5),
        legend.position = 'none')
plot_CV1


#Log_length responding to fire regime
model0<-lmer(Log_Length~ Fire.Interval + (1|Site/Transect/Location/Rep) , 
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
  labs(x = "Fire Interval", y = " Hyphal Diameter") +
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



#####################
#ommmit (not enough replicates to do this graph)
# Create the boxplot with p-values (in log length)
library(ggpubr)
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
#####################
model_1<-lmer(Log_CVLength~ mean_ammonia +mean_nitrate+ Ortho_P_mg_kg  + Avg_pH +(1|Site/Transect/Location) , 
              data=data)
summary(model_1)

Anova_1<-round(Anova(model_1,test='F'), 2) 
Anova_1
plot(model_1)
qqPlot(resid(model_1))

r2(model_1)
Log_length<-as.data.frame(emmeans(model_1, ~Fire.Interval))
Log_length
plot(Log_length)
check_collinearity(model_1)


#####################
model_2<-lmer(Log_CVLength~ mean_ammonia +mean_nitrate+ Ortho_P_mg_kg  + Avg_pH + Tree.Basal.Area_m2  + Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc +perc_myco_host_freq+ (1|Site/Transect/Location) , 
              data=data)
summary(model_2)

Anova_2<-round(Anova(model_2,test='F'), 2) 
Anova_2
plot(model_2)
qqPlot(resid(model_2))

r2(model_2)
Log_length<-as.data.frame(emmeans(model_2, ~Fire.Interval))
Log_length
plot(Log_length)
check_collinearity(model_2)


#model for log length responding to fire regime and nutrients
model_3<-lmer(Log_Length~ Fire.Interval + mean_ammonia +mean_nitrate+ Ortho_P_mg_kg + Avg_pH+ Tree.Basal.Area_m2  + Herb.Cover_0.50cm_perc+ Shrub.Cover_50.200cm_perc+ perc_myco_host_freq+(1|Site/Transect/Location/Rep) , 
              data=data)
summary(model_3)
Anova_3<-round(Anova(model_3,test='F'), 2) 
Anova_3
plot(model_3)
qqPlot(resid(model_3))

r2(model_3)
check_collinearity(model_3)
emm<- emmeans(model_3, specs = "Fire.Interval")
pairs(emm, adjust="none")
emmeans(model_3, specs = "Fire.Interval", adjust = "none")
multcomp::cld(emm)
ranef(model_3) 
predict(model_3, re.form = NULL)  # Includes both fixed & random effects
#####################
model_4<-lmer(Log_Length~ mean_ammonia +mean_nitrate+ Ortho_P_mg_kg  + Avg_pH +(1|Site/Transect/Location) , 
              data=data)
summary(model_4)

Anova_4<-round(Anova(model_4,test='F'), 2) 
Anova_4
plot(model_4)
qqPlot(resid(model_4))

r2(model_4)
Log_length<-as.data.frame(emmeans(model_4, ~Fire.Interval))
Log_length
plot(Log_length)
check_collinearity(model_4)


library(ggpubr)
ggplot(data, aes(x = Fire.Interval, y = Log_Length)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log-Transformed Width by Fire Severity",
       x = "Fire Interval",
       y = "Width (Log-transformed)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 9)) +
  stat_compare_means(aes(group = Fire.Severity), label = "p.signif")  # Add p-values (show significance)

ggplot(data, aes(x = Fire.Interval, y = Log_Length)) +
  geom_boxplot() +
  labs(title = "Boxplot of Log-Transformed Length by Fire Interval",
       x = "Fire Interval",
       y = "Length (Log-transformed)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 9)) +
  stat_compare_means(aes(group = Fire.Interval), label = "p.signif")

ggplot(data, aes(x = Site, y = Log_Length)) +
  geom_boxplot() +
  labs(title = "Log-Transformed Length by Site",
       x = "Site",
       y = "Length (Log-transformed)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 9)) +
  stat_compare_means(method = "anova", label.y = max(data$Log_Length, na.rm = TRUE) * 1.05)



length_site<- lm(Length_mm ~ Fire.Interval, data = data)
anova(length_site)
emm<- emmeans(length_site, specs = "Fire.Interval")
pairs(emm, specs="Site", adjust="none")
multcomp::cld(emm) 
site_emm <- emmeans(length_site, pairwise ~ Fire.Interval, adjust = "tukey")
site_emm


ggplot(data, aes(x = Fire.Interval, y = Length_um)) +
  geom_boxplot() +
  labs(x = "Fire Interval",
       y = "Hyphal Diameter (µm)") +
  annotate("text",
           x = 1.9,
           y = Inf,
           label = paste0("Interval (p) = ", round(Anova0["Fire.Interval", "Pr(>F)"], 3)),
           hjust = 1, vjust = 1.5, size = 3) +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.line = element_line(linewidth = 0.5),
        legend.position = 'none')+ 
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 40, 10))

ggplot(data, aes(x = Site, y = Length_um)) +
  geom_boxplot() +
  labs(title = "Log-Transformed Length by Site",
       x = "Site",
       y = "Length (Log-transformed)") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 100, 10))

