library(dplyr)
library(readxl)

site_data<- read.csv("Rawdata/site_data_Amaia.csv")
secondround_data <- site_data %>%
  filter(Site %in% c(7, 8 , 10, 12 , 26, 34))
diam_data<- read_excel("Rawdata/Updated hyphal length.xlsx")

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
  full_join(secondround_data, by = c("Site", "Transect")) %>%
  select(-X)

diamint<- lm(diamMax_Value~ Fire.Severity, diam_site)
diamfreq<- lm(diamMax_Value~ Fire.Interval*Fire.Severity, diam_site)
diamVeg <- lm(diamMax_Value~ Nitrogen, diam_site)


car::Anova(diamVeg)   #[instead of anova function given by WUR]
#post hoc LSD test to
