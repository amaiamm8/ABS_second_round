library(dplyr)
library(readxl)
library(writexl)
library(readr)
library(performance)
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
ph_data<- read.csv("outputs/pH_output.csv")%>%
  mutate(Site = sub("^(S)( *)", "\\2", Site),
       Transect = sub("^(T)( *)", "\\2", Transect),
       Location = sub("^(L)( *)", "\\2", Location))

#Ortho P for this study
Ortho_P<- read.csv("outputs/Ortho_P.csv")
Ortho_P <- Ortho_P %>%
  mutate(Site = sub("^(S)(\\d+).*", "\\2", Names),
         Transect = sub(".*(T)(\\d+).*", "\\2", Names),  
         Location = sub(".*(L)(\\d+)$", "\\2", Names)  
  )%>%
  select(Site, Transect, Location, Ortho_blanked, Ortho_P_mg_kg)

#mycorrhizal hosts (from Sol)
myco_data <- read.csv("raw/Myco_host_abundance.csv")
myco_data <- myco_data %>%
  filter(Site %in% c(7, 8 , 10, 12 , 26, 34))%>%
  mutate(Site= as.character(Site))%>%
  mutate(Transect= as.character(Transect))



#nitrogen data from Sol's first round- second round not available yet
nutri_sol<- read_csv("outputs/Resin_Nutrients_SMM_1stRnd.csv", col_select= c("Site","Transect", "Ammonia_mg_kg", "Nitrate_mg_kg"))
nutri_sol<- nutri_sol%>%
  group_by(Site, Transect)%>%
  summarise(mean_ammonia = mean(Ammonia_mg_kg, na.rm = TRUE), 
            mean_nitrate = mean(Nitrate_mg_kg, na.rm = TRUE))%>%
  mutate(Site=as.character(Site), 
         Transect= as.character(Transect))


#biomass production estimates
biomass <- read_excel("raw/biomass.xlsx") %>%  
  mutate(Location = sub(".*L(\\d+)$", "\\1", Location))%>%
  select(1:23)



################
#Putting all of the data together
#"data" contains ALL data (biomass, diameter, ph, site data and Ortho P and N from 1stRound)
#calculate Coefficient of variation (per Rep)
data<-diam_data%>%
  group_by(Site,Transect,Location,Rep)%>%
  #calculate Coefficient of Variation (CV= sd/mean)
  mutate(CV_Length = sd(Length_mm, na.rm = TRUE) / mean(Length_mm, na.rm = TRUE))%>%
  mutate(Site = sub("^(S)( *)", "\\2", Site),
         Transect = sub("^(T)( *)", "\\2", Transect),
         Location = sub("^(L)( *)", "\\2", Location))%>%
  left_join(secondround_data%>%
              mutate(Site=as.character(Site),
                     Transect=as.character(Transect)))%>%
  left_join(ph_data%>%  
              mutate(Site = sub("^(S)( *)", "\\2", Site),
                     Transect = sub("^(T)( *)", "\\2", Transect)))%>% 
  left_join(Ortho_P%>%  
              mutate(Site = sub("^(S)( *)", "\\2", Site),
                     Transect = sub("^(T)( *)", "\\2", Transect),
                     Location = sub("^(T)( *)", "\\2", Location)),
            by = c("Site", "Transect", "Location"))%>%
  left_join(myco_data, by = c("Site", "Transect"))%>%
  left_join(nutri_sol, by = c("Site", "Transect"))%>%
  left_join(biomass, by = c("Site", "Transect","Location"))



data$Fire.Interval<- as.factor(data$Fire.Interval)
data$Fire.Severity<- as.factor(data$Fire.Severity)
write_xlsx(data, "raw/alldataforlength.xlsx")
