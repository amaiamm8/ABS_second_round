library(readxl)
library(dplyr)
library(stringr)

rawdat<- read_excel("raw/Labbook.xlsx")
rawdat$Bag_w<- as.numeric(rawdat$Bag_w)

biomass<- read_excel("raw/Labbook.xlsx", "Hyphae processing")
#new weight adding reps -both for bag_w and dry_w

formuladata <- rawdat %>%
  group_by(gr = cumsum(Rep == "A")) %>%  # Group every pair of A/B
  mutate(
    harvest_w = ifelse(Rep == "A", sum(Bag_w, na.rm = TRUE), NA_real_),  # Sum of Bag_w for each group
    dry_w = ifelse(Rep == "A", sum(Dry_w, na.rm = TRUE), NA_real_)  # Sum of Dry_w for each group
  ) %>%
  ungroup() %>%
  select(-gr)

#if notes "D-*" or "no bag retrieved" remove from new dataset 
formuladata <- formuladata %>%
  filter(!is.na(Bag_w))%>%
  filter(is.na(Notes) | Notes == "" | !str_detect(Notes, "^D-"))

#add up these per halftransect 
#group half transects
formuladata <- formuladata %>%
  mutate(HalfT = case_when(
    as.numeric(sub("L", "", Location)) < 20 ~ "A",  # Less than L20
    as.numeric(sub("L", "", Location)) > 30 ~ "B",  # Greater than L30
  ))%>%
  mutate(Name= paste0(Site, Transect, HalfT))


newdat <- formuladata %>%
  group_by(Name) %>%  # Group by the 'Name' column
  mutate(
    harvest_w = sum(harvest_w, na.rm = TRUE),  # Sum of harvest_w for each Name
    dry_w = sum(dry_w, na.rm = TRUE)           # Sum of dry_w for each Name
  ) %>%
  ungroup()  # Remove grouping
newdat <- newdat %>%
  distinct(Name, harvest_w, dry_w, .keep_all = TRUE)%>%
  select(Name, harvest_w, dry_w)

#add to the new dataset the columns with the total weight of the hyphae
newdat<- cbind(newdat, biomass)
newdat <- newdat %>%
  select(Name,ID, harvest_w, dry_w, total_W)


single.lm<-lm(total_W ~  dry_w, newdat)# building a model
car::Anova(single.lm) 
summary(single.lm)

single2.lm<-lm(total_W ~  harvest_w, newdat)# building a model
car::Anova(single2.lm) 
summary(single2.lm)

compound.lm<-lm(total_W ~  harvest_w+dry_w, newdat)# building a model
car::Anova(compound.lm) 
summary(compound.lm)

cor(newdat$total_W, newdat$harvest_w, method = "pearson")
cor(newdat$total_W, newdat$harvest_w, method = "spearman")

