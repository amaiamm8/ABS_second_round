library(readxl)
library(dplyr)
library(stringr)

rawdat<- read_excel("raw/Labbook.xlsx")
rawdat$Bag_w<- as.numeric(rawdat$Bag_w)

biomass<- read_excel("raw/Labbook.xlsx", "Indiv weights")[,-4]
biomass<- biomass%>%
  mutate(Tube_ID= as.character(Tube_ID)) %>%
  rename(hyphal_weight = weight)

#new weight adding reps -both for bag_w and dry_w
formuladata <- rawdat %>%
  group_by(gr = cumsum(Rep == "A")) %>%  # Group every pair of A/B
  mutate(
    harvest_w = ifelse(Rep == "A", sum(Bag_w, na.rm = TRUE), NA_real_),  # Sum of Bag_w for each group
    dry_w = ifelse(Rep == "A", sum(Dry_w, na.rm = TRUE), NA_real_)  # Sum of Dry_w for each group
  ) %>%
  ungroup() %>%
  select(-gr)

formuladata<- full_join(formuladata, biomass, by= "Tube_ID")

#if notes "D-*" or "no bag retrieved" remove from new dataset 
formuladata <- formuladata %>%
  filter(!is.na(harvest_w))%>%
  filter(is.na(Notes.x) | Notes.x == "" | !str_detect(Notes.x, "^D-"))


formuladata <- formuladata %>%
  distinct(Site, Transect, Location, .keep_all = TRUE)%>%
  select(Site, Transect, Location,Tube_ID, harvest_w, dry_w, hyphal_weight)


#add to the new dataset the columns with the total weight of the hyphae

cor(formuladata$hyphal_weight, formuladata$harvest_w, method = "pearson")
cor(formuladata$hyphal_weight, formuladata$harvest_w, method = "spearman")

single.lm<-lm(hyphal_weight ~  dry_w, formuladata)# building a model
car::Anova(single.lm) 
summary(single.lm)

single2.lm<-lm(hyphal_weight ~  harvest_w, formuladata)# building a model
car::Anova(single2.lm) 
summary(single2.lm)

compound.lm<-lm(hyphal_weight ~  harvest_w+dry_w, formuladata)# building a model
car::Anova(compound.lm) 
summary(compound.lm)






#TO DO THE CALCULATIONS PER HALF TRANSECTS
#group half transects
formuladata <- formuladata %>%
  mutate(HalfT = case_when(
    as.numeric(sub("L", "", Location)) < 20 ~ "A",  # Less than L20
    as.numeric(sub("L", "", Location)) > 30 ~ "B",  # Greater than L30
  ))%>%
  mutate(Name= paste0(Site, Transect, HalfT))


newdat <- formuladata %>%
  group_by(Site, Transect, Location) %>%  # Group by the 'Name' column
  mutate(
    harvest_w = sum(harvest_w, na.rm = TRUE),  # Sum of harvest_w for each Name
    dry_w = sum(dry_w, na.rm = TRUE)           # Sum of dry_w for each Name
  ) %>%
  ungroup()  # Remove grouping
newdat <- newdat %>%
  distinct(Site, Transect, Location, harvest_w, dry_w, .keep_all = TRUE)%>%
  select(Site, Transect, Location, harvest_w, dry_w, Tube_ID)