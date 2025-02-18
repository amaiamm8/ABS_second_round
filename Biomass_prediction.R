library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(writexl)
library(openxlsx)
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

#Formula with known dry weight of bags (6.64 g with SE of +- 0.045):
# (harvest_wt- undamaged_bag_wt)/ (undamaged_bag_wt)  * hyphal biomass collected

formuladata <- formuladata %>%
  distinct(Site, Transect, Location, .keep_all = TRUE)%>%
  select(Site, Transect, Location,Tube_ID, harvest_w, dry_w, hyphal_weight)%>%
  mutate(corrected_weight= (harvest_w- 6.64*2)/ (6.64*2)  * hyphal_weight)


write_xlsx(formuladata, "raw/biomass.xlsx")





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


library(mgcv)
model_size <- gam(hyphal_weight ~ harvest_w, data = formuladata)
# Fit a GLM with a Gaussian distribution (normal distribution)
model_glm <- glm(hyphal_weight ~ harvest_w,
                 family = gaussian(link = "identity"), 
                 data = formuladata)

# Check model summary
summary(model_glm)
plot(model_glm$residuals)

# View the model summary
summary(model_size)
plot(model_size$residuals)
r2(model_glm)


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



#Graphs
ggplot (formuladata , aes(x= Site , y= hyphal_weight )) +
  # note the '+', which indicates that additional instructions are coming
  stat_summary ( geom ='bar', fun=mean , na.rm = TRUE ) +
  # adds the barplot layer , calculating the mean for each group
  stat_summary ( geom ='errorbar', fun.data = mean_se , width =0.25 , na.rm =TRUE)

