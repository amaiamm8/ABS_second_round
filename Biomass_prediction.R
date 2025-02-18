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

#this just makes it easier for me, your way works too, but then I dont worry about () as much
undamaged_bag_wt<-6.64*2

formuladata <- formuladata %>%
  distinct(Site, Transect, Location, .keep_all = TRUE)%>%
  select(Site, Transect, Location,Tube_ID, harvest_w, dry_w, hyphal_weight, Notes.x)%>% #keep the notes col to check which bags were damaged
  mutate(bag_damage= (undamaged_bag_wt-dry_w)/ (undamaged_bag_wt), #use dry weight not harvest_w
         correction= bag_damage +1  ,
         corrected_weight= correction* hyphal_weight)


# Sols stupid math that were were trying to avoid, but now I think we have to

initial_bag_w<-15.1257*2#this is calculated from average bag weight of undeployed bag fresh_weight

#here i calc what the harvest_w should have been if bags were undamaged, but I calculate an average per transect
#this accounts for variations in moisture content and assumes that all bags in the same transect have similar moisture contents
undamaged_bag_w<-formuladata%>%
  #remove missing or damaged bags # you need to adapt this to your own data
  #remove samples with only one bag,locations over 20g because it seems like the weight where the bags arent damaged
  #Check this number and see if you agree!
  filter(harvest_w > 20) %>%
  group_by(Site, Transect) %>%
  summarise(mean_undam_resin_w = mean(harvest_w, na.rm = TRUE))


#here I am calculating the amount of biomass of myc from each location accounting for variation in soil moisture from each site
  corrected_myc<-left_join(undamaged_bag_w,formuladata)%>%
  #(initial bag weight saturated * amount Resin collected per Location)/ avg weight of resins per location at each transect
  mutate(resin_mass_est = (initial_bag_w * harvest_w) / mean_undam_resin_w, # this is the estimate of what the bag would weigh if it had the same intial water content as when we deployed the bag
         #calc ratio of hyphae per gram of resin for each location
         hyph_w_per_bead= (hyphal_weight / resin_mass_est),
         #multiply estimated amount of hyph per bead by undamaged bag weight
         hyph_w_est_yield= hyph_w_per_bead*initial_bag_w)
  
# Log transformations and biomass calculations
  Bag_Site <- left_join(corrected_myc, ) %>%
    mutate(
      log10_hyph_w_est_yield = log10(hyph_w_est_yield),
      Biomass_day = hyph_w_est_yield / Days_Installed,
      log10_biomass_day = log10(Biomass_day),
      biomass_g_ha_day = Biomass_day * (1e+06 / 15),  # Convert to g/ha/day see lines below
    )
  #g/hectare:
  #10,000 m^2 ×0.1 m= 1,000 m^3 
  #1,000m^3 x (x mg /15cm^3) x (1g/1000mg) x 1000kg/ton x 1000g/kg= g/ha
  #(1e+06/15)
  

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

