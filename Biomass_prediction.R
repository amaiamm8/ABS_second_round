library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(writexl)
library(openxlsx)
rawdat<- read_excel("raw/Labbook.xlsx")
rawdat$Bag_w<- as.numeric(rawdat$Bag_w)
dates<- read_excel("raw/Site.Info.2nd.Round.xlsx")
dates<- dates%>%
  select("Site", "2nd_Bag_Install")%>%
  mutate(Site = sub("^ABS0*", "", Site))%>%
  dplyr::rename(Second_incubation='2nd_Bag_Install')


biomass<- read_excel("raw/Labbook.xlsx", "Indiv weights")[,-4]
biomass<- biomass%>%
  mutate(Tube_ID= as.character(Tube_ID)) %>%
  dplyr::rename(hyphal_weight = weight)


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
  dplyr::rename(Harvest_date= 'Harvest date')%>%
  select(Harvest_date,Site, Transect, Location,Tube_ID, harvest_w, dry_w, hyphal_weight, Notes.x)%>% #keep the notes col to check which bags were damaged
  mutate(bag_damage= (undamaged_bag_wt-dry_w)/ (undamaged_bag_wt), #use dry weight not harvest_w
         correction= bag_damage +1  ,
         corrected_weight= correction* hyphal_weight)


# Sols stupid math that were were trying to avoid, but now I think we have to

initial_bag_w<-15.1257*2#this is calculated from average bag weight of undeployed bag fresh_weight

#here i calc what the harvest_w should have been if bags were undamaged, but I calculate an average per transect
#this accounts for variations in moisture content and assumes that all bags in the same transect have similar moisture contents
undamaged_bag_w<-formuladata%>%
  filter(!str_detect(Notes.x, "^D-*") | is.na(Notes.x)) %>%  # Keep rows with missing notes or no 
  #remove missing or damaged bags # you need to adapt this to your own data
  #remove samples with only one bag,locations over 20g because it seems like the weight where the bags arent damaged
  #Check this number and see if you agree!
  filter(harvest_w > 20)%>%
  group_by(Site, Transect) %>%
  summarise(mean_undam_resin_w = mean(harvest_w, na.rm = TRUE))
  
#here I am calculating the amount of biomass of myc from each location accounting for variation in soil moisture from each site
corrected_myc<-left_join(undamaged_bag_w,formuladata)%>%
  #(initial bag weight saturated * amount Resin collected per Location)/ avg weight of resins per location at each transect
  mutate(resin_mass_est = (initial_bag_w * harvest_w) / mean_undam_resin_w, # this is the estimate of what the bag would weigh if it had the same intial water content as when we deployed the bag
         #calc ratio of hyphae per gram of resin for each location
         hyph_w_per_bead= (hyphal_weight / resin_mass_est),
         #multiply estimated amount of hyph per bead by undamaged bag weight
         hyph_w_est_yield= hyph_w_per_bead*initial_bag_w)%>%
  mutate(Site = sub("^S", "", Site))%>%
  mutate(Transect = sub("^T", "", Transect))

# Log transformations and biomass calculations


#better to avoid many to many joins, it creates issues later!!!! Always check the rows of your df!
Bag_Site <-left_join(corrected_myc, dates%>%distinct())%>%
    mutate(Days_Installed= as.numeric(Harvest_date- Second_incubation)) %>%
    mutate(
      log10_hyph_w_est_yield = log10(hyph_w_est_yield),
      Biomass_day = hyph_w_est_yield / Days_Installed,
      log10_biomass_day = log10(Biomass_day),
      biomass_g_ha_day = Biomass_day * (1e+06 / 15),  # Convert to g/ha/day see lines below
    )
  #g/hectare:
  #10,000m^2 ×0.1m= 1,000m^3 
  #1,000m^3 x (x mg /15cm^3) x (1g/1000mg) x 1000kg/ton x 1000g/kg= g/ha
  #(1e+06/15)

site_data<- read.csv("raw/site_data_Amaia.csv")[,-1]
#filtering the data to my sites
site_data <- site_data %>%
  filter(Site %in% c(7, 8 , 10, 12 , 26, 34))%>%
  mutate(Site=as.character(Site),
         Transect=as.character(Transect))
Bag_Site<- left_join(Bag_Site, site_data)

write_xlsx(Bag_Site, "raw/biomass.xlsx")

