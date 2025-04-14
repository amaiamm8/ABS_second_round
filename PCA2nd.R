library(vegan)
library(dplyr)
library(readxl)
library(tidyverse)
library(ggrepel)
library(ggplot2)
# Load your data
all <- read_excel("raw/alldata.xlsx")
all_clean <-all%>%
  select(Site, Transect,Fire.Interval,Fire.Severity, Tree.Basal.Area_m2:Dead.Tree.Canopy.Cover_perc,NH4:Nitrogen,Ortho_P_mg_kg, perc_myco_host_freq, mean_ammonia, mean_nitrate)%>% 
  filter(complete.cases(.))

all_numeric <- all_clean %>% 
  select(where(is.numeric)) 

explanatory.pca <- rda(all_numeric, scale = TRUE)
# Plot the PCA
plot(explanatory.pca)
summary(explanatory.pca, display = NULL)
scrs <- scores(explanatory.pca, tidy=TRUE)
scrs_spp <- scrs %>% filter(score=='species')
scrs_site <- scrs %>% filter(score=='sites')
# eigenvalues are stored in the `CA` element of the result (a list)
scrs.eig <- explanatory.pca[['CA']]$eig
# convert these to relative percent
scrs.pct <- 100 * scrs.eig/sum(scrs.eig)


# Apply the new labels to your scrs_spp dataframe

scrs_spp <- scrs_spp %>%
  mutate(label = dplyr::recode(as.character(label),
                               "Tree.Basal.Area_m2" = "Tree Basal Area",
                               "All.Tree.Canopy.Cover_perc" = "Total Canopy Cover",
                               "Live.Tree.Canopy.Cover_perc" = "Live Canopy Cover",
                               "Mean.Litter.Depth_mm" = "Litter Depth",
                               "perc_myco_host_freq" = "% Mycorrhizal Hosts",
                               "mean_nitrate" = "Mean Nitrate (1st Round)",
                               "NH4" = "Ammonium",
                               "NO3" = "Nitrate",
                               "Carbon" = "Carbon",
                               "Nitrogen" = "Nitrogen",
                               "Total.P"= "Total P",
                               "Dead.Tree.Canopy.Cover_perc"= "Dead Canopy Cover",
                               "Herb.Shrub.Connected_0.200cm_perc"= "Herb Shrub Connected",
                               "Shrub.Connected_50.200cm_perc"= "Shrub Connected",
                               "Mean.Max.Shrub.Height_cm"= "Mean Maximum Shrub Height",
                               "Herb.Cover_0.50cm_perc"="Herb Cover",
                               "Shrub.Cover_50.200cm_perc"= "Shrub Cover",
                               "Litter.Cover_20mm_perc" = "Litter Cover",
                               "Ortho_P_mg_kg"="Ortho P (2nd Round)" ))

# Calculate the centroids by taking the average PCA scores for each site
centroids <- scrs %>% 
  filter(score == 'sites') %>%  # Make sure we're looking at site scores
  mutate(Site = all_clean$Site) %>%  # Add the Site names from original data
  group_by(Site) %>%  # Group by Site
  summarize(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")  # Calculate the mean PC1 and PC2 for each site

# Now join with the original data to add Fire.Interval and other variables
combined_data <- left_join(centroids, all_clean, by = "Site")

# Create the plot using the centroids

p<-combined_data%>%
  ggplot( aes(x=PC1, y=PC2, label=Site )) + 
  geom_point(aes( colour= Fire.Interval), size=3, stroke = 4)+
  geom_segment(data=scrs_spp%>% filter(abs(PC1) > 1 | abs(PC2) > 1),
               inherit.aes = FALSE,
               aes(x=0,y=0, xend=PC1, yend=PC2, group=label),
               arrow = arrow(type = "closed",length=unit(3,'mm')),
               color= 'black') +
  geom_text_repel(data=scrs_spp%>% filter(abs(PC1) > 1 | abs(PC2) > 1),#use this filter to select most important factos
            inherit.aes = FALSE,
            aes(x=PC1, y=PC2, label=label),
            colour='black',size=3, fontface="bold")+ 
  xlab(paste('PC1 (', round(scrs.pct[1], 0), '%)', sep='')) + 
  ylab(paste('PC2 (', round(scrs.pct[2], 0), '%)', sep='')) + 
  theme_minimal() + labs(color = "Fire Interval")

# Display the plot
print(p)

a<-combined_data%>%
  ggplot( aes(x=PC1, y=PC2, label=Site )) + 
  geom_point(aes( colour= Fire.Interval,shape= Fire.Severity), size=3, stroke = 4)+
  geom_segment(data=scrs_spp%>% filter(abs(PC1) > 1 | abs(PC2) > 1),
               inherit.aes = FALSE,
               aes(x=0,y=0, xend=PC1, yend=PC2, group=label),
               arrow = arrow(type = "closed",length=unit(3,'mm')),
               color= 'black') +
  geom_text(data=scrs_spp%>% filter(abs(PC1) > 1 | abs(PC2) > 1),#use this filter to select most important factos
            inherit.aes = FALSE,
            aes(x=PC1, y=PC2, label=label),
            colour='black',size=3, fontface="bold")+ 
  xlab(paste('PC1 (', round(scrs.pct[1], 0), '%)', sep='')) + 
  ylab(paste('PC2 (', round(scrs.pct[2], 0), '%)', sep='')) + 
  theme_minimal() + labs(color = "Fire Interval", shape = "Fire Severity")
a


a <- combined_data %>%
  ggplot(aes(x = PC1, y = PC2, label = Site)) + 
  geom_point(aes(colour = Fire.Interval, shape = Fire.Severity), size = 3, stroke = 4) +  # Points
  geom_segment(data = scrs_spp %>% filter(abs(PC1) > 1 | abs(PC2) > 1),
               inherit.aes = FALSE,
               aes(x = 0, y = 0, xend = PC1, yend = PC2, group = label),
               arrow = arrow(type = "closed", length = unit(3, 'mm')),
               color = 'black') +  # Vectors for important factors
  geom_text_repel(data = scrs_spp %>% filter(abs(PC1) > 1 | abs(PC2) > 1),  # Only label important factors
                  inherit.aes = FALSE,
                  aes(x = PC1, y = PC2, label = label),
                  colour = 'black', size = 3, fontface = "bold", box.padding = 0.5, max.overlaps = 10) +  # Repel labels
  xlab(paste('PC1 (', round(scrs.pct[1], 0), '%)', sep = '')) + 
  ylab(paste('PC2 (', round(scrs.pct[2], 0), '%)', sep = '')) + 
  theme_minimal() + 
  labs(color = "Fire Interval", shape = "Fire Severity")

# Display the plot
print(a)

#checking the scores for ammonia- was filtered out based on the little pc contribution
scrs_spp %>% filter(str_detect(label, "mean_ammonia"))



