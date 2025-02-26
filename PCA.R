library(vegan)  # loads the 'vegan' library
library(dplyr)
library(readxl)
library(tidyverse)
library(readxl)
library(ggrepel)
# Load your data
#This dataframe is too long! You did some weird joins to make it, maybe I am wrong, but I would be you had at least one to many 'many-to-many' join here
#you should have I thnk 72 rows at most 
alldata <- read_excel("raw/alldata.xlsx")
alldata <- as.data.frame(alldata)



veg_nute_join <-alldata%>%
  #use coll names instead of numbers, cols change often and then you get weird stuff
  select(Tree.Basal.Area_m2:Dead.Tree.Canopy.Cover_perc,NH4:Nitrogen,Ortho_P_mg_kg)
# Filter out rows with NA values in any column of veg_nute_join
veg_nute_join_clean <- veg_nute_join %>%
  filter(!apply(veg_nute_join, 1, function(x) any(is.na(x))))
#you need to know what rows you removed from the analysis and why!- because no total P in these rows
temp<-anti_join(veg_nute_join, veg_nute_join_clean)

#why are you doing this step?
alldata_clean <- alldata %>%
  filter(NH4 %in% veg_nute_join_clean$NH4)

#You can use categorical values on the right, but not on the left BELOW IS an RDA
Veg_Nute.rda <- rda(veg_nute_join_clean~ Fire.Severity + Fire.Interval, data=alldata_clean, scale=TRUE)
#This is the PCA
Veg_Nute.pca <- rda(veg_nute_join_clean, data=alldata_clean, scale=TRUE)


# Plot the PCA
plot(Veg_Nute.pca)

# Summary of the PCA
summary(Veg_Nute.pca, display = NULL)


#extract the scores from your pca
scrs <- scores(Veg_Nute.pca, tidy=TRUE)
scrs_spp <- scrs %>% filter(score=='species')
scrs_site <- scrs %>% filter(score=='sites')
scrs_cent <- scrs %>% filter(score=='centroids')
scrs_biplot <- scrs %>% filter(score=='biplot')

# eigenvalues are stored in the `CA` element of the result (a list)
scrs.eig <- Veg_Nute.pca[['CA']]$eig
# convert these to relative percent
scrs.pct <- 100 * scrs.eig/sum(scrs.eig)

p <- ggplot(alldata_clean, aes(x = PC1, y = PC2, label = Site)) + 
  geom_point(aes(color = Fire.Interval, shape = Fire.Severity), size = 3, stroke = 4) +
  theme_minimal()

# first plot - site scores along with centroids for each group
p<-cbind(alldata_clean,scrs_site)%>%
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


p

# first plot - site scores along with centroids for each group
a<-cbind(alldata_clean,scrs_site)%>%
  ggplot( aes(x=PC1, y=PC2, colour=Fire.Interval, label=Site )) + 
  geom_point(size=5.5)+ 
  geom_segment(data=scrs_spp%>% filter(abs(PC1) > 0.1 | abs(PC2) > 0.1),
               inherit.aes = FALSE,
               aes(x=0,y=0, xend=PC1, yend=PC2, group=label),
               arrow = arrow(type = "closed",length=unit(3,'mm')),
               color= 'black') +
  geom_text_repel(data=scrs_spp%>% filter(abs(PC1) > 0.1 | abs(PC2) > 0.1),#use this filter to select most important factos
                  inherit.aes = FALSE,
                  aes(x=PC1, y=PC2, label=label),
                  colour='black',size=3, fontface="bold")+ 
  xlab(paste('PC1 (', round(scrs.pct[1], 0), '%)', sep='')) + 
  ylab(paste('PC2 (', round(scrs.pct[2], 0), '%)', sep='')) + 
  theme_minimal()

a

# set graphics window up to contain two plots
par(mfrow=c(1,2))

# produce screeplot ('main=NULL' for a tidier plot)
screeplot(Veg_Nute.pca, main=NULL)
screeplot(Veg_Nute.pca, main=NULL, bstick=T, type='l')


# load libraries
library(FactoMineR)
library(factoextra)
## Loading required package: ggplot2
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
# perform PCA on data in `varechem`
# in addition to the results object, PCA() produces ordinations showing 
# individual ('site' in `vegan`) and variables ('species' in `vegan`)



# from `vegan` output
scrs.ind <- scores(Veg_Nute.pca, display='sites')
scrs.var <- scores(Veg_Nute.pca, display='species')

# from `FactoMineR` output (only want the list element containing coordinates)
scrs.ind <- get_pca_ind(chem.pca.fmr)[['coord']]
scrs.var <- get_pca_var(chem.pca.fmr)[['coord']]

## from `vegan` output 
# eigenvalues are stored in the `CA` element of the result (a list)
scrs.eig <- Veg_Nute.pca[['CA']]$eig
# convert these to relative percent
scrs.pct <- 100 * scrs.eig/sum(scrs.eig)

#######
# establish the plot showing site coordinates
plot(scrs.ind, 
     # set axis limits (using trial and error)
     xlim=c(-2, 2), ylim=c(-2, 2), 
     # customise graphical parameters for points
     pch=16, col='grey', cex=1,  
     # customise axis labels using eigenvalue contributions (round to whole numbers)
     xlab=paste('PC1 (', round(scrs.pct[1], 0), '%)', sep=''), 
     ylab=paste('PC2 (', round(scrs.pct[2], 0), '%)', sep=''), 
     las=1  # all tick labels horizontal
)

# add labels for variable coordinates
text(scrs.var, labels=rownames(scrs.var), 
     # customise graphical parameters for labels
     col='blue', cex=0.5)


# add horizontal and vertical lines through the origin
abline(h=0, v=0, lty='dotted')
# Check the column names in scrs.ind
colnames(as.data.frame(scrs.ind))


# specify object and columns with site coordinates, after converting to data.frame
ggplot(as.data.frame(scrs.ind), aes(x=PC1, y=PC2)) + 
  # specify scatterplot geom and characteristics for points
  geom_point(colour='grey', shape=16, size=2) + 
  # set axis limits (using trial and error)
  xlim(c(-2, 2)) + ylim(c(-2, 2)) + 
  # customise axis labels using eigenvalue contributions (round to whole numbers)
  xlab(paste('PC1 (', round(scrs.pct[1], 0), '%)', sep='')) + 
  ylab(paste('PC2 (', round(scrs.pct[2], 0), '%)', sep='')) + 
  # add label layer for variable coordinates (in new data.frame)
  geom_text(data=as.data.frame(scrs.var), 
            mapping=aes(x=PC1, y=PC2, label=rownames(scrs.var)), 
            colour='blue', size=5) + 
  # change theme and save as object in workspace
  theme_bw() -> p


p
plotly::ggplotly(p)

get_pca_var()$contrib

# multiply result by 100 to get percentages
# the choices argument controls which axes are shown
100 * scores(Veg_Nute.pca, choices=1:4, display='species', scaling=0)^2

# extract contributions for first and second axes and multiply each by axis eigenvalue
contrib1 <- scores(Veg_Nute.pca, choices=1, display='species', scaling=0)^2 * scrs.eig[1] 
contrib2 <- scores(Veg_Nute.pca, choices=2, display='species', scaling=0)^2 * scrs.eig[2]
contrib <- contrib1 + contrib2

100 * contrib / sum(contrib)

