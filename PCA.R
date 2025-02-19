library(vegan)  # loads the 'vegan' library
library(dplyr)
library(readxl)
library(tidyverse)
library(readxl)
# Load your data
alldata <- read_excel("raw/alldata.xlsx")
alldata <- as.data.frame(alldata)


veg_nute_join <-alldata%>%
  select(2,3,12:29,39)

Veg_Nute.pca <- rda(veg_nute_join~ FESM_Fire.Severity.Category + fire.frequency, data=alldata, scale=TRUE)




# Check the structure of the data
str(alldata)

# Check the variance of each column (optional)
sort(apply(alldata, 2, var))

# Run the PCA with the correct formula
chem.pca <- rda(Length_mm ~ FESM_Fire.Severity.Category + fire.frequency + NO3 + NH4 + Ortho_P, 
                data = alldata, scale=TRUE)



# Plot the PCA
plot(chem.pca)

# Summary of the PCA
summary(chem.pca, display = NULL)

alldata<-read_excel("raw/alldata.xlsx")

alldata <- as.data.frame(alldata)
str(alldata)
sort(apply(alldata, 2, var))
chem.pca <- rda(alldata$Length_mm ~ alldata$FESM_Fire.Severity.Category+alldata$fire.frequency+ alldata$NO3+ alldata$NH4+alldata$Ortho_P, data = alldata)
plot(chem.pca)
summary(chem.pca, display=NULL)

plot(chem.pca)

# set graphics window up to contain two plots
par(mfrow=c(1,2))

# produce screeplot ('main=NULL' for a tidier plot)
screeplot(chem.pca, main=NULL)
screeplot(chem.pca, main=NULL, bstick=T, type='l')


# load libraries
library(FactoMineR)
library(factoextra)
## Loading required package: ggplot2
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
# perform PCA on data in `varechem`
# in addition to the results object, PCA() produces ordinations showing 
# individual ('site' in `vegan`) and variables ('species' in `vegan`)
chem.pca.fmr <- PCA(alldata)
fviz_pca_biplot(chem.pca.fmr)
fviz_pca_biplot(chem.pca.fmr, select.var=list(contrib=5))
# produce a biplot including selected variables
fviz_pca_biplot(chem.pca.fmr, select.var=list(name=c( 'NH4','Ortho_P_mg_kg', 'fire.frequency', 'FESM_Fire.Severity.Category')), 
                # remove individual labels and customise points
                col.ind='red', label='var', 
                # modify title
                title='PCA -- explanatory variables')


# from `vegan` output
scrs.ind <- scores(chem.pca, display='sites')
scrs.var <- scores(chem.pca, display='species')

# from `FactoMineR` output (only want the list element containing coordinates)
scrs.ind <- get_pca_ind(chem.pca.fmr)[['coord']]
scrs.var <- get_pca_var(chem.pca.fmr)[['coord']]

## from `vegan` output 
# eigenvalues are stored in the `CA` element of the result (a list)
scrs.eig <- chem.pca[['CA']]$eig
# convert these to relative percent
scrs.pct <- 100 * scrs.eig/sum(scrs.eig)

## from `FactoMineR` output 
# `factoextra` has a function for extracting and calculating from eigenvalues
scrs.eig <- get_eig(chem.pca.fmr)
# get the column displaying relative percent
scrs.pct <- scrs.eig[, 'variance.percent']
# just keep the column displaying actual eigenvalues
scrs.eig <- scrs.eig[, 'eigenvalue']

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


# specify object and columns with site coordinates, after converting to data.frame
ggplot(as.data.frame(scrs.ind), aes(x=Dim.1, y=Dim.2)) + 
  # specify scatterplot geom and characteristics for points
  geom_point(colour='grey', shape=16, size=2) + 
  # set axis limits (using trial and error)
  xlim(c(-2, 2)) + ylim(c(-2, 2)) + 
  # customise axis labels using eigenvalue contributions (round to whole numbers)
  xlab(paste('PC1 (', round(scrs.pct[1], 0), '%)', sep='')) + 
  ylab(paste('PC2 (', round(scrs.pct[2], 0), '%)', sep='')) + 
  # add label layer for variable coordinates (in new data.frame)
  geom_text(data=as.data.frame(scrs.var), 
            mapping=aes(x=Dim.1, y=Dim.2, label=rownames(scrs.var)), 
            colour='blue', size=5) + 
  # change theme and save as object in workspace
  theme_bw() -> p


p
plotly::ggplotly(p)

get_pca_var(chem.pca.fmr)$contrib

# get contributions of variables across both of the first two axes
facto_summarize(chem.pca.fmr, element='var', result='contrib', axes=1:2)

# multiply result by 100 to get percentages
# the choices argument controls which axes are shown
100 * scores(chem.pca, choices=1:4, display='species', scaling=0)^2

# extract contributions for first and second axes and multiply each by axis eigenvalue
contrib1 <- scores(chem.pca, choices=1, display='species', scaling=0)^2 * scrs.eig[1] 
contrib2 <- scores(chem.pca, choices=2, display='species', scaling=0)^2 * scrs.eig[2]
contrib <- contrib1 + contrib2

100 * contrib / sum(contrib)

get_pca_ind(chem.pca.fmr)$contrib[, 1:2]
