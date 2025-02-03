setwd("~/Desktop/Thesis/Rawdata")
library(readxl)
library(dplyr)
library(openxlsx)
diam <- read_excel("Rawdata/Updated hyphal length.xlsx", "Sheet1")
#average diameter measurements
diam <- diam %>%
  group_by(Slide) %>%
  mutate(
    Min_Value = min(Length_mm),
    Max_Value = max(Length_mm),
    Avg = mean(Length_mm))%>%
  ungroup()

#lowest Average
low_index <- which.min(diam$Avg)  # Find the index of the minimum value
low_slide <- diam$Slide[low_index] 
low_slide

#highest Average
hi_index <- which.max(diam$Avg) 
hi_slide <- diam$Slide[hi_index] 
hi_slide

#lowest value
min_index <- which.min(diam$Length_mm)
min_slide <- diam$Slide[min_index] 
min_slide

#highest value
max_index <- which.max(diam$Length_mm)
max_slide <- diam$Slide[max_index] 
max_slide

# Extract the maximum and minimum standard deviation
diam <- diam %>%
  group_by(Slide) %>%
  mutate(
    StDev = sd(Length_mm, na.rm = TRUE))%>%
  ungroup()

max_stdev_slide <-diam$Slide[which.max(diam$StDev)]
max_stdev_slide
min_stdev_slide <- diam$Slide[which.min(diam$StDev)]
min_stdev_slide


#counts of y in traits
trait_scoring <- read_excel("mosaic/hyphal length.xlsx", "Sheet2")
sum(trait_scoring$Septate == "y")
sum(trait_scoring$Tryp_Stain == "y")
sum(trait_scoring$Irregular_node == "y")
sum(trait_scoring$Rhizomorph == "y")
sum(trait_scoring$Spores == "y")

#save back to the existing excel
wb <- loadWorkbook("Rawdata/Updated hyphal length.xlsx")

# Write data to a specific sheet
writeData(wb, sheet = "Sheet1", x = diam)  
# Save the updated workbook
saveWorkbook(wb, "Rawdata/Updated hyphal length.xlsx", overwrite = TRUE)

