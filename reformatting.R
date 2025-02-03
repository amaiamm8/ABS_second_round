library(dplyr)
library(openxlsx)

# Path to the Excel file
file_path <- "Bags copy.xlsx"

# Load the workbook
wb <- loadWorkbook(file_path)

# Write the modified data to a specific sheet (e.g., overwrite "Sheet1")
writeData(wb, sheet = "Sheet1", x = DNA, startCol = 1, startRow = 1)

# Save the workbook, overwriting the original file
saveWorkbook(wb, file = file_path, overwrite = TRUE)
DNA <- read.xlsx('Bags copy.xlsx')
CN <- read.xlsx('CN_Amaia.xlsx')
Total_P <- read.xlsx('TotalP_Amaia.xlsx')

CN <- mutate(TotalP_weight= weight_mg[Total_P])
Total_P
DNA<- DNA %>%
  mutate(Name=  sub(".*T","T", Name))%>%
  mutate(Sample= paste0(Site, Name))%>%
  reframe(Sample,DNA_ID, weight_mg,Sample_type,Samplename_ITS, Samplename_SSU,	qbit_DNA_conc_ngul )
  
  group_by(Name)%>%
  reframe(Sample,DNA_ID, weight_mg,Sample_type,Samplename_ITS, Samplename_SSU,	qbit_DNA_conc_ngul )%>%
  distinct()
 
   group_by(DNA_ID)%>%
    reframe(DNA_ID,weight_mg,Sample_type,Samplename_ITS, Samplename_SSU,	qbit_DNA_conc_ngul )%>%
    arrange(DNA_ID)%>%
    distinct()
  
  
  
  mutate(Name = paste0(Site, Transect, Location))%>%
  mutate(Sample_type = "Hyphae")%>%
  group_by(Name, Tube_ID)%>%
  reframe(Name, Tube_ID,mass_extracted,Sample_type, DNA_ID)%>%
  arrange(Tube_ID)%>%
  distinct()
  
write.xlsx(DNA, "Bags copy.xlsx", rowNames = FALSE)

#formating the mosaic (microscope) file

library(readxl)
library(dplyr)
install.packages("openxlsx")
library(openxlsx)
diam <- read_excel("mosaic/hyphal length.xlsx", "Sheet1")
trait_scoring <- read_excel("hyphal length.xlsx", "Sheet2")


diam <- diam %>%
  mutate(Slide = sub("^(slide)( *)(\\d+)-.*", "\\3", Slide_number))%>%
  mutate(Rep = sub("^(slide)( *)(\\d+-)( *)(\\d+)", "\\5", Slide_number))  
diam$Slide_number<- NULL


rawdata <- read_excel("Thesis/Rawdata/Labbook.xlsx", "Sheet1")
names<- rawdata[c("Tube_ID", "Site", "Transect", "Location")]

diam$Site <- rawdata$Site[match(diam$Slide, rawdata$Tube_ID)]
diam$Transect <- rawdata$Transect[match(diam$Slide, rawdata$Tube_ID)]
diam$Location <- rawdata$Location[match(diam$Slide, rawdata$Tube_ID)]
diam<- diam[c("Site", "Transect", "Location", "Slide", "Rep","Length_mm")]

write.xlsx(diam, file = "hyphal length.xlsx", sheetName = "Sheet1", append = FALSE)
