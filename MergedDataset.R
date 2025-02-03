library(dplyr)

DNA <- read.xlsx('DNA_ID.xlsx')
CN <- read.xlsx('CN_Amaia.xlsx')
Total_P <- read.xlsx('TotalP_Amaia.xlsx')


colnames(DNA)[colnames(DNA) == "Sample"] <- "Sample_ID"
colnames(CN)[colnames(CN) == "Sample"] <- "Sample_ID"
colnames(Total_P)[colnames(Total_P) == "Sample"] <- "Sample_ID"

# Perform full joins on all three datasets
merged.df <- DNA %>%
  full_join(CN, by = "Sample_ID") %>%
  full_join(Total_P, by = "Sample_ID")

# View the final merged dataset
print(merged.df)

