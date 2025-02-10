library(tidyverse)

# Define the folder path
folder_path <- "Ortho_phos"

# Get a list of all file paths in the folder
file_list <- list.files(folder_path, full.names = TRUE)

# Read and combine all files
All_ortho_P <- file_list %>%
  set_names() %>%  # Preserve filenames for later
  map_dfr(read_csv, .id = "source_file") # Read files and add filename column

# Extract only the filename (not full path)
All_ortho_P <- All_ortho_P %>%
  mutate(source_file = basename(source_file))

#replace this with your own location
Amaia_Bags <- read_excel("C:/Users/90957135/OneDrive - Western Sydney University/R/ABS_second_round/raw/Bags.xlsx")

#import and make name col
Bag_names<-Amaia_Bags%>%
  unite("Names", Site, Transect, Location, sep = "", remove = TRUE) %>% # Combine columns
  select(Names)%>%distinct()

#join by name col
Amaia_Ortho<-Bag_names%>%
  left_join(All_ortho_P, by= c("Names"="Sample ID"))


# Identify duplicate Names
#Faizun ran one sample in each lot 3 times to make sure that results were consistent
duplicates <- Amaia_Ortho %>%
  group_by(Names) %>%
  filter(n() > 1)  # Keep only duplicates

# Calculate the mean of the result column for duplicate Names
mean_results <- duplicates %>%
  group_by(Names) %>%
  summarise(Result = mean(Result, na.rm = TRUE))

#remove duplicate values and then add in mean values to make downstream easier
Ortho_P_Amaia<-Amaia_Ortho%>%
  anti_join(mean_results)%>%
  bind_rows(mean_results)


#This is your final df, note that the duplicated row now only have one value for Result and the rest of the values
#are na, this shouldnt be an issue for the rest of your calculations
Ortho_P_Amaia

#select the blank values from raw data file
Blanks<-All_ortho_P%>%
  filter(str_detect(source_file, "LOT1|LOT2") & `Sample ID`=="BLANK")

#bind these rows back to original df
Ortho_P_Amaia<-Ortho_P_Amaia%>%
  bind_rows(Blanks)

Final_Ortho_numbers<-Ortho_P_Amaia%>%  
mutate(Blank_avg = mean(Result[grepl("BLANK", `Sample ID`)], na.rm = TRUE), 
         Ortho_blanked = (Result-Blank_avg),
         Ortho_blanked = ifelse(Ortho_blanked < 0, 0.0288/2, Ortho_blanked),
       
#you need to change the resin_nute_w to the weight you actually took in you bag df
         Ortho_P_mg_kg= Ortho_blanked *(7.5/resin_nute_w)/(`Manual Dil`*`Auto Dil`)) #7.5mL used for 1 g of resin
#you dont have any dilutions, but good practice to include and to make sure everything lines up
