library(tidyverse)
library(readxl)

#results of CN spiking
CN_samps<- read_excel("raw/CNH Sample weights_SMM-spiked-Amaia's result.xlsx")%>%select(-(12:13))

CNH_weights <- read_excel("raw/CNH Sample weights_spike_w.xlsx", , 
                                 sheet = "Amaia_Samples")
CNH_weights <- CNH_weights %>%
  mutate(Sample_ID = ifelse(Sample_ID == "ol", "ol spiked", Sample_ID))%>%
  mutate(Plate_ID = ifelse(Sample_ID %in% "ol spiked", Sample_ID, Plate_ID))%>%
group_by(Plate_ID) %>%
  mutate(Plate_ID = case_when(n() > 1 ~ paste0(Plate_ID, "_", row_number()), TRUE ~ Plate_ID)) %>%
  ungroup()
#select parts of df I care about
sample_w<-CNH_weights%>%select(1:3)%>%filter(!is.na(Sample_ID))

#make df pretty
spike_w<-CNH_weights%>%select(1,2,10:12)%>%rename(CO3_mg=Obtained, NH4_mg= `...11`, total = `...12`)%>%filter(!is.na(CO3_mg)&!CO3_mg=='C')%>%
  mutate(CO3_mg=as.numeric(CO3_mg),NH4_mg=as.numeric(NH4_mg))
#calc percent of N and C in spiked samples
#first sample removed because potential contam from previous analysis
CO3_mean_N_perc<-CN_samps%>%filter(name=='NaHCO3'&!weight=='6.95')%>% summarise(mean(`N%`) )%>%pull()
CO3_mean_C_perc<-CN_samps%>%filter(name=='NaHCO3'&!weight=='6.95')%>% summarise(mean(`C%`) )%>%pull()
NH4_mean_N_perc<-CN_samps%>%filter(name=='(NH)42SO4'&!weight=='6.799')%>% summarise(mean(`N%`) )%>%pull()
NH4_mean_C_perc<-CN_samps%>%filter(name=='(NH)42SO4'&!weight=='6.799')%>% summarise(mean(`C%`) )%>%pull()

CN_samps <- CN_samps %>%
  filter(!name %in% c("orchard leaves", "(NH)42SO4", "NaHCO3")) %>%
  group_by(name) %>%
  mutate(name = case_when(n() > 1 ~ paste0(name, "_", row_number()), TRUE ~ name)) %>%
  ungroup()

#calc Total spike for C and N
CN_Final<-spike_w%>%
  mutate(Carb_spike= (CO3_mg*CO3_mean_C_perc/100)+(NH4_mg*NH4_mean_C_perc/100),#total carbon spike
         Nitr_spike= (CO3_mg*CO3_mean_N_perc/100)+(NH4_mg*NH4_mean_N_perc/100))%>%#total nitrogen spike
  left_join(CN_samps, by=c('Plate_ID'='name'))%>%
  mutate(Samp_N= `N mg`-Nitr_spike,
         Samp_C= `C mg`-Carb_spike)%>%
  left_join(sample_w)%>%
  mutate(perc_N_Samp= (Samp_N/weight_mg)*100,
         perc_C_Samp= (Samp_C/weight_mg)*100)

library(writexl)

write_xlsx(CN_Final, 'outputs/CN_Final_Spike_.xlsx')
