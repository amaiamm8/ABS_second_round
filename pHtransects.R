library(readxl)
library(dplyr)
library(emmeans)
library(ggplot2)
install.packages("multcompView")
library(multcompView)


# Read the Excel file
alldata<- read_excel("raw/Labbook.xlsx", "soil pH")

#see if the weight influences pH
weight.lm <- lm(formula = pH ~ Soil_weight , data = alldata)
summary(weight.lm)
#-- not significant!

#average pH between reps
alldata <- alldata %>%
  group_by(gr = cumsum(Rep == "A")) %>%  # Group every pair of A/B
  mutate(Avg_pH = ifelse(Rep == "A", mean(pH), NA)) %>%
  ungroup() %>%
  select(-gr)

#delete reps
pH_data <- alldata %>%
  filter(!is.na(Avg_pH)) %>%
  select(-Rep) %>%
  select(-pH)

#group half transects
pH_data <- pH_data %>%
  mutate(HalfT = case_when(
    as.numeric(sub("L", "", Location)) < 20 ~ "A",  # Less than L20
    as.numeric(sub("L", "", Location)) > 30 ~ "B",  # Greater than L30
  ))
#see if pH means are sigdif between halftransects
pH_data <- pH_data %>% #naming the half transects
  mutate(Name= paste0(Site, Transect, HalfT))

single.lm<-lm(Avg_pH ~ Name, pH_data)# building a model
car::Anova(single.lm)   #[instead of anova function given by WUR]
#post hoc LSD test to 

emm<- emmeans(single.lm, specs = "Name")
pairs(emm, specs="Name", adjust="none")
emmeans(single.lm, specs = "Name", adjust = "none")
multcomp::cld(emm)  # use this to more efficiently look at differences between groups

cld_result <- multcomp::cld(emm)

# Clean up the table
cld_result_clean <- cld_result[, c("Name", "emmean", "SE", "df", "lower.CL", "upper.CL", ".group")]
colnames(cld_result_clean) <- c("Name", "emmean", "SE", "df", "lower.CL", "upper.CL", ".group")

# Create a Word document table
install.packages("flextable")
library(flextable)
ft <- flextable(cld_result_clean)
save_as_docx(ft, path = "cld_table.docx")


significant_pairs <- pairs(emm, adjust = "none") %>%
  summary() %>%
  filter(p.value < 0.05)
nonsignificant_pairs <- pairs(emm, adjust = "none") %>%
  summary() %>%
  filter(p.value >= 0.05)


# View significant pairs
print(count(significant_pairs))
print(count(nonsignificant_pairs))


# Boxplot of values by site
ggplot(alldata, aes(x = Name, y = Avg_pH)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Comparison of Values by Site",
       x = "Site",
       y = "Value")


write.csv(pH_data, "outputs/pH_output.csv", row.names = FALSE)

