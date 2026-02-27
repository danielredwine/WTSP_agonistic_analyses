# Daniel Redwine
# Last edited: 2 July 24

# Clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(ggfortify)
library(emmeans)
library(multcomp)
library(multcompView)

# Load in datasets
wtsp_sex_morph_data <- read.csv("data/WTSP_sex_morph_data.csv")
wtsp_banding_data <- read.csv("data/WTSP_banding_data.csv")
wtsp_agonistic_data <- read.csv("data/WTSP_agonistic_behavior_data.csv")

# Filter banding data to only include new and not recaps, includes all SampleID
# We will use the wing length from only the first capture
# Age will be determined from the first capture
# Select the variables we need from this dataset
# Wing to adjust wing length for sex and morph as proxy for body size
# Year, Age, and Month to use mutate to add 2023 age and 2024 age
wtsp_banding_data <- wtsp_banding_data %>%
  filter(New_Recap == "N" | (New_Recap =="R" & SampleID =="W320")) %>%
  dplyr::select(SampleID, Wing, BirdAge, Year, Month)

# Mutate to autofill age for 2023
# Have to use a lot of Boolean operators to get the right result
# AFW is after first winter, FW is first winter
wtsp_banding_data <- wtsp_banding_data %>%
          mutate(wtsp_banding_data, age_2023 = ifelse(Year == 2020 | Year == 2021 | 
          (Year == 2022 & (Month == 1 | Month == 2 | Month == 3 | Month == 4 | Month == 5)) | 
          ((Year == 2022 & BirdAge == "AHY") | (Year == 2023 & BirdAge == "ASY")), "AFW", 
          ifelse((Year == 2022 & BirdAge == "HY") | (Year == 2023 & BirdAge == "SY"), "FW",
                 NA)))

# Mutate to autofill age for 2024
wtsp_banding_data <- wtsp_banding_data %>%
          mutate(wtsp_banding_data, age_2024 = ifelse(Year == 2020 | Year == 2021 |
          Year == 2022 |
          (Year == 2023 & (Month == 1 | Month == 2 | Month == 3 | Month == 4 | Month == 5)) | 
          ((Year == 2023 & BirdAge == "AHY") | (Year == 2024 & BirdAge == "ASY")), "AFW", 
          ifelse((Year == 2023 & BirdAge == "HY") | (Year == 2024 & BirdAge == "SY"), "FW",
                 NA)))

# Select what we need from the sex and morph data
wtsp_sex_morph_data <- wtsp_sex_morph_data %>%
  dplyr::select(SampleID, PCRsex, PCRMorph)

# Select what we need from the banding data
wtsp_banding_data <- wtsp_banding_data %>%
  dplyr::select(SampleID, Wing, age_2023, age_2024)

# Select what we need, or in this case don't need from agonistic data
wtsp_agonistic_data <- wtsp_agonistic_data %>%
  dplyr::select(-Attack, -Counterattack, -Pursuit, -Contact_Aggression, -Colors.Left.Top.Bottom,
         -Colors.Right.Top.Bottom)

# Merge sex and morph with the banding data
smb_data <- full_join(wtsp_banding_data, wtsp_sex_morph_data, by = "SampleID")

# Now adjust wing by sex and morph
# Remove any NA/X sex and morph
wing_analysis_data <- smb_data %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS") %>%
  filter(PCRsex == "M" | PCRsex == "F")

wing_analysis_data$Wing <- as.numeric(wing_analysis_data$Wing)

# Model to show significance
wing_model <- lm(Wing ~ PCRsex + PCRMorph, data = wing_analysis_data)

summary(wing_model)

# Create a complete grid of PCRsex and PCRMorph
combinations <- expand.grid(
  PCRsex = unique(wing_analysis_data$PCRsex),
  PCRMorph = unique(wing_analysis_data$PCRMorph)
)

# Calculate mean of wing for all combinations of PCRsex and PCRMorph
wing_summary <- wing_analysis_data %>%
  group_by(PCRsex, PCRMorph) %>%
  summarise(mean_wing = mean(Wing, na.rm = TRUE)) %>%
  right_join(combinations, by = c("PCRsex", "PCRMorph")) %>%
  replace_na(list(mean_wing = NA))

# Join the means back to the original dataset
smb_data$Wing <- as.numeric(smb_data$Wing)

smb_data <- smb_data %>%
  left_join(wing_summary, by = c("PCRsex", "PCRMorph"))

# Adjust the Wing values by subtracting the group means
smb_data <- smb_data %>%
  mutate(adjusted_wing = Wing - mean_wing)

# Now merge with the agonistic data
# we only want to add to agonistic data so we use left_join
agonistic_analysis_data <- left_join(wtsp_agonistic_data, smb_data, by = "SampleID")

# Now we will use mutate to create new column Winter
# if Year is 2023 winter will return the value in age_2023, if not returns second if
# if Year is 2024 returns the value in age_2024, if not either 2023 or 2024 returns NA
agonistic_analysis_data <- agonistic_analysis_data %>%
  mutate(Winter = ifelse(Year == 2023, age_2023, ifelse(Year == 2024, age_2024, NA)))
 
# Clean up a bit by removing age 2023 and 2024 with the - sign and select 
# Doing some of this separate just so we can check each step if my code was right
agonistic_analysis_data <- agonistic_analysis_data %>%
   dplyr::select(-age_2023, -age_2024)


# Save the Resulting dataset which we will use for all analyses
write_excel_csv(agonistic_analysis_data, "data/agonistic_analysis_data.csv")

# Clean out NA and X
total_data <- agonistic_analysis_data %>%
  dplyr::select(SampleID, Winter, Wing, PCRsex, PCRMorph, Agonistic_Rate, Platform_Time, Aggressor_Occurrence, Recipient_Occurrence, 
                Recipient_rate, Maximum_Foraging_Density, OrdDay, Platform, Total_Recipient, Total_Agonistic, adjusted_wing) %>%
  na.omit() %>%
  filter(Winter == "FW" | Winter == "AFW") %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS") %>%
  filter(PCRsex == "M" | PCRsex == "F")

# Save the Resulting dataset which we will use for all analyses
write_excel_csv(total_data, "data/total_data.csv")