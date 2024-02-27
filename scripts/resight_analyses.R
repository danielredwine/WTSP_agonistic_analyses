# Daniel Redwine
# Last edited: 11 Jan 24

# Clear environment
rm(list = ls())

# Load packages
library(tidyverse)

# Load in datasets
wtsp_sex_morph_data <- read.csv("data/WTSP_sex_morph_data.csv")
wtsp_banding_data <- read.csv("data/WTSP_banding_data.csv")
wtsp_resight_data <- read.csv("data/WTSP_video_resight_data.csv")

# Filter banding data to only include new and not recaps, includes all SampleID
# We will use the wing length from only the first capture
# Age will be determined from the first capture
# Select the variables we need from this dataset
# Wing to adjust wing length for sex and morph as proxy for body size
# Year, Age, and Month to use mutate to add 2023 age and 2024 age
wtsp_banding_data <- wtsp_banding_data %>%
  filter(New_Recap == "N") %>%
  dplyr::select(SampleID, BirdAge, Year, Month)

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
  dplyr::select(SampleID, age_2023, age_2024)


# Merge sex and morph with the banding data
smb_data <- full_join(wtsp_banding_data, wtsp_sex_morph_data, by = "SampleID")


# Now merge with the resight data
# we only want to add to resight data so we use left_join
resight_analysis_data <- left_join(wtsp_resight_data, smb_data, by = "SampleID")

# Now we will use mutate to create new column Winter
# if Year is 2023 winter will return the value in age_2023, if not returns second if
# if Year is 2024 returns the value in age_2024, if not either 2023 or 2024 returns NA
resight_analysis_data <- resight_analysis_data %>%
  mutate(Winter = ifelse(Year == 2023, age_2023, ifelse(Year == 2024, age_2024, NA)))

# Clean up a bit by removing age 2023 and 2024 with the - sign and select 
# Doing some of this separate just so we can check each step if my code was right
resight_analysis_data <- resight_analysis_data %>%
  dplyr::select(-age_2023, -age_2024)


# Save the Resulting dataset which we will use for all analyses
write_excel_csv(resight_analysis_data, "data/resight_analysis_data.csv")

# Clean up dataset by forcing factors and numbers
resight_analysis_data$PCRsex <- as.factor(resight_analysis_data$PCRsex)
resight_analysis_data$PCRMorph <- as.factor(resight_analysis_data$PCRMorph)
resight_analysis_data$Winter <- as.factor(resight_analysis_data$Winter)
resight_analysis_data$Platform <- as.factor(resight_analysis_data$Platform)

# Create data for sex data even if other fields are missing
sex_resight_data <- resight_analysis_data %>%
  dplyr::select(SampleID, PCRsex, Platform) %>%
  filter(PCRsex == "M" | PCRsex == "F")

# Create data for morph data even if other fields are missing
morph_resight_data <- resight_analysis_data %>%
  dplyr::select(SampleID, PCRMorph, Platform) %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS")

# create data for age data even if other fields are missing
age_resight_data <- resight_analysis_data %>%
  dplyr::select(SampleID, Winter, Platform) %>%
  filter(Winter == "FW" | Winter == "AFW")

# Bar chart for sex resight occurrence
# Organize data for plotting/analysis
sex_resight_count <- sex_resight_data %>%
  count(PCRsex, Platform)

# Graph using bar chart 
sex_resight_plot <- ggplot(sex_resight_count, aes(x = PCRsex, y = n, fill = Platform)) +
  geom_bar(stat = 'identity', position = 'stack') +
  xlab("Sex") +
  ylab("Count") + 
  theme_bw()

sex_resight_plot

ggsave("output/sex_resight_plot.png")

# Bar chart for morph resight occurrence
# Organize data for plotting/analysis
morph_resight_count <- morph_resight_data %>%
  count(PCRMorph, Platform)

# Graph using bar chart 
morph_resight_plot <- ggplot(morph_resight_count, aes(x = PCRMorph, y = n, fill = Platform)) +
  geom_bar(stat = 'identity', position = 'stack') +
  xlab("morph") +
  ylab("Count") + 
  theme_bw()

morph_resight_plot

ggsave("output/morph_resight_plot.png")

# Bar chart for age resight occurrence
# Organize data for plotting/analysis
age_resight_count <- age_resight_data %>%
  count(Winter, Platform)

# Graph using bar chart 
age_resight_plot <- ggplot(age_resight_count, aes(x = Winter, y = n, fill = Platform)) +
  geom_bar(stat = 'identity', position = 'stack') +
  xlab("age") +
  ylab("Count") + 
  theme_bw()

age_resight_plot

ggsave("output/age_resight_plot.png")
