# Daniel Redwine
# Last edited: 11 Jan 24

# Clear environment
rm(list = ls())

# Load packages
library(tidyverse)

# Load in datasets
agonistic_analysis_data <- read.csv("data/agonistic_analysis_data.csv")
wtsp_sex_morph_data <- read.csv("data/WTSP_sex_morph_data.csv")
wtsp_banding_data <- read.csv("data/WTSP_banding_data.csv")
wtsp_resight_data <- read.csv("data/WTSP_video_resight_data.csv")

# Create data for banded population during the study period
population_data <- wtsp_banding_data %>%
  dplyr::select(SampleID, Year, Month, Day) %>%
  filter(Year == 2023 | Year == 2024) %>%
  filter(Month == 1 | Month == 2 | (Month == 3 & Day < 16)) %>%
  filter(SampleID != "")

# Merge the banded population with the resight data
population_data <- full_join(population_data, wtsp_resight_data, join_by(SampleID, Year),
                             relationship = "many-to-many")

# Select only the variables we need
population_data <- population_data %>%
  dplyr::select(SampleID, Year)

# Filter banding data to only include new and not recaps, includes all SampleID
# We will use the wing length from only the first capture
# Age will be determined from the first capture
# Select the variables we need from this dataset
# Wing to adjust wing length for sex and morph as proxy for body size
# Year, Age, and Month to use mutate to add 2023 age and 2024 age
wtsp_banding_data <- wtsp_banding_data %>%
  filter(New_Recap == "N" | (SampleID == "W320" & New_Recap == "R")) %>%
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

# Now we will use mutate to create new column Age
# if Year is 2023 winter will return the value in age_2023, if not returns second if
# if Year is 2024 returns the value in age_2024, if not either 2023 or 2024 returns NA
resight_analysis_data <- resight_analysis_data %>%
  mutate(Age = ifelse(Year == 2023, age_2023, ifelse(Year == 2024, age_2024, NA)))

# Clean up a bit by removing age 2023 and 2024 with the - sign and select 
# Doing some of this separate just so we can check each step if my code was right
resight_analysis_data <- resight_analysis_data %>%
  dplyr::select(-age_2023, -age_2024)


# Save the Resulting dataset which we will use for all analyses
write_excel_csv(resight_analysis_data, "data/resight_analysis_data.csv")

# Clean up dataset by forcing factors and numbers
resight_analysis_data$PCRsex <- as.factor(resight_analysis_data$PCRsex)
resight_analysis_data$PCRMorph <- as.factor(resight_analysis_data$PCRMorph)
resight_analysis_data$Age <- as.factor(resight_analysis_data$Age)
resight_analysis_data$Platform <- as.factor(resight_analysis_data$Platform)

# Now to finish the total population data
# we only want to add to population data so we use left_join
population_data <- left_join(population_data, smb_data, by = "SampleID")

# Now we will use mutate to create new column Age
# if Year is 2023 winter will return the value in age_2023, if not returns second if
# if Year is 2024 returns the value in age_2024, if not either 2023 or 2024 returns NA
population_data <- population_data %>%
  mutate(Age = ifelse(Year == 2023, age_2023, ifelse(Year == 2024, age_2024, NA)))

# Clean up a bit by removing age 2023 and 2024 with the - sign and select 
# Doing some of this separate just so we can check each step if my code was right
population_data <- population_data %>%
  dplyr::select(-age_2023, -age_2024)

# Clean up dataset by forcing factors and numbers
population_data$PCRsex <- as.factor(population_data$PCRsex)
population_data$PCRMorph <- as.factor(population_data$PCRMorph)
population_data$Age <- as.factor(population_data$Age)

# Now to fully clean the population data
population_data <- population_data %>%
  filter(PCRsex == "M" | PCRsex == "F") %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS")

# Create data without duplicate sample ID
total_population_sex_morph <- population_data %>%
  dplyr::select(SampleID, PCRsex, PCRMorph) %>%
  distinct()

# Save the resulting data which represents sex and morph of the total population
write_excel_csv(total_population_sex_morph, "data/total_population_sex_morph.csv")

# Create population data for individuals with agonistic data
agonistic_population_sex_morph <- agonistic_analysis_data %>%
  dplyr::select(SampleID, PCRsex, PCRMorph) %>%
  filter(PCRsex == "M" | PCRsex == "F") %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS") %>%
  distinct()

# Save the resulting data which represents sex and morph of the agonistic dataset
write_excel_csv(agonistic_population_sex_morph, "data/agonistic_population_sex_morph.csv")

# create the data for sex and morph of all observations
agonistic_observation_sex_morph <- agonistic_analysis_data %>%
  dplyr::select(SampleID, PCRsex, PCRMorph) %>%
  filter(PCRsex == "M" | PCRsex == "F") %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS")

# Save the resulting data which represents sex and morph of the agonistic dataset
write_excel_csv(agonistic_observation_sex_morph, "data/agonistic_observation_sex_morph.csv")

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
  dplyr::select(SampleID, Age, Platform) %>%
  filter(Age == "FW" | Age == "AFW")

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
  count(Age, Platform)

# Graph using bar chart 
age_resight_plot <- ggplot(age_resight_count, aes(x = Age, y = n, fill = Platform)) +
  geom_bar(stat = 'identity', position = 'stack') +
  xlab("age") +
  ylab("Count") + 
  theme_bw()

age_resight_plot

ggsave("output/age_resight_plot.png")

# Create counts for sex and morph of the total population
total_population_sex_morph_count <- total_population_sex_morph %>%
  count(PCRsex, PCRMorph)

# Create counts for sex and morph of the platform population
agonistic_population_sex_morph_count <- agonistic_population_sex_morph %>%
  count(PCRsex, PCRMorph)

# Create counts for sex and morph of the total population
agonistic_observation_sex_morph_count <- agonistic_observation_sex_morph %>%
  count(PCRsex, PCRMorph)

# Total population count plot
total_population_count_plot <- ggplot(total_population_sex_morph_count, 
        aes(x = PCRsex, y = n, fill = PCRMorph)) +
  geom_bar(stat = 'identity', position = 'stack') +
  xlab("Sex") +
  ylab("Count") + 
  theme_bw()

total_population_count_plot

ggsave("output/total_population_count_plot.png")

# agonistic population count plot
agonsitic_population_count_plot <- ggplot(agonistic_population_sex_morph_count, 
                                      aes(x = PCRsex, y = n, fill = PCRMorph)) +
  geom_bar(stat = 'identity', position = 'stack') +
  xlab("Sex") +
  ylab("Count") + 
  theme_bw()

agonsitic_population_count_plot

ggsave("output/agonsitic_population_count_plot.png")

# agonistic observation count plot
agonsitic_observation_count_plot <- ggplot(agonistic_observation_sex_morph_count, 
                                          aes(x = PCRsex, y = n, fill = PCRMorph)) +
  geom_bar(stat = 'identity', position = 'stack') +
  xlab("Sex") +
  ylab("Count") + 
  theme_bw()

agonsitic_observation_count_plot

ggsave("output/agonsitic_observation_count_plot.png")

# Now to merge different datasets for the chi squared
# First add a column to denote which population
total_population_sex_morph_count <- total_population_sex_morph_count %>%
  mutate(Population = "total")

agonistic_population_sex_morph_count <- agonistic_population_sex_morph_count %>%
  mutate(Population = "agonistic")

agonistic_observation_sex_morph_count <- agonistic_observation_sex_morph_count %>%
  mutate(Population = "observation")

# Merge the datasets we wish to compare
total_vs_agonistic_data <- full_join(total_population_sex_morph_count, 
                                         agonistic_population_sex_morph_count) %>%
  group_by(Population)

total_vs_observation_data <- full_join(total_population_sex_morph_count, 
                                            agonistic_observation_sex_morph_count) %>% 
  group_by(Population)


# Chi comparing the sex ratio between the total population and the agonistic population
total_vs_agonistic_sex_chi <- xtabs(n ~ Population + PCRsex,
                   data = total_vs_agonistic_data)

total_vs_agonistic_sex_chi

chisq.test(total_vs_agonistic_sex_chi)

# Chi comparing the morph ratio between the total population and the agonistic population
total_vs_agonistic_morph_chi <- xtabs(n ~ Population + PCRMorph,
                                data = total_vs_agonistic_data)

total_vs_agonistic_morph_chi

chisq.test(total_vs_agonistic_morph_chi)

# Chi comparing the sex ratio between the total population and observations
total_vs_observation_sex_chi <- xtabs(n ~ Population + PCRsex,
                                    data = total_vs_observation_data)

total_vs_observation_sex_chi

chisq.test(total_vs_observation_sex_chi)

# chi comparing the morph ratio between the total population and observations
total_vs_observation_morph_chi <- xtabs(n ~ Population + PCRMorph,
                                      data = total_vs_observation_data)

total_vs_observation_morph_chi

chisq.test(total_vs_observation_morph_chi)

# Graphs for just sex and morph
population_sex_count <- total_population_sex_morph %>%
  group_by(PCRsex) %>%
  count()

population_morph_count <- total_population_sex_morph %>%
  group_by(PCRMorph) %>%
  count()

total_count <- sum(population_sex_count$n)

population_sex_proportion <- population_sex_count %>%
  mutate(proportion = n / total_count)

population_morph_proportion <- population_morph_count %>%
  mutate(proportion = n / total_count)

population_sex_proportion <- population_sex_proportion %>%
  mutate(PCRsex = recode(PCRsex, "M" = "Male", "F" = "Female"))

population_morph_proportion <- population_morph_proportion %>%
  mutate(PCRMorph = recode(PCRMorph, "WS" = "White-stripe", "TS" = "Tan-stripe"))

population_sex_ratio <- ggplot(population_sex_proportion, aes(x = PCRsex , y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  labs(x = "Sex", y = "Population proportion") +
  theme_bw()

population_sex_ratio # call object

ggsave("output/significant_predictors/population_sex_ratio.png") # save object

population_morph_ratio <- ggplot(population_morph_proportion, aes(x = PCRMorph , y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  labs(x = "Morph", y = "Population proportion") +
  theme_bw()

population_morph_ratio # call object

ggsave("output/significant_predictors/population_morph_ratio.png") # save object
