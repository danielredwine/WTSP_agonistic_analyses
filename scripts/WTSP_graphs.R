# Daniel Redwine
# WTSP Plots

# Clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(patchwork)

# Import dataset
agonistic_analysis_data <- read.csv("data/agonistic_analysis_data.csv")

# Clean up dataset by forcing factors and numbers
agonistic_analysis_data$PCRsex <- as.factor(agonistic_analysis_data$PCRsex)
agonistic_analysis_data$PCRMorph <- as.factor(agonistic_analysis_data$PCRMorph)
agonistic_analysis_data$Winter <- as.factor(agonistic_analysis_data$Winter)
agonistic_analysis_data$Wing <- as.numeric(agonistic_analysis_data$Wing)
agonistic_analysis_data$Agonistic_Rate <- as.numeric(agonistic_analysis_data$Agonistic_Rate)

# Clean out NA and X
total_data <- agonistic_analysis_data %>%
  select(SampleID, Winter, Wing, PCRsex, PCRMorph, Agonistic_Rate) %>%
  na.omit() %>%
  filter(Winter == "FW" | Winter == "AFW") %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS") %>%
  filter(PCRsex == "M" | PCRsex == "F")

sex_data <- agonistic_analysis_data %>%
  select(SampleID, PCRsex, Agonistic_Rate) %>%
  filter(PCRsex == "M" | PCRsex == "F")

morph_data <- agonistic_analysis_data %>%
  select(SampleID, PCRMorph, Agonistic_Rate) %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS")

age_data <- agonistic_analysis_data %>%
  select(SampleID, Winter, Agonistic_Rate) %>%
  filter(Winter == "FW" | Winter == "AFW")

wing_data <- agonistic_analysis_data %>%
  select(SampleID, Wing, Agonistic_Rate)

sex_boxplot <- ggplot(sex_data, aes(PCRsex, Agonistic_Rate, fill = PCRsex)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Sex") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

sex_boxplot

ggsave("output/agonistic_sex.png")

morph_boxplot <- ggplot(morph_data, aes(PCRMorph, Agonistic_Rate, fill = PCRMorph)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Morph") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

morph_boxplot

ggsave("output/agonistic_morph.png")

age_boxplot <- ggplot(age_data, aes(Winter, Agonistic_Rate, fill = Winter)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Age") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

age_boxplot

ggsave("output/agonistic_age.png")

wing_scatterplot <- ggplot(wing_data, aes(Wing, Agonistic_Rate)) + 
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab("Interactions/sec") +
  xlab("Wing Chord (mm)")

wing_scatterplot

ggsave("output/agonistic_wing.png")