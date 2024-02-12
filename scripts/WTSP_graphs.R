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
graphing_data <- agonistic_analysis_data %>%
  select(SampleID, Winter, Wing, PCRsex, PCRMorph, Agonistic_Rate) %>%
  na.omit() %>%
  filter(Winter == "FW" | Winter == "AFW") %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS") %>%
  filter(PCRsex == "M" | PCRsex == "F")

sex_boxplot <- ggplot(graphing_data, aes(PCRsex, Agonistic_Rate, fill = PCRsex)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Sex") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

sex_boxplot

morph_boxplot <- ggplot(graphing_data, aes(PCRMorph, Agonistic_Rate, fill = PCRMorph)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Morph") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

morph_boxplot

age_boxplot <- ggplot(graphing_data, aes(Winter, Agonistic_Rate, fill = Winter)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Age") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

age_boxplot

wing_scatterplot <- ggplot(graphing_data, aes(Wing, Agonistic_Rate)) + 
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab("Interactions/sec") +
  xlab("Wing Chord (mm)")

wing_scatterplot

sex_boxplot / wing_scatterplot | morph_boxplot / age_boxplot

