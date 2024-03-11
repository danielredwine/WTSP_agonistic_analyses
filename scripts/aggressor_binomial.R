# Daniel Redwine
# aggressor binomial

# Clear environment
rm(list = ls())

oo <- options(repos = "https://cran.r-project.org/")
install.packages("Matrix")
install.packages("lme4")
options(oo)

# Load all libraries needed
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggfortify)
library(MuMIn)
library(asbio)
library(GGally)
library(performance)
library(patchwork)
library(olsrr)

# Load in the dataset
total_data <- read.csv("data/total_data.csv")

# force into numerics and factors just to make sure everything works
total_data$SampleID <- as.factor(total_data$SampleID)
total_data$PCRsex <- as.factor(total_data$PCRsex)
total_data$PCRMorph <- as.factor(total_data$PCRMorph)
total_data$Winter <- as.factor(total_data$Winter)
total_data$Wing <- as.numeric(total_data$Wing)
total_data$Feeding_Density <- as.numeric(total_data$Feeding_Density)
total_data$Platform_Time <- as.numeric(total_data$Platform_Time)
total_data$Aggressor_Occurrence <- as.numeric(total_data$Aggressor_Occurrence)

# Build the total model
aggressor_binomial_model <- glmer(Aggressor_Occurrence~PCRsex*PCRMorph+Winter+Wing+
          Feeding_Density+Platform_Time+ (1|SampleID), data = total_data, family = binomial)
