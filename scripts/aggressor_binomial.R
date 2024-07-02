# Daniel Redwine
# aggressor binomial

# Clear environment
rm(list = ls())

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
total_data$adjusted_wing <- as.numeric(total_data$adjusted_wing)

# Build the total aggressor model
aggressor_binomial_model <- glmer(Aggressor_Occurrence~PCRsex+PCRMorph+Winter+adjusted_wing+
          Feeding_Density+Platform_Time+ (1|SampleID), data = total_data, family = binomial)

# Model Summary
summary(aggressor_binomial_model)

# Check assumptions
performance::check_model(aggressor_binomial_model)


options(na.action = "na.fail") # otherwise blows up with NA values
dredge_aggressor_binomial <- dredge(aggressor_binomial_model)

subset(dredge_aggressor_binomial, delta <4)

sw(dredge_aggressor_binomial) #notice this is the global model, not just the competitive model set

# Build the total recipient model
recipient_binomial_model <- glmer(Recipient_Occurrence~PCRsex+PCRMorph+Winter+adjusted_wing+
                                    Feeding_Density+Platform_Time+ (1|SampleID), data = total_data, family = binomial)

# Model Summary
summary(recipient_binomial_model)

# Check assumptions
performance::check_model(recipient_binomial_model)


options(na.action = "na.fail") # otherwise blows up with NA values
dredge_recipient_binomial <- dredge(recipient_binomial_model)

subset(dredge_recipient_binomial, delta <4)

sw(dredge_recipient_binomial) #notice this is the global model, not just the competitive model set

