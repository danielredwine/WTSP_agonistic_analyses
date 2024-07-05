# Daniel Redwine
# nonzero negative binomial model

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


# Filter out zeros for aggressors
nonzero_aggressor_data <- total_data %>%
  filter(Total_Agonistic != 0)

# Filter out zeros for recipients
nonzero_target_data <- total_data %>%
  filter(Total_Recipient != 0)

# Build the total aggressor model
aggression_nb_model <- glmer.nb(Total_Agonistic~PCRsex+PCRMorph+Winter+adjusted_wing+
                                    Feeding_Density+ (1|SampleID) +
                                    offset(log(Platform_Time)),
                                  data = nonzero_aggressor_data)

# Model Summary
summary(aggression_nb_model)

# Check assumptions
performance::check_model(aggression_nb_model)


options(na.action = "na.fail") # otherwise blows up with NA values
dredge_aggressor_nb <- dredge(aggression_nb_model) # All combos lazy mode

subset(dredge_aggressor_nb, delta <4) # Only show less than 4 aicc

sw(dredge_aggressor_nb) #notice this is the global model, not just competitive models

# Build the total recipient model
recipient_nb_model <- glmer.nb(Total_Recipient~PCRsex+PCRMorph+Winter+adjusted_wing+
                                   Feeding_Density+ (1|SampleID) +
                                   offset(log(Platform_Time)), 
                                 data = nonzero_target_data)

# Model Summary
summary(recipient_nb_model)

# Check assumptions
performance::check_model(recipient_nb_model)


options(na.action = "na.fail") # otherwise blows up with NA values
dredge_recipient_nb <- dredge(recipient_nb_model) #all combos lazy mode

subset(dredge_recipient_nb, delta <4)

sw(dredge_recipient_nb) #notice this is the global models, not just competitive models