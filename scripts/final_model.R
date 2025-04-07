# Daniel Redwine
# Final Model

# Clear environment
rm(list = ls())

# Load libraries
library(glmmTMB)
library(MuMIn)
library(tidyverse)
library(DHARMa)
library(visreg)

# Load dataset
total_data <- read.csv("data/total_data.csv")

# Fit the truncated poisson hurdle model with random effects using glmmTMB
aggressor_hurdle_model <- glmmTMB(Total_Agonistic ~ Winter 
                                     + Feeding_Density + PCRsex * PCRMorph + (1 | SampleID),
                                     ziformula = ~.,
                                     offset = log(Platform_Time),
                                     family = truncated_poisson, 
                                     data = total_data)

# Model summary
summary(aggressor_hurdle_model)

# Fit the truncated poisson hurdle model with random effects using glmmTMB
target_hurdle_model <- glmmTMB(Total_Recipient ~ Winter 
                                  + Feeding_Density + PCRsex * PCRMorph + (1 | SampleID),
                                  ziformula = ~.,
                                  offset = log(Platform_Time),
                                  family = truncated_poisson, 
                                  data = total_data)

# Check model summary
summary(target_hurdle_model)

# Check model diagnostics for aggressor model
sim_res_aggressor_hurdle <- simulateResiduals(aggressor_hurdle_model)
plot(sim_res_aggressor_hurdle)
testDispersion(sim_res_aggressor_hurdle)
testZeroInflation(sim_res_aggressor_hurdle)

# Check model diagnostics for recipient model
sim_res_target_hurdle <- simulateResiduals(target_hurdle_model)
plot(sim_res_target_hurdle)
testDispersion(sim_res_target_hurdle)
testZeroInflation(sim_res_target_hurdle)

# AIC for aggressor model
options(na.action = "na.fail") # otherwise blows up with NA values
dredge_aggressor_hurdle <- dredge(aggressor_hurdle_model)

subset(dredge_aggressor_hurdle, delta < 3)

# AIC for target model
options(na.action = "na.fail") # otherwise blows up with NA values
dredge_target_hurdle <- dredge(target_hurdle_model)

subset(dredge_target_hurdle, delta < 3)