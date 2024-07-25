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
aggressor_tp_hurdle_model <- glmmTMB(Total_Agonistic ~ Winter + adjusted_wing 
                                     + Feeding_Density + PCRsex * PCRMorph + (1 | SampleID),
                                     ziformula = ~.,
                                     offset = log(Platform_Time),
                                     family = truncated_poisson, 
                                     data = total_data)

# Model summary
summary(aggressor_tp_hurdle_model)

# Fit the truncated poisson hurdle model with random effects using glmmTMB
target_tp_hurdle_model <- glmmTMB(Total_Recipient ~ Winter + adjusted_wing 
                                  + Feeding_Density + PCRsex * PCRMorph + (1 | SampleID),
                                  ziformula = ~.,
                                  offset = log(Platform_Time),
                                  family = truncated_poisson, 
                                  data = total_data)

# Check model summary
summary(target_tp_hurdle_model)

# Check model diagnostics for truncated negative binomial aggressor model
sim_res_aggressor_hurdle_tp <- simulateResiduals(aggressor_tp_hurdle_model)
plot(sim_res_aggressor_hurdle_tp)
testDispersion(sim_res_aggressor_hurdle_tp)
testZeroInflation(sim_res_aggressor_hurdle_tp)

# Check model diagnostics for truncated negative binomial target model
sim_res_target_hurdle_tp <- simulateResiduals(target_tp_hurdle_model)
plot(sim_res_target_hurdle_tp)
testDispersion(sim_res_target_hurdle_tp)
testZeroInflation(sim_res_target_hurdle_tp)

aggressor_model <- glmmTMB(Total_Agonistic ~ (1 | SampleID),
                      ziformula = ~  Feeding_Density +  (1 | SampleID),
                      offset = log(Platform_Time),
                      family = truncated_poisson, 
                      data = total_data)

target_model <- glmmTMB(Total_Recipient ~ PCRsex * PCRMorph + (1 | SampleID),
                        ziformula = ~ Feeding_Density + Winter + (1 | SampleID),
                        offset = log(Platform_Time),
                        family = truncated_poisson, 
                        data = total_data)

# Simulate residuals
sim_res_aggressor <- simulateResiduals(aggressor_model)
sim_res_target <- simulateResiduals(target_model)

# Plot simulated residuals
plot(sim_res_aggressor)
plot(sim_res_target)

# Test dispersion
testDispersion(sim_res_aggressor)
testDispersion(sim_res_target)

# Model Summary
summary(aggressor_model)
summary(target_model)