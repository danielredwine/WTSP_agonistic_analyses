# Daniel Redwine
# Final Model

# Clear environment
rm(list = ls())

# Load libraries
library(glmmTMB)
library(MuMIn)
library(tidyverse)

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

# Dredge to create every possible variable combination
dredge_aggressor_tp_hurdle <- dredge(aggressor_tp_hurdle_model, rank = "AIC")
dredge_target_tp_hurdle <- dredge(target_tp_hurdle_model, rank = "AIC")

# Create a list of the top models
top_aggressor_models <- subset(dredge_aggressor_tp_hurdle, delta < 3 )
top_target_models <- subset(dredge_target_tp_hurdle, delta < 3)

# Call the list of the top models
top_aggressor_models
top_target_models
