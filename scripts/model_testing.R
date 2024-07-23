# Daniel Redwine
# Model Testing

# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(lme4)
library(DHARMa)
library(MASS)
library(pscl)
library(glmmTMB)
library(MuMIn)

# Load dataset
total_data <- read.csv("data/total_data.csv")

# Fit Poisson GLMM for aggressors and targets
aggression_poisson_model <- glmer(Total_Agonistic ~ Winter + adjusted_wing + 
                         Feeding_Density + PCRsex * PCRMorph + 
                         (1 | SampleID) + offset(log(Platform_Time)), 
                       data = total_data, 
                       family = poisson)

summary(aggression_poisson_model) # model summary

target_poisson_model <- glmer(Total_Recipient ~ Winter + adjusted_wing +
                                    Feeding_Density + PCRsex * PCRMorph + 
                                    (1 | SampleID) + offset(log(Platform_Time)), 
                                  data = total_data, 
                                  family = poisson)

summary(target_poisson_model) # model summary

# Check for overdispersion in the aggressor model
sim_res_aggressor_poisson <- simulateResiduals(aggression_poisson_model)
plot(sim_res_aggressor_poisson)
testDispersion(sim_res_aggressor_poisson)
testZeroInflation(sim_res_aggressor_poisson)

# Check for overdispersion in the target model
sim_res_target_poisson <- simulateResiduals(target_poisson_model)
plot(sim_res_target_poisson)
testDispersion(sim_res_target_poisson)
testZeroInflation(sim_res_target_poisson)

# Fit Negative Binomial GLMM for aggressors and targets
nb_aggressor_model <- glmer.nb(Total_Agonistic ~ Winter + adjusted_wing + 
                       Feeding_Density + PCRsex * PCRMorph + 
                       (1 | SampleID) + offset(log(Platform_Time)), 
                     data = total_data, 
                     control = glmerControl(optimizer = "bobyqa", 
                                            optCtrl = list(maxfun = 100000)))

summary(nb_aggressor_model) # model summary

nb_target_model <- glmer.nb(Total_Recipient ~ Winter + adjusted_wing + 
                                 Feeding_Density + PCRsex * PCRMorph + 
                                 (1 | SampleID) + offset(log(Platform_Time)), 
                               data = total_data, 
                               control = glmerControl(optimizer = "bobyqa", 
                                                      optCtrl = list(maxfun = 100000)))

summary(nb_target_model)

# Check model diagnostics for aggressor model
sim_res_aggressor_nb <- simulateResiduals(nb_aggressor_model)
plot(sim_res_aggressor_nb)
testDispersion(sim_res_aggressor_nb)
testZeroInflation(sim_res_aggressor_nb)


# Check model diagnostics for target model
sim_res_target_nb <- simulateResiduals(nb_target_model)
plot(sim_res_target_nb)
testDispersion(sim_res_target_nb)
testZeroInflation(sim_res_target_nb)



# Fit the truncated poisson hurdle model with random effects using glmmTMB
aggressor_tp_hurdle_model <- glmmTMB(Total_Agonistic ~ Winter + adjusted_wing 
                                  + Feeding_Density + PCRsex * PCRMorph + (1 | SampleID) 
                                  + offset(log(Platform_Time)),
                                  ziformula = ~.,
                                  family = truncated_poisson, 
                                  data = total_data)

# Model summary
summary(aggressor_tp_hurdle_model)

# Fit the truncated poisson hurdle model with random effects using glmmTMB
target_tp_hurdle_model <- glmmTMB(Total_Recipient ~ Winter + adjusted_wing 
                               + Feeding_Density + PCRsex * PCRMorph + (1 | SampleID) 
                               + offset(log(Platform_Time)),
                               ziformula = ~.,
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

# Fit the truncated nb hurdle model with random effects using glmmTMB
aggressor_tnb_hurdle_model <- glmmTMB(Total_Agonistic ~ Winter + adjusted_wing 
                                      + Feeding_Density + PCRsex * PCRMorph 
                                      + (1 | SampleID) + offset(log(Platform_Time)),
                                      ziformula = ~.,
                                      family = truncated_nbinom2, 
                                      data = total_data)

# Model summary
summary(aggressor_tnb_hurdle_model)

# Fit the truncated nb hurdle model with random effects using glmmTMB
target_tnb_hurdle_model <- glmmTMB(Total_Recipient ~ Winter + adjusted_wing 
                                   + Feeding_Density + PCRsex * PCRMorph + (1 | SampleID) 
                                   + offset(log(Platform_Time)),
                                   ziformula = ~.,
                                   family = truncated_nbinom2, 
                                   data = total_data)

# Check model summary
summary(target_tnb_hurdle_model)

# Check model diagnostics for truncated negative binomial aggressor model
sim_res_aggressor_hurdle_tnb <- simulateResiduals(aggressor_tnb_hurdle_model)
plot(sim_res_aggressor_hurdle_tnb)
testDispersion(sim_res_aggressor_hurdle_tnb)
testZeroInflation(sim_res_aggressor_hurdle_tnb)

# Check model diagnostics for truncated negative binomial target model
sim_res_target_hurdle_tnb <- simulateResiduals(target_tnb_hurdle_model)
plot(sim_res_target_hurdle_tnb)
testDispersion(sim_res_target_hurdle_tnb)
testZeroInflation(sim_res_target_hurdle_tnb)

# Compare AIC values for aggressor models
AIC(aggression_poisson_model, nb_aggressor_model, aggressor_tnb_hurdle_model,
    aggressor_tp_hurdle_model)

# Compare BIC values
BIC(aggression_poisson_model, nb_aggressor_model, aggressor_tnb_hurdle_model,
    aggressor_tp_hurdle_model)

# Compare AIC values for target models
AIC(target_poisson_model, nb_target_model, target_tnb_hurdle_model,
    target_tp_hurdle_model)

# Compare BIC values
BIC(target_poisson_model, nb_target_model, target_tnb_hurdle_model,
    target_tp_hurdle_model)

# ZIP models keep breaking, so they need to be figured out
# Unsure of where to go from here, AIC indicates NB, while assumptions indicate TP Hurdle
# We will use Truncated Poisson for now as it seems to violate the least assumptions?

# AIC for aggressor model
options(na.action = "na.fail") # otherwise blows up with NA values
dredge_aggressor_tp_hurdle <- dredge(aggressor_tp_hurdle_model)

subset(dredge_aggressor_tp_hurdlel, delta <4)

sw(dredge_aggressor_tp_hurdle) # note this is the global model, not just the competitive


# AIC for target model
options(na.action = "na.fail") # otherwise blows up with NA values
dredge_target_tp_hurdle <- dredge(target_tp_hurdle_model)

subset(dredge_target_tp_hurdlel, delta <4)

sw(dredge_target_tp_hurdle) # note this is the global model, not just competitive



