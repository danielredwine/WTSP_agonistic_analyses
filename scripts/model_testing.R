# Daniel Redwine
# Model Testing

# Clear environment
rm(list = ls())

library(tidyverse)
library(lme4)
library(DHARMa)
library(MASS)
library(pscl)
library(glmmTMB)

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



# Fit the hurdle model with random effects using glmmTMB
aggressor_hurdle_model <- glmmTMB(Total_Agonistic ~ Winter + adjusted_wing + 
                                    Feeding_Density + Platform_Time + PCRsex * PCRMorph + 
                                    (1 | SampleID),
                        ziformula = ~1, # Zero-inflation part, adjust as needed
                        family = truncated_nbinom2, # Negative binomial distribution
                        data = total_data)

# Model summary
summary(aggressor_hurdle_model)

# Fit the hurdle model with random effects using glmmTMB
target_hurdle_model <- glmmTMB(Total_Recipient ~ Winter + adjusted_wing + 
                                    Feeding_Density + Platform_Time + PCRsex * PCRMorph + 
                                    (1 | SampleID),
                                  ziformula = ~1, # Zero-inflation part, adjust as needed
                                  family = truncated_nbinom2, # Negative binomial distribution
                                  data = total_data)

# Check model summary
summary(target_hurdle_model)

# Check model diagnostics for aggressor model
sim_res_aggressor_hurdle <- simulateResiduals(aggressor_hurdle_model)
plot(sim_res_aggressor_hurdle)
testDispersion(sim_res_aggressor_hurdle)
testZeroInflation(sim_res_aggressor_hurdle)

# Check model diagnostics for target model
sim_res_target_hurdle <- simulateResiduals(target_hurdle_model)
plot(sim_res_target_hurdle)
testDispersion(sim_res_target_hurdle)
testZeroInflation(sim_res_target_hurdle)


# Compare AIC values for aggressor models
AIC(aggression_poisson_model, nb_aggressor_model, aggressor_hurdle_model)

# Compare BIC values
BIC(aggression_poisson_model, nb_aggressor_model, aggressor_hurdle_model)

# Compare AIC values for target models
AIC(target_poisson_model, nb_target_model, target_hurdle_model)

# Compare BIC values
BIC(target_poisson_model, nb_target_model, target_hurdle_model)
