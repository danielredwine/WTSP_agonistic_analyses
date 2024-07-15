# Daniel Redwine
# Model Testing

# Clear environment
rm(list = ls())

library(tidyverse)
library(pscl)
library(boot)

total_data <- read.csv("data/total_data.csv")

aggression_histogram <- ggplot(total_data, aes(Total_Agonistic)) +
  geom_histogram() +
  theme_bw()

aggression_histogram

target_histogram <- ggplot(total_data, aes(Total_Recipient)) +
  geom_histogram() +
  theme_bw()

target_histogram

aggressor_zip <- zeroinfl(Total_Agonistic ~ PCRsex * PCRMorph + Winter + adjusted_wing +
                                     Feeding_Density + (1|SampleID) +
                                     offset(log(Platform_Time)), data = total_data)
