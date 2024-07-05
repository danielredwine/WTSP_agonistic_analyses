# Daniel Redwine
# WTSP Plots

# Clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(patchwork)

# Import dataset
total_data <- read.csv("data/total_data.csv")

# Create nonzero datasets
aggressor_nonzero_data <- total_data %>%
  filter(Total_Agonistic > 0)

target_nonzero_data <- total_data %>%
  filter(Total_Recipient > 0)

# Aggressor Binomial model graph for platform time
# Graph aggressor occurrence for platform time
platform_time_binomial <- ggplot(total_data, aes(Platform_Time, Aggressor_Occurrence)) +
  geom_point(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Occurrence of Aggression") +
  xlab ("Conspecific Foraging Time (s)") +
  theme_bw()

platform_time_binomial #Call object 

# Recipient age binomial proportion plot
# Calculate proportions
age_target_proportion <- total_data %>%
  group_by(Winter) %>%
  summarise(proportion = mean(Recipient_Occurrence))

# Create bar plot
age_targetted_plot <- ggplot(age_target_proportion, aes(x = Winter , y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  labs(x = "Age", y = "Proportion Targetted") +
  theme_bw()

age_targetted_plot

# Graph recipient occurrence for feeding density
density_recipient_binomial <- ggplot(total_data, aes(Feeding_Density, Recipient_Occurrence)) +
  geom_count(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Target of Aggression") +
  xlab ("Feeding Density") +
  theme_bw()

density_recipient_binomial #Call object 
