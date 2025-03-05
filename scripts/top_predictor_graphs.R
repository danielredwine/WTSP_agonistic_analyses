# Daniel Redwine
# Plots of top model predictors
# Density for binomial aggressors
# Age and feeding density for binomial recipient
# Null for aggressor truncated poisson
# sex, morph, and interaction for truncated poisson

# Clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(patchwork)
library(MASS)

# Import dataset
total_data <- read.csv("data/total_data.csv")

total_data <- total_data %>%
  mutate(Winter = recode(Winter, "FW" = "First-winter", "AFW" = "After-first-winter")) %>%
  mutate(PCRsex = recode(PCRsex, "M" = "Male", "F" = "Female")) %>%
  mutate(PCRMorph = recode(PCRMorph, "WS" = "White-stripe", "TS" = "Tan-stripe"))

# Create nonzero datasets
aggressor_nonzero_data <- total_data %>%
  filter(Total_Agonistic > 0)

target_nonzero_data <- total_data %>%
  filter(Total_Recipient > 0)

# Aggressor offset graph for platform time
# Graph aggressor occurrence for platform time
aggressor_time_offset <- ggplot(total_data, aes(Platform_Time, Total_Agonistic)) +
  geom_point(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="lm",, 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Aggression count") +
  xlab ("Foraging time (s)") +
  theme_bw()

aggressor_time_offset #Call object 

ggsave("output/significant_predictors/aggressor_time_offset.png") # Save object

# Target offset graph for platform time
# Graph target count for platform time
target_time_offset <- ggplot(total_data, aes(Platform_Time, Total_Recipient)) +
  geom_point(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="lm",, 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Target count") +
  xlab ("Foraging time (s)") +
  theme_bw()

target_time_offset #Call object 

ggsave("output/significant_predictors/target_time_offset.png") # Save object

# Graph aggressor occurrence for feeding density
aggressor_density_binomial <- ggplot(total_data, aes(Feeding_Density, Aggressor_Occurrence)) +
  geom_count(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Aggressor occurrence") +
  xlab ("Foraging density") +
  theme_bw()

aggressor_density_binomial #Call object 

ggsave("output/significant_predictors/aggressor_density_binomial.png")

# Recipient age binomial proportion plot
# Calculate proportions
age_target_proportion <- total_data %>%
  group_by(Winter) %>%
  summarise(proportion = mean(Recipient_Occurrence))

# Create bar plot
target_age_binomial <- ggplot(age_target_proportion, aes(x = Winter , y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  labs(x = "Age", y = "Proportion targeted") +
  theme_bw()

target_age_binomial # call object

ggsave("output/significant_predictors/target_age_binomial.png") # save object

# Aggressor age binomial proportion plot
# Calculate proportions
age_aggressor_proportion <- total_data %>%
  group_by(Winter) %>%
  summarise(proportion = mean(Aggressor_Occurrence))

# Create bar plot
aggressor_age_binomial <- ggplot(age_aggressor_proportion, aes(x = Winter , y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  labs(x = "Age", y = "Proportion of aggressors") +
  theme_bw()

aggressor_age_binomial # call object

ggsave("output/significant_predictors/aggressor_age_binomial.png") # save object

# Graph recipient occurrence for feeding density
target_density_binomial <- ggplot(total_data, aes(Feeding_Density, Recipient_Occurrence)) +
  geom_count(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Target occurrence") +
  xlab ("Foraging density") +
  theme_bw()

target_density_binomial #Call object 

ggsave("output/significant_predictors/target_density_binomial.png") # save object

# graph target rate by sex for nonzero data 
# create summary data 
target_sex_summary_nonzero <- target_nonzero_data %>%
  group_by(PCRsex) %>%
  summarise(mean_recipient = mean(Total_Recipient/Platform_Time), se_recipient = 
              sd((Total_Recipient/Platform_Time)/sqrt(n())))

# create bar chart 
target_sex_rate_nonzero <- ggplot(target_sex_summary_nonzero, 
                                  aes(x = PCRsex, y = mean_recipient)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_recipient - se_recipient,
                    ymax = mean_recipient + se_recipient), width = 0.1) +
  theme_bw() +
  xlab("Sex") +
  ylab("Mean rate targeted \u00B1 SE")

target_sex_rate_nonzero # call 

ggsave("output/significant_predictors/target_sex_rate_nonzero.png") # save 

# graph target rate by morph for nonzero data 
# create summary data 
target_morph_summary_nonzero <- target_nonzero_data %>%
  group_by(PCRMorph) %>%
  summarise(mean_recipient = mean(Total_Recipient/Platform_Time), se_recipient = 
              sd((Total_Recipient/Platform_Time)/sqrt(n())))

# create bar chart 
target_morph_rate_nonzero <- ggplot(target_morph_summary_nonzero, 
                                  aes(x = PCRMorph, y = mean_recipient)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_recipient - se_recipient,
                    ymax = mean_recipient + se_recipient), width = 0.1) +
  theme_bw() +
  xlab("Morph") +
  ylab("Mean rate targeted \u00B1 SE")

target_morph_rate_nonzero # call 

ggsave("output/significant_predictors/target_morph_rate_nonzero.png") # save 

sex_count <- total_data %>%
  count(PCRsex)

morph_count <- total_data %>%
  count(PCRMorph)


age_count <- total_data %>%
  count(Winter)

total_count <- sum(sex_count$n)

sex_count <- sex_count %>%
  mutate(proportion = n / total_count)

morph_count <- morph_count %>%
  mutate(proportion = n / total_count)

age_count <- age_count %>%
  mutate(proportion = n / total_count)

sex_ratio <- ggplot(sex_count, aes(x = PCRsex , y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  labs(x = "Sex", y = "Observation proportion") +
  theme_bw()

sex_ratio # call object

ggsave("output/significant_predictors/observation_sex_ratio.png") # save object

morph_ratio <- ggplot(morph_count, aes(x = PCRMorph , y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  labs(x = "Morph", y = "Observation proportion") +
  theme_bw()

morph_ratio # call object

ggsave("output/significant_predictors/observation_morph_ratio.png") # save object

age_ratio <- ggplot(age_count, aes(x = Winter , y = proportion)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  labs(x = "Age", y = "Observation Proportion") +
  theme_bw()

age_ratio # call object

ggsave("output/significant_predictors/observation_age_ratio.png") # save object

# Plots for recipient interaction effects
# Sex and morph
# Summary
sex_morph_recipient_summary <- target_nonzero_data %>%
  group_by(PCRsex, PCRMorph) %>%
  summarise(mean_recipient = mean(Total_Recipient/Platform_Time), se_recipient = 
              sd((Total_Recipient/Platform_Time)/sqrt(n())))

# Plot                
sex_morph_recipient_bar <- ggplot(sex_morph_recipient_summary, 
                                  aes(x = PCRsex, y = mean_recipient, fill = PCRMorph)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_recipient - se_recipient,
                    ymax = mean_recipient + se_recipient), 
                width = 0.2, 
                position = position_dodge(width = 0.9),
                color = "black") +
  theme_bw() +
  xlab("Sex") +
  ylab("Mean rate targeted \u00B1 SE") +
  labs(fill = "Morph") +
  scale_fill_manual(values = c("aquamarine4", "skyblue"))

# Call
sex_morph_recipient_bar

# Save
ggsave("output/significant_predictors/sex_morph_recipient_bar.png", width = 6, height = 4)
