# Daniel Redwine
# Plots of top model predictors

# Clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(patchwork)
library(MASS)

# Import dataset
total_data <- read.csv("data/total_data.csv")

# Create nonzero datasets
aggressor_nonzero_data <- total_data %>%
  filter(Total_Agonistic > 0)

target_nonzero_data <- total_data %>%
  filter(Total_Recipient > 0)

# Aggressor Binomial model graph for platform time
# Graph aggressor occurrence for platform time
aggressor_time_binomial <- ggplot(total_data, aes(Platform_Time, Aggressor_Occurrence)) +
  geom_point(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Aggression Occurrence") +
  xlab ("Foraging Time (s)") +
  theme_bw()

aggressor_time_binomial #Call object 

ggsave("output/significant_predictors/aggressor_time_binomial.png") # Save object

# nonzero scatterplot for density data and rate of aggression
# Rate is obtained by dividing total aggression by log platform time
aggressor_density_rate_nonzero <- ggplot(aggressor_nonzero_data, 
                            aes(Feeding_Density, Total_Agonistic/log(Platform_Time))) + 
  geom_point() +
  geom_smooth(method="glm.nb", 
              color = "slateblue", fill = "lightskyblue2") +
  theme_bw() +
  ylab("Aggression Rate") +
  xlab("Foraging Density") +
  coord_cartesian(xlim = c(2, 8), ylim = c(0, 1.5))

aggressor_density_rate_nonzero # call object

ggsave("output/significant_predictors/aggressor_density_rate_nonzero.png") # save object

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

# Graph recipient occurrence for feeding density
target_density_binomial <- ggplot(total_data, aes(Feeding_Density, Recipient_Occurrence)) +
  geom_count(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Target Occurrence") +
  xlab ("Foraging Density") +
  theme_bw()

target_density_binomial #Call object 

ggsave("output/significant_predictors/target_density_binomial.png") # save object

# target binomial model graph for platform time
# Graph target occurrence for platform time
target_time_binomial <- ggplot(total_data, aes(Platform_Time, Recipient_Occurrence)) +
  geom_point(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Target Occurrence") +
  xlab ("Foraging Time (s)") +
  theme_bw()

target_time_binomial #Call object 

ggsave("output/significant_predictors/target_time_binomial.png") # save object

# nonzero scatterplot for density data and target rate
target_density_rate_nonzero <- ggplot(target_nonzero_data, 
                            aes(Feeding_Density, Total_Recipient/log(Platform_Time))) + 
  geom_point() +
  geom_smooth(method="glm.nb", 
              color = "slateblue", fill = "lightskyblue2") +
  theme_bw() +
  ylab("Rate targeted") +
  xlab("Feeding Density") +
  coord_cartesian(xlim = c(2, 8), ylim = c(0, 3))

target_density_rate_nonzero # call

ggsave("output/significant_predictors/target_density_rate_nonzero.png") # save 


# graph target rate by age for nonzero data 
# create summary data 
target_age_summary_nonzero <- target_nonzero_data %>%
  filter(Winter == "FW" | Winter == "AFW") %>%
  group_by(Winter) %>%
  summarise(mean_recipient = mean(Total_Recipient/log(Platform_Time)), se_recipient = 
              sd((Total_Recipient/log(Platform_Time))/sqrt(n())))

# create bar chart 
target_age_rate_nonzero <- ggplot(target_age_summary_nonzero, 
                                  aes(x = Winter, y = mean_recipient)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_recipient - se_recipient,
                    ymax = mean_recipient + se_recipient), width = 0.1) +
  theme_bw() +
  xlab("Age") +
  ylab("Rate targeted")

target_age_rate_nonzero # call 

ggsave("output/significant_predictors/target_age_rate_nonzero.png") # save 

# graph target rate by age for all data 
# create summary
target_age_summary <- total_data %>%
  filter(Winter == "FW" | Winter == "AFW") %>%
  group_by(Winter) %>%
  summarise(mean_recipient = mean(Total_Recipient/log(Platform_Time)), se_recipient = 
              sd((Total_Recipient/log(Platform_Time))/sqrt(n())))

# create bar chart
target_age_rate <- ggplot(target_age_summary, 
                                  aes(x = Winter, y = mean_recipient)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_recipient - se_recipient,
                    ymax = mean_recipient + se_recipient), width = 0.1) +
  theme_bw() +
  xlab("Age") +
  ylab("Rate targeted")

target_age_rate # call

ggsave("output/significant_predictors/target_age_rate.png") # save 

#scatterplot for density data and target rate for all data 
target_density_rate <- ggplot(total_data, 
                            aes(Feeding_Density, Total_Recipient/log(Platform_Time))) + 
  geom_point() +
  geom_smooth(method="glm.nb", 
              color = "slateblue", fill = "lightskyblue2") +
  theme_bw() +
  ylab("Rate targeted") +
  xlab("Foraging Density") +
  coord_cartesian(xlim = c(2, 8), ylim = c(0, 3))

target_density_rate # call

ggsave("output/significant_predictors/target_density_rate.png") # save 

# scatterplot for wing data adjusted
target_wing_rate <- ggplot(total_data, 
                           aes(adjusted_wing, Total_Recipient/log(Platform_Time))) + 
  geom_count(show.legend = FALSE) +
  geom_smooth(method="glm.nb", 
              color = "slateblue", fill = "lightskyblue2") +
  theme_bw() +
  ylab("Rate targeted") +
  xlab("Adjusted Wing") +
  coord_cartesian(ylim = c( 0 , 1.75))

target_wing_rate # call

ggsave("output/significant_predictors/target_wing_rate.png") # save

# scatterplot for wing data adjusted
target_wing_rate_nonzero <- ggplot(target_nonzero_data, 
                           aes(adjusted_wing, Total_Recipient/log(Platform_Time))) + 
  geom_point(show.legend = FALSE) +
  geom_smooth(method="glm.nb", 
              color = "slateblue", fill = "lightskyblue2") +
  theme_bw() +
  ylab("Rate targeted") +
  xlab("Adjusted Wing") +
  coord_cartesian(ylim = c( 0 , 3))

target_wing_rate_nonzero # call

ggsave("output/significant_predictors/target_wing_rate_nonzero.png") # save

sex_count <- total_data %>%
  count(PCRsex)

morph_count <- total_data %>%
  count(PCRMorph)

age_count <- total_data %>%
  count(Winter)

sex_ratio <- ggplot(sex_count, aes(x = PCRsex , y = n)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  labs(x = "Age", y = "Count") +
  theme_bw()

sex_ratio # call object

ggsave("output/significant_predictors/sex_ratio.png") # save object

morph_ratio <- ggplot(morph_count, aes(x = PCRMorph , y = n)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  labs(x = "Morph", y = "Count") +
  theme_bw()

morph_ratio # call object

ggsave("output/significant_predictors/morph_ratio.png") # save object

age_ratio <- ggplot(age_count, aes(x = Winter , y = n)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  labs(x = "Age", y = "Count") +
  theme_bw()

age_ratio # call object

ggsave("output/significant_predictors/age_ratio.png") # save object