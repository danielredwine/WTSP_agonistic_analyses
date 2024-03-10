# Daniel Redwine
# WTSP Plots

# Clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(patchwork)

# Import dataset
agonistic_analysis_data <- read.csv("data/agonistic_analysis_data.csv")

# Clean up dataset by forcing factors and numbers
agonistic_analysis_data$PCRsex <- as.factor(agonistic_analysis_data$PCRsex)
agonistic_analysis_data$PCRMorph <- as.factor(agonistic_analysis_data$PCRMorph)
agonistic_analysis_data$Winter <- as.factor(agonistic_analysis_data$Winter)
agonistic_analysis_data$Wing <- as.numeric(agonistic_analysis_data$Wing)
agonistic_analysis_data$Agonistic_Rate <- as.numeric(agonistic_analysis_data$Agonistic_Rate)
agonistic_analysis_data$Lunge_Rate <- as.numeric(agonistic_analysis_data$Lunge_Rate)
agonistic_analysis_data$Lunge_Occurrence <- as.numeric(agonistic_analysis_data$Lunge_Occurrence)
agonistic_analysis_data$Recipient_Occurrence <- as.numeric(agonistic_analysis_data$Recipient_Occurrence)
agonistic_analysis_data$Recipient_rate <- as.numeric(agonistic_analysis_data$Recipient_rate)
agonistic_analysis_data$Feeding_Density <- as.numeric(agonistic_analysis_data$Feeding_Density)



# Clean out NA and X
total_data <- agonistic_analysis_data %>%
  dplyr::select(SampleID, Winter, Wing, PCRsex, PCRMorph, Agonistic_Rate, Platform_Time, Aggressor_Occurrence, Recipient_Occurrence, 
                Recipient_rate, Feeding_Density) %>%
  na.omit() %>%
  filter(Winter == "FW" | Winter == "AFW") %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS") %>%
  filter(PCRsex == "M" | PCRsex == "F")

# Create data for sex data even if other fields are missing
sex_data <- agonistic_analysis_data %>%
  dplyr::select(SampleID, PCRsex, Agonistic_Rate, Lunge_Rate, Lunge_Occurrence,
                Aggressor_Occurrence) %>%
  filter(PCRsex == "M" | PCRsex == "F")

# Create data for morph data even if other fields are missing
morph_data <- agonistic_analysis_data %>%
  dplyr::select(SampleID, PCRMorph, Agonistic_Rate, Lunge_Rate, Lunge_Occurrence,
                Aggressor_Occurrence) %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS")

# create data for age data even if other fields are missing
age_data <- agonistic_analysis_data %>%
  dplyr::select(SampleID, Winter, Agonistic_Rate, Lunge_Rate, Lunge_Occurrence,
                Aggressor_Occurrence) %>%
  filter(Winter == "FW" | Winter == "AFW")

# create data for wing data even if other fields are missing
wing_data <- agonistic_analysis_data %>%
  dplyr::select(SampleID, Wing, Agonistic_Rate, Lunge_Rate, Lunge_Occurrence,
                Aggressor_Occurrence)

# boxplot for sex data
sex_agonistic <- ggplot(sex_data, aes(PCRsex, Agonistic_Rate, fill = PCRsex)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Sex") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

sex_agonistic

ggsave("output/agonistic_sex.png")

# boxplot for morph data 
morph_agonistic <- ggplot(morph_data, aes(PCRMorph, Agonistic_Rate, fill = PCRMorph)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Morph") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

morph_agonistic

ggsave("output/agonistic_morph.png")

# boxplot for age data
age_agonistic <- ggplot(age_data, aes(Winter, Agonistic_Rate, fill = Winter)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Age") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

age_agonistic

ggsave("output/agonistic_age.png")

# scatterplot for wing data
wing_agonistic <- ggplot(total_data, aes(Wing, Agonistic_Rate)) + 
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab("Interactions/sec") +
  xlab("Wing Chord (mm)")

wing_agonistic

ggsave("output/agonistic_wing.png")

# scatterplot for density data
density_agonistic <- ggplot(total_data, aes(Feeding_Density, Agonistic_Rate)) + 
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab("Interactions/sec") +
  xlab("Feeding Density")

density_agonistic

ggsave("output/agonistic_density.png")

# histogram for rate of agonistic behavior
agonistic_histogram <- ggplot(total_data, aes(x = Agonistic_Rate)) +
  geom_histogram(binwidth = 0.025, colour="black", fill="skyblue") +
  theme_bw()+
  ylab("Count") +
  xlab("Interactions/sec")

agonistic_histogram

ggsave("output/agonistic_histogram.png")

# Bar chart for sex aggression occurrence
# Organize data for plotting/analysis
sex_count_data <- total_data %>%
  count(PCRsex, Aggressor_Occurrence)

sex_count_data$Aggressor_Occurrence <- as.factor(sex_count_data$Aggressor_Occurrence)

sex_count_data <- sex_count_data %>%
  mutate(Aggressor_Occurrence = ifelse(Aggressor_Occurrence == "1", 
                                       "Aggressor", "Nonaggressor"))
  
# Graph using bar chart 
sex_aggressor_count <- ggplot(sex_count_data, aes(x = PCRsex, y = n, 
                                                       fill = Aggressor_Occurrence)) +
  geom_bar(stat = 'identity', position = 'fill') +
  xlab("Sex") +
  ylab("Proportion") + 
  theme_bw()+
  theme(legend.title=element_blank())

sex_aggressor_count

ggsave("output/sex_aggressor_count.png")

# Bar chart for morph aggression occurrence
# Organize data for plotting/analysis
morph_count_data <- total_data %>%
  count(PCRMorph, Aggressor_Occurrence)

morph_count_data$Aggressor_Occurrence <- as.factor(morph_count_data$Aggressor_Occurrence)

morph_count_data <- morph_count_data %>%
  mutate(Aggressor_Occurrence = ifelse(Aggressor_Occurrence == "1", 
                                       "Aggressor", "Nonaggressor"))

# Graph using bar chart 
morph_aggressor_count <- ggplot(morph_count_data, aes(x = PCRMorph, y = n, 
                                                  fill = Aggressor_Occurrence)) +
  geom_bar(stat = 'identity', position = 'fill') +
  xlab("Morph") +
  ylab("Proportion") + 
  theme_bw()+
  theme(legend.title=element_blank())

morph_aggressor_count

ggsave("output/morph_aggressor_count.png")

# Bar chart for age aggression occurrence
# Organize data for plotting/analysis
age_count_data <- total_data %>%
  count(Winter, Aggressor_Occurrence)

age_count_data$Aggressor_Occurrence <- as.factor(age_count_data$Aggressor_Occurrence)

age_count_data <- age_count_data %>%
  mutate(Aggressor_Occurrence = ifelse(Aggressor_Occurrence == "1", 
                                       "Aggressor", "Nonaggressor"))

# Graph using bar chart 
age_aggressor_count <- ggplot(age_count_data, aes(x = Winter, y = n, 
                                                  fill = Aggressor_Occurrence)) +
  geom_bar(stat = 'identity', position = 'fill') +
  xlab("Age") +
  ylab("Proportion") + 
  theme_bw()+
  theme(legend.title=element_blank())

age_aggressor_count

ggsave("output/age_aggressor_count.png")

# Graph aggressor occurrence for wing 
wing_binomial <- ggplot(total_data, aes(Wing, Aggressor_Occurrence)) +
  geom_count(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
    color = "slateblue", fill = "lightskyblue2") +
  ylab ("Occurrence of Aggression") +
  xlab ("Wing Chord (mm)") +
  theme_bw()
# Create a graph with male/female (1/0) on y and wing chord on x 

wing_binomial #Call object 

ggsave("output/wing_binomial.png")

# Graph aggressor occurrence for feeding density
density_binomial <- ggplot(total_data, aes(Feeding_Density, Aggressor_Occurrence)) +
  geom_count(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Occurrence of Aggression") +
  xlab ("Feeding Density") +
  theme_bw()
# Create a graph with male/female (1/0) on y and wing chord on x 

density_binomial #Call object 

ggsave("output/density_binomial.png")

# Graph aggressor occurrence for platform time
platform_time_binomial <- ggplot(total_data, aes(Platform_Time, Aggressor_Occurrence)) +
  geom_point(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Occurrence of Aggression") +
  xlab ("Conspecific Feeding Time (s)") +
  theme_bw()
# Create a graph with male/female (1/0) on y and wing chord on x 

platform_time_binomial #Call object 

ggsave("output/platform_time_binomial.png")

# looking at total interactions with nonzero data
sex_nonzero <- sex_data %>%
  filter(Agonistic_Rate != 0)

morph_nonzero <- morph_data %>%
  filter(Agonistic_Rate != 0)

age_nonzero <- age_data %>%
  filter(Agonistic_Rate != 0)

wing_nonzero <- wing_data %>%
  filter(Agonistic_Rate != 0)

# boxplot for sex nonzero data
sex_agonistic_nonzero <- ggplot(sex_nonzero, aes(PCRsex, Agonistic_Rate, fill = PCRsex)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Sex") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

sex_agonistic_nonzero

ggsave("output/agonistic_sex_nonzero.png")

# boxplot for nonzero morph data 
morph_agonistic_nonzero <- ggplot(morph_nonzero, aes(PCRMorph, Agonistic_Rate, 
    fill = PCRMorph)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Morph") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

morph_agonistic_nonzero

ggsave("output/agonistic_morph_nonzero.png")

# boxplot for nonzero age data
age_agonistic_nonzero <- ggplot(age_nonzero, aes(Winter, Agonistic_Rate, fill = Winter)) + 
  geom_boxplot() +
  theme_bw() +
  xlab("Age") +
  ylab("Interactions/sec") +
  theme(legend.position  = "none")

age_agonistic_nonzero

ggsave("output/agonistic_age_nonzero.png")

# scatterplot for nonzero wing data
wing_agonistic_nonzero <- ggplot(wing_nonzero, aes(Wing, Agonistic_Rate)) + 
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab("Interactions/sec") +
  xlab("Wing Chord (mm)")

wing_agonistic_nonzero

ggsave("output/agonistic_wing_nonzero.png")

# Aggression of individuals in the study
individual_aggression <- total_data %>%
  filter(SampleID != "") %>%
  group_by(SampleID, PCRsex, PCRMorph) %>%
  summarise(
    mean_agonistic = mean(Agonistic_Rate),
    se_agonistic = sd(Agonistic_Rate)/sqrt(n()))

# Reorder to plot
individual_aggression_plot <- ggplot(individual_aggression, aes(x = reorder(SampleID,
    -mean_agonistic), y = mean_agonistic)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill ="skyblue") +
  geom_errorbar(aes(ymin = mean_agonistic - se_agonistic,
                    ymax = mean_agonistic + se_agonistic), width = 0.1) +
  theme_bw() +
  ylab("Mean Agonistic Rate (interactions/s)") +
  xlab("Sample ID") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

individual_aggression_plot

ggsave("output/individual_aggression_plot.png")

# Bar charts from mean data
sex_agonistic_summary <- total_data %>%
  filter(PCRsex == "M" | PCRsex == "F") %>%
  group_by(PCRsex) %>%
  summarise(mean_agonistic = mean(Agonistic_Rate), se_agonistic = 
              sd(Agonistic_Rate/sqrt(n())))

sex_agonistic_bar <- ggplot(sex_agonistic_summary, aes(x = PCRsex, y = mean_agonistic)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_agonistic - se_agonistic,
                    ymax = mean_agonistic + se_agonistic), width = 0.1) +
  theme_bw() +
  xlab("Sex") +
  ylab("Mean Agonistic Rate (interactions/s)")

sex_agonistic_bar

ggsave("output/sex_agonistic_bar.png")

morph_agonistic_summary <- total_data %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS") %>%
  group_by(PCRMorph) %>%
  summarise(mean_agonistic = mean(Agonistic_Rate), se_agonistic = 
              sd(Agonistic_Rate/sqrt(n())))

morph_agonistic_bar <- ggplot(morph_agonistic_summary, aes(x = PCRMorph, 
        y = mean_agonistic)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_agonistic - se_agonistic,
                    ymax = mean_agonistic + se_agonistic), width = 0.1) +
  theme_bw() +
  xlab("Morph") +
  ylab("Mean Agonistic Rate (interactions/s)")

morph_agonistic_bar

ggsave("output/morph_agonistic_bar.png")

age_agonistic_summary <- total_data %>%
  filter(Winter == "FW" | Winter == "AFW") %>%
  group_by(Winter) %>%
  summarise(mean_agonistic = mean(Agonistic_Rate), se_agonistic = 
              sd(Agonistic_Rate/sqrt(n())))

age_agonistic_bar <- ggplot(age_agonistic_summary, aes(x = Winter, y = mean_agonistic)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_agonistic - se_agonistic,
                    ymax = mean_agonistic + se_agonistic), width = 0.1) +
  theme_bw() +
  xlab("Age") +
  ylab("Mean Agonistic Rate (interactions/s)")

age_agonistic_bar

ggsave("output/age_agonistic_bar.png")

# count of number of times each individual occurs at each platform
individual_platform_count <- agonistic_analysis_data %>%
  count(SampleID, Platform) %>%
  filter(SampleID != "")

# Reorder to plot
individual_platform_plot <- ggplot(individual_platform_count, 
      aes(x = reorder(SampleID, -n),y = n, fill = Platform)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() +
  ylab("Count") +
  xlab("Sample ID") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

individual_platform_plot

ggsave("output/individual_platform_plot.png")

# histogram for rate of recipient
recipient_histogram <- ggplot(total_data, aes(x = Recipient_rate)) +
  geom_histogram(binwidth = 0.025, colour="black", fill="skyblue") +
  theme_bw()+
  ylab("Count") +
  xlab("Recipient of aggression/sec")

recipient_histogram

ggsave("output/recipient_histogram.png")

# Bar chart for sex recipient occurrence
# Organize data for plotting/analysis
sex_count_rec_data <- total_data %>%
  count(PCRsex, Recipient_Occurrence)

sex_count_rec_data$Recipient_Occurrence <- as.factor(sex_count_rec_data$Recipient_Occurrence)

sex_count_rec_data <- sex_count_rec_data %>%
  mutate(Recipient_Occurrence = ifelse(Recipient_Occurrence == "1", 
                                       "Recipient", "Nonrecipient"))

# Graph using bar chart 
sex_recipient_count <- ggplot(sex_count_rec_data, aes(x = PCRsex, y = n, 
                                                  fill = Recipient_Occurrence)) +
  geom_bar(stat = 'identity', position = 'fill') +
  xlab("Sex") +
  ylab("Proportion") + 
  theme_bw()+
  theme(legend.title=element_blank())

sex_recipient_count

ggsave("output/sex_recipient_count.png")

# Bar chart for morph recipient occurrence
# Organize data for plotting/analysis
morph_count_rec_data <- total_data %>%
  count(PCRMorph, Recipient_Occurrence)

morph_count_rec_data$Recipient_Occurrence <- as.factor(morph_count_rec_data$Recipient_Occurrence)

morph_count_rec_data <- morph_count_rec_data %>%
  mutate(Recipient_Occurrence = ifelse(Recipient_Occurrence == "1", 
                                       "Recipient", "Nonrecipient"))

# Graph using bar chart 
morph_recipient_count <- ggplot(morph_count_rec_data, aes(x = PCRMorph, y = n, 
                                                      fill = Recipient_Occurrence)) +
  geom_bar(stat = 'identity', position = 'fill') +
  xlab("Morph") +
  ylab("Proportion") + 
  theme_bw()+
  theme(legend.title=element_blank())

morph_recipient_count

ggsave("output/morph_recipient_count.png")

# Bar chart for age recipient occurrence
# Organize data for plotting/analysis
age_count_rec_data <- total_data %>%
  count(Winter, Recipient_Occurrence)

age_count_rec_data$Recipient_Occurrence <- as.factor(age_count_rec_data$Recipient_Occurrence)

age_count_rec_data <- age_count_rec_data %>%
  mutate(Recipient_Occurrence = ifelse(Recipient_Occurrence == "1", 
                                       "Recipient", "Nonrecipient"))

# Graph using bar chart 
age_recipient_count <- ggplot(age_count_rec_data, aes(x = Winter, y = n, 
                                                  fill = Recipient_Occurrence)) +
  geom_bar(stat = 'identity', position = 'fill') +
  xlab("Age") +
  ylab("Proportion") + 
  theme_bw()+
  theme(legend.title=element_blank())

age_recipient_count

ggsave("output/age_recipient_count.png")

# Graph recipient occurrence for wing 
wing_recipient_binomial <- ggplot(total_data, aes(Wing, Recipient_Occurrence)) +
  geom_count(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Recipient of Aggression") +
  xlab ("Wing Chord (mm)") +
  theme_bw()
# Create a graph with male/female (1/0) on y and wing chord on x 

wing_recipient_binomial #Call object 

ggsave("output/wing_recipient_binomial.png")

# Graph recipient occurrence for feeding density
density_recipient_binomial <- ggplot(total_data, aes(Feeding_Density, Recipient_Occurrence)) +
  geom_count(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Recipient of Aggression") +
  xlab ("Feeding Density") +
  theme_bw()
# Create a graph with male/female (1/0) on y and wing chord on x 

density_recipient_binomial #Call object 

ggsave("output/density_recipient_binomial.png")

# Graph recipient occurrence for platform time
platform_time_recipient_binomial <- ggplot(total_data, aes(Platform_Time, Recipient_Occurrence)) +
  geom_point(show.legend = FALSE) + # Geom count changes size of points to count
  geom_smooth(method="glm", method.args=list(family="binomial"(link="logit")), 
              color = "slateblue", fill = "lightskyblue2") +
  ylab ("Recipient of Aggression") +
  xlab ("Conspecific Feeding Time (s)") +
  theme_bw()
# Create a graph with male/female (1/0) on y and wing chord on x 

platform_time_recipient_binomial #Call object 

ggsave("output/platform_time_recipient_binomial.png")

# Bar charts from mean data
sex_recipient_summary <- total_data %>%
  filter(PCRsex == "M" | PCRsex == "F") %>%
  group_by(PCRsex) %>%
  summarise(mean_recipient = mean(Recipient_rate), se_recipient = 
              sd(Recipient_rate/sqrt(n())))

sex_recipient_bar <- ggplot(sex_recipient_summary, aes(x = PCRsex, y = mean_recipient)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_recipient - se_recipient,
                    ymax = mean_recipient + se_recipient), width = 0.1) +
  theme_bw() +
  xlab("Sex") +
  ylab("Mean Recipient Rate (interactions/s)")

sex_recipient_bar

ggsave("output/sex_recipient_bar.png")

morph_recipient_summary <- total_data %>%
  filter(PCRMorph == "WS" | PCRMorph == "TS") %>%
  group_by(PCRMorph) %>%
  summarise(mean_recipient = mean(Recipient_rate), se_recipient = 
              sd(Recipient_rate/sqrt(n())))

morph_recipient_bar <- ggplot(morph_recipient_summary, aes(x = PCRMorph, 
                                                           y = mean_recipient)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_recipient - se_recipient,
                    ymax = mean_recipient + se_recipient), width = 0.1) +
  theme_bw() +
  xlab("Morph") +
  ylab("Mean Recipient Rate (interactions/s)")

morph_recipient_bar

ggsave("output/morph_recipinet_bar.png")

age_recipient_summary <- total_data %>%
  filter(Winter == "FW" | Winter == "AFW") %>%
  group_by(Winter) %>%
  summarise(mean_recipient = mean(Recipient_rate), se_recipient = 
              sd(Recipient_rate/sqrt(n())))

age_recipient_bar <- ggplot(age_recipient_summary, aes(x = Winter, y = mean_recipient)) +
  geom_bar(stat = "identity", position = "dodge", colour = "black", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_recipient - se_recipient,
                    ymax = mean_recipient + se_recipient), width = 0.1) +
  theme_bw() +
  xlab("Age") +
  ylab("Mean Recipient Rate (interactions/s)")

age_recipient_bar

ggsave("output/age_recipient_bar.png")

# scatterplot for wing data
wing_recipient <- ggplot(total_data, aes(Wing, Recipient_rate)) + 
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab("Recipient Interactions/sec") +
  xlab("Wing Chord (mm)")

wing_recipient

ggsave("output/recipient_wing.png")

# scatterplot for density data
density_recipient <- ggplot(total_data, aes(Feeding_Density, Recipient_rate)) + 
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab("Recipient Interactions/sec") +
  xlab("Feeding Density")

density_recipient

ggsave("output/recipient_density.png")
