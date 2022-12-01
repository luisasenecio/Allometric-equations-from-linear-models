####################################
#                                  #
#      Script on Tutorial 4        #
#      For Data Science in EES     #
#      Luisa-Marie Dickenmann      #
#      Semester 1, 2022            #
#                                  #
###################################################################
#                                                                 #
# This script guides through how to build a regression model      #
# and use the model's output to produce allometric equations      #
# for required variables                                          #    
#                                                                 #
###################################################################

library(tidyverse)
library(readxl)
library(skimr)

raw <- read.csv("Data/Biomass_trees.csv")

# Data wrangling ----
# Filter for chosen species (e.g., Scots pine)
# Remove rows that contain NAs
# Select required explanatory (e.g., DBH) and response (e.g., total live biomass) variables
unique(raw$Species)
trees <- raw %>% 
  filter(Species=="Pinus sylvestris L.") %>% 
  drop_na(Ptot, DBH) %>% 
  select(Species, DBH, Ptot)

range(trees$DBH)
range(trees$Pptot)
min(trees$Ptot)

# Visualise the relationship between response and explanatory variable ---- 
  # Visualise the distribution of the variables to determine whether data transformation is necessary
(distribution_DBH <- ggplot(trees, aes(x=DBH)) +
   geom_histogram(fill="#006400", bins=30) +
   theme_bw())
(distribution_biomass <- ggplot(trees, aes(x=Ptot)) +
    geom_histogram(fill="#006400", bins=50) +
    theme_bw())
  # Both response and explanatory variables are not normally distributed, but right-skewed
  # Since both our variables are not discrete but continuous (i.e., contain decimal points), 
    # sticking with a linear model but transforming the data is the more sensible approach

# Log10 transforming both variables
trees <- trees %>% 
  mutate(logPtot = log10(Ptot)) %>% 
  mutate(logDBH = log10(DBH))
# Careful! Log transformation only works for positive (i.e. non-zero) data. 
# If the data is not that strongly right-skewed, you might find that taking the square root (sqrt(x))
 # is a better option, as it is a 'weaker' transformation

# What does the distribution look like now?
(distribution_logDBH <- ggplot(trees, aes(x=logDBH)) +
    geom_histogram(fill="#1C86EE", bins=30))
(distribution_logbiomass <- ggplot(trees, aes(x=logPtot)) +
    geom_histogram(fill="#1C86EE", bins=50))

# What are the maximum values for both variables?
  # This is useful for setting the limits for the x and y axes should ggplot not set them correctly
max(trees$logDBH)
max(trees$logPtot)

# Visualising the relationship
(DBH_biomass <- ggplot(trees, aes(x = logDBH, y = logPtot)) +
    geom_point(size=2, colour="#528B8B")) +
  geom_smooth(method="", colour="#8F8F8F") +
  theme_bw() + 
  xlab("DBH (cm)") +
  ylab("Total aboveground biomass (kg)")  +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        panel.grid=element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,0.5), units = , "cm"),
        axis.title.x=element_text(margin = margin(t = 10)),
        axis.title.y=element_text(margin = margin(r = 10)),
        axis.title=element_text(size=14)
  ) +
  xlim(0,2) +
  ylim(0,4)
  # The relationship now looks linear and we can use a linear model to quantify the relationship between the variables


# Making the model ----
  # Original data
DBH_biomass_mod_original <- lm(Ptot~DBH, data=trees)
anova(DBH_biomass_mod_original)
summary(DBH_biomass_mod_original)

  # Transformed data
DBH_biomass_mod_transformed <- lm(logPtot~logDBH, data=trees)
  # Extract coefficients
summary(DBH_biomass_mod_transformed)

# The model with the original data has a larger standard error
# The p-value is the same for both the original and the transformed data

# Assess whether our models follow the assumptions of linear models
  # Original data
qqnorm(DBH_biomass_mod_original$residuals, main = 'Q-Q Plot Original Data')
qqline(DBH_biomass_mod_original$residuals)

  # Transformed data
qqnorm(DBH_biomass_mod_transformed$residuals, main = 'Q-Q Plot Transformed Data')
qqline(DBH_biomass_mod_transformed$residuals)

# Back-transform the data to make units of predictions sensible
trees <- trees %>% 
  mutate(reversePtot = 10^log(Ptot)) %>% 
  mutate(reverseDBH = 10^log(DBH))
  # The back-transformed data should match the original data

# ALLOMETRIC EUQATIONS ---- 
  # Total live biomass = slope * DBH + intercept
  # To predict the response variable (total live biomass) with explanatory variable (DBH), 
    # we need the slope and the intercept of the model's regression line
  # Intercept = -1.13
  # Slope = 2.52
  # Because we have log-transformed our data, some values will be negative. 
  # The allometric equation is therefore:
    # Total live biomass = 2.52 * DBH + (-1.13)
  
  
# ERROR QUANTIFICATION ----
  # We do have an equation now, but we need to know how good our predictions are
  # For this, we can compare the actual measured data with what our model would 
    # predict the response variable to be and produce a root mean squared error (RMSE)

# Data frame with observed explanatory and response variables
  # Live biomass is predicted using observed log-transformed data
  # The difference between the observed and predicted data is calculated

RMSE_calculations <- trees %>% 
    select(logPtot, logDBH) %>%
    mutate(intercept=-1.13, slope=2.52, predicted_biomass=slope*logDBH+intercept,
           difference=predicted_biomass-logPtot, reverse_difference=10^log(difference))

  # RMSE = the square root of the sum of all differences divided by the number of observations
RMSE_log10 <- (sqrt(sum(RMSE_calculations$difference))/length(RMSE_calculations$difference))

RMSE <- 10^log(RMSE)
