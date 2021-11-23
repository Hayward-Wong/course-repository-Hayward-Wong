# Challenge 3 Statistical Modelling
# Data Science in EES 2021
# Script written by Hayward Wong
# Using starter script from Isla Myers-Smith
# 16th November 2021
# Starter code ----
# Libraries
library(tidyverse)
library(MCMCglmm)
library(ggplot2)
library(ggeffects)
library(GGally)
library(lme4)
library(MuMIn)
library(sjPlot)

# Load Living Planet Data
load("data/LPI_species.Rdata")

# Choose your species from this list - don't forget to register your species
# on the issue for the challenge and check that no other student has chosen 
# the species already. Every student must choose a different species!

# Check species in the data
unique(LPI_species$Common.Name)

# Filter your species here

Goose <-  LPI_species %>%
  filter(Common.Name == "Canada goose") %>%  # keep goose observations only
  mutate(year = as.character(year)) %>% # Mutate year into character
  mutate(year = parse_number(year)) %>% # Mutate year into numeric
  group_by(id) 

# Check Countries in the goose data
Goose %>%
  group_by(Country.list) %>%
  summarize(n = n()) %>%
  ungroup() # 552 observations in Canada, 234 observations in the US, and 114 between US and CA. Some more in other countries

US_goose <-  Goose %>%
  filter(Country.list == "United States") %>%  # keep observations in US only
  mutate(year = as.character(year)) %>% # Mutate year into character
  mutate(year = parse_number(year)) %>% # Mutate year into numeric
  group_by(id) 

# Check sum of Canada Goose in the US
sum(US_goose$pop) #39183100 individuals

# Check sources of data
US_goose %>%
  group_by(Data.source.citation) %>%
  summarize(n = n()) %>%
  ungroup() # Mostly from Rusch, et al. (1995). Canada Geese in North America

# Check sources in the goose data
US_goose %>%
  group_by(Location.of.population) %>%
  summarize(n = n()) %>%
  ungroup() 

# Check sampling method
US_goose %>%
  group_by(Sampling.method) %>%
  summarize(n = n()) %>%
  ungroup() # Survey or Count, mostly Survey during autumn and winter

# Check Units
US_goose %>%
  group_by(Units) %>%
  summarize(n = n()) %>%
  ungroup() # 3 different units but just different ways of saying individuals


# Statistical analysis ----

# Abundance distribution before scaling
ggplot(data = US_goose, 
       aes(x = pop)) +
  geom_histogram(bins = sqrt(nrow(US_goose))) # different data collection methods, different units

# normality test 
shapiro.test(US_goose$pop) # p-value = 6.068e-16 (p-value ≤ 0.05 = higher chance data is statistically significant)



# Try different methods of scaling
scalegoose <- US_goose %>%
  group_by(id) %>% 
  mutate(scalepop = (pop - min(pop)) / (max(pop) - min(pop))) %>%  # range scaling
  mutate(stpop = scale(pop)) %>%  # standardization scaling
  mutate(logpop = log(pop))  # logarithmic transformation

# Distribution of range-scaled population
ggplot(data = scalegoose, 
       aes(x = year, y = scalepop, colour = Location.of.population)) +
  geom_point()+
  labs(title = "Goose Population Over Time Across Location of Observations", x = "Year", y = "Scaled Population", colour = "Locations") 


# Distribution of standardized population
ggplot(data = scalegoose, 
       aes(x = year, y = stpop, colour = Location.of.population)) +
  geom_point()+
  labs(title = "Goose Population Over Time Across Location of Observations", x = "Year", y = "Standardised Population", colour = "Locations") 

# Distribution of log transformed values
ggplot(data = scalegoose, 
       aes(x = year, y = logpop, colour = Location.of.population)) +
  geom_point()+
  labs(title = "Goose Population Over Time Across Location of Observations", x = "Year", y = "log(Population)", colour = "Locations") 


## Analysis of Variance on related variables ------------------------------------

anova(lm(logpop ~ year, 
         data = scalegoose)) # F = 2.4236

anova(lm(logpop ~ biome, 
         data = scalegoose)) # F = 74.895

anova(lm(logpop ~ Location.of.population, 
         data =scalegoose)) # F = 130.42
# The difference in abundance is significant across year, biome and location.

# scaling the year variable so it starts at 1

scalegoose$year.scaled <- (scalegoose$year - 1969)


# Hierarchical linear model ----

# Base Models----

# log(Population) against year
goose_bm1 <- lm(logpop ~ year.scaled, 
                data = scalegoose)
summary(goose_bm1) # F-statistic: 2.424 on 1 and 232 DF,  p-value: 0.1209

# log(Population) against biome
goose_bm2 <- lm(logpop ~ biome, 
                data = scalegoose)
summary(goose_bm2) # F-statistic: 74.89 on 1 and 232 DF,  p-value: 8.419e-16

# log(Population) against location
goose_bm3 <- lm(logpop ~ Location.of.population, 
                data = scalegoose)
summary(goose_bm3) # F-statistic: 130.4 on 9 and 224 DF,  p-value: < 2.2e-16


# Adding biome as a second fixed effect
goose_m1 <- lm(logpop ~ year.scaled  + biome, 
            data = scalegoose)
summary(goose_m1) # F-statistic: 39.89 on 2 and 231 DF,  p-value: 1.318e-15

# Adding Location as a random effect
goose_m2 <- lm(logpop ~ year.scaled  + Location.of.population, 
            data = scalegoose)
summary(goose_m2) # F-statistic: 147.2 on 10 and 223 DF,  p-value: < 2.2e-16

# Mixed-Effect Models 
goose_mm <- lmer(logpop ~ year.scaled  + biome + (1|Location.of.population), 
           scalegoose, REML = FALSE)
summary(goose_mm)

# Comparing between models
AICc(goose_bm1, goose_bm2, goose_bm3, goose_m1, goose_m2, goose_mm) #goose_m2 is selected as it has the lowest AICc despite having highest df

# Null model with 1 as fixed effects year and biome
null <- lmer(logpop ~ 1 + 1 + (1|Location.of.population), 
             scalegoose, REML = FALSE)

# Comparing between null and selected model
AICc(null, goose_m2)

# Model and data visualisation ----

# Create a df of predicted values and their resulting error and uncertainties
pred.mm <- ggpredict(goose_m2, terms = c("year.scaled"))  

# Plotting the predicted model 
ggplot(pred.mm) +
  geom_line(aes(x = (x+1969), y = predicted), size = 0.5) +
  geom_ribbon(aes(x = (x+1969), ymin = predicted - std.error, ymax = predicted + std.error), 
              fill = "gray", alpha = 0.5) + 
  geom_point(data = scalegoose, aes(x = year, y = logpop, colour = Location.of.population)) +
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "log(population)", 
       title = "Change in US Canada Goose Population Over Time")+
  labs(colour="Location")+
  theme(plot.title = element_text(size = 15, hjust = 0.5)) 

  
# Plotting by each locations

ggplot(scalegoose, aes(x = (year.scaled + 1969), y = logpop, colour = biome)) +
  facet_wrap(~Location.of.population, nrow=2) +   
  geom_point(alpha = 0.5) +
  labs(x = "Year", y = "log(population)", 
       title = "Change in US Canada Goose Population Over Time by Observation Locations") +
  theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  theme(legend.position = "bottom") +
  geom_line(data = cbind(scalegoose, pred = predict(goose_mm)), aes(y = pred), size = 1) 

# Visualizing random effects
plot_model(goose_mm, type = "re", show.values = TRUE)



