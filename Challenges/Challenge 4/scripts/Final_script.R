# Challenge 4, Group 3
# Date: 25 November 2021
# Authors: Zuzi Koscikova, Filoteea Moldovan
# Contact: zuzkakoscikova@gmail.com, 

#rm(list=ls())

# Load libraries ---- 
library("tidyverse")    # collection of packages including "dplyr", "ggplot2", "tidyr", etc...
library("ggplot2")
library("viridis")


# Load data ----
forest_data <- read.csv("inputs/forest_change_High_Uintas.csv")

# Checking the structure of the data
glimpse(forest_data)
summary(forest_data)

# Data wrangling  ----
forest_data_long <- forest_data %>%      # change data to the long format
 pivot_longer(cols= 30:45,               # long format for GAIN variable
              names_to = "g_years", 
              values_to = "gain") %>% 
  pivot_longer(cols= 30:45,              # long format for LOSS variable
               names_to = "l_years",  
               values_to = "loss") %>% 
  mutate(years_gain = parse_number(g_years)   # make year a number for gain     
         )
forest_data_long <- forest_data_long %>% 
  mutate(years_loss = parse_number(l_years))   # make year a number for loss       
  
forest_data_calculations <- forest_data_long %>% 
  select(gain, loss, years_gain, years_loss) %>% 
  group_by(years_gain)


# Dividing data-sets to the individual for gains and losses  
# Forest data Gain
forest_data_gain <- forest_data_long %>% 
  subset( select = -c(l_years, loss, g_years))    # removing unnecessary columns 
# Forest Data Loss
forest_data_loss <- forest_data_long %>%            
  subset (select = -c(g_years, years, gain)) %>%  # removing unnecessary 
  mutate(years = parse_number (l_years)) %>% 
  subset (select = -c(l_years))


# Final data set ----
Final_data <- left_join(forest_data_gain, forest_data_loss, by = "years")
# we joined two datasets by year 
# we did it because we had a problem with different order of years for Loss and Gains
Final_data1 <- Final_data %>% 
  mutate(difference = gain - loss)


# plotting a basic scatterplot ---- 
(Scatterplot <- ggplot(Final_data1, aes(years, difference)) +
    geom_point(aes(color = difference)) +
    scale_color_viridis(option = "D")+
    theme_bw() +
    stat_smooth(method = "lm") +   # adding trendline
    labs(title = "Changes in forest cover from 2001-2016", 
         x = "\n Years", y = "Overall changes in forest cover\n"))

ggsave("Outputs/Scatterplot.png")  # saving plots
