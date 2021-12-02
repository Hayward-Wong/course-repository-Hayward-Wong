# Challenge 4, Group 3
# Date: 25 November 2021
# Authors: Zuzi Koscikova, Filoteea Moldovan
# Contact: zuzkakoscikova@gmail.com, 

#rm(list=ls())

# Load libraries ---- 
library(tidyverse)    # collection of packages including "dplyr", "ggplot2", "tidyr", etc...
library("ggplot2")

# Load data ----
forest_data <- read.csv("inputs/forest_change_High_Uintas.csv")

# Checking the structure of the data
glimpse(forest_data)
summary(forest_data)

# Data wrangling 
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

Final_data <- left_join(forest_data_gain, forest_data_loss, by = "years")

Final_data1 <- Final_data %>% 
  mutate(difference = gain - loss)


bird_models_traits <- left_join(aus_models, bird_diet, by = "species.name") %>%
  drop_na()
head(bird_models_traits)

summary(forest_data_long)


library("viridis")
theme <- function(){
    theme_bw() +
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 14, face = "plain"),             
            axis.title.y = element_text(size = 14, face = "plain"),
            panel.grid.major.x = element_blank(),                                          
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),  
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
            plot.title = element_text(size = 15, vjust = 1, hjust = 0.5),
            legend.text = element_text(size = 12, face = "italic"),          
            legend.title = element_blank(),                              
            legend.position = c(0.5, 0.8))
}

# plotting a basic scatterplot
(Scatterplot <- ggplot(Final_data1, aes(years, difference)) +
    geom_point(aes(color = difference)) +
    scale_color_viridis(option = "D")+
    theme_bw() +
    labs(title = "Changes in forest cover from 2001-2016", 
         x = "\n Years", y = "Overall changes in forest cover\n"))




  
  

theme <- function(){
  theme_minimal() +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 15, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.5, 0.8), 
          geom_point(aes(color = VALUES))
  }



ggplot(iris, aes(Sepal.Length, Sepal.Width))+
  geom_point(aes(color = Sepal.Length)) +
  scale_color_viridis(option = "D")+
  theme_minimal() +
  theme(legend.position = "bottom")


scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                  breaks = c("Narnia","Hogsmeade"),
                  name = "Land of magic",
                  labels = c("Narnia", "Hogsmeade")) +
  labs(title = "Species richness by plot", 
       x = "\n Plot number", y = "Number of species \n") + 
  
