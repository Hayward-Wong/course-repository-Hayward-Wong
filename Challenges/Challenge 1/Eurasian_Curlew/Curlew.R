# Challenge 1 - Basic Data Wrangling with LPI Bird Data

## Work flow ----

# 1. Import raw data 
# 2. Clean data: convert data to long format, remove N/As
# 3. Manipulate data: take only Curlew population in the U.K., 
#    create variables 'duration', and select only observations 
#    with duration greater than 15 years.
# 4. Graph Goose populations trend in U.K. and map the populations.
# 5. Export graphs

#Libraries----
library(tidyverse) # contains 'dplyr' and 'ggplot2' for data wrangling and graph making
library(ggthemes) # contains functions to customize graphs from ggplot
library(gridExtra) # used for our map


#1. Importing Raw data----
LPI_birds <- read.csv("Raw_data/LPI_birds.csv") # read LPI bird observation
site_coords <- read.csv("Raw_data/site_coords.csv") # read site coordinates

glimpse(LPI_birds) # see all column names, data type, first few data entries
glimpse(site_coords) # see all column names, data type, first few data entries


## 2. Clean data: convert data to long format for later use ----
LPI_long <- gather(LPI_birds, "year", "pop", 25:69) # Reshapes year and abundance into a long format
rm(LPI_birds) #removes LPI birds from the environment

LPI_long <- LPI_long %>% 
  filter(is.finite(pop)) %>%  # keep only observations with finite values
  mutate(year = parse_number(year)) %>%   # turn all year into numeric value
  select(id, Common.Name, Country.list, year, pop)  # select relevant columns


## 3. Manipulate data ----

# Create data.frame "UK_goose" for trends plotting
UK_curlew<- LPI_long %>%
  filter(Country.list == "United Kingdom") %>%  # keep observations in UK only
  filter(Common.Name == "Eurasian curlew")%>%# keep Eurasian curlew observations only
  group_by(id) %>% 
  mutate(duration = max(year) - min(year)) %>%  # create new variable "duration" as length of observation for each id
  mutate(scalepop = (pop - min(pop)) / (max(pop) - min(pop))) %>%  # create new variable "scalepop"
  filter(duration > 15)  # keep observations with >15 years of duration only

#removes data.frame from the environment that won't be needed later
rm(LPI_long)


## 4. Graph curlew populations trends in the UK and map the populations ---- 

# Creating trend line of populations of Goose in U.K.
(fig_1<- ggplot(UK_curlew, aes(x = year, y = scalepop, colour = Common.Name))+ # graph curlew populations over years
   geom_line()+ # add line to the plot
   geom_point(size = 0.5)+ # add data points to the plot
   xlab("Year")+ # add x-axis title
   ylab("Scaled Population") +  # add y-axis title
   theme(legend.position = "right") +  # position legends at the bottom
   labs(title = "Curlew trends") +  # add title to plot
   labs(color = "Species") +  # rename legend title
   theme(plot.title = element_text(size = 15, hjust = 0.5)))  # adjust title size and position to middle


UK_curlew <- left_join(UK_curlew, site_coords, by = "id") # add coordinates of observations to UK_curlew according to their id

# Create map illustrating the location of observation and species of observation
(fig_2 <- ggplot(UK_curlew, # Creating plot and naming it Fig_2
                 aes(x = Decimal.Longitude, y = Decimal.Latitude, colour = Common.Name)) +  # set x and y input as longitude and latitude 
    borders("world", colour = "gray40", fill = "gray75", size = 0.3) +  # specifying format of the map
    coord_cartesian(xlim = c(-10, 3), ylim = c(50,60)) + # specifying coordinates of the graph
    theme_map() + #displaying map
    geom_point(size = 4) + # set size of data points
    theme(legend.position = "right") + # add legend to the map
    labs(title = "UK curlew Population map")+ # add title to the map
    labs(color = "Species") +  # rename legend title
    theme(plot.title = element_text(size = 15, hjust = 0.5)) )# adjust title size and position to middle) 

## 5. Save graph ----

ggsave(filename = "Eurasian_Curlew/Curlew_Figures/UK_Curlew_trends.pdf", plot = fig_1,width = 5, height = 6) # saving fig_1
ggsave(filename = "Eurasian_Curlew/Curlew_Figures/UK_Curlew_site.pdf", plot = fig_2,width = 5, height = 6) # Saving fig_2

(fig_3 <- grid.arrange(fig_1, fig_2, widths= c(0.6,0.4), ncol = 2))  # Combining the two graphs into one

ggsave(filename = "Eurasian_Curlew/Curlew_Figures/UK_Curlew_Population.pdf", plot = fig_3, width = 12, height = 6) # Saves the combined plot 
