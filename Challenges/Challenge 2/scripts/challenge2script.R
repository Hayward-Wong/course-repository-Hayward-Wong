# Challenge 2 -------------------------------------------------------------
# Visualizing change in crime rate in London boroughs from 2011-2020
# Data Science in Ecological and Environmental Sciences, University of Edinburgh

# Group member names: Adela Pafkova, Zuzi Koscikova, Hayward Wong, Sarka Ondrouchova, Susanna Fraser, Nadia Sotgiu
# Date: 21/10/21 - 4/11/21
# Email:  s1837612@ed.ac.uk, s1894439@ed.ac.uk, s1861053@ed.ac.uk, s1978469@ed.ac.uk
# Purpose: Present a publicly available data set for a BBC news publication


# Clear r to establish a fresh work space
rm(list = ls())

# Install packages and load libraries---- 
# install.packages("tidyverse") 
# tidyverse will install the core tidyverse packages including packages required for this script such as:
# install.packages("janitor") 

library(tidyverse) # will load the core tidyverse packages including: dplyr and ggplot2
library(janitor)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scales)
library(janitor) # will load simple functions to clean data sets


# Set working directory----
# Establish where your working directory is currently:
getwd()

# Import Crime Data----
crime_data <- read.csv("data/crime-data.csv")

# Check the data structure
glimpse(crime_data)
str(crime_data)

# Formatting the data ----

# Clean names of colums
crime_data <- janitor::clean_names(crime_data) # rename columns to lowercase without spaces
names(crime_data) # see changed column names
  
# Format the dataset to long format
annual_crime <- crime_data %>% # create a new dataset not to overwrite original data
  rowwise() %>% # set the functions to work by row instead of column
  transmute (borough, major_text, minor_text, 
             X2011 = sum(c_across(4:15)), 
          X2012 = sum(c_across(16:27)), 
          X2013 = sum(c_across(28:39)), 
          X2014 = sum(c_across(40:51)), 
          X2015 = sum(c_across(52:63)), 
          X2016 = sum(c_across(64:75)), 
          X2017 = sum(c_across(76:87)), 
          X2018 = sum(c_across(88:99)), 
          X2019 = sum(c_across(100:111)), 
          X2020 = sum(c_across(112:123)),) %>% # sum total crime by years
  adorn_totals(where = "row", fill = "-", na.rm = TRUE, name = "total") %>% # create a column for totals to check that the data are correct
  pivot_longer(cols = 4:13, names_to = "year",values_to = "crime_rate") %>% # format the data to long format
  mutate (year = parse_number(year)) # only keeps the numbers in column year

# Plotting bar chart for total crimes in London----
Total_annual_crime_london <- total_annual_crime %>% #creating new data frame
  filter(borough == "total")# keep only the total crime rates

fig_1 <- ggplot(Total_annual_crime_london, # Creating plot and naming it Fig_1
                aes(y=crime_rate,x=year)) +  # Graphs total crimes in London
  geom_bar(stat = "identity")+ #add bars to the plot
  scale_x_continuous(breaks = pretty_breaks())+ # changing x axis to integers
  xlab("Year")+ # add x-axis title
  ylab("Crime rate")+   # adjust title size and position to middle
  labs(title = "Total crimes in London") +  # add title to plot
  theme(plot.title = element_text(size = 25, hjust = 0.5))  # adjust title size and position to middle

ggsave(filename = "figures/Total_Crimes_in_London.pdf", plot = fig_1, width = 15, height = 7.5) # saving fig_1


# Sum yearly crime events in each borough----

total_annual_crime <- annual_crime %>% # create a new dataset not to overwrite original data
  select(borough, year, 'crime_rate') %>% # only keep selected columns
  group_by(borough, year) %>% # group data by borough and by year
  summarise_each(funs(sum)) # output summarized data for each borough per year
  ungroup() #Ungroup data

total_annual_crime_boroughs <- subset(total_annual_crime, borough!="total")#removing total from the borough column to keep only boroughs


total_wide <- total_annual_crime_boroughs%>% # create a new data frame
  spread(key = year, value = 'crime_rate')%>% # changing the data to wide format for ordering
  mutate(total= rowSums(across(where(is.numeric))))%>% # Finding the total crimes for each borough in the past 10 years
  group_by(borough)%>% # group data by borough
  arrange(desc(total))%>% # arrange the boroughs from the most total crimes to least
  select(1:11)%>% # remove column for total crimes
  ungroup()#Ungroup data


# Plotting trends for total crimes in the 6 boroughs with most crimes----
Top_6 <- slice_head(total_wide, n=6) # Creates new data frame keeping the 6 boroughs with most crimes

Top6_long <- gather(data = Top_6, key = "year", value = "crime_rate", 2:11) %>% #changes the data frame Top_6 to long format
  group_by(borough)%>%# group data by borough
  mutate (year= parse_number(year)) # only keeps the numbers in column year
  

(fig_2 <- ggplot(data=Top6_long, # Creating plot and naming it Fig_2
                 aes(x=year, y=crime_rate, colour=borough))+ # graph crime rate trends of top 6 boroughs
    geom_point(size=1)+ # add data points to the plot
    geom_line()+# add trend lines to the plot
    scale_x_continuous(breaks = pretty_breaks())+ # changing x axis to integers
    xlab("Year")+ # add x-axis title
    ylab("Crime rate") +  # add y-axis title
    theme(legend.position = "bottom") +  # position legends at the bottom
    labs(title = "Total crime trends in the 6 borough with most crimes") +  # add title to plot
    labs(colour = "Boroughs") +  # rename legend title
    theme(plot.title = element_text(size = 15, hjust = 0.5)))  # adjust title size and position to middle

# Plotting trends for total crimes in the 6 boroughs with least crimes----
Bottom_6 <- slice_tail(total_wide, n=6)# Creates new data frame keeping the 6 boroughs with least crimes

Bottom6_long <- gather(data = Bottom_6, key = "year", value = "crime_rate", 2:11) %>% #changes the data frame Bottom_6 to long format
  group_by(borough)%>%# group data by borough
  mutate (year= parse_number(year))# only keeps the numbers in column year


(fig_3 <- ggplot(data=Bottom6_long, # Creating plot and naming it Fig_3
                 aes(x=year, y=crime_rate, colour=borough))+ # graph crime rate trends of bottom 6 boroughs
    geom_point(size=1)+ # add data points to the plot
    geom_line()+# add trend lines to the plot
    scale_x_continuous(breaks = pretty_breaks())+ # changing x axis to integers
    xlab("Year")+ # add x-axis title
    ylab("Crime rate") +  # add y-axis title
    theme(legend.position = "bottom") +  # position legends at the bottom
    labs(title = "Total crime trends in the 6 boroughs with least crimes") +  # add title to plot
    labs(colour = "Boroughs") +  # rename legend title
    theme(plot.title = element_text(size = 15, hjust = 0.5)))  # adjust title size and position to middle
# Combining the plots for top and bottom 6 boroughs ----
(fig_4 <- grid.arrange(fig_2, fig_3, heights= c(0.5,0.5), nrow = 2)) #combining fig_2 and fig_3
  
ggsave(filename = "figures/Total_Crime_trends_top&bottom.pdf", plot = fig_4, width = 10, height = 10 ) # saving fig_4


# Sum yearly total crimes by crime types----

annual_crime_by_types <- crime_data %>% # create a new dataset not to overwrite original data
  rowwise() %>% # set the functions to work by row instead of column
  transmute (major_text, 
             X2011 = sum(c_across(4:15)), 
             X2012 = sum(c_across(16:27)), 
             X2013 = sum(c_across(28:39)), 
             X2014 = sum(c_across(40:51)), 
             X2015 = sum(c_across(52:63)), 
             X2016 = sum(c_across(64:75)), 
             X2017 = sum(c_across(76:87)), 
             X2018 = sum(c_across(88:99)), 
             X2019 = sum(c_across(100:111)), 
             X2020 = sum(c_across(112:123)),)%>% # sum total crime by years
  adorn_totals(where = "row", fill = "-", na.rm = TRUE, name = "total") %>%
  group_by(major_text) %>% 
  summarise_each(funs(sum))

crime_by_types_long <- annual_crime_by_types %>%
  gather(key="year", value="crime_rate",2:11)%>%
  group_by(major_text)%>%
  mutate (year= parse_number(year))

# Plotting stacked bar chart of the total Crimes in London by crime types----

fig_5 <- ggplot(crime_by_types_long, aes(fill=major_text, y=crime_rate, x=year)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_continuous(breaks = pretty_breaks())+ # changing x axis to integers
  xlab("Year")+ # add x-axis title
  ylab("Crime rate")+   # adjust title size and position to middle
  theme(legend.position = "bottom") +  # position legends at the bottom
  labs(title = "Total crimes in London") +  # add title to plot
  labs(fill = "Crimes type")+ # add title to plot
  theme(plot.title = element_text(size = 25, hjust = 0.5))  # adjust title size and position to middle


ggsave(filename = "figures/Total_Crimes_in_London_by_type.pdf", plot = fig_5, width = 15, height = 7.5)

# Plotting facet bar charts of total crimes in the 3 boroughs with most crime and the 3 with least----

top3 <- slice_head(total_wide, n=3)
bottom3 <- slice_tail(total_wide, n=3)

top3bottom3_long <- rbind(top3,bottom3)%>%
  gather(key="year", value="crime_rate",2:11)%>%
  group_by(borough)%>%
  mutate (year= parse_number(year))
str(top3bottom3_long)

fig_6 <- ggplot(top3bottom3_long) +
  aes(x = year, y=crime_rate, fill = borough)+
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = pretty_breaks())+ # changing x axis to integers
  theme_classic() +
  labs(title = "Top 3 and Bottom 3 Boroughs with the most crimes") +
  ylab("Crime rate") +
  facet_wrap(~factor(borough, levels=c("Westminster","Lambeth","Southwark","Sutton","Kingston upon Thames","Aviation Security(SO18)"))) +
  labs(fill = "B0roughs")+
  theme(plot.title = element_text(size=25, hjust=0.5), legend.position = "bottom")

ggsave(filename = "figures/Top3&Bottom3.pdf", plot = fig_5, width = 10, height = 5)

# Combining the plot for London and the facet plots for top 3 and bottom 3 boroughs----

(fig_7 <- grid.arrange(fig_5, fig_6, heights= c(0.75,0.5), nrow = 2))
ggsave(filename = "figures/Total_crimes_in_London_Boroughs.pdf", plot = fig_7, width = 15, height = 15)




# !!! TASK
# new column for change in crime rate over 10 years is needed for each Borough
# then we can make a bar plot 

   
                                    
     


  



