# Challenge 2 -------------------------------------------------------------
# Visualizing change in crime rate in London boroughs from 2011-2020
# Data Science in Ecological and Environmental Sciences, University of Edinburgh

# Group member names: Adela Pafkova, Zuzi Koscikova, Hayward Wong, Sarka Ondrouchova, Susanna Fraser, Nadia Sotgiu
# Date: 21/10/21 - 4/11/21
# Email:  s1837612@ed.ac.uk, s1894439@ed.ac.uk, s1861053@ed.ac.uk, s1978469@ed.ac.uk, s1869523@ed.ac.uk
# Purpose: Present a publicly available data set for a BBC news publication



# Clear r to establish a fresh work space
rm(list = ls())


# Install libraries---- [commented out]
# install.packages("tidyverse") # tidyverse will install the core tidyverse packages including packages required for this script such as:


# Load libraries----
library(tidyverse) # will load the core tidyvsere packages including: dplyr and ggplot2
  

# Set working directory----
# Establish where your working directory is currently:
getwd()
# Set the working directory (on Windows) if its not already in the correct place:
setwd("C:/challenge2-challenge2-group4")


# Import Living Planet Data----
crime_data <- read.csv("data/crime-data.csv")


# Workflow:
# Step 1: sum monthly crime count to make a column for total crime each year for each borough
# Step 2: ...

