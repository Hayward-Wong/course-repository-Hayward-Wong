setwd("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Coding Club tutorials/2.Data Manipulation/tutorial 7")

install.packages("rvest") # To import a html file
install.packages("dplyr") # To use pipes

library(rvest)
library(dplyr)
library(tidyverse)


Penguin_html <- readLines("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Coding Club tutorials/2.Data Manipulation/tutorial 7/Aptenodytes forsteri (Emperor Penguin).html")

grep("Scientific Name:", Penguin_html)

Penguin_html[131:135]

# Double check the line containing the scienific name.
grep("Scientific Name:", Penguin_html)
Penguin_html[131:135]
Penguin_html[133]

# Isolate line in new vector
species_line <- Penguin_html[133]

## Use pipes to grab the text and get rid of unwanted information like html tags
species_name <- species_line %>%
  gsub("<td class=\"sciName\"><span class=\"notranslate\"><span class=\"sciname\">", "", .) %>%  # Remove leading html tag
  gsub("</span></span></td>", "", .) %>%  # Remove trailing html tag
  gsub("^\\s+|\\s+$", "", .)  # Remove whitespace and replace with nothing
species_name

#Common Name
grep("Common Name", Penguin_html)
Penguin_html[130:160]
Penguin_html[150:160]
Penguin_html[151]

common_name <- Penguin_html[151] %>%
  gsub("<td>", "", .) %>%
  gsub("</td>", "", .) %>%
  gsub("^\\s+|\\s+$", "", .)
common_name


#ICUN Category
grep("Red List Category", Penguin_html)
Penguin_html[179:185]
Penguin_html[182]

red_cat <- gsub("^\\s+|\\s+$", "", Penguin_html[182])
red_cat


#Date Assessed
grep("Date Assessed:", Penguin_html)
Penguin_html[192:197]
Penguin_html[196]

date_assess <- Penguin_html[196] %>%
  gsub("<td>", "",.) %>%
  gsub("</td>", "",.) %>%
  gsub("\\s", "",.)
date_assess

iucn <- data.frame(species_name, common_name, red_cat, date_assess)

