setwd("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Coding Club tutorials/2.Data Manipulation/tutorial 5")

# LIBRARIES
library(dplyr)     # for data manipulation
library(ggplot2)   # for making graphs; make sure you have it installed, or install it now

# Set your working directory

# LOAD DATA
trees <- read.csv(file = "trees.csv", header = TRUE)

head(trees)  # make sure the data imported OK, familiarise yourself with the variables

# Count the number of trees for each species

trees.grouped <- group_by(trees, CommonName)    # create an internal grouping structure, so that the next function acts on groups (here, species) separately.

trees.summary <- summarise(trees.grouped, count = length(CommonName))   # here we use length to count the number of rows (trees) for each group (species). We could have used any row name.

# Alternatively, dplyr has a tally function that does the counts for you!
trees.summary <- tally(trees.grouped)


# Count the number of trees for each species, with a pipe!

trees.summary <- trees %>%                   # the data frame object that will be passed in the pipe
  group_by(CommonName) %>%    # see how we don't need to name the object, just the grouping variable?
  tally()                     # and we don't need anything at all here, it has been passed through the pipe!

trees.subset <- trees %>%
  filter(CommonName %in% c('Common Ash', 'Rowan', 'Scots Pine')) %>%
  group_by(CommonName, AgeGroup) %>%
  tally()

summ.all <- summarise_all(trees, mean)

vector <- c(4, 13, 15, 6)      # create a vector to evaluate

ifelse(vector < 10, "A", "B")  # give the conditions: if inferior to 10, return A, if not, return B

# Congrats, you're a dancing queen! (Or king!)

vector2 <- c("What am I?", "A", "B", "C", "D")

case_when(vector2 == "What am I?" ~ "I am the walrus",
          vector2 %in% c("A", "B") ~ "goo",
          vector2 == "C" ~ "ga",
          vector2 == "D" ~ "joob")


unique(trees$LatinName)  # Shows all the species names

# Create a new column with the tree genera

trees.genus <- trees %>%
  mutate(Genus = case_when(               # creates the genus column and specifies conditions
    grepl("Acer", LatinName) ~ "Acer",
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula",
    grepl("Populus", LatinName) ~ "Populus",
    grepl("Laburnum", LatinName) ~ "Laburnum",
    grepl("Aesculus", LatinName) ~ "Aesculus",
    grepl("Fagus", LatinName) ~ "Fagus",
    grepl("Prunus", LatinName) ~ "Prunus",
    grepl("Pinus", LatinName) ~ "Pinus",
    grepl("Sambucus", LatinName) ~ "Sambucus",
    grepl("Crataegus", LatinName) ~ "Crataegus",
    grepl("Ilex", LatinName) ~ "Ilex",
    grepl("Quercus", LatinName) ~ "Quercus",
    grepl("Larix", LatinName) ~ "Larix",
    grepl("Salix", LatinName) ~ "Salix",
    grepl("Alnus", LatinName) ~ "Alnus")
  )

library(tidyr)
trees.genus.2 <- trees %>%
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>%  
  dplyr::select(-Species)

# we're creating two new columns in a vector (genus name and species name), "sep" refers to the separator, here space between the words, and remove = FALSE means that we want to keep the original column LatinName in the data frame

trees.genus <- trees.genus %>%   # overwriting our data frame
  mutate(Height.cat =   # creating our new column
           case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",
                     Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",
                     Height == "20 to 25 meters" ~ "Tall")
  )

## Reordering a factor's levels

levels(trees.genus$Height.cat)  # shows the different factor levels in their default order

trees.genus$Height.cat <- factor(trees.genus$Height.cat,
                                 levels = c('Short', 'Medium', 'Tall'),   # whichever order you choose will be reflected in plots etc
                                 labels = c('SHORT', 'MEDIUM', 'TALL')    # Make sure you match the new names to the original levels!
)   

levels(trees.genus$Height.cat)  # a new order and new names for the levels

library(ggplot2)


# Subset data frame to fewer genera

trees.five <- trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# Map all the trees

(map.all <- ggplot(trees.five) +
    geom_point(aes(x = Easting, y = Northing, size = Height.cat, colour = Genus), alpha = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
)

# Plotting a map for each genus

tree.plots <-  
  trees.five  %>%      # the data frame
  group_by(Genus) %>%  # grouping by genus
  do(plots =           # the plotting call within the do function
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 12),
             plot.title = element_text(hjust = 0.5),
             legend.position = "bottom")
  )

# You can view the graphs before saving them
tree.plots$plots

# Saving the plots to file

tree.plots %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))


## Calculate the quadrants

# Find the center coordinates that will divide the data (adding half of the range in longitude and latitude to the smallest value)

lon <- (max(trees.genus$Easting) - min(trees.genus$Easting))/2 + min(trees.genus$Easting)
lat <- (max(trees.genus$Northing) - min(trees.genus$Northing))/2 + min(trees.genus$Northing)

# Create the column

trees.genus <- trees.genus %>%
  mutate(Quadrant = case_when(
    Easting < lon & Northing > lat ~ 'NW',
    Easting < lon & Northing < lat ~ 'SW',
    Easting > lon & Northing > lat ~ 'NE',
    Easting > lon & Northing < lat ~ 'SE')
  )

# We can check that it worked
ggplot(trees.genus) +
  geom_point(aes(x = Easting, y = Northing, colour = Quadrant)) +
  theme_bw()

trees.genus <- trees.genus %>%
  mutate(Quadrant = case_when(
    Easting <= lon & Northing > lat ~ 'NW',  # using inferior OR EQUAL ensures that no point is forgotten
    Easting <= lon & Northing < lat ~ 'SW',
    Easting > lon & Northing > lat ~ 'NE',
    Easting > lon & Northing < lat ~ 'SE')
  )

sp.richness <- trees.genus %>%
  group_by(Quadrant) %>%
  summarise(richness = length(unique(LatinName)))

acer.percent <- trees.genus %>%
  group_by(Quadrant, Genus) %>%
  tally() %>%                      # get the count of trees in each quadrant x genus
  group_by(Quadrant) %>%           # regroup only by quadrant
  mutate(total = sum(n)) %>%       # sum the total of trees in a new column
  filter(Genus == 'Acer') %>%      # keep only acer
  mutate(percent = n/total)        # calculate the proportion

# We can make a plot representing the %

ggplot(acer.percent) +
  geom_col(aes(x = Quadrant, y = percent)) +
  labs(x = 'Quadrant', y = 'Proportion of Acer') +
  theme_bw()
# Create an Acer-only data frame

acer <- trees.genus %>%
  filter(Genus == 'Acer')


# Rename and reorder age factor

acer$AgeGroup <- factor(acer$AgeGroup,
                        levels = c('Juvenile', 'Semi-mature', 'Middle Aged', 'Mature'),
                        labels = c('Young', 'Young', 'Middle Aged', 'Mature'))


# Plot the graphs for each quadrant

acer.plots <- acer %>%
  group_by(Quadrant) %>%
  do(plots =           # the plotting call within the do function
       ggplot(data = .) +
       geom_bar(aes(x = AgeGroup)) +
       labs(title = paste('Age distribution of Acer in ', .$Quadrant, ' corner', sep = ''),
            x = 'Age group', y = 'Number of trees') +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.title = element_text(size = 14),
             axis.text = element_text(size = 14),
             plot.title = element_text(hjust = 0.5))
  )

# View the plots (use the arrows on the Plots viewer)
acer.plots$plots
