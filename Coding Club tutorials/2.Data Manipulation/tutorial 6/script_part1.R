# Title: Creative Data Manipulation: familiarising with the diversity of dplyr
# Your name Hayward
# Date

# Set working directory to where you saved the folder with tutorial materials on your computer
setwd("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Coding Club tutorials/2.Data Manipulation/tutorial 6")

# Libraries ----
library(dplyr)
# when we add new libraries throughout the tutorial, you can paste them here

# If you've never used a given package, install it with `install.packages("name")`

# Most of them are a part of `tidyverse` but we will be loading them separately so that you are aware of the contexts in which they are used

### PART I: OCEAN ANIMALS ----

# Load the datasets
animal_p1 <- read.csv("data/animal_p1.csv")
animal_p2 <- read.csv("data/animal_p2.csv")
animal_rp <- read.csv("data/animal_rp.csv")
animal_meal <- read.csv("data/animal_meal.csv")


# 2. Combining tables ----
# a) Combining rows ----

# Firstly view the p1 and p2 datasets, which will appear in your console
animal_p1
animal_p2

# Now, bind them on top of each other
# Adding brackets around the expression prints out the result
(animal <- bind_rows(animal_p1, animal_p2))  # 8 observations

# b) Set operations ----

setequal(animal_p1, animal_p2)  # FALSE is returned, so the tables are not identical

# Since they differ, let's see how many observations you and your rp have in common.
# You're not creating an object yet so the output will be shown in the console without having to add brackets

intersect(animal, animal_rp)  # 6 observations in common

# View which observations you have and your rp doesn't

setdiff(animal, animal_rp)  # id no. 2 and 5

# Now view which ones your rp has that you don't

setdiff(animal_rp, animal)  # id no. 6 and 10

# Let's now connect them with `union()` but removing any duplicated rows

(animal_weight <- union(animal, animal_rp) %>%
    arrange(id))  # puts the id in numerical order (useful function for rearranging cases!)

# c) Mutating joins ----

animal_meal  # there are 21 observations!

(animal_joined <- left_join(animal_weight, animal_meal,
                            by = c("id" = "IDs")))  # indicate which columns in two tables refer to the same `id` variable

# We can also write it differently by using the pipe operator
(animal_joined <- animal_weight %>%
    left_join(animal_meal, by = c("id" = "IDs")))

inner_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # only rows with id's appearing in both tables were left (turtle of id = 2 is now missing)

right_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # we have all meal id's but various NAs for `animal` and `weight` columns were introduced because there was no match for them in `animal_weight`

full_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # all possible id's from both tables are retained and various NAs are introduced


full_join(animal_p1, animal_p2, by = c("id", "animal", "weight"))

# d) Filtering joins ----

semi_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # returns `animal_weight` dataset apart from rows of which `id` is not present in `animal_meal`

anti_join(animal_weight, animal_meal, by = c("id" = "IDs"))  # returns only one row from `animal_weight` of which `id` was not present in `animal_meal`

# e) Challenge ----

animal_new <- read.csv("data/animal_new.csv")
str(animal_new)  # check the dataset

animal_final <- animal_joined %>%
  full_join(animal_new,
            by = c("id" = "ID", "animal" = "Animals", "weight", "meal" = "Meal"))

# Libraries
library(ggplot2)    # one of the tidyverse packages for beautiful graphs
library(gridExtra)  # for the panel

# Barplot of diet
(barplot <- ggplot(animal_final, aes(animal, fill = meal)) +
    geom_bar(alpha = 0.8) +
    labs(title = "Diversity of meals", x = NULL) +
    scale_fill_brewer(palette = "Set3", type = "seq", na.value = "grey") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

# Boxplot of weight
(boxplot <- ggplot(animal_final) +
    geom_boxplot(aes(animal, weight, fill = animal), alpha = 0.5, position = "dodge2") +
    scale_y_continuous(limits = c(0, 30)) +
    labs(title = "Mean weights of animals", x = NULL, y = "Weight (kg)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

# Combine the plots into one panel
animal_panel <- grid.arrange(barplot, boxplot, ncol = 2)

# Save the panel if you want! You will have to create `images` folder where you've set your working directory (or change filename to "animal_panel.png")
ggsave(filename = "images/animal_panel.png", plot = animal_panel, width = 10, height = 5)


