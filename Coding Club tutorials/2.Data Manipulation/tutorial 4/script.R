setwd("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Coding Club tutorials/2.Data Manipulation/tutorial 4")

# Load the elongation data
elongation <- read.csv("EmpetrumElongation.csv", header = TRUE)   

# Check import and preview data
head(elongation)   # first few observations
str(elongation)    # types of variables

# Let's get some information out of this object!
elongation$Indiv   # prints out all the ID codes in the dataset
length(unique(elongation$Indiv))   # returns the number of distinct shrubs in the data

# Here's how we get the value in the second row and fifth column
elongation[2,5]

# Here's how we get all the info for row number 6
elongation[6, ]

# And of course you can mix it all together!
elongation[6, ]$Indiv   # returns the value in the column Indiv for the sixth observation
# (much easier calling columns by their names than figuring out where they are!)


# Let's access the values for Individual number 603
elongation[elongation$Indiv == 603, ]


# Subsetting with one condition

elongation[elongation$Zone < 4, ]    # returns only the data for zones 2-3
elongation[elongation$Zone <= 4, ]   # returns only the data for zones 2-3-4


# This is completely equivalent to the last statement
elongation[!elongation$Zone >= 5, ]   # the ! means exclude


# Subsetting with two conditions
elongation[elongation$Zone == 2 | elongation$Zone == 7, ]    # returns only data for zones 2 and 7
elongation[elongation$Zone == 2 & elongation$Indiv %in% c(300:400), ]    # returns data for shrubs in zone 2 whose ID numbers are between 300 and 400

## CHANGING VARIABLE NAMES AND VALUES IN A DATA FRAME

# Let's create a working copy of our object
elong2 <- elongation

# Now suppose you want to change the name of a column: you can use the names() function
# Used on its own, it returns a vector of the names of the columns. Used on the left side of the assign arrow, it overwrites all or some of the names to value(s) of your choice.

names(elong2)                 # returns the names of the columns

names(elong2)[1] <- "zone"    # Changing Zone to zone: we call the 1st element of the names vector using brackets, and assign it a new value
names(elong2)[2] <- "ID"      # Changing Indiv to ID: we call the 2nd element and assign it the desired value

# Now suppose there's a mistake in the data, and the value 5.1 for individual 373 in year 2008 should really be 5.7

## - option 1: you can use row and column number
elong2[1,4] <- 5.7

## - option 2: you can use logical conditions for more control
elong2[elong2$ID == 373, ]$X2008 <- 5.7   # completely equivalent to option 1

## CREATING A FACTOR

# Let's check the classes
str(elong2)

# The zone column shows as integer data (whole numbers), but it's really a grouping factor (the zones could have been called A, B, C, etc.) Let's turn it into a factor:

elong2$zone <- as.factor(elong2$zone)        # converting and overwriting original class
str(elong2)                                  # now zone is a factor with 6 levels

## CHANGING A FACTOR'S LEVELS

levels(elong2$zone)  # shows the different factor levels

levels(elong2$zone) <- c("A", "B", "C", "D", "E", "F")   # you can overwrite the original levels with new names

# You must make sure that you have a vector the same length as the number of factors, and pay attention to the order in which they appear!


install.packages("tidyr")  # install the package
library(tidyr)             # load the package


elongation_long <- gather(elongation, Year, Length,                           # in this order: data frame, key, value
                          c(X2007, X2008, X2009, X2010, X2011, X2012))        # we need to specify which columns to gather

# Here we want the lengths (value) to be gathered by year (key)

# Let's reverse! spread() is the inverse function, allowing you to go from long to wide format
elongation_wide <- spread(elongation_long, Year, Length)

elongation_long2 <- gather(elongation, Year, Length, c(3:8))

boxplot(Length ~ Year, data = elongation_long,
        xlab = "Year", ylab = "Elongation (cm)",
        main = "Annual growth of Empetrum hermaphroditum")

install.packages("dplyr")  # install the package
library(dplyr)              # load the package

elongation_long <- rename(elongation_long, zone = Zone, indiv = Indiv, year = Year, length = Length)     # changes the names of the columns (getting rid of capital letters) and overwriting our data frame

# As we saw earlier, the base R equivalent would have been
names(elongation_long) <- c("zone", "indiv", "year", "length")


# FILTER OBSERVATIONS

# Let's keep observations from zones 2 and 3 only, and from years 2009 to 2011

elong_subset <- filter(elongation_long, zone %in% c(2, 3), year %in% c("X2009", "X2010", "X2011")) # you can use multiple different conditions separated by commas

# For comparison, the base R equivalent would be (not assigned to an object here):
elongation_long[elongation_long$zone %in% c(2,3) & elongation_long$year %in% c("X2009", "X2010", "X2011"), ]

# SELECT COLUMNS

# Let's ditch the zone column just as an example

elong_no.zone <- dplyr::select(elongation_long, indiv, year, length)   # or alternatively
elong_no.zone <- dplyr::select(elongation_long, -zone) # the minus sign removes the column

# For comparison, the base R equivalent would be (not assigned to an object here):
elongation_long[ , -1]  # removes first column

# A nice hack! select() lets you rename and reorder columns on the fly
elong_no.zone <- dplyr::select(elongation_long, Year = year, Shrub.ID = indiv, Growth = length)

# Neat, uh?

# CREATE A NEW COLUMN

elong_total <- mutate(elongation, total.growth = X2007 + X2008 + X2009 + X2010 + X2011 + X2012)

# GROUP DATA

elong_grouped <- group_by(elongation_long, indiv)   # grouping our dataset by individual

# SUMMARISING OUR DATA

summary1 <- summarise(elongation_long, total.growth = sum(length))
summary2 <- summarise(elong_grouped, total.growth = sum(length))

summary3 <- summarise(elong_grouped, total.growth = sum(length),
                      mean.growth = mean(length),
                      sd.growth = sd(length))

# Load the treatments associated with each individual

treatments <- read.csv("EmpetrumTreatments.csv", header = TRUE, sep = ";")
head(treatments)

# Join the two data frames by ID code. The column names are spelled differently, so we need to tell the function which columns represent a match. We have two columns that contain the same information in both datasets: zone and individual ID.

experiment <- left_join(elongation_long, treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))

# We see that the new object has the same length as our first data frame, which is what we want. And the treatments corresponding to each plant have been added!

experiment2 <- merge(elongation_long, treatments, by.x = c("zone", "indiv"), by.y = c("Zone", "Indiv"))  
# same result!

boxplot(length ~ Treatment, data = experiment)



dragons <- read.csv('dragons.csv', header = TRUE)

dragons <- rename(dragons, turmeric = paprika)

correct.values  <- dragons$tabasco[dragons$species == 'hungarian_horntail'] - 30   # create a vector of corrected values

dragons[dragons$species == 'hungarian_horntail', 'tabasco'] <- correct.values      # overwrite the values in the dragons object

dragons.2 <- mutate(dragons, tabasco = ifelse(species == 'hungarian_horntail', tabasco - 30, tabasco))


dragons_long <- gather(dragons, key = 'spice', value = 'plume', c('tabasco', 'jalapeno', 'wasabi', 'turmeric'))


# Convert the data into meters

dragons_long <- mutate(dragons_long, plume.m = plume/100)    # Creating a new column turning cm into m


# Create a subset for each species to make boxplots

horntail <- filter(dragons_long, species == 'hungarian_horntail')            # the dplyr way of filtering
green <- filter(dragons_long, species == 'welsh_green')
shortsnout <- dragons_long[dragons_long$species == 'swedish_shortsnout', ]   # maybe you opted for a base R solution instead?


# Make the boxplots

par(mfrow=c(1, 3))      # you need not have used this, but it splits your plotting device into 3 columns where the plots will appear, so all the plots will be side by side.

boxplot(plume.m ~ spice, data = horntail,
        xlab = 'Spice', ylab = 'Length of fire plume (m)',
        main = 'Hungarian Horntail')


boxplot(plume.m ~ spice, data = green,
        xlab = 'Spice', ylab = 'Length of fire plume (m)',
        main = 'Welsh Green')


boxplot(plume.m ~ spice, data = shortsnout,
        xlab = 'Spice', ylab = 'Length of fire plume (m)',
        main = 'Swedish Shortsnout')

