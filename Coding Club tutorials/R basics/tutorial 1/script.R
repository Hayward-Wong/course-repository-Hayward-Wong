# Coding Club Workshop 1 - R Basics
# Learning how to import and explore data, and make graphs about Edinburgh's biodiversity
# Written by Hayward Wong 29/09/2021 University of Edinburgh

install.packages("dplyr")
library(dplyr)
# Note that there are quotation marks when installing a package, but not when loading it
# and remember that hashtags let you add useful notes to your code! 

setwd("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Coding Club tutorials/R basics/tutorial 1")
# This is an example filepath, alter to your own filepath

edidiv <- read.csv("tutorial 1/edidiv.csv")  # This is the file path based on where I saved the data, your filepath will be different

head(edidiv)                # Displays the first few rows
tail(edidiv)                # Displays the last rows
str(edidiv)                 # Tells you whether the variables are continuous, integers, categorical or characters

head(edidiv$taxonGroup)     # Displays the first few rows of this column only
class(edidiv$taxonGroup)    # Tells you what type of variable we're dealing with: it's character now but we want it to be a factor

edidiv$taxonGroup <- as.factor(edidiv$taxonGroup)     # What are we doing here?!

# More exploration
dim(edidiv)                 # Displays number of rows and columns
summary(edidiv)             # Gives you a summary of the data
summary(edidiv$taxonGroup)  # Gives you a summary of that particular variable (column) in your dataset


Beetle <- filter(edidiv, taxonGroup == "Beetle")
# The first argument of the function is the data frame, the second argument is the condition you want to filter on. Because we only want the beetles here, we say: the variable taxonGroup MUST BE EXACTLY (==) Beetle - drop everything else from the dataset. (R is case-sensitive so it's important to watch your spelling! "beetle" or "Beetles" would not have worked here.)

Bird <- filter(edidiv, taxonGroup == "Bird")   # We do the same with birds. It's very similar to filtering in Excel if you are used to it.
# You can create the objects for the remaining taxa. If you need to remind yourself of the names and spellings, type summary(edidiv$taxonGroup)


Butterfly <- filter(edidiv, taxonGroup == "Butterfly") 
Dragonfly <- filter(edidiv, taxonGroup == "Dragonfly") 
Flowering.Plants <- filter(edidiv, taxonGroup == "Flowering.Plants") 
Fungus <- filter(edidiv, taxonGroup == "Fungus") 
Hymenopteran <- filter(edidiv, taxonGroup == "Hymenopteran") 
Lichen <- filter(edidiv, taxonGroup == "Lichen") 
Liverwort <- filter(edidiv, taxonGroup == "Liverwort") 
Mammal <- filter(edidiv, taxonGroup == "Mammal") 
Mollusc <- filter(edidiv, taxonGroup == "Mollusc") 




a <- length(unique(Beetle$taxonName))
b <- length(unique(Bird$taxonName))
c <- length(unique(Butterfly$taxonName))
d <- length(unique(Dragonfly$taxonName))
e <- length(unique(Flowering.Plants$taxonName))
f <- length(unique(Fungus$taxonName))
g <- length(unique(Hymenopteran$taxonName))
h <- length(unique(Lichen$taxonName))
i <- length(unique(Liverwort$taxonName))
j <- length(unique(Mammal$taxonName))
k <- length(unique(Mollusc$taxonName))
# You can choose whatever names you want for your objects, here I used a, b, c, d... for the sake of brevity.

biodiv <- c(a,b,c,d,e,f,g,h,i,j,k)     # We are chaining together all the values; pay attention to the object names you have calculated and their order
names(biodiv) <- c("Beetle",
                   "Bird",
                   "Butterfly",
                   "Dragonfly",
                   "Flowering.Plants",
                   "Fungus",
                   "Hymenopteran",
                   "Lichen",
                   "Liverwort",
                   "Mammal",
                   "Mollusc")

barplot(biodiv)

help(barplot)     # For help with the barplot() function
help(par)         # For help with plotting in general

png("barplot.png", width=1600, height=600)  # look up the help for this function: you can customise the size and resolution of the image
barplot(biodiv, xlab="Taxa", ylab="Number of species", ylim=c(0,600), cex.names= 1.5, cex.axis=1.5, cex.lab=1.5)
dev.off()
# The cex code increases the font size when greater than one (and decreases it when less than one). 

# Creating an object called "taxa" that contains all the taxa names
taxa <- c("Beetle",
          "Bird",
          "Butterfly",
          "Dragonfly",
          "Flowering.Plants",
          "Fungus",
          "Hymenopteran",
          "Lichen",
          "Liverwort",
          "Mammal",
          "Mollusc")
# Turning this object into a factor, i.e. a categorical variable
taxa_f <- factor(taxa)

# Combining all the values for the number of species in an object called richness
richness <- c(a,b,c,d,e,f,g,h,i,j,k)

# Creating the data frame from the two vectors
biodata <- data.frame(taxa_f, richness)

# Saving the file
write.csv(biodata, file="biodata.csv")  # it will be saved in your working directory

png("barplot2.png", width=1600, height=600)
barplot(biodata$richness, names.arg=c("Beetle",
                                      "Bird",
                                      "Butterfly",
                                      "Dragonfly",
                                      "Flowering.Plants",
                                      "Fungus",
                                      "Hymenopteran",
                                      "Lichen",
                                      "Liverwort",
                                      "Mammal",
                                      "Mollusc"),
        xlab="Taxa", ylab="Number of species", ylim=c(0,600))
dev.off()

# Calculate the mean wingspan for each bird species. The function to do that is simply: mean()
sparrow <- mean(22, 24, 21)
kingfisher <- mean(26, 23, 25)
eagle <- mean(195, 201, 185)
hummingbird <- mean(8, 9, 9)

# Chain them together in a vector
wingspan <- c(sparrow, kingfisher, eagle, hummingbird)

# Create a bird species vector (careful to match the order of the previous vector!)
bird_sp <- c("sparrow", "kingfisher", "eagle", "hummingbird")
# notice how we put quotation marks around the names. It's because we're creating (character) values; writing sparrow without the "" would call the object we created in the code above, which would return the value 22!

# Bird species is currently in character form, but it should be a factor. Let's fix that:
# (To be honest it does not make any difference to the output here, but it would for some other types of plot. Take good habits early!)
class(bird_sp)                      # currently character
bird_sp <- as.factor(bird_sp)       # transforming into factor
class(bird_sp)                      # now a factor! 


# Then, combine the two vectors in a data frame
wings <- data.frame(bird_sp, wingspan)

# Plot the bar plot & save it to file

png("wingspan_plot.png", width=800, height=600)
barplot(wings$wingspan, names.arg = wings$bird_sp,    
        xlab = "Bird species", 
        ylab = "Average wingspan (cm)",               
        ylim = c(0, 200))
dev.off()

