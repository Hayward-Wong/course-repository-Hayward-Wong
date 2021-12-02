#Hi I am Hayward Wong

# Makes a flower pattern

t  <- 1:500 # Made a vector from 1-500

p <- (1 + sqrt(5))*pi # Made a vector with math

jpeg("rplot.jpg", width = 350, height = 350) # Saves the image to local
plot(sqrt(t) * cos(p*t), sqrt(t) * sin(p*t), type = "p", axes = FALSE, ann=FALSE) # Plots the flower pattern
dev.off() # Closes changes to the plot
