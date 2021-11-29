install.packages("readr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("broom")
install.packages("ggplot2")
install.packages("ggExtra")
install.packages("maps")
install.packages("RColorBrewer")

library(readr)
library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)
library(ggExtra)
library(maps)
library(RColorBrewer)

load("LPIdata_Feb2016.RData")
load("puffin_GBIF.RData")

#1. Formatting and tidying data using tidyr
#Reshaping data frames using gather()

View(head(LPIdata_Feb2016))

LPI_long <- gather(data = LPIdata_Feb2016, key = "year", value = "pop", 26:70)

LPI_long$year <- parse_number(LPI_long$year)

#Using sensible variable names
names(LPI_long)

names(LPI_long) <- gsub(".", "_", names(LPI_long), fixed = TRUE)
names(LPI_long) <- tolower(names(LPI_long))

LPI_long$genus_species_id <- paste(LPI_long$genus, LPI_long$species, LPI_long$id, sep = "_")

View(LPI_long[c(1:5,500:505,1000:1005),])
# You can use [] to subset data frames [rows, columns]
# If you want all rows/columns, add a comma in the row/column location

LPI_long$country_list <- gsub(",", "", LPI_long$country_list, fixed = TRUE)
LPI_long$biome <- gsub("/", "", LPI_long$biome, fixed = TRUE)

#2. Efficiently manipulating data using dplyr

LPI_long <- distinct(LPI_long)

LPI_long_fl <- filter(LPI_long, is.finite(pop))

LPI_long <- LPI_long_fl %>%
  group_by(genus_species_id) %>%  # group rows so that each group is one population
  mutate(maxyear = max(year), minyear = min(year),  # Create columns for the first and most recent years that data was collected
         lengthyear = maxyear-minyear,  # Create a column for the length of time data available
         scalepop = (pop-min(pop))/(max(pop)-min(pop))) %>%  # Scale population trend data so that all values are between 0 and 1
  filter(is.finite(scalepop),  # remove NAs
         lengthyear > 5) %>%  # Only keep rows with more than 5 years of data
  ungroup()  # Remove any groupings you've greated in the pipe

LPI_biome_summ <- LPI_long %>%
  group_by(biome) %>%  # Group by biome
  summarise(populations = n(),   # Create columns, number of populations
            mean_study_length_years = mean(lengthyear),  # mean study length
            max_lat = max(decimal_latitude),  # max latitude
            min_lat = min(decimal_latitude),  # max longitude
            dominant_sampling_method = names(which.max(table(sampling_method))),  # modal sampling method
            dominant_units = names(which.max(table(units))))  # modal unit type

#3. Automating data manipulation using lapply(), loops and pipes
# Create a list of data frames by splitting `LPI_long` by population (`genus_species_id`)
LPI_long_list <- split(LPI_long, f = LPI_long$genus_species_id)  # This takes a couple minutes to run

# `lapply()` a linear model (`lm`) to each data frame in the list and store as a list of linear models
LPI_list_lm <- lapply(LPI_long_list, function(x) lm(scalepop ~ year, data = x))

# Extract model coefficients and store them in a data frame
LPI_models_lapply <- filter(data.frame(
  "genus_species_id" = names(LPI_list_lm),
  "n" = unlist(lapply(LPI_list_lm, function(x) df.residual(x))),
  "intercept" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[1])),
  "slope" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[2])),
  "intercept_se" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[3])),
  "slope_se" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[4])),
  "intercept_p" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[7])),
  "slope_p" = unlist(lapply(LPI_list_lm, function(x) summary(x)$coeff[8])),
  "lengthyear" = unlist(lapply(LPI_long_list, function(x) max((x)$lengthyear)))
), n > 5)

# Create a data frame to store results
LPI_models_loop <- data.frame()

for(i in unique(LPI_long$genus_species_id)) {
  frm <- as.formula(paste("scalepop ~ year"))
  mylm <- lm(formula = frm, data = LPI_long[LPI_long$genus_species_id == i,])
  sum <- summary(mylm)
  
  # Extract model coefficients
  n <- df.residual(mylm)
  intercept <- summary(mylm)$coeff[1]
  slope <- summary(mylm)$coeff[2]
  intercept_se <- summary(mylm)$coeff[3]
  slope_se <- summary(mylm)$coeff[4]
  intercept_p <- summary(mylm)$coeff[7]
  slope_p <- summary(mylm)$coeff[8]
  
  # Create temporary data frame
  df <- data.frame(genus_species_id = i, n = n, intercept = intercept, 
                   slope = slope, intercept_se = intercept_se, slope_se = slope_se,
                   intercept_p = intercept_p, slope_p = slope_p, 
                   lengthyear = LPI_long[LPI_long$genus_species_id == i,]$lengthyear, stringsAsFactors = F)
  
  # Bind rows of temporary data frame to the LPI_mylmels_loop data frame
  LPI_models_loop <- rbind(LPI_models_loop, df)
  
}

# Remove duplicate rows and rows where degrees of freedom <5
LPI_models_loop <- distinct(LPI_models_loop)
LPI_models_loop <- filter(LPI_models_loop, n > 5)

LPI_models_pipes <- LPI_long %>%
  group_by(genus_species_id, lengthyear) %>% 
  do(mod = lm(scalepop ~ year, data = .)) %>%  # Create a linear model for each group
  mutate(n = df.residual(mod),  # Create columns: degrees of freedom
         intercept = summary(mod)$coeff[1],  # intercept coefficient
         slope = summary(mod)$coeff[2],  # slope coefficient
         intercept_se = summary(mod)$coeff[3],  # standard error of intercept
         slope_se = summary(mod)$coeff[4],  # standard error of slope
         intercept_p = summary(mod)$coeff[7],  # p value of intercept
         slope_p = summary(mod)$coeff[8]) %>%  # p value of slope
  ungroup() %>%
  mutate(lengthyear = lengthyear) %>%  # adding back the duration column, otherwise it won't be saved in the object
  filter(n > 5) # Remove rows where degrees of freedom <5

system.time(
  LPI_models_pipes <- LPI_long %>%
    group_by(., genus_species_id) %>%
    do(mod = lm(scalepop ~ year, data = .)) %>%
    mutate(., n = df.residual(mod),
           intercept=summary(mod)$coeff[1],
           slope=summary(mod)$coeff[2],
           intercept_se=summary(mod)$coeff[3],
           slope_se=summary(mod)$coeff[4],
           intercept_p=summary(mod)$coeff[7],
           slope_p=summary(mod)$coeff[8]) %>%
    filter(., n > 5)
)

save(LPI_models_pipes, file = "LPI_models_pipes.RData")

LPI_models_pipes_mod <- LPI_models_pipes %>% select(-mod)  # Remove `mod`, which is a column of lists (... META!)
write.csv(LPI_models_pipes_mod, file="LPI_models_pipes.csv", )  # This takes a long time to save, don't run it unless you have time to wait.

#4. Automating data visualisation using ggplot2 and dplyr

theme_LPI <- function(){
  theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),
          axis.title.y = element_text(size = 14, face = "plain"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9))
}

install.packages("colourpicker")

biome.plots <- LPI_long %>%
  nest_by(genus_species_id, biome) %>% # Group by genus species ID and biome
  mutate(mod =list(lm(scalepop ~ year, data = data))) %>% # Run your linear model
  summarise(tidy(mod)) %>%  # Extract model coefficients
  dplyr::select(genus_species_id, biome, term, estimate) %>%  # Selecting only the columns we need
  spread(term, estimate)  %>% # Splitting the estimate values in two columns - one for intercept, one for year
  unnest(cols = c(genus_species_id,biome)) %>% # We need to get out of our previous grouping to make a new one 
  do(ggsave(ggplot(., aes(x = year)) + geom_histogram(colour="#8B5A00", fill="#CD8500") + theme_LPI() 
            + xlab("Rate of population change (slopes)"), 
            filename = gsub("", "", paste("Biome_LPI/", unique(as.character(.$biome)), ".pdf", sep = "")), device = "pdf"))

(all_slopes <- ggplot(LPI_models_pipes, aes(x = lengthyear, y = slope)) +
    geom_pointrange(aes(ymin = slope - slope_se, ymax = slope + slope_se)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_LPI() +
    ylab("Population change\n") +  # \n adds a blank line
    xlab("\nDuration (years)"))

map_world <- borders(database = "world", colour = "gray50", fill = "#383838")  # We used the `Colour Picker` Addin to pick the colours

(map_world_puffin <- ggplot() + map_world +  # Plot the map
    geom_point(data = puffin_GBIF,  # Specify the data for geom_point()
               aes(x = decimallongitude,  # Specify the x axis as longitude
                   y = decimallatitude,  # Specify the y axis as latitude
                   colour = scientificname),  # Colour the points based on species name
               alpha = 0.4,  # Set point opacity to 40%
               size = 1) +  # Set point size to 1
    scale_color_brewer(palette = "Set1") +   # Specify the colour palette to colour the points
    theme_classic() +  # Remove gridlines and shading inside the plot
    ylab(expression("Latitude ("*degree*")" )) +  # Add a smarter x axis label
    xlab(expression("Longitude ("*degree*")" )) +  # Add a smarter y axis label
    theme(legend.position = "bottom",  # Move the legend to below the plot
          legend.title = element_blank()))  # Remove the legend title

#5. Species occurrence maps based on GBIF and Flickr data

install.packages("rgbif")
library("rgbif")

UK_code <- isocodes[grep("United Kingdom", isocodes$name), "code"]

occur <- occ_search(scientificName = "Fratercula arctica", country = UK_code, hasCoordinate = TRUE, limit = 3000, year = '2006,2016', return = "data")

str(occur)

library(ggplot2)
library(maps)
library(ggthemes)

(map <- ggplot(occur$data, aes(x = decimalLongitude, y = decimalLatitude)) + 
    # Specify to only present the UK region of the world in the map 
    # Also change the colour, size of map country borders
    borders(database = "world", regions = "UK", colour = "gray40", size = 0.3) +  
    theme_map() + 
    # Change the colour and transparency of the plotted occurrence points 
    geom_point(alpha = 0.4, colour = "red")) 

flickr <- read.table("./flickr_puffins.txt", header = T, sep = "\t")
str(flickr)

library(sp)                                           # load the package
geopics <- flickr[, c(4,5)]                           # subset the dataset to keep coordinates only
coordinates(geopics) <- c("longitude", "latitude")    # make it spatial
plot(geopics)                                         # plot it

which(flickr$latitude < 49.9)
flickr <- flickr[-which(flickr$latitude < 49.9),]

coordinates(flickr) <- c("longitude", "latitude")             # go back to original dataframe and make it spatial
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")     # geographical, datum WGS84
proj4string(flickr) <- crs.geo                                # assign the coordinate system
plot(flickr, pch = 20, col = "steelblue")                     # plot the data

library(rworldmap)
data(countriesLow)
plot(countriesLow, add = T)

library(rgdal)
library(rgeos)
library(raster)
library(maptools)

UK <- getData("GADM", country = "GB", level = 0)

UK_proj <- spTransform(UK, CRS("+proj=tmerc +lat_0=50 +lon_0=-2 +units=m"))
flickr_proj <- spTransform(flickr, CRS("+proj=tmerc +lat_0=50 +lon_0=-2 +units=m"))

UK_diss <- gUnaryUnion(UK_proj)

flickr_terr <- flickr_proj[which(is.na(over(flickr_proj, UK_diss, fn = NULL)) == FALSE),]
flickr_mar <- flickr_proj[which(is.na(over(flickr_proj, UK_diss, fn = NULL)) == TRUE),]

par(mfrow = c(1,2))
plot(flickr_terr)
plot(flickr_mar)

UK_coast <- as(UK_diss, 'SpatialLines')

dist <- gWithinDistance(flickr_terr, UK_coast, dist = 1000, byid = T)
dist.df <- as.data.frame(dist)
flickr_coast <- flickr_terr[which(dist.df == "TRUE"),]

plot(flickr_coast)

flickr_correct <- spRbind(flickr_mar, flickr_coast)
plot(UK_coast)
points(flickr_correct, pch = 20, col = "steelblue")

#density maps

UK.Df <- fortify(UK_diss, region = "ID_0")
flickr.points <- fortify(cbind(flickr_correct@data, flickr_correct@coords))

(plot.years <- ggplot(data = flickr.points, aes(x = longitude, y = latitude)) +  # plot the flickr data
    geom_polygon(data = UK.Df,aes(x = long, y = lat, group = group),            # plot the UK
                 color = "black", fill = "gray82") + coord_fixed() +    # coord_fixed() ensures that one unit on the x-axis is the same length as one unit on the y-axis
    geom_point(color = "dodgerblue4",size = 2,shape = ".")+                   # graphical parameters for points
    stat_density2d(aes(x = longitude,                           # create the density layer based on where the points are
                       y = latitude,  fill = ..level.., alpha = ..level..),   # colour and transparency depend on density
                   geom = "polygon", colour = "grey95",size=0.3) +            # graphical parameters for the density layer
    scale_fill_gradient(low = "yellow", high = "red") +                 # set colour palette for density layer
    scale_alpha(range = c(.25, .5), guide = FALSE) +                    # set transparency for the density layer 
    facet_wrap(~ year) +                                                 # multipanel plot according to the variable "year" in the flickr dataset
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),  # don't display x and y axes labels, titles and tickmarks 
          axis.ticks.x = element_blank(),axis.title.y = element_blank(),   
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          text = element_text(size = 18),legend.position = c(.9, .15),       # size of text and position of the legend
          panel.grid.major = element_blank(),                            # eliminates grid lines from background
          panel.background = element_blank()))                            # set white background

# This will take a while to plot!

