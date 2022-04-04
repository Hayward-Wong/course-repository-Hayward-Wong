library(data.table)
library(rnrfa)
library(ggplot2)
library(OpenStreetMap)
library(rgdal)
library(tidyverse)
library(dplyr)
library(ggspatial)
library(ggthemes)

install.packages("ggspatial")


cs_bnm <-read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/Chequered Skipper Scotland.csv", stringsAsFactors = FALSE)

table(cs_bnm$rec_year)

cs_2000 <- cs_bnm[cs_bnm$rec_year == 2019,]

cs_2000 <- cs_2000 %>% 
  mutate(adults = parse_number(adults))

cs_2000[is.na(cs_2000)] = 0

cs<- cs_2000 %>% 
  mutate(abundance = adults, easting = x, northing = y)

cs<- cs %>% 
  select(gridref,km_sq,abundance,rec_month,rec_year,easting,northing)

ggplot(cs, aes(easting, northing))+geom_point()+coord_fixed()

# Produce a map with background
# get easting and northing as lat/long
ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"
# Create spatial dataframe
cs_sp <- SpatialPoints(cs[,c("easting","northing")],
                       proj4string = CRS(ukgrid))

# Convert to lat/long
cs_sp_ll <- spTransform(cs_sp, CRS(latlong))
cs_df_ll <- data.frame(cs_sp_ll)
colnames(cs_df_ll) <- c("Long","Lat")
# Get bounding box values for mf points
bbox <- cs_sp_ll@bbox
# Define area for base map
map <- openmap(upperLeft = c(bbox[2,2]+1, bbox[1,1]-1), 
               lowerRight =  c(bbox[2,1]-1,bbox[1,2]+1),
               type = "osm", minNumTiles = 20)
# Set CRS for basemap
map.latlon <- openproj(map, projection =  cs_sp@proj4string)




OpenStreetMap::autoplot.OpenStreetMap(map.latlon)+
  geom_point(data = cs, aes(easting, northing))+
  theme(panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA))+
  xlab(element_blank())+ylab(element_blank())+
  theme_void()+
  xlim(min(cs$easting)-20000, max(cs$easting)+20000)+
  ylim(min(cs$northing)-20000, max(cs$northing)+20000)+  
  annotation_scale()+
  annotation_north_arrow(location = "tr", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering, 
                         height = unit(0.5,"in"), 
                         width = unit(0.5,"in"))+
  labs(title = "Scotland Carterocephalus palaemon Presence Records in 2019")+ # add title to the map
  theme(plot.title = element_text(size = 10, hjust = 0.5))



minE  <- min(cs$easting)-20000
maxE <- max(cs$easting)+20000
minN <- min(cs$northing)-20000
maxN <- max(cs$northing)+20000


