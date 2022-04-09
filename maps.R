library(data.table)
library(biomod2)
library(ggplot2)
library(rgdal)
library(raster)
library(ncdf4)
library(sf)

# Read in CS data (already prepped)

cs_bnm <-read.csv("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/Chequered Skipper Scotland.csv", stringsAsFactors = FALSE)

cs_bnm <- cs_bnm %>% subset(km_sq !="")


cs_bnm <- cs_bnm[cs_bnm$rec_year == 2009,]

cs_bnm<- cs_bnm %>% 
  mutate(adults = parse_number(adults))



cs_bnm[is.na(cs_bnm)] = 0

cs_bnm<- cs_bnm %>% 
  filter(is.finite(adults))

cs_bnm <- cs_bnm %>% mutate(ID = X_uid_)



bnm2 <- cs_bnm %>% group_by(km_sq) %>%
  summarise(Presence = sum(adults))

bnm2<- bnm2 %>% mutate_if(is.numeric, ~1 * (. > 0))


bnm2<- bnm2 %>%  subset(Presence !="0")

cs_bnm<- cs_bnm %>% 
  mutate(easting = x, northing = y)


bnm<- cs_bnm %>% select(km_sq,easting,northing)

bnm_n <- bnm[!duplicated(bnm$km_sq), ]

bnm_3 <- merge(bnm_n,bnm2,by="km_sq")


csdata <- bnm_3

write.csv(csdata, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/data/BNM_2009.csv")


ggplot(csdata, aes(easting, northing, color=Presence))+geom_point()


# Read in weather data
climate <- stack("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/haduk_1km_av_climate_2000_2017.tif")
names(climate) <- c("WinterT","SpringT","SummerT","AutumnT","Rainfall")

#plot(climate)

cor(as.matrix(climate), use = "complete.obs")

# Read in land cover data
lcm <- stack("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/LCM2015_GB_1km_percent_cover_aggregate_class.tif")
lcnames <- c("broadleaf.woodland",
             "coniferous.woodland",
             "arable",
             "improved.grassland",
             "semi.natural.grassland",
             "mountain.heath.bog",
             "saltwater","freshwater",
             "coastal",
             "urban")
names(lcm) <- lcnames

lcm <- subset(lcm, lcnames[!lcnames %in% c("saltwater","freshwater")])

lcmScot <- stack(crop(lcm, extent(90000,233000,621000,817000)))


elev <-  raster("C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/scripts and model templates/elevation_wscot_1km_raster.tif")
elev <- crop(elev, extent(lcmScot))
crs(lcmScot) <- crs(elev)

# Clip landcover to area of interest using elevation raster
lcmScot <- mask(lcmScot, elev)

#plot(lcmScot)

#plot(elev)

# Add a layer to the lcm raster stack for the total lc cover
temp <- stackApply(lcmScot, indices = c(rep(1,10)), fun=sum, na.rm=TRUE)

# Clip climate
crs(climate) <- crs(elev)
climateScot <- mask(climate, elev)

# Remove 1km with less than 25% landcover classified
lcmScot[temp < 25] <- NA
elev[temp < 25] <- NA
climateScot[temp < 25] <- NA

covs <- stack(lcmScot, elev, climateScot)

cov_cors <- cor(as.matrix(covs), use = "complete.obs")

apply(cov_cors, 2, function(x){x[x<1 & x > .7]})

csdata_sp <- csdata[, .(easting, northing, Presence)]
coordinates(csdata_sp) <- ~ easting + northing
crs(csdata_sp) <- crs(elev)

# subset to less weather variables due to high correlation
covsfit <- subset(covs, c(lcnames,"SummerT","elevation_wscot_1km_raster"))


#writeRaster(covsfit, "C:/Users/heiwu/OneDrive/Documents/course-repository-Hayward-Wong/Dissertation/Environment.tif", overwrite=TRUE)


# Format data for biomod
biomdata <- BIOMOD_FormatingData(resp.var = csdata_sp,
                                 expl.var = covsfit,
                                 resp.name="Chequered Skipper")
plot(biomdata)