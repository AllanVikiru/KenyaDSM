#select food data
install.packages('tidyverse', 'fs', 'readxl')
library(tidyverse)
library(fs)
library(readxl)
library(dplyr)

#import raw data
setwd("D:/r/KenyaDSM/data")
raw_foods <- as.data.frame(read_excel("food_composition_tables.xlsx", 4))

# clean data
raw_foods <- raw_foods[307:470, c(2, 12,14,16,17)] # select vegetables and nutrients
colnames(raw_foods) <- c('Vegetable', 'Calcium', 'Magnesium', 'Potassium', 'Sodium')

duplicated(raw_foods$Vegetable) #check for duplicates
is.null(raw_foods) #check for nulls
str(raw_foods) #check data types

raw_foods <- raw_foods[!(is.na(raw_foods$Vegetable) | raw_foods$Vegetable==""), ] # remove null foods
raw_foods <- raw_foods[!duplicated(raw_foods$Vegetable), ] # remove duplicate foods
raw_foods <- raw_foods %>% mutate_at(c('Calcium', 'Magnesium', 'Potassium', 'Sodium'), as.numeric) # convert nutrients to num
raw_foods <- raw_foods %>% mutate(across(where(is.numeric), round, 3)) # round off to 3.dp
write.csv(raw_foods, "veg_nutrients.csv", row.names=FALSE) # export csv

# descriptive stats of veg nutrients
summary(raw_foods)
boxplot(raw_foods[,2:5])
# select potassium because of uniform distribution and reject sodium 
boxplot(raw_foods$Potassium)
# compare Mg and Ca since they are similar
hist(raw_foods$Magnesium)
hist(raw_foods$Calcium)
# select Magnesium for slightly more varied distribution between Mg and Ca

#export veg, potassium and magnesium data
write.csv(raw_foods[,c(1,3,4)], "veg_k_mg.csv", row.names=FALSE) # export csv

#prepare soil data
#read TIFF files
library(raster)
library(terra)
library(sf)
af_k <- raster("af250m_nutrient_k_m_agg30cm.tif")
af_mg <- raster("af250m_nutrient_mg_m_agg30cm.tif")

# bind KE based on coordinates
ken<- st_read("./ke_shp", "ken_admbnda_adm1_iebc_20191031")
ke_crs <- "+proj=longlat +datum=WGS84 +no_defs" # set crs
ke_epsg <- 32737 # "+init=EPSG:32737"
ken_k <- crop(af_k, ken)
ken_mg <- crop(af_mg,ken)
writeRaster(ken_k, 'kenya_k.tif') # export .tif files to relieve memory usage
writeRaster(ken_mg, 'kenya_mg.tif')

# derive data points from raster layers
ken_k <- raster("kenya_k.tif")
ken_mg <- raster("kenya_mg.tif")
ken_k_spdf <- extract(ken_k, SpatialPoints(ken_k),sp=T)
ken_mg_spdf <- extract(ken_mg, SpatialPoints(ken_mg), sp=T)
ken_k_mg_df <- cbind(ken_k_spdf@coords, ken_k_spdf@data, ken_mg_spdf@data)

# prepare full soil dataset
colnames(ken_k_mg_df) <- c('long', 'lat', 'k', 'mg')
summary(ken_k_mg_df)
ken_k_mg_df <- na.omit(ken_k_mg_df) # remove null values
write.csv(ken_k_mg_df, "soil_k_mg.csv", row.names=FALSE) # export csv

## PROCESSES DONE IN QGIS ##

#aggregate/cluster data points to reduce data size (DBSCAN CANT DERIVE CENTRES)
library(dbscan)

k_train <- st_read("./soil_shp", "k_train_pts")
k_test <- st_read("./soil_shp", "k_test_pts")
  
k_coords <- ken_k_mg_pts[1:2]
eps_plot <- kNNdistplot(k_, minPts = 3) # define suitable distance for k with minPts = dimensionality + 1

db <- dbscan(ken_coords, eps = 0.0045, minPts = 2) # define clusters
plot(ken_coords$long, ken_coords$lat, col = db$cluster, pch=20)
clr_centres <- st_coordinates(st_centroid(st_sfc(st_multipoint(ken_coords[db$cluster != 0,]))))

# perform point_in_polygon
ken_k_mg_sf <- st_as_sf(ken_k_mg_pts, coords = c("long", "lat")) %>% st_set_crs(., ke_crs)
ken_k_mg_pts <- st_intersection(ke_area_sf,ken_k_mg_sf$geometry)

# prepare data points and plot on map
library(sf)
library(ggplot2)
library(RColorBrewer)

soil_pts = st_as_sf(isda_soil, coords = c('longitude', 'latitude'), crs = "+init=EPSG:32737")
plot(st_geometry(ke_area$geometry), axes = TRUE, graticule = TRUE)
plot(soil_pts, axes = TRUE, graticule = TRUE, pch = 16, 
     pal = brewer.pal(5, 'Paired'), add = TRUE)