#set project directory
proj_dir <- 'D:/r/KenyaDSM'

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
ke_epsg <- 32737 # "+init=EPSG:32737" # world espg: 4326 (recommended for covariates)
ken_k <- crop(af_k, ken)
ken_mg <- crop(af_mg,ken)
writeRaster(ken_k, 'ken_k.tif') # export .tif files to relieve memory usage
writeRaster(ken_mg, 'ken_mg.tif')

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

## PROJECT DISTINCT SOIL DATA PREPARATION DONE IN QGIS ##