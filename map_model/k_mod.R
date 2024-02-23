# set project directory projections and retrieve soil potassium data
proj_dir <- 'D:/r/KenyaDSM'
wsg84 <- "+init=epsg:4326"
utm <- "+init=epsg:32236"

setwd(paste(proj_dir,'data/soil_shp/soil_counties/soil_pts/k_pts', sep="/"))
soil_path <- getwd()
k_dat <- as.data.frame(read.csv("k_model_cov.csv", header = TRUE, sep = ","))
# train_path <- getwd()
# k_dat <- as.data.frame(read.csv("k_model_cov.csv", header = TRUE, sep = ","))
# setwd(paste(proj_dir,'data/soil_shp/k_test', sep="/"))
# test_path <- getwd()
# k_test <- as.data.frame(read.csv("k_model_cov.csv", header = TRUE, sep = ","))
# k_merge <- rbind(k_dat, k_test)

# select training covariates
library(raster)
setwd(paste(proj_dir,'data/covariates_shp/soil_cov/', sep="/"))
covs_path<- getwd()
k_covs <-  names(k_dat[1:11])
k_files <- paste0(covs_path,"/",k_covs, ".tif")
k_covs <- stack(k_files)
plot(k_covs$occ)
summary(k_covs)

# data prep: convert to spatial format
library(sp)
coordinates(k_dat) <- ~ X + Y
k_dat@proj4string <- CRS(wsg84)
points(k_dat)

# EDA
k_train <- k_dat@data # prepare df
k_train <- k_train[, c("k", names(k_covs))] # reorder columns
# k_train <- k_train[(k_train$annual_precip & k_train$land_elevation & k_train$occ) != 0, ] # remove covs 0s for modelling
hist(k_train$k) # right skewed
hist(log(k_train$k)) #log transformation of targets for normal distribution

k_eqn <- as.formula(log(k) ~ annual_precip+clay+h20cap_pf2_0+land_elevation+occ+ph20+silt+temp_annual_range+terrain_slope+tree_cover+wet_index)

# run MLR
# k_merge <- k_merge[, -c(4:5)]
library(caret)
cv <- trainControl(method = "cv", number = 10) # 10 fold CV
k_mlr <- train(k_eqn, data = k_train, method= "lm", trControl = cv)

print(k_mlr) # R-squared  - 0.683 RMSE - 0.2593

## TODO: PRINT MLR MODEL MAPS

# transform CRS to projected coordinates for kriging
k_train <- spTransform(k_dat, CRS(utm))
k_covs <- projectRaster(k_covs, crs = CRS(utm),method='ngb')
k_covs <- as(k_covs, "SpatialGridDataFrame")
k_train <- k_train[which(!duplicated(k_train@coords)),] #remove any duplicates


# run OK
library(automap)
k_ok <- autoKrige.cv(log(k) ~ 1, k_train, model = c("Sph"))
summary(ok)
ok_mod <- lm(ok$krige_output@data$var1.pred ~ ok$krige_output@data$observed)

# regression-kriging prediction.
k_mlkr <- autoKrige.cv(formula = as.formula(k_eqn),
                     input_data = k_train,
                     new_data = k_covs)

summary(k_krige) # RMSE   0.08748

k_mlkr_cv <- lm(k_krige$krige_output@data$var1.pred ~ kkrige$krige_output@data$observed)
summary(k_mlkr_cv) # Adjusted R-squared:  0.9562 

# transformed plots
k_rkpred <- exp(raster(k_krige$krige_output[1])) # predicted K map
k_rkpredsd <- exp(raster(k_krige$krige_output[3])) # st error of predictions

# plot and export maps
plot(k_rkpred)
plot(k_rkpredsd)

setwd(soil_path)
writeRaster(k_rkpred, filename = "k_train/rk_pred.tif")
writeRaster(k_rkpredsd, filename = "k_train/rk_pred_sd.tif")

#export models
setwd(paste(proj_dir,'map_model', sep="/"))
saveRDS(k_mlr, file="models/k_mlr.Rds")
saveRDS(k_krige, file="models/k_mlrk.Rds")