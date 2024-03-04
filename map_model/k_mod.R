# set project directory projections and retrieve soil potassium data
proj_dir <- 'D:/r/KenyaDSM'
wsg84 <- "+init=epsg:4326"
utm <- "+init=epsg:32236"

setwd(paste(proj_dir,'data/soil_shp/k_pts', sep="/"))
soil_path <- getwd()
k_dat <- as.data.frame(read.csv("k_subsample_model_cov.csv", header = TRUE, sep = ","))

# select training covariates
library(raster)
setwd(paste(proj_dir,'data/covariates_shp/soil_cov/', sep="/"))
covs_path<- getwd()
k_covs <-  names(k_dat[1:5])
k_files <- paste0(covs_path,"/",k_covs, ".tif")
k_covs <- stack(k_files)
plot(k_covs$elevation)
summary(k_covs)

# data prep: convert to spatial format
library(sp)
coordinates(k_dat) <- ~ X + Y
k_dat@proj4string <- CRS(wsg84)
points(k_dat)

# EDA
# k_train <- k_dat@data # prepare df
# k_train <- k_dat[, c("k", names(k_covs))] # reorder columns
# k_train <- k_train[(k_train$annual_precip & k_train$land_elevation & k_train$occ) != 0, ] # remove covs 0s for modelling
hist(k_dat$k) # right skewed
hist(log(k_dat$k)) #log transformation of targets for normal distribution
k_dat$log_k <- log(k_dat$k)


k_eqn <- as.formula(log_k ~ elevation+slope+temp_annual_range+treecover+wetness_index)
# k_eqn <- as.formula(log_k ~ annual_precip+clay+h20cap_pf2_0)


# run MLR
k_lm <- lm(k_eqn, k_dat)
summary(k_lm) 

# run MLR-CV
k_train <- k_dat@data # prepare df
library(caret)
cv <- trainControl(method = "cv", number = 5) # 5 fold CV
k_mlr <- train(k_eqn, data = k_train, method= "lm", trControl = cv)
print(k_mlr) # R-squared  - 0.66 RMSE - 0.25

## TODO: PRINT MLR MODEL MAPS ##

# transform CRS to projected coordinates for kriging
k_dat <- spTransform(k_dat, CRS(utm))
k_covs <- projectRaster(k_covs, crs = CRS(utm),method='bilinear')
k_covs <- as(k_covs, "SpatialGridDataFrame")
k_dat <- k_dat[which(!duplicated(k_dat@coords)),] #remove any duplicates

## run IDW
library(gstat)
k_idw <- krige(log_k ~ 1, k_dat, k_covs)
summary(k_idw)

k_idw.na <- na.omit(k_idw)
idw.pred <- rasterFromXYZ(as.data.frame(k_idw)[, c("s1", "s2", "var1.pred")])
plot(idw.pred)

library(automap)
k_idw.cv <- autoKrige.cv(log_k ~ 1, k_dat)
idw_mod <- lm(k_idw.cv$krige.cv_output@data$var1.pred ~ k_idw.cv$krige.cv_output@data$observed)

#####
# run OK
library(gstat)
library(automap)

k_autovgm <- formula(log_k ~ 1, k_dat, model="Sph")

# variogram: closer things are more predictable and have less variability
# set K to variogram model - for each pair of points, the gamma value/semivariance between them is plotted against the distance between them

k_exp_vgm <- variogram(log_k ~ 1, k_dat)
plot(k_exp_vgm) # experimental variogram

# nugget: start of points at x; range: point at y that flats out
# sill : point at x that flats out; p-sill: sill - nugget

# theoretical variogram
k_th_vgm <- vgm(psill = 0.147, model="Lin", range=39500, nugget = 0.008) # N - 0.02, R: 38000, S: 0.07 PS: 0.055 
plot(k_exp_vgm, k_th_vgm)

# fit theoretical and experimental variograms
k_fit_vgm <- fit.variogram(k_exp_vgm, k_th_vgm)
plot(k_fit_vgm)

#ordinary kriging
k_ok <- krige(log_k ~ 1, 
              locations = k_dat,
              newdata = k_covs, 
              model= k_fit_vgm,
              beta = 0)

plot(k_ok["var1.pred"], main = "Log Soil Potassium (OK)")
k_ok$pred_k <- exp(k_ok$var1.pred)
plot(k_ok["pred_k"], main = "Predicted Soil Potassium (OK)")

#####
library(gstat)
library(automap)
k_dat$residuals <- residuals(k_lm) # fit residuals
k_autovgm <- formula(residuals ~ 1, k_dat, model="Sph")

# variogram: closer things are more predictable and have less variability
# set residuals to variogram model - for each pair of points, the gamma value/semivariance between them is plotted against the distance between them

k_resid_var <- variogram(residuals ~ 1, data = k_dat)
plot(k_resid_var) # experimental variogram

# nugget: start of points at x; range: point at y that flats out
# sill : point at x that flats out; p-sill: sill - nugget

#theoretical variogram
k_resid_vgm <- vgm(psill = 0.056, model = "Sph", range=38000, nugget = 0.014) # N - 0.02, R: 38000, S: 0.07 PS: 0.055 
plot(k_resid_var, k_resid_vgm)

# fit theoretical and experimental variograms
kr_vgm <- fit.variogram(k_resid_var, k_resid_vgm)
plot(k_resid_var, kr_vgm)

#ordinary kriging
k_ok <- krige(residuals ~ 1, 
              locations = k_dat,
              newdata = k_covs, 
              model= kr_vgm,
              beta = 0)

plot(k_ok["var1.pred"], main = "Log Soil Potassium (OK)")
k_ok$pred_k <- exp(k_ok$var1.pred)
plot(k_ok["pred_k"], main = "Predicted Soil Potassium (OK)")




library(automap)
k_ok <- autoKrige(log_k ~ 1, 
                  input_data = k_dat, 
                  verbose = TRUE)
plot(k_ok)
ok_mod <- lm(k_ok$krige_output@data$var1.pred ~ k_ok$krige_output@data$observed)

# run MLRK
k_mlrk <- autoKrige(formula = as.formula(k_eqn), 
                  input_data = k_dat, 
                  new_data = k_covs,
                  verbose = TRUE)
plot(k_mlrk)
## TODO: convert plotted maps


# 10 fold cv of OK model
k_ok_cv <- autoKrige.cv(formula = k ~ 1,
                       input_data = k_dat,
                       verbose = c(interactive(), interactive()), 
                       nfold = 10)

ok_mod <- lm(k_ok_cv$krige.cv_output@data$var1.pred ~ k_ok_cv$krige.cv_output@data$observed)
summary(ok_mod) # R2 = 0.939

k_mlrk_cv <- autoKrige.cv(formula = as.formula(k_eqn),
                       input_data = k_dat, 
                       verbose = c(interactive(), interactive()), 
                       nfold = 10)
summary(k_ok_cv)
mlkr_mod <- lm(k_mlrk_cv$krige.cv_output@data$var1.pred ~ k_mlrk_cv$krige.cv_output@data$observed)
summary(mlkr_mod) # R2 = 0.944

# transformed plots
k_okpred <- exp(raster(k_ok_cv$krige.cv_output[1])) # predicted K map
k_okpredsd <- exp(raster(k_ok_cv$krige.cv_output[3])) # st error of predictions

# plot and export maps
plot(k_okpred)
plot(k_okpredsd)

setwd(soil_path)
writeRaster(k_rkpred, filename = "k_train/rk_pred.tif")
writeRaster(k_rkpredsd, filename = "k_train/rk_pred_sd.tif")

#export models
setwd(paste(proj_dir,'map_model', sep="/"))
saveRDS(k_mlr, file="models/k_mlr.Rds")
saveRDS(k_krige, file="models/k_mlrk.Rds")