# set project directory projections and retrieve soil potassium data
proj_dir <- 'D:/r/KenyaDSM'
wsg84 <- "+init=epsg:4326"
utm <- "+init=epsg:32236"

# select sample data - 1099 :: TODO - ADD NOISE AND SELECT FEATURES
setwd(paste(proj_dir,'data/soil_shp/mg_pts', sep="/"))
soil_path <- getwd()
mg_dat <- as.data.frame(read.csv("sample/mg_sample_model.csv", header = TRUE, sep = ","))

# convert to 70:30 training/testing, fill noise in training to avoid generalisation
library(caret)
part <- createDataPartition(mg_dat$mg, times = 1, p = 0.7, list = FALSE)
mg_train <- mg_dat[part, ]
mg_test <- mg_dat[-part, ]
head(mg_train)
head(mg_test)
# write data
write.csv(mg_train, "sample/mg_train.csv", row.names=FALSE)
write.csv(mg_test, "sample/mg_test.csv", row.names=FALSE)

# select training covariates
library(raster)
setwd(paste(proj_dir,'data/covariates_shp/soil_cov/', sep="/"))
covs_path<- getwd()
mg_covs <-  names(mg_dat[1:5])
mg_files <- paste0(covs_path,"/",mg_covs, ".tif")
mg_covs <- stack(mg_files)
summary(mg_covs)
plot(mg_covs$ph20)

# data prep: convert to spatial format
library(sp)
coordinates(mg_train) <- ~ X + Y
mg_train@proj4string <- CRS(wsg84)
points(mg_train)
coordinates(mg_test) <- ~ X + Y
mg_test@proj4string <- CRS(wsg84)
points(mg_test)

# EDA
hist(mg_dat$mg) # right skewed
hist(log(mg_dat$mg)) #log transformation of targets for normal distribution

# regression equation
mg_eqn <- as.formula(log(mg) ~ elevation+occ+ph20+slope+wetness_index)
# cv <- trainControl(method = "cv", number = 5) # 5 fold CV (80% training, 20% testing)

### run SVM
library(e1071)

mg_svm <- train(mg_eqn, data = mg_train@data, method = 'svmRadial')
print(mg_svm) # RMSE - 0.148 R2 - 0.688, gamma = 0.69, C = 0.5

# confirm tuned hyperparameters of gamma and cost in 5 fold cv
# ct <- tune.control(cross = 5) # 5 fold CV (80% training, 20% testing)
svm_tune <- tune(svm, 
                 mg_eqn, 
                 data = mg_train@data[,c("mg",names(mg_covs))],
                 ranges = list(
                   gamma = seq(0.2,0.8,0.05),
                   cost = seq(0.1,1,0.1)))
plot(svm_tune) # towards 0.2 gamma, 1 cost
svm_tune$best.parameters

# model with tuned hyperparameters
hp <- expand.grid(C = 1,sigma = 0.4)

mg_svm <- train(mg_eqn, data = mg_train@data, method = 'svmRadial', tuneGrid = hp)
# RMSE - 0.145 R2 - 0.699

# generate svm maps
mg_svmpred <- exp(predict(mg_covs, mg_svm))
plot(mg_svmpred)

# export model and map
setwd(paste(proj_dir,'map_model/models', sep="/"))
model_path <- getwd()
saveRDS(mg_svm, file="mg_svm.Rds")

setwd(soil_path)
writeRaster(mg_svmpred, filename = "models/mgsvm_pred.tif")

### run MLR
mg_mlr <- train(mg_eqn, data = mg_train@data, method= "lm")
print(mg_mlr) # R-squared - 0.511 RMSE - 0.186

# generate MLR model maps
mg_mlrpred <- exp(predict(mg_covs, mg_mlr))
plot(mg_mlrpred)

# export model and map
setwd(model_path)
saveRDS(mg_mlr, file="mg_mlr.Rds")

setwd(soil_path)
writeRaster(mg_mlrpred, filename = "models/mgmlr_pred.tif")

# transform CRS to projected coordinates for kriging
mg_train <- spTransform(mg_train, CRS(utm))
mg_covs <- projectRaster(mg_covs, crs = CRS(utm),method='bilinear')
mg_covs <- as(mg_covs, "SpatialGridDataFrame")
mg_train <- mg_train[which(!duplicated(mg_train@coords)),] #remove any duplicates

#### run OK
library(automap)
mg_ok <- autoKrige(log(mg) ~ 1, 
                  input_data = mg_train, 
                  verbose = TRUE)
plot(mg_ok)
summary(mg_ok)

# 5 fold cv of OK model
mg_ok_cv <- autoKrige.cv(formula = log(mg) ~ 1,
                        input_data = mg_train,
                        verbose = c(interactive(), interactive()),
                        nfold = 5)
summary(mg_ok_cv) # RMSE 0.1008
mg_ok_mod <- lm(mg_ok_cv$krige.cv_output@data$var1.pred ~ mg_ok_cv$krige.cv_output@data$observed)
summary(mg_ok_mod) # R2 = 0.855

# generate OK maps
mg_okpred <- exp(raster(mg_ok$krige_output[1])) # predicted K map
# mg_okpredsd <- exp(raster(mg_ok$krige_output[3])) # st error of predictions

mg_okpred <- projectRaster(mg_okpred, crs = wsg84)
# mg_okpredsd <- projectRaster(mg_okpredsd, crs = wsg84)

plot(mg_okpred)
# plot(mg_okpredsd)

# export model and map
setwd(model_path)
saveRDS(mg_ok, file="mg_ok.Rds")

setwd(soil_path)
writeRaster(mg_okpred, filename = "models/mgok_pred.tif")

# run MLRK
mg_mlrk <- autoKrige(formula = as.formula(mg_eqn), 
                    input_data = mg_train, 
                    new_data = mg_covs,
                    verbose = TRUE)
plot(mg_mlrk)

# mg_mlrk_cv <- autoKrige.cv(formula = as.formula(mg_eqn),
#                        input_data = mg_train,
#                        verbose = c(interactive(), interactive()),
#                        nfold = 5)
# summary(mg_mlrk_cv) # RMSE 0.1015
# 
# mg_mlkr_mod <- lm(mg_mlrk_cv$krige.cv_output@data$var1.pred ~ mg_mlrk_cv$krige.cv_output@data$observed)
# summary(mg_mlkr_mod) # R2 = 0.853

# generate MLRK maps
mg_mlrkpred <- exp(raster(mg_mlrk$krige_output[1])) # predicted K map
# mg_mlrkpredsd <- exp(raster(mg_mlrk$krige_output[3])) # st error of predictions
mg_mlrkpred <- projectRaster(mg_mlrkpred, crs = wsg84)
plot(mg_mlrkpred)

# export model and map
setwd(model_path)
saveRDS(mg_mlrk, file="mg_mlrk.Rds")
setwd(soil_path)
writeRaster(mg_mlrkpred, filename = "models/mgmlrk_pred.tif", overwrite=TRUE)