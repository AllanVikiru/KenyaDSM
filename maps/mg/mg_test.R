# set project directory projections and retrieve soil potassium data
proj_dir <- 'D:/r/KenyaDSM'
wsg84 <- "+init=epsg:4326"
utm <- "+init=epsg:32236"

# select test data - 323
setwd(paste(proj_dir,'data/soil_shp/mg_pts', sep="/"))
soil_path <- getwd()
mg_test <- as.data.frame(read.csv("sample/mg_test.csv", header = TRUE, sep = ","))
coordinates(mg_test) <- ~ X + Y
mg_test@proj4string <- CRS(wsg84)

# select generated maps
library(raster)
mgsvm_pred <- raster("models/mgsvm_pred.tif", sep="/")
mgmlr_pred <- raster("models/mgmlr_pred.tif", sep="/")
mgok_pred <- raster("models/mgok_pred.tif", sep="/")
mgmlrk_pred <- raster("models/mgmlrk_pred.tif", sep="/")

# estimate prediction errors
mg_test <- extract(x = mgsvm_pred, y = mg_test, sp = TRUE)
mg_test <- extract(x = mgmlr_pred, y = mg_test, sp = TRUE)
mg_test <- extract(x = mgok_pred, y = mg_test, sp = TRUE)
mg_test <- extract(x = mgmlrk_pred, y = mg_test, sp = TRUE)
mg_test$PE_svm <- mg_test$mgsvm_pred - mg_test$mg
mg_test$PE_mlr <- mg_test$mgmlr_pred - mg_test$mg
mg_test$PE_ok <- mg_test$mgok_pred - mg_test$mg
mg_test$PE_mlrk <- mg_test$mgmlrk_pred - mg_test$mg

summary(mg_test)
# export validation tests
write.csv(mg_test, "models/validation.csv", row.names = F)

# calculate R-squared
R2_svm <- lm(mg_test$mgsvm_pred ~ mg_test$mg)
summary(R2_svm) # Adjusted R-squared:  0.71
R2_mlr <- lm(mg_test$mgmlr_pred ~ mg_test$mg)
summary(R2_mlr) # Adjusted R-squared:  0.42
R2_ok <- lm(mg_test$mgok_pred ~ mg_test$mg)
summary(R2_ok) # Adjusted R-squared:  0.91 
R2_mlrk <- lm(mg_test$mgmlrk_pred ~ mg_test$mg)
summary(R2_mlrk) # Adjusted R-squared:  0.93

# calculate Root Mean Squared Error (RMSE)
RMSE_svm <- sqrt(sum(mg_test$PE_svm^2, na.rm=TRUE) / length(mg_test$PE_svm)) # 39.95
RMSE_mlr <- sqrt(sum(mg_test$PE_mlr^2, na.rm=TRUE) / length(mg_test$PE_mlr)) # 59.25
RMSE_ok <- sqrt(sum(mg_test$PE_ok^2, na.rm=TRUE) / length(mg_test$PE_ok)) # 21.70
RMSE_mlrk <- sqrt(sum(mg_test$PE_mlrk^2, na.rm=TRUE) / length(mg_test$PE_mlrk)) # 20.5