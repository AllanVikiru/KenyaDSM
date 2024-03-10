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