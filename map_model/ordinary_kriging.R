# REFER TO RK FOR PREP
## OK ATTEMPT
library(automap)
k_autovgm <- formula(residuals ~ 1, k_train, model="Sph")

k_train$residuals <- residuals(k_mlr) # fit residuals
mg_train$residuals <- residuals(mg_mlr)

# variogram: closer things are more predictable and have less variability
# set residuals to variogram model - for each pair of points, the gamma value/semivariance between them is plotted against the distance between them
k_resid_var <- variogram(residuals ~ 1, k_train)
mg_resid_var <- variogram(residuals ~1, mg_train)
plot(k_resid_var) # experimental variogram
plot(mg_resid_var)
# nugget: start of points at x; range: point at y that flats out
# sill : point at x that flats out; p-sill: sill - nugget

#theoretical variogram
k_resid_vgm <- vgm(psill = 0.03, model = "Sph", range=23000, nugget = 0.01) # N - 0.01, R: 23000, S: 0.04 PS: 0.03 
plot(k_resid_var, k_resid_vgm)

mg_resid_vgm <- vgm(psill = 0.016, model = "Sph", range=28000, nugget = 0.014) # N - 0.014, R: 28000, S: 0.03 PS: 0.016 
plot(mg_resid_var, mg_resid_vgm)

# fit theoretical and experimental variograms
kr_vgm <- fit.variogram(k_resid_var, k_resid_vgm)
plot(k_resid_var, kr_vgm)
mgr_vgm <- fit.variogram(mg_resid_var, mg_resid_vgm)
plot(mg_resid_var, mgr_vgm)

#ordinary kriging
kr_sk <- krige(residuals ~ 1, k_train, newdata = k_covs, model= kr_vgm)