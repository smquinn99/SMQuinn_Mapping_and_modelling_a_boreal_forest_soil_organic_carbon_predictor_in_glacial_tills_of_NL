
####now to apply our calibration factor to our raster layer##########   
s_sd<- rast("results/spatial_sd.tif")

cal_s_sd<- s_sd*a+b

writeRaster(cal_s_sd, file= "results/redo_cal_spatial_sd.tif", overwrite = TRUE)

uncal_mean<- global(s_sd, fun= "mean", na.rm= TRUE)
cal_mean<- global(cal_s_sd, fun = "mean", na.rm=TRUE)

print(uncal_mean)
print(cal_mean)

