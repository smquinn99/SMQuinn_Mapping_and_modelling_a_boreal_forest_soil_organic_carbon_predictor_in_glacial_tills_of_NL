####now to apply our calibration factor to our raster layer##########
A_sd<- rast("aspatial_sd.tif")

cal_a_sd<- A_sd*a + b

writeRaster(cal_a_sd, file= "CAL_Aspatial_sd.tif", overwrite = TRUE)

uncal_mean<- global(A_sd, fun= "mean", na.rm= TRUE)
cal_mean<- global(cal_a_sd, fun = "mean", na.rm=TRUE)

print(uncal_mean)
print(cal_mean)

