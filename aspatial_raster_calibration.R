####now to apply our calibration factor to our raster layer##########
A_sd<- rast("Results/aspatial_sd.tif")

cal_a_sd<- A_sd*a + b
print(cal_a_sd)
writeRaster(cal_a_sd, file= "Results/final_asp_sd1.tif", overwrite = TRUE)

uncal_mean<- global(A_sd, fun= "mean", na.rm= TRUE)
cal_mean<- global(cal_a_sd, fun = "mean", na.rm=TRUE)

print(uncal_mean)

print(cal_mean)

