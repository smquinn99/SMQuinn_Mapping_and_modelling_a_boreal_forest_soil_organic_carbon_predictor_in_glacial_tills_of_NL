library(dplyr)
#rf_nonspatial_alldata<- randomForest(Al_avail~ ruggedness + bedrockgeo + unit_size + NLDEM + climate_zones_RS + distOcean_RS + slope+ surficial_regional_RS, till_df, ntree=1000, mtry=4)

p3<-predict(rfspatial, till_df, predict.all=TRUE) 
p3mean<- apply(p3$individual, MARGIN=1, mean, na.rm= TRUE)
p3sd<- apply(p3$individual, MARGIN=1, sd, na.rm= TRUE)

ind3<-data.frame(observed=till_df$Al_avail, predicted=p3, mean=p3mean, sd=p3sd)
ind3$error<- (ind3$observed- ind3$predicted.aggregate)

#add my calibrated values
ind3<- ind3 %>% mutate(cal = (sd*a+b))

#r value plot 
#first make the r statistic (r = residual/sd) 
ind3<- ind3 %>% mutate(rC = error/cal)
rC<- ind3$rC
ind3<- ind3 %>% mutate(rUC = error/sd)
rUC<-ind3$rUC

#calculate the mean and standard deviations
mean_Rstat_C<- mean(ind3$rC)
sd_Rstat_C<-sd(ind3$rC)

mean_Rstat_UC<- mean(ind3$rUC)
sd_Rstat_UC<-sd(ind3$rUC)

uncal_RMSE<- RMSE(ind3$error, ind3$sd)
Cal_RMSE<-RMSE(ind3$error, ind3$cal)
#Now plot

png( filename = "D:/GIS_SCOUT/GIS_SCOUT/figures/cal_asp_hist.png", width = 3.25, height = 3, units = "in", res = 200)
par(mar= c(3, 3, 1, 1))
hist(rUC, breaks=25, freq=FALSE, cex.main = 1,
     xlab="", cex.lab=0.8, cex.axis=0.8,
     main="",  col="grey",
     ylim=c(0, 1.5),
     xlim=c(-4, 4))
hist(rC, breaks=50, freq=FALSE,
     xlab="", cex = 0.8,
     main="", add=TRUE, col=rgb(0,0,1,0.4),
     ylim=c(0, 1),
     xlim=c(-4, 4))
curve(dnorm(x, mean=0, sd=1), 
      col="red", lwd=2, add=TRUE, yaxt="n")
title(xlab= "residuals/sd", ylab = "Density", line = 2, cex.lab = "0.8")
legend("topright", c("Uncalibrated", "Calibrated", "Normal Distribution"), cex = 0.8,
       pch = c(15, 15, NA), lty = c(NA, NA, 1), col =c("grey", rgb(0, 0, 1, 0.4), "red"))
dev.off()

#make an RMSE function
RMSE = function(observed_data, predicted_data){
  sqrt(mean((observed_data - predicted_data)^2))
}

#binning

ind3<- ind3 %>% 
  ungroup() %>%
  mutate(sd_bin = cut_number(ind3$sd, n = 30))
#Obtain RMSE of the bin for uncalibrated values
Stats4 <- ind3 %>% group_by(sd_bin) %>% 
  summarize(mean_sd_group = mean(sd), RMS_group_UC = RMSE(observed, predicted.aggregate), NumFramesUC = n())

# do the same thing for the calibrated values
ind3<- ind3 %>% 
  ungroup() %>%
  mutate(sdcal_bin = cut_number(ind3$cal, n = 30))
#now obtain the RMSE of calibrated bins
Stats5 <- ind3 %>% group_by(sdcal_bin) %>% 
  summarize(mean_sd_group = mean(sd), mean_sdcal_group = mean(cal), RMS_group = RMSE(observed, predicted.aggregate), NumFramesC = n())

#Identify bins with few values in them
lowcountsC<- subset(Stats5, NumFramesC<30)
adequate_countsC<- subset(Stats5, NumFramesC>30)

lowcountsUC<- subset(Stats4, NumFramesUC<30)
adequate_countsUC<- subset(Stats4, NumFramesUC>30)

#now make a plot (RMSE residuals vs standard deviation of the bootstrapped estimates)
png( filename = "D:/GIS_SCOUT/GIS_SCOUT/figures/cal_sp_scattercode_equalsize.png", width = 3.25, height = 2.5, units = "in", res = 200)
par(mar= c(3, 3, 0, 1))
plot(adequate_countsC$mean_sdcal_group, adequate_countsC$RMS_group, 
     cex.lab=0.7, cex.axis=0.6, cex.main= 0.2,
     xlab = "", 
     ylab = "",
     abline(a=0, b=1, col= "red"),
     pch = 21, bg = "blue", col = "blue",
     xlim=c(0, 0.6),
     ylim=c(0, .6))
points( adequate_countsUC$mean_sd_group, adequate_countsUC$RMS_group_UC,pch = 21, bg = "grey", col = "grey" )
points( lowcountsC$mean_sdcal_group, lowcountsC$RMS_group, col="blue")
points( lowcountsUC$mean_sd_group, lowcountsUC$RMS_group_UC, col="grey")
title(xlab= "Mean Binned SD", ylab = "RMS Residuals", line = 2, cex.lab = "0.7")
legend("bottomright", c("Uncalibrated", "Calibrated", "Low Counts", "Identity Function"), cex = 0.7, 
       pch = c(16, 16, 1, NA), lty = c(NA, NA, NA, 1), col =c("grey", "blue", "black", "red"))
dev.off()


#plot histogram of bins
png( filename = "D:/GIS_SCOUT/GIS_SCOUT/figures/cal_sp_bins_eqfreq.png", width = 3.25, height = 1.25, units = "in", res = 200)
par(mar= c(3, 3, 1, 1))
hist(ind3$sd, breaks=55,  cex.lab=0.7, cex.axis=0.7, cex.main= 0.7, xaxt= "n",
     xlab=" ", 
     main="", col="grey" ,
     ylab= "",
     ylim=c(0, 10000),
     xlim=c(0, 0.6))
hist(ind3$cal, breaks=35,  add=TRUE, col=rgb(0,0,1,0.4),,
     ylim=c(0, 10000),
     xlim=c(0, 0.6))
legend("topright", c("Uncalibrated", "Calibrated"), cex = 0.8, fill =c("grey", rgb(0,0,1,0.4)))
title( ylab = "Bin Counts", line = 2, cex.lab = "0.7")
dev.off()