

#make predictions over the whole dataframe
p4<-predict(rfspatial, till_df, predict.all=TRUE) 

p_oobs4 <- p4$individual


#calculate the mean and sd using only the oobs.
p4mean<- apply(p_oobs4, MARGIN=1, mean, na.rm= TRUE)
p4sd<- apply(p_oobs4, MARGIN=1, sd, na.rm= TRUE)

#Then calculate the error
ind4<-data.frame(observed=till_df$Al_avail, predicted=p4mean, sd=p4sd) 
ind4$error<- (ind4$observed- ind4$predicted)

#add my calibrated values
ind4<- ind4 %>% mutate(cal = (sd*a+b))

#r value plot 
#first make the r statistic (r = residual/sd) 
ind4<- ind4 %>% mutate(rC = error/cal)
rC<- ind4$rC
ind4<- ind4 %>% mutate(rUC = error/sd)
rUC<-ind4$rUC

#calculate the mean and standard deviations
mean_Rstat_C<- mean(ind4$rC)
sd_Rstat_C<-sd(ind4$rC)

mean_Rstat_UC<- mean(ind4$rUC)
sd_Rstat_UC<-sd(ind4$rUC)

uncal_RMSE<- RMSE(ind4$error, ind4$sd)
Cal_RMSE<-RMSE(ind4$error, ind4$cal)

#Now plot

png( filename = "cal_sp_hist.png", width = 3.25, height = 3, units = "in", res = 200)
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

#binning the uncalibrated values
ind4<- ind4 %>% 
  ungroup() %>%
  mutate(sd_bin = cut_number(ind4$sd, n = 15))
#Obtain RMSE of the bin for uncalibrated values
Stats6 <- ind4 %>% group_by(sd_bin) %>% 
  summarize(mean_sd_group = mean(sd), RMS_group_UC = RMSE(observed, predicted), NumFramesUC = n())

# do the same thing for the calibrated values
ind4<- ind4 %>% 
  ungroup() %>%
  mutate(sdcal_bin = cut_number(ind4$cal, n = 15))
#now obtain the RMSE of calibrated bins
Stats7 <- ind4 %>% group_by(sdcal_bin) %>% 
  summarize(mean_sd_group = mean(sd), mean_sdcal_group = mean(cal), RMS_group = RMSE(observed, predicted), NumFramesC = n())


#Identify bins with few values in them
lowcountsC<- subset(Stats7, NumFramesC<30)
adequate_countsC<- subset(Stats7, NumFramesC>30)

lowcountsUC<- subset(Stats6, NumFramesUC<30)
adequate_countsUC<- subset(Stats6, NumFramesUC>30)


#Visualize large cloud of datapoints
ind4<- ind4 %>% 
  ungroup() %>%
  mutate(sd_bin = cut_number(sd, 4500))
#Obtain RMSE of the bin for uncalibrated values
stats8 <- ind4 %>% group_by(sd_bin) %>% 
  summarize(mean_sd_group = mean(sd), RMS_group_UC = RMSE(observed, predicted), NumFramesUC = n())

# do the same thing for the calibrated values
ind4<- ind4 %>% 
  ungroup() %>%
  mutate(sdcal_bin = cut_number(cal, 4500))
#now obtain the RMSE of calibrated bins
stats9 <- ind4 %>% group_by(sdcal_bin) %>% 
  summarize(mean_sd_group = mean(sd), mean_sdcal_group = mean(cal), RMS_group = RMSE(observed, predicted), NumFramesC = n())



#now make a plot (RMSE residuals vs standard deviation of the bootstrapped estimates)
png( filename = "cal_sp_scattercode.png", width = 3.25, height = 2.5, units = "in", res = 200)

par(mar= c(3, 3, 0, 1))
plot(stats9$mean_sdcal_group, stats9$RMS_group, 
     cex.lab=0.7, cex.axis=0.6, cex.main= 0.2,
     xlab = "", 
     ylab = "",
     abline(a=0, b=1, col= "red"),
     pch = 16, col =rgb(0.6, 0.6, 1, 0.3), cex = 0.5,
     xlim=c(0, 0.6),
     ylim=c(0, .6))
points(stats8$mean_sd_group, stats8$RMS_group_UC,pch = 19, bg = rgb(0.8, 0.8, 0.8), col = rgb(0.8, 0.8, 0.8, 0.3), cex = 0.5)

#add all points to the scatterplot
abline(a=0, b=1, col= "red")
points(adequate_countsC$mean_sdcal_group, adequate_countsC$RMS_group,pch = 21, bg = "blue", col = "blue", cex = 0.5)
points( adequate_countsUC$mean_sd_group, adequate_countsUC$RMS_group_UC,pch = 21, bg = "grey35", col = "grey35" , cex = 0.5)
points( lowcountsC$mean_sdcal_group, lowcountsC$RMS_group, col="blue", cex = 0.5)
points( lowcountsUC$mean_sd_group, lowcountsUC$RMS_group_UC, col="grey35", cex = 0.5)


title(xlab= "Mean Binned SD", ylab = "RMS Residuals", line = 2, cex.lab = "0.7")
legend("bottomright", c("Uncalibrated Bins", "Calibrated Bins", "Raw Data", "Identity Function"), cex = 0.7, 
       pch = c(16, 16, 1, NA), lty = c(NA, NA, NA, 1), col =c("grey35", "blue", "black", "red"))

dev.off()

#plot histogram of bins
png( filename = "cal_sp_bins.png", width = 3.25, height = 1.25, units = "in", res = 200)
par(mar= c(0, 3, 1, 1))
hist(ind4$sd, breaks=55,  cex.lab=0.7, cex.axis=0.7, cex.main= 0.7, xaxt= "n",
     xlab=" ", 
     main="", col="grey" ,
     ylab= "",
     ylim=c(0, 6000),
     xlim=c(0, 0.6))
hist(ind4$cal, breaks=35,  add=TRUE, col=rgb(0,0,1,0.4),,
     ylim=c(0, 6000),
     xlim=c(0, 0.6))
legend("topright", c("Uncalibrated", "Calibrated"), cex = 0.8, fill =c("grey", rgb(0,0,1,0.4)))
title( ylab = "Bin Counts", line = 2, cex.lab = "0.7")
dev.off()
