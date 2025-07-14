
#predict test data
p2<-predict(rf_nonspatial_alldata, test, predict.all=TRUE)
p2mean<- apply(p2$individual, MARGIN=1, mean, na.rm= TRUE)
p2sd<- apply(p2$individual, MARGIN=1, sd, na.rm= TRUE)

ind2<-data.frame(observed=test$Al_avail, predicted=p2, mean=p2mean, sd=p2sd) 
ind2$error<- (ind2$observed- ind2$predicted.aggregate)

#add in calibrated sd values
ind2<- ind2 %>% mutate(cal = (sd*a + b))

# r value
ind2<- ind2 %>% mutate(rC = error/cal)
rC<- ind2$rC
ind2<- ind2 %>% mutate(rUC = error/sd)
rUC<-ind2$rUC
hist(rC)

#calculate the mean and standard deviations
mean_Rstat_C<- mean(ind2$rC)
sd_Rstat_C<-sd(ind2$rC)

mean_Rstat_UC<- mean(ind2$rUC)
sd_Rstat_UC<-sd(ind2$rUC)

#Now plot histogram of r statistic
hist(rUC, breaks=25, freq=FALSE, cex.main = 1,
     xlab="residuals/SD", cex.lab=0.8, cex.axis=0.8,
     main="C vs UC R Statistic (Aspatial)",  col="grey",
     ylim=c(0, 1.5),
     xlim=c(-4, 4))
hist(rC, breaks=50, freq=FALSE,
     xlab="residuals/SD", cex = 0.8,
     main="C vs UC R Statistic (Aspatial)", add=TRUE, col=rgb(0,0,1,0.4),
     ylim=c(0, 1),
     xlim=c(-4, 4))
curve(dnorm(x, mean=0, sd=1), 
      col="red", lwd=2, add=TRUE, yaxt="n")

legend("topright", c("Uncalibrated", "Calibrated", "Normal Distribution"), cex = 0.8,
       pch = c(15, 15, NA), lty = c(NA, NA, 1), col =c("grey", rgb(0, 0, 1, 0.4), "red"))

#binning
#first group by the uncalibrated values
ind2<- ind2 %>% 
  ungroup() %>%
  mutate(sd_bin = cut(sd, 15))
#now get the RMSE of the bin for uncalibrated values
Stats2 <- ind2 %>% group_by(sd_bin) %>% 
  summarize(mean_sdcal_group = mean(cal), mean_sd_group = mean(sd), RMS_group_UC = RMSE(observed, predicted.aggregate), NumFramesUC = n())

#calibrated values
ind2<- ind2 %>% 
  ungroup() %>%
  mutate(sdcal_bin = cut(cal, 15))
#rmse of calibrated bins
Stats3 <- ind2 %>% group_by(sdcal_bin) %>% 
  summarize(mean_sdcal_group = mean(cal), RMS_group = RMSE(observed, predicted.aggregate), NumFramesC = n())


#Identify bins with few values in them
lowcountsC<- subset(Stats3, NumFramesC<30)
adequate_countsC<- subset(Stats3, NumFramesC>30)

lowcountsUC<- subset(Stats2, NumFramesUC<30)
adequate_countsUC<- subset(Stats2, NumFramesUC>30)

#plot (RMSE residuals vs standard deviation of the bootstrapped estimates)
plot(adequate_countsC$mean_sdcal_group, adequate_countsC$RMS_group, 
     main= " RMS Residuals vs Mean Binned SD (Aspatial)", cex.lab=0.8, cex.axis=0.8, cex.main= 0.9,
     xlab = "Mean Binned SD", 
     ylab = "RMS Residuals",
     abline(a=0, b=1, col= "red"),
     pch = 21, bg = "blue", col = "blue",
     xlim=c(0, 1.5),
     ylim=c(0, 1.5))

points( adequate_countsUC$mean_sd_group, adequate_countsUC$RMS_group_UC,pch = 21, bg = "grey", col = "grey" )
points( lowcountsC$mean_sdcal_group, lowcountsC$RMS_group, col="blue")
points( lowcountsUC$mean_sd_group, lowcountsUC$RMS_group_UC, col="grey")
legend("bottomright", c("Uncalibrated", "Calibrated", "Low Counts", "Identity Function"), cex = 0.8, 
       pch = c(16, 16, 1, NA), lty = c(NA, NA, NA, 1), col =c("grey", "blue", "black", "red"))

#check the overall distribution of points
hist(ind2$sd, breaks=15,
     xlab=" ", 
     main="Bin Counts", col="grey" ,
     ylim=c(0, 1000),
     xlim=c(0, 1.5))
hist(ind2$cal, breaks=15,  add=TRUE, col=rgb(0,0,1,0.4),
     ylim=c(0, 1000),
     xlim=c(0, 1.5))
legend("topright", c("Uncalibrated", "Calibrated"), cex = 01, fill=c("grey", rgb(0,0,1,0.4)))

uncal_test_RMSE<- RMSE(ind2$error, ind2$sd)
cal_test_RMSE<-RMSE(ind2$error, ind2$cal)
