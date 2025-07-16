#this takes you from the model all the way to getting the calibration values
#along the way we are going to check our training data across several different graphs to determine whether or not we actually need to 
#calibrate our data, or if it's okay on it's own. In our case, the SD could benefit from some calibration, although it's not far off
library(dplyr)
library(randomForest)
library(tidyverse)
library(terra)

#Split the data into test and train sets
k<-10                                      
fold<-rep(1:k, length.out= nrow(till_df))  
fold<-sample(fold)                        
till_df$fold<-fold
  for (i in 1:k) {      
    print(i)   
    test1<- till_df[till_df$fold==i, ]  
    train1<- till_df[till_df$fold!=i, ]}

#predict uncertainty for the dataset
p<-predict(rfspatial, train1, predict.all=TRUE)#predict the test data according to the training data
pmean<- apply(p$individual, MARGIN=1, mean, na.rm= TRUE)
psd<- apply(p$individual, MARGIN=1, sd, na.rm= TRUE)

ind<-data.frame(observed=train1$Al_avail, predicted=p, mean=pmean, sd=psd) 
ind$error<- (ind$observed- ind$predicted.aggregate)

#Plot the r value, that is check the distribution of the error/predicted sd
ind<- ind %>% mutate(r = error/sd)
r<- ind$r
mean_Rstat<- mean(r)
sd_Rstat<-sd(r)

hist(r, breaks=25, freq=FALSE,
     xlab="r statistic", 
     main="",  col="grey",
     ylim=c(0, 1),
     xlim=c(-4, 4))
curve(dnorm(x, mean=0, sd=1), 
      col="red", lwd=2, add=TRUE, yaxt="n")

#Bin the values and check their averaged RMSE against their average SD
ind<- ind %>% 
  ungroup() %>%
  mutate(sd_bin = cut(sd, 15))

Stats <- ind %>% group_by(sd_bin) %>% 
  summarize(mean_sd_group = mean(sd), RMS_group = RMSE(observed, predicted.aggregate), NumFrames = n())

# Plot RMSE residuals vs standard deviation of the bootstrapped estimates
model<- lm(Stats$mean_sd_group~Stats$RMS_group)
summary(model)
plot( Stats$mean_sd_group, Stats$RMS_group, 
      main= " RMS Residuals vs Mean Binned SD (Spatial)",
      xlab = "Mean Binned SD", 
      ylab = "RMS Residuals",
      abline(a=0, b=1, col= "RED"),
      
      xlim=c(0, 1.5),
      ylim=c(0, 1.5)
)


#Calculate calibration factors to scale the data with
#First, make log likelihood function
lfun<- function(x, sd, r) {                   
  x1 <- x[1]                                 
  x2 <- x[2]                                 
  sum(log(2*pi) + log((x1*sd+x2)^2) + ((r^2)/((x1*sd+x2)^2))) 
}

##Next, optimize NLL
optimization<-optim(c(0.80, 0.3895), lfun, sd=ind$sd, r=ind$error,  method = "Nelder-Mead")
optimization
oparams<- optimization$par
a<-oparams[1]
b<-oparams[2]


