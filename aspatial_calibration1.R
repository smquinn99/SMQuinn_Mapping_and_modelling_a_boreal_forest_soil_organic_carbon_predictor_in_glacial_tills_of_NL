#this takes you from the model all the way to getting the calibration values
#along the way we are going to check our training data across several different graphs to determine whether or not we actually need to 
#calibrate our data, or if it's okay on it's own. In our case, the SD could benefit from some calibration, although it's not far off

library(dplyr)
library(tidyverse)
library(ggplot2)

#Split the data into test and train sets
k<-10                                    
fold<-rep(1:k, length.out= nrow(till_df))  
fold<-sample(fold)                         
till_df$fold<-fold
for (i in 1:k) {    
  print(i)  
  test<- till_df[till_df$fold==i, ] 
  train<- till_df[till_df$fold!=i, ]}

#predict uncertainty for the dataset
p<-predict(rf_nonspatial_alldata, train, predict.all=TRUE) 
#p<-predict(rf_nonspatial_alldata, till_df, predict.all=TRUE) 
#Now we want to only use the oob (out of bag) predictions
inbagmatrix<- rf_nonspatial_alldata$inbag

#make a logical matrix where it's TRUE when the sample is OOB
oobs_logical<- (inbagmatrix == 0)

#extract only the oob predictions
p_oobs <- p$individual

#but this isn't the same size as our other matrix! So we can't do the stuff we want. 
#Maybe can make a new matrix which is the same size as the other matrix, 
#and add the values from this one in and leave the others as NAS?

#so first make a blank matrix that's the same exact dimensions as the desired matrix
blank<- inbagmatrix
#overwrite with NA
blank[]<- NA

#easier to work with as a dataframe
blank<-as.data.frame(blank)
p_oobs<- as.data.frame(p_oobs)

#make a common name using the rowname which considers the missing test data
p_oobs$sno<- rownames(p_oobs)
blank$sno<-rownames(blank)

#populate the dataframe with desired data
blank<- full_join(select(blank, "sno"), p_oobs)

#get rid of the common name column
p_oobs$sno<- NULL
blank$sno<-NULL

#Change both back into matrices
p_oobs<- as.matrix(p_oobs)
blank<- as.matrix(blank)

# Set in-bag predictions to NA
blank[oobs_logical]<- NA

#calculate the mean and sd using only the oobs.
pmean<- apply(blank, MARGIN=1, mean, na.rm= TRUE)
pmean<- na.omit(pmean)
psd<- apply(blank, MARGIN=1, sd, na.rm= TRUE)
psd<- na.omit(psd)

#create a dataframe
ind<-data.frame(observed=train$Al_avail, 
                predicted=pmean,
                sd=psd) 

ind$error<- (ind$observed- ind$predicted)

#Plot the r value, that is check the distribution of the error/predicted sd
ind<- ind %>% mutate(r = error/sd)
r<- ind$r
mean_Rstat<- mean(r)
ssd_Rstat<-sd(r)

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
  summarize(mean_sd_group = mean(sd), RMS_group = RMSE(observed, predicted), NumFrames = n())

# Plot RMSE residuals vs standard deviation of the bootstrapped estimates
model<- lm(Stats$mean_sd_group~Stats$RMS_group)
summary(model)
plot( Stats$mean_sd_group, Stats$RMS_group, 
     main= " RMS Residuals vs Mean Binned SD (Aspatial)",
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
  (sum(log(2*pi) + log((x1*sd+x2)^2) + ((r^2)/((x1*sd+x2)^2)))) 
}

##Next, optimize NLL
optimization<-optim(c(1.5, -0.02111), lfun,  sd=ind$sd, r=ind$error, method = "Nelder-Mead")
optimization
oparams<- optimization$par
a<-oparams[1]
b<-oparams[2]