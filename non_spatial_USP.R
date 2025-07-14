
setwd("D:/GIS_SCOUT/GIS_SCOUT")

#then, add in packaged I want to use
library(terra)
library(randomForest)
#install.packages("MultiscaleDTM")
library(MultiscaleDTM)

#load rasters
bedrockgeo<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/resample_attempts/bedrockgeo5_20.tif")
unit_size<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/resample_attempts/unit_size_nn.tif")
CanDEMelev_RS<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/RS_20/CanDEMelev_20.tif")
climate_zones_RS<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/REVISED_CLIMATE_zones.TIF")
distOcean_RS<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/RS_20/NEWdistOcean_20.tif")
NLDEM<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/RS_20/NLDEM_20.tif")
surficial_regional_RS<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/resample_attempts/new_surficialgeo_20.tif")
ruggedness<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/RS_20/ruggedness_20.tif")
slope<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/RS_20/slope_20.tif")

#make factor data into factors
BG_df<- data.frame(id= 1:46,bedrockgeo= as.character(1:46))
climate_df<- data.frame(id= 1:7, climate_zones_RS= as.character(1:7))
surficial_df<- data.frame(id= c(1, 2, 3, 4, 5, 6, 7, 8, 9), surficial_regional_RS= as.character(c("colluvium", "rogen moraine", "ablation drift", "till blanket", "glaciofluvial", "drift poor", "alluvium", "till, undifferentiated", "glaciomarine and marine")))


#now assign the levels as defined in the data frame
levels(bedrockgeo)<- BG_df
levels(climate_zones_RS)<- climate_df
levels(surficial_regional_RS)<- surficial_df

#Make them factors
is.factor(bedrockgeo)
is.factor(climate_zones_RS)
is.factor(surficial_regional_RS)

#stack rasters
rasterstack_nonspatial<- c(ruggedness, bedrockgeo, unit_size, NLDEM, climate_zones_RS, distOcean_RS, 
                slope, surficial_regional_RS)

#Bring in till geochemistry points
till<- vect("D:/GIS_SCOUT/GIS_SCOUT/Till Info/till_with_climate.shp")
till_df<- as.data.frame(till)

#extract till point raster data
allrastersTill<- terra::extract(rasterstack_nonspatial, till)

#add all of the extracted data to the data frame
till_df$bedrockgeo<-allrastersTill$bedrockgeo
till_df$ruggedness<- allrastersTill$adjSD
till_df$NLDEM<- allrastersTill$NLDEM_pcrm
till_df$climate_zones_RS<-till_df$climate_zo
till_df$distOcean_RS<- allrastersTill$NEWdistOcean_20
till_df$surficial_regional_RS<- allrastersTill$surficial_regional_RS
till_df$unit_size<- allrastersTill$unit_size_nn
till_df$slope<- allrastersTill$slope


#Clean up NA values, replace nas with a value
till_df$surficial_regional_RS[is.na(till_df$surficial_regional_RS)] = "drift poor"
till_df$distOcean_RS[is.na(till_df$distOcean)]<- mean(till_df$distOcean)
till_df$NLDEM[is.na(till_df$NLDEM)]<- 0
till_df$ruggedness[is.na(till_df$ruggedness)]<- 0
till_df$slope[is.na(till_df$slope)]<- 0


#remove Nas which were not converted to other values
till_df<- till_df[ !is.na(till_df$climate_zones_RS), ]
till_df<- till_df[ !is.na(till_df$distOcean_RS), ]
till_df<- till_df[ !is.na(till_df$NLDEM), ]
till_df<- till_df[ !is.na(till_df$ruggedness), ]
till_df<- till_df[ !is.na(till_df$surficial_regional_RS), ]
till_df<- till_df[ !is.na(till_df$unit_size), ]
till_df<- till_df[ !is.na(till_df$bedrockgeo), ]


#renaming the rasters in rasterstack to match the columns of till_df
names(rasterstack_nonspatial)<- c( "ruggedness", "bedrockgeo", "unit_size", "NLDEM", "climate_zones_RS", "distOcean_RS", 
                       "slope", "surficial_regional_RS")

rf_nonspatial_alldata<- randomForest(Al_avail~ ruggedness + bedrockgeo + unit_size + NLDEM + climate_zones_RS + distOcean_RS + slope+ surficial_regional_RS, till_df, ntree=1000, mtry=4)

#cross validation

k<-10                                       #make an object which is equal to the amount of times I want to run my validation
fold<-rep(1:k, length.out= nrow(till_df))  # make an object "fold" which repeating the sequence of "k" over and over again the same amount of times as rows in my dataframe
fold<-sample(fold)                         #scramble these using the sample function
till_df$fold<-fold                         #Make a new column in my dataframe which is filled by "fold"
#make a list
l<-list()

#loop
for (i in 1:k) {      #set up my loop which is to say make 1 every one of 1-k in sequence
  print(i)   #just for fun
  test<- till_df[till_df$fold==i, ]   #partition off some test data (ie, take out all entries which are within this fold)
  train<- till_df[till_df$fold!=i, ]  #partition off all values not in this fold to use for building the df
  rf_nonspatial<- randomForest(Al_avail~ ruggedness + bedrockgeo + unit_size + NLDEM + climate_zones_RS + distOcean_RS + slope
                               +                   + surficial_regional_RS, train, ntree=1000, mtry=4) #pop in the model and use TRAIN as the data source
  p<-predict(rf_nonspatial, test) #predict the test data according to the training data
  validation<-data.frame(observed=test$Al_avail, predicted=p, fold=i) #make a new datafrom which is going to house my observations, predictions and fold number
  l[[i]]<-validation #add this thing to a list but it's actually using double brackets to say put this in a certain position (equal to i in sequence) on the list
}



m<- do.call("rbind", l) #this binds the predictions and observations into a data frame

RMSE = function(observed_data, predicted_data){
  sqrt(mean((observed_data - predicted_data)^2))
}
model_one_RMSE<-RMSE(m$observed, m$predicted)
plot(m$observed, m$predicted, 
     main= " Model One: Predicted vs Observed Till Al Availability",
     xlab = "Observed Al Availability", 
     ylab = "Predicted Al Availability",
     abline(a=0, b=1, col= "RED"),
     xlim=c(0, 5),
     ylim=c(0, 5))


#####adding a new column in my dataframe for my residuals
m$res<- abs(m$observed-m$predicted)
till_df$res<- m$res

#############################NOW WE CHECK THE VARIANCE VS THE RESIDUALS######################################
Asd<-rast("D:/GIS_SCOUT/GIS_SCOUT/Results/sd_maps/non_spatial_sd_new.tif")
Ssd<-rast("D:/GIS_SCOUT/GIS_SCOUT/Results/sd_maps/spatial_ox_sd.tif")

write.csv(m, "residuals.csv")

RMSE(rf_nonspatial$y, rf_nonspatial$predicted)
original_data<- validation$observed
predicted_data<- validation$predicted


datalist<- list()
for (i in 1:k) {      #set up my loop which is to say make 1 every one of 1-k in sequence
  print(i)   #just for fun
  test<- till_df[till_df$fold==i, ]   #partition off some test data (ie, take out all entries which are within this fold)
  train<- till_df[till_df$fold!=i, ]  #partition off all values not in this fold to use for building the df
  rf_nonspatial<- randomForest(Al_avail~ ruggedness + bedrockgeo + unit_size + NLDEM + climate_zones_RS + distOcean_RS + slope
                               +                   + surficial_regional_RS, train, ntree=1000, mtry=4) #pop in the model and use TRAIN as the data source
  p<-predict(rf_nonspatial, test, predict.all=TRUE) #predict the test data according to the training data
  validation<-data.frame(observed=test$Al_avail, predicted=p, fold=i) #make a new datafrom which is going to house my observations, predictions and fold number
  l[[i]]<-validation #add this thing to a list but it's actually using double brackets to say put this in a certain position (equal to i in sequence) on the list
}

ns_m<- do.call("rbind", l) #this binds the predictions and observations into a data frame

save.image("D:/GIS_SCOUT/GIS_SCOUT/nsbackup_13th.r")

##exploration
rf_trees<- as.data.frame(rf$mse)
rf_trees$rsq<- rf$rsq
till_df$oob.times<- rf$oob.times

#write csvs
write.csv(rf_trees, "D:/GIS_SCOUT/GIS_SCOUT/results/nonspatial_rf_trees.csv")
write.csv(till_df, "D:/GIS_SCOUT/GIS_SCOUT/results/nonspatial_till_geochem_and_predictions.csv")

plot(original_data, predicted_data, 
     main= " Model One: Predicted vs Observed Till Al Availability",
     xlab = "Observed Al Availability", 
     ylab = "Predicted Al Availability",
     abline(a=0, b=1, col= "RED"),
     xlim=c(0, 5),
     ylim=c(1, 4))

predict(rasterstack, rf, )

#now I want to predict my SD
  really_good_pfun <- function(rf, rasterstack){
  library(terra)
  library(randomForest)
  v <- terra::predict(rf, rasterstack, predict.all=TRUE)
  v <- v$individual
  apply(v, 1, sd)
  }

even_better_fun <- terra::predict(rasterstack, rf, fun=really_good_pfun)
  #cross fingers...

  # Isolate part of the raster
  #first test below
  window(rasterstack_nonspatial)
  rast_ext<-ext(rasterstack_nonspatial)
  #make a new extent which is essentially just a little strip of the old one
  new_ext<- ext(c(319679.458773474, 829719.458773474, 5167511.83158952, 5190000.83158952))
  #assign the window to be this new little strip
  window(rasterstack_nonspatial)<- new_ext

#predict

#Write to a file

####yeehaw


value1<- (ext(rasterstack_nonspatial)[4]- ext(rasterstack_nonspatial)[3])/500

for(i in seq(ext(rasterstack_nonspatial)[3], ext(rasterstack_nonspatial)[4], value1)){
  print(i)
  rast_ext<-ext(rasterstack_nonspatial)
  rast_ext[3]<-i
  rast_ext[4]<-i+value1
  window(rasterstack_nonspatial)<-rast_ext
  terra::predict(rasterstack_nonspatial, rf_nonspatial_alldata, fun=loop_fun(), filename= paste0("D:/GIS_SCOUT/GIS_SCOUT/ns_sd_newDO/ns_sd_2_", i, ".tif"), overwrite= TRUE)
  window(rasterstack)<-NULL
  gc()
}

##remember, the predict all function returns all of the predictions for all trees!
predict(rasterstack_nonspatial, rf_nonspatial, fun=really_good_pfun2, filename=("D:/GIS_SCOUT/GIS_SCOUT/testsdAS.tif"), overwrite= TRUE)



v<-predict(rasterstack_nonspatial, rf_nonspatial, predict.all=TRUE)








####get the concurrence plot or whatever the heck it's called 
plot(rf)

#looking at the data
abline(linear_model<- lm(original_data~predicted_data), col="purple", asp = 1)



original_data<- rf$y
predicted_data<- rf$predicted
RMSE(predicted_data, original_data)

RMSE = function(observed_data, predicted_data){
   sqrt(mean((observed_data - predicted_data)^2))
}


dpredicted_data<- rf$predicted
doriginal_data<- rf$y

save.image(file= 'non_spatial_USP_rf_workspace.Rdata')
load('non_spatial_rf_workspace.Rdata'
)

#partial plots
partialPlot(x = rf, pred.data = till_df, x.var = bedrockgeo)
partialPlot(x = rf, pred.data = till_df, x.var = distOcean_RS, ylim=c(0,4))
partialPlot(x = rf, pred.data = till_df, x.var = NLDEM, ylim=c(0,4))
partialPlot(x = rf, pred.data = till_df, x.var = ruggedness, ylim=c(0,4))
partialPlot(x = rf, pred.data = till_df, x.var = slope, ylim=c(0,4))
partialPlot(x = rf, pred.data = till_df, x.var = climate_zones_RS, ylim=c(0,4))
partialPlot(x = rf, pred.data = till_df, x.var = unit_size, ylim=c(0,4))
