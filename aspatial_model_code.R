###this is the code to generate the random forest model for Aspatial Al availability predictions
#this code must be run before running the Aspatial calibration code

#set working directory
setwd("D:/GIS_SCOUT/GIS_SCOUT/smquinn_data")

#Add in all packages to be used
library(terra)
library(randomForest)
library(tidyr)
library(MultiscaleDTM)
library(dplyr)

#load rasters
bedrockgeo<- rast("aspatial_model_layers/bedrockgeo5_20.tif")
unit_size<- rast("aspatial_model_layers/unit_size_nn.tif")
CanDEMelev_RS<- rast("aspatial_model_layers/CanDEMelev_20.tif")
climate_zones_RS<- rast("aspatial_model_layers/REVISED_CLIMATE_zones.TIF")
distOcean_RS<- rast("aspatial_model_layers/NEWdistOcean_20.tif")
NLDEM<- rast("aspatial_model_layers/NLDEM_20.tif")
surficial_regional_RS<- rast("aspatial_model_layers/new_surficialgeo_20.tif")
ruggedness<- rast("aspatial_model_layers/ruggedness_20.tif")
slope<- rast("aspatial_model_layers/slope_20.tif")

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
rasterstack_nonspatial<- c(ruggedness, bedrockgeo, unit_size, NLDEM, climate_zones_RS, distOcean_RS, slope, surficial_regional_RS)

#Bring in till geochemistry points
till<- vect("till_geochemistry/till_with_climate.shp")
till_df<- as.data.frame(till)

#extract till point raster data
allrastersTill<- terra::extract(rasterstack_nonspatial, till)

#add all of the extracted data to the data frame
till_df$bedrockgeo<-allrastersTill$bedrockgeo
till_df$ruggedness<- allrastersTill$adjSD
till_df$NLDEM<- allrastersTill$NLDEM_pcrm
till_df$climate_zones_RS<-allrastersTill$climate_zones_RS
till_df$distOcean_RS<- allrastersTill$NEWdistOcean_20
till_df$surficial_regional_RS<- allrastersTill$surficial_regional_RS
till_df$unit_size<- allrastersTill$unit_size_nn
till_df$slope<- allrastersTill$slope

#we extrapolated some climate values to points in ArcGIS, so must make sure they are also factors.
till_df$climate_zo<- as.factor(till_df$climate_zo)
is.factor(till_df$climate_zo)

#Clean up NA values, replace nas with a value
till_df$surficial_regional_RS[is.na(till_df$surficial_regional_RS)] = "drift poor"
till_df$distOcean_RS[is.na(till_df$distOcean)]<- mean(till_df$distOcean)
till_df$NLDEM[is.na(till_df$NLDEM)]<- 0
till_df$ruggedness[is.na(till_df$ruggedness)]<- 0
till_df$slope[is.na(till_df$slope)]<- 0
till_df$climate_zones_RS[is.na(till_df$climate_zones_RS)]<- till_df$climate_zo[is.na(till_df$climate_zones_RS)]


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

#Train model
rf_nonspatial_alldata<- randomForest(Al_avail~ ruggedness + bedrockgeo + unit_size + NLDEM + climate_zones_RS +
                                       distOcean_RS + slope+ surficial_regional_RS, till_df, ntree=1000, mtry=4, keep.inbag = TRUE)
save.image(file = "aspatial_data.RData")

#Create an RMSE function
RMSE = function(observed_data, predicted_data){
  sqrt(mean((observed_data - predicted_data)^2))
}
#Visualize the importance
aspatial_importance<-importance(rf_nonspatial_alldata)
aspatial_importance_table<-as.data.frame(aspatial_importance)
write.csv(aspatial_importance_table, file="spatial_importance_table.csv")

#Check the variable importance
varImpPlot(rf_nonspatial_alldata, main= "Model One: Predicted vs Observed Till Al Availability")
oobRMSEaspatial<- RMSE(rf_nonspatial_alldata$y, rf_nonspatial_alldata$predicted)



#Cross validation of aspatial model
k<-10                                       
fold<-rep(1:k, length.out= nrow(till_df))  
fold<-sample(fold)                         
till_df$fold<-fold 
l<-list()

#loop
for (i in 1:k) {      
  print(i)  
  test<- till_df[till_df$fold==i, ]  
  train<- till_df[till_df$fold!=i, ] 
  rf_nonspatial<- randomForest(Al_avail~ ruggedness + bedrockgeo + unit_size + NLDEM + climate_zones_RS + distOcean_RS + slope
                               +                   + surficial_regional_RS, train, ntree=1000, mtry=4)
  p<-predict(rf_nonspatial, test)
  validation<-data.frame(observed=test$Al_avail, predicted=p, fold=i) 
  l[[i]]<-validation 
}


#check my RMSE of my model
mm<- do.call("rbind", l)
model_one_RMSE<-RMSE(mm$observed, mm$predicted)

#Make Al availability prediction
aspatial_prediction<-predict( rasterstack_nonspatial, rf_nonspatial_alldata, filename = "results/aspatial_prediction.tif", overwrite=TRUE)

####Now map uncertainty. This is very computationally heavy and may need to be split up
loop_fun<- function(rf_nonspatial_alldata, rasterstack_nonspatial){
  library(randomForest)
  library(terra)
  v <- terra::predict(rf_nonspatial_alldata, rasterstack_nonspatial, predict.all=TRUE)
  v <- v$individual
  apply(v, 1, sd)
}
###predict all at once if you can
#prediction <- terra::predict(rf_nonspatial_alldata, rasterstack_nonspatial, fun=loop_fun, filename= ("asd.tif"), overwrite= TRUE)


# OR If computing power is limited, slice the raster into pieces and compute one at a time
window(rasterstack_nonspatial)
rast_ext<-ext(rasterstack_nonspatial)
window(rasterstack_nonspatial)<- NULL
value1<- (ext(rasterstack_nonspatial)[1]- ext(rasterstack_nonspatial)[2])/500

for(i in seq(ext(rasterstack_nonspatial)[1], ext(rasterstack_nonspatial)[2], value1)){
  print(i)
  rast_ext<-ext(rasterstack_nonspatial)
  rast_ext[1]<-i
  rast_ext[2]<-i+value1
  window(rasterstack_nonspatial)<-rast_ext
  terra::predict(rasterstack_nonspatial, rf_nonspatial_alldata, fun=loop_fun, filename= paste0("strips/asdstrips/redo_asdstrips", i, ".tif"), overwrite= TRUE)
  window(rasterstack_nonspatial)<-NULL
  gc()
}
value1

#here is where you can mosaic the raster strips together
rastlist1 <- list.files(path = "strips/asdstrips", pattern='.tif$', all.files= T, full.names= T)
print(rastlist1)

allrasters1 <- lapply(rastlist1, FUN = rast)

mos<- do.call(mosaic, allrasters1)
test_mos<- writeRaster(mos, filename = "redo_asd_mosaic.tif")
plot(mos)
