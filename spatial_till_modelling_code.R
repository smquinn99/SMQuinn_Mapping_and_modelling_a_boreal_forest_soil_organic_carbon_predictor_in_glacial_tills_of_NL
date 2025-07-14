###this is the code to generate the random forest model for spatial Al availability predictions
#this code must be run before running the spatial calibration code

#set working directory
setwd("D:/GIS_SCOUT/GIS_SCOUT")


#Add in all packages to be used
library(terra)
library(randomForest)
library(MultiscaleDTM)
library(tidyr)
library(dplyr)

#load rasters
unit_size<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/resample_attempts/unit_size_nn.tif")
CanDEMelev_RS<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/RS_20/CanDEMelev_20.tif")
climate_zones_RS<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/REVISED_CLIMATE_zones.TIF")
distOcean_RS<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/RS_20/NEWdistOcean_20.tif")
NLDEM<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/RS_20/NLDEM_20.tif")
surficial_regional_RS<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/resample_attempts/new_surficialgeo_20.tif")
ruggedness<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/RS_20/ruggedness_20.tif")
slope<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/RS_20/slope_20.tif")


###load bedrock rasters
u1<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist1.tif")
u1<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist1.tif")
u2<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist2.tif")
u3<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist3.tif")
u4<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist4.tif")
u8<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist8.tif")
u9<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist9.tif")
u10<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist10.tif")
u12<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist12.tif")
u13<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist13.tif")
u14<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist14.tif")
u15<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist15.tif")
u16<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist16.tif")
u17<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist17.tif")
u18<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist18.tif")
u19<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist19.tif")
u20<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist20.tif")
u21<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist21.tif")
u23<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist23.tif")
u24<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist24.tif")
u25<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist25.tif")
u26<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist26.tif")
u27<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist27.tif")
u28<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist28.tif")
u29<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist29.tif")
u30<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist30.tif")
u31<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist31.tif")
u32<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist32.tif")
u33<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist33.tif")
u34<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist34.tif")
u35<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist35.tif")
u36<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist36.tif")
u37<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist37.tif")
u39<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist39.tif")
u40<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist40.tif")
u41<- rast("D:/GIS_SCOUT/GIS_SCOUT/Clipped_layers_for_model/batch_dist/dist41.tif")


#tell r that factor data are factors
climate_df<- data.frame(id= 1:7, climate_zones_RS= as.character(1:7))
surficial_df<- data.frame(id= c(1, 2, 3, 4, 5, 6, 7, 8, 9), surficial_regional_RS= as.character(c("colluvium", "rogen moraine", "ablation drift", "till blanket", "glaciofluvial", "drift poor", "alluvium", "till, undifferentiated", "glaciomarine and marine")))

#now assign the levels as defined in the data frame
levels(climate_zones_RS)<- climate_df
levels(surficial_regional_RS)<- surficial_df

#is.factor(bedrockgeo_RS)
is.factor(climate_zones_RS)
is.factor(surficial_regional_RS)

#stack rasters
rasterstack<- c(ruggedness, unit_size, NLDEM,  climate_zones_RS, distOcean_RS, 
                slope, surficial_regional_RS, u1,  u2,  u3,  u4,  u8,  u9,  u10, u12, u13, u14, u15,
                u16, u17, u18, u19, u20, u21, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35,
                u36, u37, u39, u40, u41)

#Bring in till geochemistry points
till<- vect("Till Info/till_with_climate.shp")
till_df<- as.data.frame(till)

#extract till point raster data
allrastersTill<- terra::extract(rasterstack, till)

#add all of the extracted data to the data frame
till_df$ruggedness<- allrastersTill$adjSD
till_df$NLDEM<- allrastersTill$NLDEM_pcrm
till_df$climate_zones_RS<-allrastersTill$climate_zones_RS
till_df$distOcean_RS<- allrastersTill$NEWdistOcean_20
till_df$surficial_regional_RS<- allrastersTill$surficial_regional_RS
till_df$unit_size<- allrastersTill$unit_size_nn
till_df$slope<- allrastersTill$slope
till_df$u1<- allrastersTill$distance1
till_df$u2<- allrastersTill$dist2  
till_df$u3<- allrastersTill$dist3  
till_df$u4<- allrastersTill$dist4  
till_df$u8<- allrastersTill$dist8  
till_df$u9<- allrastersTill$dist9 
till_df$u10<- allrastersTill$dist10 
till_df$u12<- allrastersTill$dist12
till_df$u13<- allrastersTill$dist13 
till_df$u14<- allrastersTill$dist14 
till_df$u15<- allrastersTill$dist15
till_df$u16<- allrastersTill$dist16 
till_df$u17<- allrastersTill$dist17 
till_df$u18<- allrastersTill$dist18 
till_df$u19<- allrastersTill$dist19 
till_df$u20<- allrastersTill$dist20 
till_df$u21<- allrastersTill$dist21
till_df$u23<- allrastersTill$dist23 
till_df$u24<- allrastersTill$dist24 
till_df$u25<- allrastersTill$dist25 
till_df$u26<- allrastersTill$dist26 
till_df$u27<- allrastersTill$dist27 
till_df$u28<- allrastersTill$dist28 
till_df$u29<- allrastersTill$dist29 
till_df$u30<- allrastersTill$dist30 
till_df$u31<- allrastersTill$dist31 
till_df$u32<- allrastersTill$dist32 
till_df$u33<- allrastersTill$dist33  
till_df$u34<- allrastersTill$dist34
till_df$u35<- allrastersTill$dist35 
till_df$u36<- allrastersTill$dist36
till_df$u37<- allrastersTill$dist37
till_df$u39<- allrastersTill$dist39
till_df$u40<- allrastersTill$dist40
till_df$u41<- allrastersTill$dist41

#we extrapolated some climate values to points in ArcGIS, so must make sure they are also factors.
till_df$climate_zo<- as.factor(till_df$climate_zo)
is.factor(till_df$climate_zo)

#Clean up NA values, replace nas with a value
till_df$surficial_regional_RS[is.na(till_df$surficial_regional_RS)] = "drift poor"
till_df$distOcean_RS[is.na(till_df$distOcean_RS)]<- mean(till_df$distOcean_RS)
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
till_df<- till_df[ !is.na(till_df$u1), ]
till_df<- till_df[ !is.na(till_df$u2), ]
till_df<- till_df[ !is.na(till_df$u3), ]
till_df<- till_df[ !is.na(till_df$u4), ]
till_df<- till_df[ !is.na(till_df$u8), ]
till_df<- till_df[ !is.na(till_df$u9), ]
till_df<- till_df[ !is.na(till_df$u10), ]
till_df<- till_df[ !is.na(till_df$u12), ]
till_df<- till_df[ !is.na(till_df$u13), ]
till_df<- till_df[ !is.na(till_df$u14), ]
till_df<- till_df[ !is.na(till_df$u15), ]
till_df<- till_df[ !is.na(till_df$u16), ]
till_df<- till_df[ !is.na(till_df$u17), ]
till_df<- till_df[ !is.na(till_df$u18), ]
till_df<- till_df[ !is.na(till_df$u19), ]
till_df<- till_df[ !is.na(till_df$u20), ]
till_df<- till_df[ !is.na(till_df$u21), ]
till_df<- till_df[ !is.na(till_df$u23), ]
till_df<- till_df[ !is.na(till_df$u24), ]
till_df<- till_df[ !is.na(till_df$u25), ]
till_df<- till_df[ !is.na(till_df$u26), ]
till_df<- till_df[ !is.na(till_df$u27), ]
till_df<- till_df[ !is.na(till_df$u28), ]
till_df<- till_df[ !is.na(till_df$u29), ]
till_df<- till_df[ !is.na(till_df$u30), ]
till_df<- till_df[ !is.na(till_df$u31), ]
till_df<- till_df[ !is.na(till_df$u32), ]
till_df<- till_df[ !is.na(till_df$u33), ]
till_df<- till_df[ !is.na(till_df$u34), ]
till_df<- till_df[ !is.na(till_df$u35), ]
till_df<- till_df[ !is.na(till_df$u36), ]
till_df<- till_df[ !is.na(till_df$u37), ]
till_df<- till_df[ !is.na(till_df$u39), ]
till_df<- till_df[ !is.na(till_df$u40), ]
till_df<- till_df[ !is.na(till_df$u41), ]


#renaming the rasters in rasterstack to match the columns of till_df
names(rasterstack)<- c("ruggedness", "unit_size", "NLDEM", "climate_zones_RS", "distOcean_RS", 
                       "slope", "surficial_regional_RS", "u1",  "u2", "u3", "u4", "u8", "u9", "u10", "u12", "u13", "u14", "u15",
                       "u16", "u17", "u18", "u19", "u20", "u21", "u23", "u24", "u25", "u26", "u27", "u28", "u29", "u30", "u31", "u32", "u33", "u34", "u35",
                       "u36", "u37", "u39", "u40", "u41")

#train model
rfspatial<- randomForest(Al_avail~ ruggedness + unit_size + NLDEM + climate_zones_RS + slope + distOcean_RS
                           + surficial_regional_RS + u1 + u2 + u3 + u4 + u8 + u9 + u10 + u12 + u13 + u14 
                           + u15 + u16 + u17 + u18 + u19 + u20 + u21 + u23 + u24 + u25 + u26 + u27 +
                             u28 + u29 + u30 + u31 + u32 + u33 + u34 + u35 + u36 + u37 + u39 + u40 + u40 , till_df, nodesize = 4, ntree=2000, mtry=13, progress = TRUE)
#Visualize the importance
spatial_importance<-importance(rfspatial)
spatial_importance_table<-as.data.frame(spatial_importance)
write.csv(spatial_importance_table, file="D:/GIS_SCOUT/GIS_SCOUT/spatial_importance_table2.csv")

#Create an RMSE function
RMSE = function(observed_data, predicted_data){
  sqrt(mean((observed_data - predicted_data)^2))
}

#Check the variable importance
varImpPlot(rfspatial, main= "Model Two: Predicted vs Observed Till Al Availability")
oobRMSEspatial<- RMSE(rfspatial$y, rfspatial$predicted)


#time to do some cross validation#
k<-10                                      
fold<-rep(1:k, length.out= nrow(till_df)) 
fold<-sample(fold)                        
till_df$fold<-fold                        
#make a list
l<-list()

#loop
for (i in 1:k) {      
  print(i)  
  test<- till_df[till_df$fold==i, ]  
  train<- till_df[till_df$fold!=i, ]  
  rf_spatial<-randomForest(Al_avail~ ruggedness + unit_size + NLDEM + climate_zones_RS + distOcean_RS + slope
                           + surficial_regional_RS + u1 + u2 + u3 + u4 + u8 + u9 + u10 + u12 + u13 + u14 
                           + u15 + u16 + u17 + u18 + u19 + u20 + u21 + u23 + u24 + u25 + u26 + u27 +
                             u28 + u29 + u30 + u31 + u32 + u33 + u34 + u35 + u36 + u37 + u39 + u40 + u40, train, ntree=2000) 
  pp<-predict(rfspatial, test) 
  
  mvalidation<-data.frame(observed=test$Al_avail, predicted=pp, fold=i) 
  
  l[[i]]<-mvalidation 
}

#check rmse of the model
mm<- do.call("rbind", l) 
model_two_RMSE<-RMSE(mm$observed, mm$predicted)

#Predict values from model
spatial_predict<-predict( rasterstack, rfspatial,filename = "spatial_prediction.tif", overwrite=TRUE)

####Now map uncertainty. This is very computationally heavy and may need to be split up
loop_fun<- function(rfspatial, rasterstack){
  library(terra)
  library(randomForest)
  v <- terra::predict( rfspatial,  rasterstack, predict.all=TRUE)
  v <- v$individual
  apply(v, 1, sd)
}
##predict all at once
pred_fun<- terra::predict( rfspatial, rasterstack, fun=loop_fun)

# OR If computing power is limited, slice the raster into pieces and compute one at a time
window(rasterstack)
rast_ext<-ext(rasterstack)
window(rasterstack)<- NULL
value1<- (ext(rasterstack)[4]- ext(rasterstack)[3])/500
for(i in seq(ext(rasterstack)[3], ext(rasterstack)[4], value1)){
  print(i)
  rast_ext<-ext(rasterstack)
  rast_ext[3]<-i
  rast_ext[4]<-i+value1
  window(rasterstack)<-rast_ext
  terra::predict(rasterstack, rfspatial, fun=loop_fun, filename= paste0("D:/GIS_SCOUT/GIS_SCOUT/temp_rasters/ssd_test2", i, ".tif"), overwrite= TRUE)
  window(rasterstack)<-NULL
  gc()
}
value1


