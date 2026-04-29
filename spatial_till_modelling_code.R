###this is the code to generate the random forest model for spatial Al availability predictions
#this code must be run before running the spatial calibration code

#set working directory
setwd("D:/GIS_SCOUT/GIS_SCOUT/smquinn_data")

#Add in all packages to be used
library(terra)
library(randomForest)
library(MultiscaleDTM)
library(tidyr)
library(dplyr)

#load rasters
unit_size<- rast("spatial_model_layers/unit_size_nn.tif")
#CanDEMelev_RS<- rast("spatial_model_layers/CanDEMelev_20.tif")
climate_zones_RS<- rast("spatial_model_layers/REVISED_CLIMATE_zones.TIF")
distOcean_RS<- rast("spatial_model_layers/NEWdistOcean_20.tif")
NLDEM<- rast("spatial_model_layers/NLDEM_20.tif")
surficial_regional_RS<- rast("spatial_model_layers/new_surficialgeo_20.tif")
ruggedness<- rast("spatial_model_layers/ruggedness_20.tif")
slope<- rast("spatial_model_layers/slope_20.tif")


###load bedrock rasters
u1<- rast("spatial_model_layers/dist1.tif")
u1<- rast("spatial_model_layers/dist1.tif")
u2<- rast("spatial_model_layers/dist2.tif")
u3<- rast("spatial_model_layers/dist3.tif")
u4<- rast("spatial_model_layers/dist4.tif")
u8<- rast("spatial_model_layers/dist8.tif")
u9<- rast("spatial_model_layers/dist9.tif")
u10<- rast("spatial_model_layers/dist10.tif")
u12<- rast("spatial_model_layers/dist12.tif")
u13<- rast("spatial_model_layers/dist13.tif")
u14<- rast("spatial_model_layers/dist14.tif")
u15<- rast("spatial_model_layers/dist15.tif")
u16<- rast("spatial_model_layers/dist16.tif")
u17<- rast("spatial_model_layers/dist17.tif")
u18<- rast("spatial_model_layers/dist18.tif")
u19<- rast("spatial_model_layers/dist19.tif")
u20<- rast("spatial_model_layers/dist20.tif")
u21<- rast("spatial_model_layers/dist21.tif")
u23<- rast("spatial_model_layers/dist23.tif")
u24<- rast("spatial_model_layers/dist24.tif")
u25<- rast("spatial_model_layers/dist25.tif")
u26<- rast("spatial_model_layers/dist26.tif")
u27<- rast("spatial_model_layers/dist27.tif")
u28<- rast("spatial_model_layers/dist28.tif")
u29<- rast("spatial_model_layers/dist29.tif")
u30<- rast("spatial_model_layers/dist30.tif")
u31<- rast("spatial_model_layers/dist31.tif")
u32<- rast("spatial_model_layers/dist32.tif")
u33<- rast("spatial_model_layers/dist33.tif")
u34<- rast("spatial_model_layers/dist34.tif")
u35<- rast("spatial_model_layers/dist35.tif")
u36<- rast("spatial_model_layers/dist36.tif")
u37<- rast("spatial_model_layers/dist37.tif")
u39<- rast("spatial_model_layers/dist39.tif")
u40<- rast("spatial_model_layers/dist40.tif")
u41<- rast("spatial_model_layers/dist41.tif")


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
till<- vect("till_geochemistry/till_with_climate.shp")
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
                           u28 + u29 + u30 + u31 + u32 + u33 + u34 + u35 + u36 + u37 + u39 + u40 + u40 ,
                         till_df, nodesize = 4, ntree=2000, mtry=13, progress = TRUE, keep.inbag = TRUE)
save.image(file = "spatial_data.RData")
#Visualize the importance
spatial_importance<-importance(rfspatial)
spatial_importance_table<-as.data.frame(spatial_importance)
write.csv(spatial_importance_table, file="spatial_importance_table.csv")

#Create an RMSE function
RMSE = function(observed_data, predicted_data){
  sqrt(mean((observed_data - predicted_data)^2))
}

#Check the variable importance
varImpPlot(rf_spatial, main= "Model Two: Predicted vs Observed Till Al Availability")
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
  rfspatial<-randomForest(Al_avail~ ruggedness + unit_size + NLDEM + climate_zones_RS + distOcean_RS + slope
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
spatial_predict<-predict( rasterstack, rfspatial,filename = "results/spatial_prediction.tif", overwrite=TRUE)

####Now map uncertainty. This is very computationally heavy and may need to be split up
loop_fun<- function(rfspatial, rasterstack){
  library(terra)
  library(randomForest)
  v <- terra::predict( rfspatial,  rasterstack, predict.all=TRUE)
  v <- v$individual
  apply(v, 1, sd)
}

save.image( file = "spatial.Rdata")
##predict all at once
#prediction<- terra::predict( rfspatial, rasterstack, fun=loop_fun, filename= ("ssd.tif"), overwrite= TRUE)

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
  terra::predict(rasterstack, rfspatial, fun=loop_fun, filename= paste0("strips/ssdstrips/redo_ssdstrips", i, ".tif"), overwrite= TRUE)
  window(rasterstack)<-NULL
  gc()
}
value1

#mosaic the raster strips together
rastlist1 <- list.files(path = "strips/ssdstrips", pattern='.tif$', all.files= T, full.names= T)
print(rastlist1)

allrasters1 <- lapply(rastlist1, FUN = rast)

mos<- do.call(mosaic, allrasters1)
test_mos<- writeRaster(mos, filename = "ssd_mosaic.tif")
plot(mos)

