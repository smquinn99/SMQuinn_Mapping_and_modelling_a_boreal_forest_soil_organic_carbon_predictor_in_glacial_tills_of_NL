The following details the use of each code included in this repository:

This code may be used to validate results of the associated till geochemistry model, 
but can also be applied to different purposes. For example, one could follow the 
same workflow for approximating a geostatistical model with one’s own datasets with
minimal editing. Additionally, the calibration of Random Forest uncertainty code can
easily be applied to a user’s own model predictions.

 The spatial and aspatial codes are very similar, in some cases nearly identical.
 For this reason, if a user is executing both a spatial and aspatial model,
 the workflows should be executed in separate R sessions so as to not overwrite
 one another’s outputs.

aspatial_model_code
	Code for importing rasters, extracting their values to sample points, and using
 these values to train a Random Forest model solely on the strength of 
 environmental covariates. Following, this cross validation is performed, 
 predictions are generated and mapped, and corresponding uncertainty is also 
 calculated and mapped.
 
aspatial_calibration1
	This code partitions the predictions into a test and train set. The train set is
 used to develop calibration factors (slope and intercept) for model uncertainty.
 Aspatial_model_code must be run at least to the point of training the Random
 Forest model before this code can be run.
 
aspatial_calibration2
	This code applies the calibration factors developed by the training set to the 
 remaining test set and graphically and statistically evaluates the calibration 
 factors performance. Aspatial_calibration1 must be run prior to running this
 code.
 
aspatial_calibration3
	This code applies the calibration factors to the entire dataset and 
 graphically and statistically evaluates their performance. It also generates
 figures demonstrating the graphical evaluation. Aspatial_calibration2 must be
 run prior to running this code.
 
aspatial_raster_calibration
	The final code in this series applies the calibration factors to mapped 
 uncertainty estimates. Aspatial_calibration3 must be run prior to running
 this code.
 
spatial_model_code
Code for importing rasters, extracting their values to sample points, and
using these values to train a Random Forest model on both the strength of
environmental covariates and their spatial context. Following, this cross 
validation is performed, predictions are generated and mapped, and 
corresponding uncertainty is also calculated and mapped.

spatial_calibration1
This code partitions the predictions into a test and train set. The train set is 
used to develop calibration factors (slope and intercept) for model uncertainty. 
Spatial_model_code must be run at least to the point of training the Random 
Forest model before this code can be run.

spatial_calibration2
This code applies the calibration factors developed by the training set to the 
remaining test set and graphically and statistically evaluates the calibration 
factors performance. Spatial_calibration1 must be run prior to running this code.

Spatial_calibration3
This code applies the calibration factors to the entire dataset and graphically
and statistically evaluates their performance. It also generates figures 
demonstrating the graphical evaluation. Spatial_calibration2 must be run prior 
to running this code.

Spatial_raster_calibration
The final code in this series applies the calibration factors to mapped 
uncertainty estimates. Aspatial_calibration3 must be run prior to running this code.
