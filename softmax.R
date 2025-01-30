library(terra)
library(pbapply)
source('check_radiometric_consistency.R')

#Calculate the softmax function for each band of a raster.

softmax = function(raster, normalize = T){
  
  # exp_ = function(r){exp(r)} #helper function
  
  sm_bands = pblapply(X = 1:nlyr(raster), FUN = function(i){
    
    denom = global(exp(raster[[i]]), 'sum')
  })
}

# #calculate hue index
# hue_ind = function(r){
#   atan((r[['green']]-r[['blue']])*(2*r[['red']]-r[['green']]-r[['blue']])/30.5)
# }
