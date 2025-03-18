# library(terra)
# library(pbapply)
# source('check_radiometric_consistency.R')

#Calculate the softmax function for each band of a raster.

softmax = function(raster, append_name = FALSE){
  #Calculate softmax function on a multiband spatraster.
  #Input and output == multiband spatraster.
  #append_names will add '_softmax' on the end of hte original band names.
  #NOTE: it is recommended to rescale data to low values (e.g. [0,1]) before using this function.
  sums = global(exp(raster), 'sum', na.rm = T)
  
  sm_l = pblapply(X = 1:nlyr(raster), FUN = function(i){
    num = exp(raster[[i]])
    denom = sums$sum[i]
    sm = num/denom
    return(sm)
  })
  
  sm = rast(sm_l)
  
  if(append_name == T){
    names(sm) = paste0(names(raster),'_softmax')
  }
  
  return(sm)
  
}

# #calculate hue index
# hue_ind = function(r){
#   atan((r[['green']]-r[['blue']])*(2*r[['red']]-r[['green']]-r[['blue']])/30.5)
# }

# path = "D:/Quesnel/data/planet_scenes/VegIndices/20210617_182517_55_2459_VegIndices.tif"
# r = rast(path)
# 
# r
# r_sm = softmax(r)
