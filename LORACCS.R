library(terra)
library(tidyverse)
library(pbapply)
library(data.table)
source('check_radiometric_consistency.R') #load functions for checking radiometric consistency


##implement the LORACCS algorithm for radiometric correction of slightly overlapping images. 
#Based on https://github.com/swegmueller/LORACCS_Mosaic_Correction

LORACCS = function(target,reference){ #target is corrected to match reference
  
  # if(T %in% (origin(reference) != origin(target))){
  #   target = resample(target, reference)
  # }
  
  int = terra::intersect(
    # ext(
      reference
      # )
    ,
    # ext(
      target
      # )
    ) #get intersected area 
  
  if(is.null(int)){print('Reference and target do not overlap.')}
  
  overlap_ref = crop(reference,int,mask=T)
  overlap_tar = crop(target,int,mask=T)
  
  #develop loess models
  loess_models = pblapply(X = 1:nlyr(overlap_tar), FUN = function(i){
    
    band_name = names(overlap_tar)[i]
    
    dt = data.table(targ = as.vector(overlap_tar[[i]]), ref = as.vector(overlap_ref)) %>%
      group_by(targ) %>%
      summarise(count = n(),
                avg_ref = mean(ref)) %>%
      filter(count >= 6)
    
    loess_model = loess(avg_ref ~ targ, data = as.data.frame(dt), degree = 2)
    
    loess_models[[i]] = loess_model
    
  })
  
  names(loess_models) = names(reference)
  
  
  #correct rasters
  corrected_bands = pblapply(X = 1:nlyr(target), FUN = function(i){
    names(target[[i]]) = 'targ'
    corrected = terra::predict(target[[i]], loess_models[[i]])
    return(corrected)
  })
  
  corrected_rast = rast(corrected_bands)
  names(corrected_rast) = names(target)
  
  return(corrected_rast)
}



#----debugging, testing----

# data_dir = "data/PSScene"
# 
# file_names = list.files(data_dir, full.names = T)
# 
# raster_file_names = file_names[str_detect(file_names, '\\.tif$')]
# 
# scene_file_names = raster_file_names[!str_detect(raster_file_names, 'udm2\\.tif')]
# udm_file_names = raster_file_names[str_detect(raster_file_names, 'udm2\\.tif')]
# 
# scenes = lapply(X = scene_file_names, FUN = terra::rast)
# 
# ref = scenes[[1]]
# tar = scenes[[2]]
# 
# #simple rasters
# layers = c('blue', 'green', 'red', 'nir')
# 
# v1 = array(dim = c(1000, 1000, 4), data = round(rnorm(n = 4*1000000, mean = 1000, sd = 500)))
# reference = rast(v1)
# names(reference) = layers
# 
# v2 = array(dim = c(1000, 1000, 4), data = round(rnorm(n = 4*1000000, mean = 1100, sd = 400)))
# target = rast(v2)
# names(target) = layers
# 
# rmse_pre = rmse(reference, target)
# 
# target_corrected = LORACCS(target = target, reference = reference)
# 
# rmse_post = rmse(reference, target_corrected)
