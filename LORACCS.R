library(terra)
library(tidyverse)
library(here)
source(here('check_radiometric_consistency.R'))
##implement the LORACCS algorithm for radiometric correction of slightly overlapping images. Based on hhttps://github.com/swegmueller/LORACCS_Mosaic_Correction

LORACCS = function(target,reference){ #target is corrected to match reference
  
  int = intersect(reference,target) #get intersected area 
  
  if(is.null(int)){print('Reference and target do not overlap.')}
  
  overlap_ref = crop(reference,int,mask=T)
  overlap_tar = crop(target,int,mask=T)
  
  corrected_bands = list()
  
  pblapply(X = 1:nlyr(overlap_tar, FUN = function(i){
    
    band_name = names(overlap_tar)[i]
    
    
  }))
  
  
  
  
  
  r = rast(list(overlap_ref,overlap_tar))  #combine the overlaps into a raster stack
  
  v = values(r)
  
  d = tibble(ref = v[,1], tar = v[,2]) %>% filter(!(is.na(ref) | is.na(tar)))
  
  d_grouped = d %>% #get the mean reference value that corresponds with each target value. remove target values that have fewer than 6 unique correpsonding reference values
    group_by(tar) %>%
    # filter(n_distinct(ref) >= 6) %>% 
    summarize(ref_mean = round(mean(ref)))
  
  m = loess(tar ~ ref_mean, data = d_grouped, span=0.15, degree = 2) #span and degree parameters come from the original loraccs code
  
  tar_new = predict(target, m) #apply loess model to the target raster
  
  return(tar_new)
  
}

#debugging, testing

# wd = "C:/Users/spenshi/OneDrive/Desktop/UBC/MSc/Planetscope data/overlap_footprint_PSScene_analytic_sr_udm2/PSScene"
# 
# reference = ps_tbl$SR[[6]][['blue']]
# target = ps_tbl$SR[[7]][['blue']]
# 
# r1 = rlogo[['blue']]
# r2 = rlogo[['blue']] + 47
# 
# reference = r1
# target = r2
# 
# LORACCS(reference = reference, target = target)

wd = "C:/Users/spenshi/OneDrive/Desktop/UBC/MSc/Planetscope data/overlap_footprint_PSScene_analytic_sr_udm2/PSScene"
setwd(wd)

file_names = list.files(wd)

scene_file_names = file_names[str_detect(file_names, '\\.tif$') & !str_detect(file_names, 'udm')]

scenes = lapply(X = scene_file_names, FUN = terra::rast)

ref = scenes[[1]]
targ = scenes[[2]]

corrected = LORACCS(reference = ref, target = targ)
