#Functions to check radiometric consistency of Planetscope scenes using area of overlap

library(tidyverse)
library(terra)
library(pbapply)

#----Calculate root mean squared error----

rmse = function(raster1, raster2, normalise_rmse = F, method = 'mean', verbose = T, maxcell = max(ncell(raster1), ncell(raster2))){
  #Calculate the root mean squared error between two rasters.
  #Normalization based on code in https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/ .
  # raster1, raster2: SpatRaster objects being compared.
  #normalise_rmse: Specify whether or not to normalize the RMSE.Enables the comparison of rasters with different units (e.g. different vegetation indices).
  #method: If normalise_rmse = T, specifies the denominator used for normalization (one of 'mean', 'median', 'range', 'iqr' [interquartile range], or 'stdev').
  #verbose: Should the function print messages.
  #maxcell: Maximum number of cells to use. May be specified in order to process very large rasters.
  
  #check if extents match
  if(ext(raster1) != ext(raster2)){
    int = terra::intersect(raster1,raster2)
    raster1 = crop(raster1,int,mask=T)
    raster2 = crop(raster2,int,mask=T)
    
    if(verbose == T){print('Raster extents do not match, calculation based on intersecting area.')}
  }
  
  #calculate error
  error = raster1 - raster2
  
  #calculate RMSE
  
  RMSE = global(error, na.rm = T, maxcell = maxcell, fun = 'rms')
  names(RMSE) = 'rmse'
  
  #check for normalization, normalize if T
  if(normalise_rmse == T){
    #define dummy functions to use in global
    getiqr = function(r){IQR(r, na.rm= T)}
    getmedian = function(r){median(r, na.rm = T)}
    
    
    denominator <- switch(method,
                          'mean'   = global(raster1, na.rm = T, maxcell = maxcell, fun = 'mean'),
                          'stdev'  = global(raster1, na.rm = T, maxcell = maxcell, fun = 'std'),
                          'range'  = global(raster1, na.rm = T, maxcell = maxcell, fun = 'range') %>% mutate(range = max - min),
                          'iqr'    = setNames(global(raster1, maxcell = maxcell, fun = function(i) getiqr(i)), 'global'),
                          'median' = setNames(global(raster1, maxcell = maxcell, fun = function(i) getmedian(i)), 'global'),
                          stop("Invalid method")
    )
    
    RMSE = cbind(RMSE, denominator) %>% select(all_of(c('rmse',method)))
    RMSE$nrmse = RMSE[['rmse']]/RMSE[[2]]
  }
  
  #return RMSE
  return(RMSE)
  
}

#----Perform a Kolmogorov-Smirnov test----

ks = function(raster1, raster2, n_cells = 1000000){ #size is the number of cells to sample. Default is 1,000,000
  
  #check if extents match
  if(ext(raster1) != ext(raster2)){
    int = terra::intersect(raster1,raster2)
    raster1 = crop(raster1,int,mask=T)
    raster2 = crop(raster2,int,mask=T)
    
    print('Raster extents do not match, calculation based on intersecting area.')
  }
  
  #generate list with results from ks between corresponding bands of each raster
  
  pts = spatSample(raster1[[1]], size = n_cells
                   , xy = T
                   , values = F
  )
  
  ks_list = pblapply(X = 1:nlyr(raster1), FUN = function(i){
    
    ks.test(
      terra::extract(raster1[[i]], pts)[[1]],
      terra::extract(raster2[[i]], pts)[[1]]
    )
  })
  
  names(ks_list) = names(raster1)
  
  return(ks_list)
}

#----Plot reflectance histograms for each band----

band.hists = function(raster1, raster2, on_overlap = T, bands = NULL){
  #wrapper function for 
  #bands can take a numeric or character vector of what bands to plot, 
  #on_overlap determines whether the operation is applied to hte whole raster or just the overlapping portion 
  
  #check if extents match
  if(on_overlap == T){
    
    if(ext(raster1) != ext(raster2)){
      int = terra::intersect(raster1,raster2)
      raster1 = crop(raster1,int,mask=T)
      raster2 = crop(raster2,int,mask=T)
      
      print('Raster extents do not match, calculation based on terra::intersecting area.')
    }
  }
  
  #make density plots
  if(is.null(bands)){
    bands = 1:nlyr(raster1)
  } else {
    bands}
  r1 = raster1[[bands]]
  r2 = raster2[[bands]]
  
  
  v1 = as_tibble(values(r1)) %>% mutate(Raster = 'raster1')
  v2 = as_tibble(values(r2)) %>% mutate(Raster = 'raster2')
  
  v_all = rbind(v1, v2) %>%
    pivot_longer(cols = bands, names_to = 'bands', values_to = 'vals')
  
  p = ggplot(v_all, aes(x = vals, fill = Raster))+
    geom_density(alpha = 0.5)+
    facet_wrap(facets = vars(bands)
               , ncol = ceiling(sqrt(length(bands)))
               , nrow = ceiling(sqrt(length(bands)))
    )
  
  return(p)
  
}


#----test, debug----
#(uncomment to use)
#make rasters
ncolumns = 1000
nrows = 1000
nlayers = 4
r1 = rast(ncol = ncolumns, nrow = nrows, nlyr = nlayers, xmin = 0, xmax = ncolumns, ymin = 0, ymax = nrows,
          vals = rnorm(ncolumns * nrows * nlayers, mean = runif(1), sd = runif(1)))

r2 = runif(1)*r1+runif(1)

#make rasters partially overlapping by assigning NA values to columns
r1[,1:(nrows/4)] = NA
r2[,(nrows:(nrows-(nrows/4)))] = NA

#check rmse
rmse(r1, r2)
#check nrmse
rmse(r1, r2, normalise_rmse = T)
