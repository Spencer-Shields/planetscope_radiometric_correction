#Functions to check radiometric consistency of Planetscope scenes using area of overlap

library(tidyverse)
library(terra)

#----Calculate the Root Mean Squared Error of two single-band rasters----

rmse = function(raster1, raster2) { #raster1 and raster2 are SpatRaster objects to be compared
  
  error = raster1 - raster2
  error_squared = error^2
  RMSE = sqrt(mean(values(error_squared),na.rm=TRUE))
  return(RMSE)
} 


#----Calculate the Normalized Root Mean Squared Error of two single-band rasters----
#for a discussion on different methods, see https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/
nrmse = function(raster1, raster2, method = 'mean'){
  
  #calculate Root Mean Squared Error
  RMSE = rmse(raster1, raster2)
  
  #get denominator to normalize
  v = c(values(raster1, na.rm = T), values(raster2, na.rm = T)) #pool raster values
  
  denominator = case_match(method,
                           'mean' ~ mean(v),
                           'median' ~ median(v),
                           'range' ~ max(v)-min(v),
                           'interquartile' ~ IQR(v),
                           'stdev' ~ sd(v)
  )
  NRMSE = RMSE/denominator
  return(NRMSE)
}

#----Perform a Kolmogorov-Smirnov test----

ks = function(raster1, raster2){
  
  raster1 = values(raster1)
  raster2 = values(raster2)
  
  ks.test(raster1, raster2)
}

#----Plot reflectance histograms for each band----

band_hists = function(raster1, raster2, bands = NULL){ #bands can take a numeric or character vector of what bands to plot
  
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
  
  ggplot(v_all, aes(x = vals, fill = Raster))+
    geom_density(alpha = 0.5)+
    facet_wrap(facets = vars(bands)
               , ncol = ceiling(sqrt(length(bands)))
               , nrow = ceiling(sqrt(length(bands)))
               )
  
}


#----test, debug----

#make rasters
r1 = rast(ncol = 500, nrow = 500, xmin = 0, xmax = 500, ymin = 0, ymax = 500, vals = rnorm(500 * 500, mean = runif(1), sd = runif(1)))
r2 = runif(1)*r1+runif(1)
  
# na_indices <- sample(ncell(r1), 25)  # Randomly select 25 cell indices
# values(r1)[na_indices] <- NA

hist(r1, col = 'blue')
hist(r2, col = 'red', add = T)

hist(r1, col = rgb(0, 0, 1, 0.5), nclass = 20, xlim = c(min(c(values(r1), values(r2))), max(c(values(r1), values(r2)))), 
     main = "Histograms of Rasters", xlab = "Pixel Values", ylab = "Frequency")
hist(r2, nclass = 20, col = rgb(1, 0, 0, 0.5), add = TRUE)

a = density(values(r1), col = 'blue', plot = T)
lines(density(values(r2), col = 'red'))
