#Functions to check radiometric consistency of Planetscope scenes using area of overlap

library(tidyverse)
library(terra)
library(pbapply)

#----Calculate the Root Mean Squared Error of two single-band rasters----

rmse = function(raster1, raster2, verbose = T) { 
  #Calculate the root mean squared error between bands of two rasters. Each raster should have the same number of bands in the same order.
  
  #check if extents match
  if(ext(raster1) != ext(raster2)){
    int = terra::intersect(raster1,raster2)
    raster1 = crop(raster1,int,mask=T)
    raster2 = crop(raster2,int,mask=T)
    
    if(verbose == T){print('Raster extents do not match, calculation based on intersecting area.')}
  }
  
  #calculate rmse
  error = raster1 - raster2 #return raster which is the difference of raster1 and raster2
  
  RMSE = global(error, 'rms', na.rm = T) #return data.frame of rmse values
  names(RMSE) = 'rmse' #assign dataframe column name
  
  return(RMSE)
} 


#----Calculate the Normalized Root Mean Squared Error of two single-band rasters (INCOMPLETE)----
# #for a discussion on different methods, see https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/
# nrmse = function(raster1, raster2, method = 'mean'){ #method can be 'mean', 'median', 'range', 'interquartile', or 'stdev'
# #calculate normalized root mean squared error. NOTE: this is much slower than the RMSE function because it does not leverage the terra::global functions which are created with Rcpp.  
#   #check if extents match
#   if(ext(raster1) != ext(raster2)){
#     int = terra::intersect(raster1,raster2)
#     raster1 = crop(raster1,int,mask=T)
#     raster2 = crop(raster2,int,mask=T)
#     
#     print('Raster extents do not match, calculation based on terra::intersecting area.')
#   }
#   
#   #calculate Root Mean Squared Error
#   RMSE = rmse(raster1, raster2)
#   
#   #get denominator to normalize
#   v = c(values(raster1[[1]], na.rm = T), values(raster2[[1]], na.rm = T)) #pool raster values
#   
#   denominator = case_match(method,
#                            'mean' ~ mean(v),
#                            'median' ~ median(v),
#                            'range' ~ max(v)-min(v),
#                            'interquartile' ~ IQR(v),
#                            'stdev' ~ sd(v)
#   )
#   
#   #get denominator to normalize
#   
#   denominator = case_match(method,
#                            'mean' ~ global(),
#                            'median' ~ median(v),
#                            'range' ~ max(v)-min(v),
#                            'interquartile' ~ IQR(v),
#                            'stdev' ~ sd(v)
#   
#   NRMSE = RMSE/denominator
#   return(NRMSE)
# }


#----Perform a Kolmogorov-Smirnov test----

ks = function(raster1, raster2, n_cells = 1000000){ #size is the number of cells to sample. Default is 1,000,000
  
  #check if extents match
  if(ext(raster1) != ext(raster2)){
    int = terra::intersect(raster1,raster2)
    raster1 = crop(raster1,int,mask=T)
    raster2 = crop(raster2,int,mask=T)
    
    print('Raster extents do not match, calculation based on terra::intersecting area.')
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

#----Plot reflectance histograms for each band (INCOMPLETE)----

# band_hists = function(raster1, raster2, bands = NULL){ #bands can take a numeric or character vector of what bands to plot
#   
#   #check if extents match
#   if(ext(raster1) != ext(raster2)){
#     int = terra::intersect(raster1,raster2)
#     raster1 = crop(raster1,int,mask=T)
#     raster2 = crop(raster2,int,mask=T)
#     
#     print('Raster extents do not match, calculation based on terra::intersecting area.')
#   }
#   
#   #make histograms
#   if(is.null(bands)){
#     bands = 1:nlyr(raster1)
#   } else {
#     bands}
#   r1 = raster1[[bands]]
#   r2 = raster2[[bands]]
#   
#   
#   v1 = as_tibble(values(r1)) %>% mutate(Raster = 'raster1')
#   v2 = as_tibble(values(r2)) %>% mutate(Raster = 'raster2')
#   
#   v_all = rbind(v1, v2) %>%
#     pivot_longer(cols = bands, names_to = 'bands', values_to = 'vals')
#   
#   ggplot(v_all, aes(x = vals, fill = Raster))+
#     geom_density(alpha = 0.5)+
#     facet_wrap(facets = vars(bands)
#                , ncol = ceiling(sqrt(length(bands)))
#                , nrow = ceiling(sqrt(length(bands)))
#                )
#   
# }


#----test, debug----

# #make rasters
# r1 = rast(ncol = 500, nrow = 500, xmin = 0, xmax = 500, ymin = 0, ymax = 500, vals = rnorm(500 * 500, mean = runif(1), sd = runif(1)))
# r2 = runif(1)*r1+runif(1)
#   
# # na_indices <- sample(ncell(r1), 25)  # Randomly select 25 cell indices
# # values(r1)[na_indices] <- NA
# 
# hist(r1, col = 'blue')
# hist(r2, col = 'red', add = T)
# 
# hist(r1, col = rgb(0, 0, 1, 0.5), nclass = 20, xlim = c(min(c(values(r1), values(r2))), max(c(values(r1), values(r2)))), 
#      main = "Histograms of Rasters", xlab = "Pixel Values", ylab = "Frequency")
# hist(r2, nclass = 20, col = rgb(1, 0, 0, 0.5), add = TRUE)
# 
# a = density(values(r1), col = 'blue', plot = T)
# lines(density(values(r2), col = 'red'))
