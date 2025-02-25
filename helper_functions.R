#____Helper functions for processing remote sensing data____


landstack = function(dir, what= 'bands'){
  #Stacks individual Landsat bands into SpatRaster objects using metadata. Input is a directory and output is a list of spatrasters.
  #dir: the path to a directory containing landsat data and metadata.
  #what: the type of data to stack. Can be 'bands' or 'qa' for quality assurance layers.
  
  lsat_files = list.files(dir, full.names = T)
  meta_files = lsat_files[stringr::str_detect(lsat_files, 'MTL.txt')]
  
  raster_files = switch(what,
                        'bands' = lsat_files[stringr::str_detect(lsat_files, 'B\\d.TIF')],
                        'qa' = lsat_files[stringr::str_detect(lsat_files, '_QA_[A-Za-z]+\\.TIF')],
                        stop('Value provided to `what` not recognized.'))
  
  stack_list = pbapply::pblapply(X = 1:length(meta_files), FUN = function(i){
    
    #get product id
    lsatMeta = RStoolbox::readMeta(meta_files[i], raw=T)
    product_id = lsatMeta$PRODUCT_CONTENTS[rownames(lsatMeta$PRODUCT_CONTENTS)=='LANDSAT_PRODUCT_ID',]
    
    id_rast_files = raster_files[stringr::str_detect(raster_files, product_id)]
    rast_list = lapply(id_rast_files, rast)
    stacked_rast = terra::rast(rast_list)
    
    #apply band names
    band_names = sapply(1:length(rast_list), function(j){
      n = names(rast_list[[j]])
      sl = stringr::str_split_1(n, '_')
      sl[length(sl)]
    })
    names(stacked_rast) = band_names
    
    return(stacked_rast)
  })
  return(stack_list)
}
