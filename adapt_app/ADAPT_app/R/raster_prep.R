#prep rasters for app upload

#file reads in rasters and combines them into a brick then saves them
raster_prep <- function(species_name){
  
  #read in rasters
  file_ssp2_2100 <- rast(paste0(species_name,"_ssp2_2100.tif"))
  file_ssp2_2060 <- rast(paste0(species_name,'_ssp2_2060.tif'))
  file_ssp5_2100 <- rast(paste0(species_name,'_ssp5_2100.tif'))
  file_ssp5_2060 <- rast(paste0(species_name,'_ssp5_2060.tif'))
  file_current <- rast(paste0(species_name,"_current.tif"))
  
  # #calculate differences
  # ssp2_2100_2060 <- file_ssp2_2100-file_ssp2_2060
  # names(ssp2_2100_2060) <- paste0(species_name,"_ssp2_2100_2060")
  # ssp2_2100_current <- file_ssp2_2100-file_current
  # names(ssp2_2100_current) <- paste0(species_name,"_ssp2_2100_current")
  # ssp2_2060_current <- file_ssp2_2060-file_current
  # names(ssp2_2060_current) <- paste0(species_name,"_ssp2_2060_current")
  # ssp5_2100_2060 <- file_ssp5_2100-file_ssp5_2060
  # names(ssp5_2100_2060) <- paste0(species_name,"_ssp5_2100_2060")
  # ssp5_2100_current <- file_ssp5_2100-file_current
  # names(ssp5_2100_current) <- paste0(species_name,"_ssp5_2100_current")
  # ssp5_2060_current <- file_ssp5_2060-file_current
  # names(ssp5_2060_current) <- paste0(species_name,"_ssp5_2060_current")
  # ssp2_ssp5_2060 <- file_ssp5_2060-file_ssp2_2060
  # names(ssp2_ssp5_2060) <- paste0(species_name,"_ssp2_ssp5_2060")
  # ssp2_ssp5_2100 <- file_ssp5_2100-file_ssp2_2100
  # names(ssp2_ssp5_2100) <- paste0(species_name,"_ssp2_ssp5_2100")
  # 
  # #combine
  # 
  # file_brick <- c(file_current, 
  #                 file_ssp2_2060,
  #                 file_ssp5_2060,
  #                 file_ssp2_2100,
  #                 file_ssp5_2100,
  #                 ssp2_2100_2060,
  #                 ssp2_2100_current,
  #                 ssp2_2060_current,
  #                 ssp5_2100_2060,
  #                 ssp5_2100_current,
  #                 ssp5_2060_current,
  #                 ssp2_ssp5_2100,
  #                 ssp2_ssp5_2060)
  
  species_brick <- list(current = file_current, 
                        mid = c(file_ssp2_2060,
                                file_ssp5_2060),
                        end = c(file_ssp2_2100,
                                file_ssp5_2100)
  )
  
  #save as brick
  
  #writeRaster(file_brick,paste0(species_name,"_brick.tif"),overwrite=TRUE)
}
