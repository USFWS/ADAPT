## make rasters from preds output

make_raster_file <- function(raster_in,species){
  raster_name <- names(raster_in)
  years <- c(2060,2100)
  
  for(i in 1:(length(raster_in)-1)){
    for(j in 1:length(raster_in[[i]])){
    write_stars(raster_in[[i]][[j]], 
                paste0(species,"_",raster_name[i],"_",years[j],".tif"))
    }
  }
  write_stars(raster_in[[3]],
              paste0(species,"_current.tif"))
}