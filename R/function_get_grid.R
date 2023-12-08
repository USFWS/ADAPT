get_grid <- function(study_grid){
  
  grid_dat <- st_read(study_grid)
  grid_dat2 <- st_transform(grid_dat, crs=st_crs("EPSG:5070"))  
  na_idx <- which(is.na(grid_dat2$slope) | is.na(grid_dat2$LSSP22020))
  grid_dat2 <- grid_dat2[-na_idx,]
  
  #buffer around points to create grid
  grid_buff <- st_buffer(grid_dat2, dist = 500, endCapStyle = "SQUARE")
  
  return(grid_buff) 
}