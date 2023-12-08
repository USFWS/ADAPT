
get_data <- function(count_dat){
  
  count_dat_tmp <- read.csv(count_dat)
  count_dat_tmp <- st_as_sf(count_dat_tmp, coords = c("longitude", "latitude"))
  st_crs(count_dat_tmp) <- st_crs("EPSG:4326")
  count_dat_out <- st_transform(count_dat_tmp, crs=st_crs("EPSG:5070"))
  count_dat_out$year <- year(count_dat_out$observation_date)
  count_dat_out$observation_count[count_dat_out$observation_count=="X"] = 1
  count_dat_out$observation_count <- as.numeric(count_dat_out$observation_count)
  
  return(count_dat_out)
  
  }
