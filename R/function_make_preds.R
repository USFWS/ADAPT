#make predictions based on inla output

#needs to make predictions for current time period,
# SSP2 yrs 2050 and 2100 and SSP5 yrs 2050 and 2100

make_preds <- function(inla_run,clim_covs,grid_buff,e_buff,raster_out){
  
  ssp_idx <- unique(clim_covs$ssp)
  ssp_idx2 <- c(NA,2,5)
  year_idx <- unique(clim_covs$year[clim_covs$year > 2021])

  for(i in 1:length(ssp_idx)){
    
  if(is.na(ssp_idx[i])){
    next
    } 
    
    for(j in 1:length(year_idx)){
      
  clim_covs_tmp <- clim_covs |> 
    filter(ssp == ssp_idx[i] & year == year_idx[j])
  
  lssp_tmp <- grid_buff |> 
    select(starts_with("LSSP")) |> 
    pivot_longer(
      cols = starts_with("LSSP"),
      names_to = "ssp_year",
      values_to = "LC") |> 
    mutate(
      ssp_year = parse_number(ssp_year)) |> 
    separate_wider_position(
      ssp_year,
      widths = c(ssp=1, year=4)
    ) |> 
    mutate(
      ssp = parse_double(ssp)) |>
    mutate(
      year = parse_double(year)) |> 
    filter(ssp == ssp_idx2[i] & year == year_idx[j])

  print(year_idx[j])
  
  pred_dat_tmp <- data.frame(
    rh = clim_covs_tmp$RH_scale, 
    LSSP22020 = factor(lssp_tmp$LC),
    geom = grid_buff$geom,
    year_scale = rep(unique(e_buff$year_scale)[5],dim(grid_buff)[1]),
    log_effort = rep(0,dim(grid_buff)[1]),
    elev_scale = e_buff$elev_scale[1:dim(grid_buff)[1]],
    id = factor(e_buff$id[1:dim(grid_buff)[1]]))
  
  pred_dat_245_tmp <- st_as_sf(pred_dat_tmp)
  pred_dat_245 <- st_centroid(pred_dat_245_tmp)
  st_geometry(pred_dat_245) <- "geom"  
  
  pred_245 <- predict(
    inla_run,
    pred_dat_245, 
    re.form = NA,
    ~ 1/(1+exp(-(alpha+elev+eps+year+clim+veg+effort))))
  
  template <- read_stars("~/BETH/BUOW_dem_nb.tiff")
  template[[1]][] = NA
  
  raster_out[[i-1]][[j]] <- st_rasterize(pred_245["mean"],template = template)
  
  #write_stars(raster_ssp2,"lewo_ssp2_2100.tif")
  }}
  
  current_pred <- e_buff[which(e_buff$year==2021),]
  current_pred$log_effort <- rep(0,dim(current_pred)[1])
  current_pred_out <- st_centroid(current_pred)
  
  pred_current <- predict(
    inla_run,
    current_pred_out,
    ~ 1/(1+exp(-(alpha+effort+elev+year+clim+veg+eps))))
  
  raster_out$current <- st_rasterize(pred_current["mean"], template = template)
  
  return(raster_out)
}
