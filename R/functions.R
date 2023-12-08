#R/functions.R


#function for predictions

#get_preds <- function(e_buff,inla_run){
#  length_ebuff <- length(which(e_buff$year==2017))
  
  
#  pred_dat_tmp <- data.frame(
#    rh = rh_245_scale_tmp[(length(rh_245_scale_tmp)-dim(grid_buff)[1]+1):(length(rh_245_scale_tmp))], 
#    #LSSP22020 = factor(grid_buff$LSSP22100),
#    geom = e_buff$geom[1:2639305],
#    year_scale = rep(unique(e_buff$year_scale)[5],dim(grid_buff)[1]),
#    log_effort = rep(0,dim(grid_buff)[1]),
#    elev_scale = e_buff$elev_scale[1:2639305],
#    id = factor(e_buff$id[1:2639305]))
  
#  pred_dat_245_tmp <- st_as_sf(pred_dat_tmp)
#  pred_dat_245 <- st_centroid(pred_dat_245_tmp)
#  st_geometry(pred_dat_245) <- "geom"
  
#  pred_245_50 <- predict(output_50,
#                 pred_dat_245, 
#                 re.form = NA,
#   ~ exp(alpha+clim)/(1+exp(alpha+effort+elev+year+clim+veg+eps)),
#                 n.samples = 1000))
  
#}
