#make current data

make_current_dat <- function(clim_covs,grid_buff,count_dat_out){

rh_curr <- clim_covs |> 
  filter(year < 2025) |> 
  arrange(year)

e_buff <- bind_rows(replicate(5,grid_buff[,1:7],simplify=FALSE))
e_buff$year = rep(2017:2021,each=dim(grid_buff)[1])
e_buff$effort = NA
e_buff$obs = NA

for(t in 2017:2021){
  
  year_sub = count_dat_out[count_dat_out$year==t,]
  
  count_ras = st_intersects(grid_buff, count_dat_out[count_dat_out$year==t,])
  count_ras[sapply(count_ras,function(x) length(x)==0L)] <- NA
  effort = rep(NA,length(count_ras))
  obs = rep(999,length(count_ras))
  
  for(i in 1:length(count_ras)){
    if(!is.na(count_ras[[i]][1])){
      #effort is number of surveys done in a grid cell, doesn't
      # relate to effort of sampling in that cell beyond that
      effort[i] = length(count_ras[[i]])
      obs[i] = sum(year_sub$observation_count[count_ras[[i]]])
    }
    else{
      effort[i] = 0
      obs[i] = NA
    }
  }
  
  e_buff$effort[which(e_buff$year==t)] = effort
  e_buff$obs[which(e_buff$year==t)] = obs
  
}

e_buff$rh <- rh_curr$RH_scale
e_buff$LSSP22020 <- factor(grid_buff$LSSP22020)

e_buff$elev_scale = as.numeric(scale(e_buff$elev))
e_buff$year_scale = as.numeric(scale(e_buff$year))
log_effort = log(e_buff$effort)
e_buff$log_effort = log_effort

return(e_buff)
}