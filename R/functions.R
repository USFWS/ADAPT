#R/functions.R

#function for data entry
#study_grid <- "BUOW_pointgridALL.gpkg"
#count_dat <- "~/eBird_Data/buow_ebird_data.csv"

get_data <- function(study_grid, count_dat){
  grid_dat <- st_read(study_grid)
  count_dat <- read.csv(count_dat)
  count_dat <- st_as_sf(count_dat, coords = c("longitude", "latitude"))
  st_crs(count_dat) <- st_crs("EPSG:4326")
  grid_dat2 <- st_transform(grid_dat, crs=st_crs("EPSG:5070"))
  count_dat2 <- st_transform(count_dat, crs=st_crs("EPSG:5070"))
  count_dat2$year <- year(count_dat2$observation_date)
  count_dat2$observation_count[count_dat2$observation_count=="X"] = 1
  count_dat2$observation_count <- as.numeric(count_dat2$observation_count)
  
  na_idx <- which(is.na(grid_dat2$slope) | is.na(grid_dat2$LSSP22020))
  grid_dat2 <- grid_dat2[-na_idx,]
  
  #buffer around points to create grid
  grid_buff <- st_buffer(grid_dat2, dist = 500, endCapStyle = "SQUARE")
  
  #test for correlation
  #cor(beth_clim_dat[,c(5:29,380)])
  #which(abs(cor(beth_clim_dat[,c(5:29)])[,1]) < 0.6)
  #elev cor w/ AHM, bFFP, DD.0, DD.18, MAP, MSP
  #AHM cor w/ CMD, DD1040, DD5, eFFP,EMT, Eref, EXT, FFP, MAT, MCMT,MWMT,NFFD,SHM
  #not correlated: slope, elev, PAS, RH, TD
  # PAS: precip as snow - doesn't makes sense here
  # RH: relative humidity
  # TD: temp diff between warmest and coldest month
  
  rh_245 <- c(grid_buff$RH.2017,
              grid_buff$RH.2018,
              grid_buff$RH.2019,
              grid_buff$RH.2020,
              grid_buff$RH.2021,
              grid_buff$RH.ssp245.2001.2020,
              grid_buff$RH.ssp245.2021.2040,
              grid_buff$RH.ssp245.2041.2060,
              grid_buff$RH.ssp245.2061.2080,
              grid_buff$RH.ssp245.2081.2100)
  rh_245_scale_tmp <- scale(rh_245)
  
  rm(grid_dat)
  rm(grid_dat2)
  rm(count_dat)
  e_buff <- bind_rows(replicate(5,grid_buff[,1:7],simplify=FALSE))
  e_buff$year = rep(2017:2021,each=dim(grid_buff)[1])
  e_buff$effort = NA
  e_buff$obs = NA
  
  for(t in 2017:2021){
    
    year_sub = count_dat2[count_dat2$year==t,]
    
    count_ras = st_intersects(grid_buff, count_dat2[count_dat2$year==t,])
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
  
  rm(year_sub)
  
  e_buff$rh <- rh_245_scale_tmp[1:dim(e_buff)[1]]
  #rm(grid_buff)
  
  #rh_245_scale <- matrix(rh_245_scale_tmp,dim(e_buff)[1],6)
  #colnames(rh_245_scale) <- c("current", "ssp245_20","ssp245_40","ssp245_60","ssp245_80","ssp245_100")
  
  #rh_585 <- c(beth_clim_t$RH.ssp585.2001.2020,
  #            beth_clim_t$RH.ssp585.2021.2040,
  #            beth_clim_t$RH.ssp585.2041.2060,
  #            beth_clim_t$RH.ssp585.2061.2080,
  #            beth_clim_t$RH.ssp585.2081.2100)
  #rh_585_scale_tmp <- scale(rh_585)
  #rh_585_scale <- matrix(rh_585_scale_tmp,dim(beth_ras_tmp)[1],5)
  #colnames(rh_585_scale) <- c("ssp585_20","ssp585_40","ssp585_60","ssp585_80","ssp585_100")
  
  e_buff$LSSP22020 <- factor(grid_buff$LSSP22020)
  
  e_buff$elev_scale = as.numeric(scale(e_buff$elev))
  e_buff$year_scale = as.numeric(scale(e_buff$year))
  log_effort = log(e_buff$effort)
  #log_effort[which(!is.finite(log_effort))] = 0
  e_buff$log_effort = log_effort
  
  return(e_buff)
}
  
#reduce to just those sites w/ observations (0/1)
reduce_dat <- function(e_buff){  
  pts_pt2 <- e_buff[!is.na(e_buff$obs),]
  pts_pt = st_centroid(pts_pt2)
  
  pts_pt$obs[pts_pt$obs > 0] = 1
  pts_pt$id = factor(pts_pt$id)
  
  return(pts_pt)
}

#function for spatial field

build_spatial <- function(pts_pt,
                          max.edge,
                          cutoff){
  
  boundary <- fm_extensions(pts_pt, 
                            convex = c(200000,500000), 
                            concave = c(150000,300000))
  
  #### create INLA "mesh" over study area ####
  # tried max edge of 25 km, seemed fine (no spatial artifacts)
  # when increased to 100 km, could start to see spatial artifacts
  spatial_field = fm_mesh_2d_inla(boundary = boundary,
                              max.edge = max.edge, #original: 50000,100000
                              cutoff = cutoff,  #original: 25000
                              offset = c(2000,100000),
                              crs = fm_crs(pts_pt))
  
  return(spatial_field)
  
  }
  ##spatial INLA model
  # prior for the range is currently set such that the prob
  # of the range exceeding 500 km is 0.5
make_matern <- function(spatial_field,
                        prior.range.mu){ 
  
  matern <- inla.spde2.pcmatern(spatial_field, 
                     prior.range=c(prior.range.mu, 0.5), #was c(10000,.5)
                     prior.sigma=c(1, 0.01))
  
  return(matern)
}

#function for analysis

inla_func <- function(vars,pts_pt,matern){
  cov_vec <- vars
  n_cov <- length(cov_vec)
  list_covs <- as.list(NA)
  n_forms <- 0
  
  #calculate number of formulas to run
  for(i in 1:n_cov){
    n_forms <- sum(n_forms,choose(n_cov,i))
  }
  
  if(n_forms > 10) {warning(paste0("wow that's ",n_forms," models!"))}
  
  list_formula <- as.list(NA)
  list_formula_tmp <- as.list(NA)
  
  for(i in 1:n_cov){
    #each combo is an element of the list
    list_formula_tmp <- as.list(NA)
    for(j in 1:choose(n_cov,i)){
      # list_formula_tmp[j] <- paste(combn(cov_vec,i)[,j],
      #            "(st_coordinates,weights=",
      #            combn(cov_vec,i)[,j],
      #            ",model=matern)",sep="",collapse="+")
      list_formula_tmp[j] <- paste(combn(cov_vec,i)[,j],collapse = "+")
    }
    list_formula[[i]] <- list_formula_tmp
  }
  
  list_formula <- unlist(list_formula)
  list_formula2 <- as.list(NA)
  for(i in 1:n_forms){
    list_formula2[[i]] <- list_formula[i]
  }
  
  output <- as.list(NA)
  
  for(i in 1:n_forms){
    inla_formula = as.formula(paste('obs ~ -1 +', list_formula2[i]))
    
    output[[i]] <- bru(components = inla_formula,
                       family = "binomial",data = pts_pt, 
                       options = list(control.compute = list(waic = TRUE, cpo = TRUE),
                                      control.inla = list(strategy = "simplified.laplace",
                                                          int.strategy = "eb"),
                                      control.predictor = list(compute=TRUE,link=1),
                                      verbose = FALSE))
    
    if(sum(output[[i]]$cpo$failure) > 0){
      warning("Model ",n_forms," has convergence issues (sum(CPO$Failure > 0))")
    }
    
  }
  
  return(output=output)
}

#function for predictions

get_preds <- function(e_buff,inla_run){
  length_ebuff <- length(which(e_buff$year==2017))
  
  
  pred_dat_tmp <- data.frame(
    rh = rh_245_scale_tmp[(length(rh_245_scale_tmp)-dim(grid_buff)[1]+1):(length(rh_245_scale_tmp))], 
    #LSSP22020 = factor(grid_buff$LSSP22100),
    geom = e_buff$geom[1:2639305],
    year_scale = rep(unique(e_buff$year_scale)[5],dim(grid_buff)[1]),
    log_effort = rep(0,dim(grid_buff)[1]),
    elev_scale = e_buff$elev_scale[1:2639305],
    id = factor(e_buff$id[1:2639305]))
  
  pred_dat_245_tmp <- st_as_sf(pred_dat_tmp)
  pred_dat_245 <- st_centroid(pred_dat_245_tmp)
  st_geometry(pred_dat_245) <- "geom"
  
  pred_245_50 <- predict(output_50,
                 pred_dat_245, 
                 re.form = NA,
   ~ exp(alpha+clim)/(1+exp(alpha+effort+elev+year+clim+veg+eps)),
                 n.samples = 1000))
  
}
