library(INLA)
library(inlabru)
library(targets)
library(tidyverse)
library(parallel)

#tar_manifest(fields = all_of("command"))
#tar_visnetwork()

Sys.setenv(TAR_PROJECT = "project_lewo")
tar_visnetwork(targets_only = TRUE)
tar_make()
tar_read(inla_run_lewo)
tar_load(inla_run_lewo)
summary(inla_run_lewo[[1]])
tar_load(preds)
source("R/function_make_raster_file.R")
make_raster_file(preds,"lewo")

Sys.setenv(TAR_PROJECT = "project_buow")
tar_visnetwork()
tar_make()
tar_read(inla_run_buow)
tar_load(inla_run_buow)
summary(inla_run_buow[[1]])
tar_load(preds)
source("R/function_make_raster_file.R")
make_raster_file(preds,"buow")

#subset of data to test prediction function faster
count_dat_test <- slice_sample(count_dat_out, n=1000)
e_buff_test <- slice_sample(e_buff,by=obs, n=1000)
clim_covs_test <- clim_covs |> 
  filter(year > 2025) |> 
  slice_sample(by=c(ssp,year),n=1000)
grid_buff_test <- slice_sample(grid_buff, n=1000)

pts_pt_test <- reduce_dat(e_buff_test)
spatial_field_test <- build_spatial(pts_pt_test, max.edge = c(100000,500000),cutoff=10000)
matern_test <- make_matern(spatial_field_test, prior.range.mu = 500000)

inla_test <- inla_func(vars = 'alpha(geom, model = matern) + 
                        elev(elev_scale, model = "linear") +
                        eps(id, model = "iid", constr = TRUE, hyper = list(prec = pc_prec)) +
                        year(year_scale, model = "linear") +
                        clim(geom, weights = rh, model = matern) + 
                        veg(LSSP22020, model = "factor_full") +
                        effort(log_effort, model = "linear")',
                       pts_pt = pts_pt_test,
                       matern = matern_test,
                       global = TRUE)

raster_out = list(ssp2 = c(list(NA),list(NA)), ssp5 = c(list(NA), list(NA)), current=list(NA))
  
system.time(test_preds <- make_preds(inla_test, clim_covs,
            grid_buff_test, e_buff_test,raster_out = raster_out))
#run predictions in parallel
test <- list()

nc = length(test)
clus = makeCluster(nc)
clusterExport(clus, "inla_run_lewo")
clusterEvalQ(clus, {
  library(inlabru) 
  library(INLA)
  library(stars)
})

run_preds <- function(inla_run,pred_dat){
  pred_test <- predict(
    inla_run,
    pred_dat, 
    re.form = NA,
    ~ 1/(1+exp(-(alpha+elev+eps+year+clim+veg+effort))))
  
  template <- read_stars("~/BETH/BUOW_dem_nb.tiff")
  template[[1]][] = NA
  
  raster_out <- st_rasterize(pred_test["mean"],template = template)
  
  #write_stars(raster_ssp2,"lewo_ssp2_2100.tif")
}

raster_out <- parLapply(
  cl = clus, 
  X = list(test[[1]], test[[2]]),
  fun = run_preds,
  inla_run = inla_run_lewo
)
#test for correlation
#cor(beth_clim_dat[,c(5:29,380)])
#which(abs(cor(beth_clim_dat[,c(5:29)])[,1]) < 0.6)
#elev cor w/ AHM, bFFP, DD.0, DD.18, MAP, MSP
#AHM cor w/ CMD, DD1040, DD5, eFFP,EMT, Eref, EXT, FFP, MAT, MCMT,MWMT,NFFD,SHM
#not correlated: slope, elev, PAS, RH, TD
# PAS: precip as snow - doesn't makes sense here
# RH: relative humidity
# TD: temp diff between warmest and coldest month

beth_short <- read.csv("beth_short.csv")
#import data from pts_pt in "run.R"
#buow_short <- st_read("buow_short.shp")
#lewo_short <- st_read("lewo_short.shp")

#function requires shape file above and the short name
# of the species

data_transform <- function(dat_short,species_name){
  
  dat_short_tmp <- st_as_sf(dat_short,coords = c("POINT_X","POINT_Y"))
  st_crs(dat_short_tmp) <- st_crs("EPSG:5070")
  dat_short2 <- st_transform(dat_short_tmp, crs = st_crs("EPSG:4326"))
  states = st_as_sf(maps::map("state",plot=FALSE, fill=TRUE))
  states = st_transform(states, crs = st_crs("EPSG:4326"))
  
  #states is not valid
  #st_is_valid(states)
  sf_use_s2(FALSE)
  states = st_make_valid(states)
  dat_short_state = st_join(dat_short2,states)
  
  dat_out <- dat_short_state |> 
    select(year, effort, obs, ID, geom) |> 
    add_column(species = species_name)
  
  return(dat_out = dat_out)
  
}

beth_dat <- data_transform(beth_short,"beth")
buow_dat <- data_transform(pts_pt_buow,"buow")
lewo_dat <- data_transform(pts_pt_lewo,"lewo")

adapt_pts <- bind_rows(beth_dat, buow_dat, lewo_dat)

adapt_pts2 <- adapt_pts |> 
  filter(ID %in% c("nevada","utah","california","arizona","colorado","new mexico","texas","oklahoma"))

st_write(adapt_pts2,"dat_short.shp",layer="pts_out")

