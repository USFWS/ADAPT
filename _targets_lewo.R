# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(crew)

# Set target options:
# packages that your targets need to run
tar_option_set(
  packages = c("inlabru","INLA","sf","ggplot2","maps",
               "tidyverse","stars","crew",
               "lubridate","viridis","janitor"),
  controller = crew_controller_local(workers = 5)
  )

# Run the R scripts in the R/ folder with your custom functions:
#tar_source("R/functions.R")
tar_source("R/function_get_data.R")
tar_source("R/function_get_grid.R")
tar_source("R/function_make_clim_covs.R")
tar_source("R/function_make_cur_dat.R")
tar_source("R/function_reduce_dat.R")
tar_source("R/function_build_spatial.R")
tar_source("R/function_make_matern.R")
tar_source("R/function_inla_func.R")
tar_source("R/function_make_preds.R")

list(
  tar_target(study_grid,"~/BETH/BUOW_pointgridALL.gpkg", 
             format = "file"),
  tar_target(count_dat,"~/eBird_Data/lewo_ebird_data.csv",
             format = "file"),
  tar_target(count_dat_out, get_data(count_dat)),
  tar_target(grid_buff, get_grid(study_grid)),
  tar_target(clim_covs, make_clim_covs(count_dat_out,grid_buff)),
  tar_target(e_buff, make_current_dat(clim_covs,grid_buff,count_dat_out)),
  tar_target(pts_pt_lewo, reduce_dat(e_buff)),
  tar_target(name = spatial_field, 
             command = build_spatial(pts_pt = pts_pt_lewo,
                                     max.edge = c(10000,500000),
                                     cutoff = 5000)),
  tar_target(name = matern,
             command = make_matern(spatial_field, 
                                   prior.range.mu = 50000)),
  
  tar_target(name = inla_run_lewo,
             command = inla_func(
               vars = 'alpha(geom, model = matern) + 
                        elev(elev_scale, model = "linear") +
                        eps(id, model = "iid", constr = TRUE, hyper = list(prec = pc_prec)) +
                        year(year_scale, model = "linear") +
                        clim(geom, weights = rh, model = matern) + 
                        veg(LSSP22020, model = "factor_full") +
                        effort(log_effort, model = "linear")',
               pts_pt = pts_pt_lewo,
               matern = matern,
               global = TRUE)
                                   ),
  tar_target(name = preds,
             command = make_preds(
               inla_run_lewo, clim_covs, grid_buff, e_buff, 
               list_preds = list())
             ))
  



