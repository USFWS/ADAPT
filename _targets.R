# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
# packages that your targets need to run
tar_option_set(
  packages = c("inlabru","INLA","sf","ggplot2","maps",
               "tidyverse","terra","tidyterra","stars",
               "lubridate","viridis")
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions.R")
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(study_grid,"~/BETH/BUOW_pointgridALL.gpkg", 
             format = "file"),
  tar_target(count_dat,"~/eBird_Data/buow_ebird_data.csv",
             format = "file"),
  tar_target(e_buff, get_data(study_grid,count_dat)),
  tar_target(pts_pt, reduce_dat(e_buff)),
  tar_target(name = spatial_field, 
             command = build_spatial(pts_pt = pts_pt,
                                     max.edge = c(50000,500000),
                                     cutoff = 10000)),
  tar_target(name = matern,
             command = make_matern(spatial_field, prior.range.mu = 1000000)),
  
  tar_target(name = inla_run,
             command = inla_func(vars = c('alpha(geom, model = matern)',
                                          'clim(rh, model = "linear")'),
                                 pts_pt = pts_pt,
                                 matern = matern
                                   ))
  )



