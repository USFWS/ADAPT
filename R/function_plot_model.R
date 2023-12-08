clim_out <- inla_run_buwo$summary.random$clim

# sig_clim <- clim_out |> 
#   mutate(effect = case_when(
#     `0.025quant` > 0 & `0.975quant` > 0 ~ "pos",
#     `0.025quant` < 0 & `0.975quant` < 0 ~ "neg",
#     `0.025quant` < 0 & `0.975quant` > 0 ~ "not_sig"
#     )
#   )
  
template <- read_stars("~/BETH/BUOW_dem_nb.tiff")
template_pts <- st_as_sf(template, as_points=TRUE)

mesh_proj <- fm_evaluator(
  spatial_field,
  loc = template_pts
)

clim <- clim_out |> 
  select(c(median = `0.5quant`,
           sd, 
           upper = `0.975quant`,
           lower = `0.025quant`)
         )
  
pred_grid <- fm_evaluate(mesh_proj,clim)

pred_grid_effect <- data.frame(pred_grid) #|>
  # mutate(effect = case_when(
  #   lower > 0 & upper > 0 ~ "pos",
  #   lower < 0 & upper < 0 ~ "neg",
  #   lower < 0 & upper > 0 ~ "not_sig"
  # ))

template_pts_preds <- template_pts |> 
  bind_cols(pred_grid_effect)

template[[1]][] = NA
clim_rast_med <- st_rasterize(template_pts_preds["median"],template=template)
clim_rast_sd <- st_rasterize(template_pts_preds["sd"],template=template)
clim_rast_lower <- st_rasterize(template_pts_preds["lower"],template=template)
clim_rast_upper <- st_rasterize(template_pts_preds["upper"],template=template)

clim_rast_effect_brick <- c(clim_rast_lower,clim_rast_upper)
clim_rast_effect <- clim_rast_effect_brick |> 
  mutate(effect = case_when(
    is.na(lower) ~ NA,
    lower > 0 & upper > 0 ~ "pos",
    lower < 0 & upper < 0 ~ "neg",
    lower < 0 & upper > 0 ~ "not_sig"
  ))

ggplot() + 
  geom_stars(data = clim_rast_effect,aes(fill=effect))
