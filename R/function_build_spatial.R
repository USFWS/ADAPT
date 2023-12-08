
build_spatial <- function(pts_pt,
                          max.edge,
                          cutoff){
  
  boundary <- fm_extensions(pts_pt, 
                            convex = c(100000,200000), 
                            concave = c(50000,100000))
  
  #### create INLA "mesh" over study area ####
  # tried max edge of 25 km, seemed fine (no spatial artifacts)
  # when increased to 100 km, could start to see spatial artifacts
  spatial_field = fm_mesh_2d_inla(boundary = boundary,
                                  max.edge = max.edge, #original: 50000,100000
                                  cutoff = cutoff,  #original: 25000
                                  #offset = c(2000,100000),
                                  crs = fm_crs(pts_pt))
  
  return(spatial_field)
  
}