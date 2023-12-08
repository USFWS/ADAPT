##spatial INLA model
# prior for the range is currently set such that the prob
# of the range exceeding 500 km is 0.5
make_matern <- function(spatial_field,
                        prior.range.mu){ 
  
  matern <- inla.spde2.pcmatern(spatial_field, 
                                prior.range=c(prior.range.mu, 0.01), #was c(10000,.5)
                                prior.sigma=c(1, 0.01))
  
  return(matern)
}
