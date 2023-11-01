library(INLA)

tar_manifest(fields = all_of("command"))
tar_visnetwork()

tar_make()
tar_load(e_buff)

# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint
tar_load(spatial_field)
plot(spatial_field)

tar_load(inla_run)
summary(inla_run[[1]])
