make_clim_covs <- function(count_dat_out,grid_buff){

ann_cov_out <- grid_buff |> 
  select(starts_with("RH.2"),id) |> 
  clean_names()  |> 
  pivot_longer(
    cols = starts_with("rh"),
    names_to = "year",
    values_to = "RH"
  ) |> 
  mutate(
    year = parse_number(year)
  ) 

cov_future <- grid_buff |> 
  select(starts_with("RH.s"),id) |> 
  clean_names() |>  
  pivot_longer(
    cols = starts_with("rh"),
    names_to = c("char","ssp","start_year","year"),
    names_sep = "_",
    values_to = "RH"
  ) |> 
  filter(
    ssp %in% c("ssp245", "ssp585") & year %in% c(2060,2100)
  ) |> 
  select(!char & !start_year)

cov_future$year <- as.numeric(cov_future$year)
ann_cov_out$ssp <- NA

clim_covs <- ann_cov_out |> 
  add_row(cov_future) 

clim_covs$RH_scale <- as.numeric(scale(clim_covs$RH))

return(clim_covs)

}

