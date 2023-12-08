#reduce to just those sites w/ observations (0/1)
reduce_dat <- function(e_buff){  
  pts_pt2 <- e_buff[!is.na(e_buff$obs),]
  pts_pt = st_centroid(pts_pt2)
  
  pts_pt$obs[pts_pt$obs > 0] = 1
  pts_pt$id = factor(pts_pt$id)
  
  return(pts_pt)
}