#' Disperse points
#'
#' @param sf_pts An `sf` point object to disperse.
#' @param space_dist Maximum spacing threshold of points within `sf_pts` to disperse. Under this threshold, neighboring points will be considered a cluster to be dispersed. Units are projecction units of `sf_pts`.
#'
#' @return An `sf` point object.
#' @export
#'
#' @examples
disperse_pts <- function(sf_pts, space_dist=50000){
  dist_mat <- st_distance(sf_pts) %>% unclass()
  neighb <- apply(dist_mat < space_dist, 1, sum)

  buf_disp <- bufferct(sf_pts, space_dist*mincircle(neighb))

  # do hexagonal sampling. It seems like exact =TRUE doesn't work
  # so instead, add 1 to size and repeat until get exact number
  sf_pts_disp <- buf_disp %>%
    st_sample(size = .$count, type = 'hexagonal', exact=TRUE)

  ct_dif <- sapply(st_intersects(buf_disp, sf_pts_disp), length) - buf_disp$count
  iterate_size <- 0
  while(any(ct_dif<0)){
    iterate_size <- iterate_size + 1
    sf_pts_disp <- buf_disp %>%
      st_sample(size = .$count + iterate_size, type = 'hexagonal', exact=TRUE)

    ct_dif <- sapply(st_intersects(buf_disp, sf_pts_disp), length) - buf_disp$count
  }

  # calculate distance from original points
  dist_orig <- st_distance(sf_pts, sf_pts_disp) %>% unclass()

  # iterate through the original points finding the closest dispersed point
  closest_disp <- c()  # vector to store index of closest dispersed point
  pts_disp_left <- 1:ncol(dist_orig) # keeps track of unselected points
  for(i in 1:nrow(dist_orig)){
    dist_red <- dist_orig[i,pts_disp_left]  # this distance matrix gets shrunk down
    which_min <- which(dist_red == min(dist_red)) # which, among left, is min

    closest_disp <- append(closest_disp, pts_disp_left[which_min]) # save min in vector
    pts_disp_left <- pts_disp_left[-which_min]                     # remove min from indexes left
  }

  # bind the original point attributes to the new dispersed points
  final <- sf_pts_disp[closest_disp] %>% st_as_sf() %>%
    bind_cols(sf_pts %>% st_drop_geometry())

  return(final)
}
