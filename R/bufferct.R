#' Create dissolved buffers around points with point count attribute
#'
#' @param sf_pts An `sf` point object to buffer.
#' @param bufsize Size of buffer.
#'
#' @return An `sf` polygon object.
#' @export
#'
#' @examples
bufferct <- function(sf_pts, bufsize){
  buf <- st_buffer(sf_pts, bufsize)                   # buffer points
  buf_valid <- st_union(buf) %>%
    st_make_valid()                                # fix problems
  buf_valid <- buf_valid[!st_is_empty(buf_valid),  # drop empty
                         drop=FALSE]

  buf_sf <- st_cast(buf_valid, "POLYGON") %>%      # split into polygons
    st_as_sf()                                     # convert to sf

  # add counts of points within buffers
  buf_sf$count <- sapply(st_intersects(buf_sf, sf_pts), length)

  if(nrow(sf_pts) != sum(buf_sf$count)){
    "Buffer point count doesn't match number of rows of input point sf"
  }

  return(buf_sf)
}
