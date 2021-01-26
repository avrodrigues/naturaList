#' Define environmental space
#'
#' Based on two environmental variables, it defines a bi-dimensional environmental
#' space.
#'
#' @param env matrix or data frame with two columns representing a bi-dimensional
#'   environmental space.
#' @param buffer.size numeric value indicating a buffer size around each point
#'  which will delimit the environmental space border. See details.
#' @param plot logical. whether to plot the polygon
#'
#' @details The environmental variables are standardized by range, which turns the
#'  range of each variable in 0 to 1. Then, it is delimited a buffer of size equal
#'  to\code{buffer.size} arround each point in this space. A polygon is returned
#'  indication the enviromental space based in the variables.
#'
#' @return sfc_POLYGON
#'
#' @importFrom stats na.omit
#' @export
#'
define_env_space <- function(env,
                             buffer.size,
                             plot = TRUE){

  env.range <- vegan::decostand(na.omit(env), "range")
  env.point <-  sf::st_multipoint(as.matrix(env.range))

  box <- c(0,1,0,1)
  r <- raster::raster(raster::extent(box), resolution = 0.025)
  r.cell <- unique(raster::cellFromXY(r, env.range))
  xy.cell <- raster::xyFromCell(r, r.cell)
  ch.point <-  sf::st_multipoint(as.matrix(xy.cell))
  ch.buffer <- sf::st_buffer(ch.point, buffer.size)

  box2 <- c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
  env.space <- sf::st_crop(sf::st_geometry(ch.buffer), box2)

  if(plot) plot(env.space)
  return(env.space)
}
