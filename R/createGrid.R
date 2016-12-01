#'
#' Converting a given area range and cell size into a grid of reference points.
#'
#' @param latRange numeric vector, range of latitudes to be represented
#' @param lonRange numeric vector, range of longitudes to be represented
#' @param cell.size numeric value specifying desired size of each grid point in degrees
#' @param convert.to.spatial TRUE/FALSE, whether to convert the object to a spatial object before returning
#'
#' @keywords GPS, spatial
#'
#' @export
#'
#' @return data.frame
#'
#' @import sp
#'

createGrid <- function( latRange, lonRange, cell.size = 0.1, convert.to.spatial = FALSE ) {

  lat.list <- seq( from = floor( latRange[1] / cell.size ) * cell.size,
                   to = ceiling( latRange[2] / cell.size ) * cell.size,
                   by = cell.size
  )

  lon.list <- seq( from = floor( lonRange[1] / cell.size ) * cell.size,
                   to = ceiling( lonRange[2] / cell.size ) * cell.size,
                   by = cell.size
  )

  lon.list.expanded <- rep( lon.list, length( lat.list ) )
  lat.list.expanded <- sort( rep( lat.list, length( lon.list ) ) )

  grd <- data.frame( lon = lon.list.expanded,
                     lat = lat.list.expanded )


  if( convert.to.spatial ) {
    sp::coordinates( grd ) <- c( "lon", "lat" )
    sp::gridded( grd ) <- TRUE
    sp::proj4string( grd ) <- sp::CRS( "+init=epsg:4326" )
  }

  return( grd )
}
