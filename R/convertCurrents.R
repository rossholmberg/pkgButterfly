#' Convert current from .nc file to speed and direction values.
#'
#'
#' @param zonal.current Extracted using ncvar_get
#' @param meridional.current Extracted using ncvar_get
#' @keywords current speed direction
#' @export
#' @return A list containing 2 elements, "direction" and "speed"


convertCurrents <- function( zonal.current, meridional.current ) {

  stopifnot(
    identical( dim( zonal.current ), dim( meridional.current ) )
  )

  # through this loop we will use information on u and v to calculate water speed and direction on a pixel basis and for all available dates
  # water direction. Angles range from -180 a 180
  water.direction <- atan2( zonal.current, meridional.current ) * 45.0 / atan( 1.0 )

  # we rescale wdir to values ranging from 0 to 360
  water.direction <- ifelse( water.direction > 0,
                             water.direction,
                             360 + water.direction
  )

  water.speed <- sqrt( zonal.current^2 + meridional.current^2 )

  return( list( direction = water.direction, speed = water.speed ) )

}
