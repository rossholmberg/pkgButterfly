#' Calculate a "cost sink" based on direction, speed, and angle.
#' Based on Afán et al. 2015
#'
#'
#' @param water.direction A numeric array expressing wind directions.
#' @param water.speed An array with dimensions matching that of water.direction
#' @param angle.sink A matrix of angles calculated using angleSink.
#' @keywords cost wind sink
#' @export
#' @return A numeric array, with dimensions matching water.direction and water.speed


costFun <- function( water.direction, water.speed, angle.sink ) {

  # ensure all array dimensions match correctly before proceeding
  stopifnot(
    identical( dim( water.direction ), dim( water.speed ) ),
    identical( dim( water.direction )[ 1:2 ], dim( angle.sink ) )
  )

  betta <- base::mapply(
    FUN = function( x ) {
      abs( rad( water.direction[,,x] ) - rad( angle.sink ) )
    },
    x = seq_along( water.direction[1,1,] ),
    SIMPLIFY = T
  )

  betta <- array( betta, dim = dim( water.direction ) )

  cost.sink <- ( 0.6184 * betta - 0.0984 * betta ^ 2 ) * water.speed

  return( cost.sink )

}
