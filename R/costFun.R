#' Calculate a "cost sink" based on direction, speed, and angle.
#' Based on Afán et al. 2015
#'
#'
#' @param water.direction A numeric array expressing wind directions.
#' @param water.speed An array with dimensions matching that of water.direction
#' @param angle.sink A matrix of angles calculated using angleSink.
#' @keywords cost wind sink
#' @importFrom circular rad
#' @importFrom plyr laply
#' @importFrom plyr alply
#' @export
#' @return A numeric array, with dimensions matching water.direction and water.speed


costFun <- function( water.direction, water.speed, angle.sink ) {

    # ensure all array dimensions match correctly before proceeding
    stopifnot(
        identical( dim( water.direction ), dim( water.speed ) )
    )

    # try to determine which dimensions represent the ones we're after.
    if( identical( dim( water.direction )[ 1:2 ], dim( angle.sink ) ) ) {
        dir.dims <- c( 1L, 2L )
        seq.dim <- 3L
    } else if( identical( dim( water.direction )[ 2:3 ], dim( angle.sink ) ) ) {
        dir.dims <- c( 2L, 3L )
        sequence <- seq_along( water.direction[,1,1] )
        seq.dim <- 1L
    } else if( identical( dim( water.direction )[ 3:1 ], dim( angle.sink ) ) ) {
        dir.dims <- c( 3L, 1L )
        sequence <- seq_along( water.direction[1,,1] )
        seq.dim <- 2L
    } else {
        stop( "Could not find appropriate dimensions to use in currents array." )
    }

    betta <- plyr::laply(
        .data = plyr::alply( water.direction, .margins = seq.dim, as.array ),
        .fun = function( x, angle.sink ) {
            abs( circular::rad( x ) - circular::rad( angle.sink ) )
        },
        angle.sink = angle.sink
    )

    cost.sink <- ( 0.6184 * betta - 0.0984 * betta ^ 2 ) * water.speed

    cost.sink <- plyr::alply( .data = cost.sink, .margins = seq.dim, as.array )

    return( cost.sink )

}
