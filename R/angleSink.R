#' Calculate a sink angle matrix based on the Point of Interest
#' Based on Afán et al. 2015
#'
#'
#' @param POI central reference point (eg: colony location)
#' @param lat latitudes to compare (dimensions must match lon)
#' @param lon longitudes to compare (dimensions must match lat)
#' @keywords cost currents sink
#' @import geosphere
#' @export
#' @return A numeric array, with dimensions matching water.direction and water.speed


angleSink <- function( POI = c( 145.2, -38.8 ), lat, lon ) {

    # we need the latitude and longitude objects to be in matrix form. Convert them if they're not already
    if( length( dim( lat ) ) == 1L && length( dim( lon ) ) == 1L ) {
        lat <- matrix( data = sort( lat, decreasing = TRUE ), nrow = dim( lat ), ncol = dim( lon ) )
        lon <- matrix( data = sort( lon ), nrow = dim( lat ), ncol = dim( lon ), byrow = TRUE )
    }

    # make sure we've got compatible matrices
    stopifnot(
        identical( dim( lat ), dim( lon ) )
    )

    angle.sink <- array( 0, dim = dim( lat ) )

    dimensions <- list( seq_len( dim( angle.sink )[ 1 ] ),
                        seq_len( dim( angle.sink )[ 2 ] )
    )

    # these loops will calculate the angles on a pixel basis
    # so far, this is made for calculating the angles with respect a single pixel within LP foraging area (e.g. centroid). This is not exactly the same we did in Afán et al 2015, but it should be pretty similar
    for( i in dimensions[[ 1 ]] ) {
        for( j in dimensions[[ 2 ]] ){
            angle.sink[ i , j ] <- geosphere::bearing( p1 = POI, p2 = c( lon[ i , j ], lat[ i , j ] ) )
        }
    }

    # rescale sink angles for 0-360 degree range
    angle.sink[ angle.sink <= 0 ] <- angle.sink[ angle.sink <= 0 ] + 360

    return( angle.sink )

}
