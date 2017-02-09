#' matrixRotate
#'
#' @param x a matrix
#' @param angle numeric, multiple of 90, positive (clockwise) or negative (anti-clockwise)
#'
#' @export

matrixRotate <- function( x, angle = c( 0, 90, 180, 270 ) ) {

    # convert angle to numeric, in case it was passed as character
    angle <- as.numeric( angle )

    # if the input angle is out of the -360 to 360 range, bring it back
    while( angle >= 360 ) {
        angle <- angle - 360
    }
    while( angle < 0 ) {
        angle <- angle + 360
    }

    # make sure angle is a multiple of 90 degrees
    if( !angle %in% seq.int( from = 0, to = 360, by = 90 ) ) {
        stop( "angle must be zero, or a multiple of 90." )
    }

    # don't do anything if we don't need to
    if( angle == 0 ) {
        return( x )
    }

    if( angle == 90 ) {
        return( t( apply( x, 2, rev ) ) )
    }

    if( angle == 180 ) {
        return( matrix( data = rev( x ), nrow = nrow( x ) ) )
    }

    if( angle == 270 ) {
        return( apply( t( x ), 2, rev ) )
    }

}
