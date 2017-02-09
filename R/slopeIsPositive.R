#' slopeIsPositive check whether the slope of a vector is positive
#'
#' @param x a vector
#' @return boolean TRUE if slope is positive, FALSE if negative or zero
#'
#' @export
#'

slopeIsPositive <- function( x ) {

    slope <- coefficients( lm( x ~ seq_along( x ) ) )[2]

    slope <- unname( slope )

    return( slope > 0 )

}
