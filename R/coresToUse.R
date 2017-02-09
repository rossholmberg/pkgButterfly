#' coresToUse, a function for guessing at a good number of cores to use in parallel processing,
#' base on the computer being used
#'
#' @importFrom parallel detectCores
#' @export
#' @return integer value of number of cores to be used

coresToUse <- function() {
    logicalCores <- parallel::detectCores( logical = FALSE )
    virtualCores <- parallel::detectCores( logical = TRUE )
    if( anyNA( c( logicalCores, virtualCores ) ) ) {
        return( 1L )
    } else if( virtualCores > logicalCores ) {
        return( logicalCores )
    } else if( logicalCores > 2 ) {
        return( logicalCores - 1L )
    } else {
        return( 1L )
    }
}
