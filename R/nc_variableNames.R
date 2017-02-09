#' nc_variableNames
#' a simple function for viewing the names of the variables within a .nc file
#'
#' @param file character string linking to .nc file
#' @return a character vector of variable names
#'
#' @import ncdf4
#' @import magrittr
#' @export
#'

nc_variableNames <- function( file ) {

    file %>%
        ncdf4::nc_open( filename = . ) %>%
        sapply( names ) %>%
        unlist() %>%
        unname() %>%
        subset( . != "" )

}
