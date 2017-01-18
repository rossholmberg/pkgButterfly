#' extractFromNc get data from a .nc file
#'
#'
#' @param file .nc file containing chlorophyll levels for the entire area of influence
#' @param data.variable character string corresponding to the relevant data subset within the .nc file (defaults to "chlorophyll")
#' @keywords file import nc
#'
#' @import data.table
#' @import magrittr
#' @import ncdf4
#'
#' @export
#' @return A data.table, containing variables for date and chlorophyll level
#'

extractFromNc <- function( file, data.variable ) {

  con <- ncdf4::nc_open( filename = file )
  data <- ncdf4::ncvar_get( nc = con, varid = data.variable )

  lonlist <- ncdf4::ncvar_get( nc = con, varid = "longitude" ) %>%
    as.vector( mode = "numeric" )
  latlist <- ncdf4::ncvar_get( nc = con, varid = "latitude" ) %>%
    as.vector( mode = "numeric" )

  output <- data.table::data.table(
    lat = rep( sort( latlist, decreasing = TRUE ), length( lonlist ) ),
    lon = sort( rep( lonlist, length( latlist ) ) )
  )

  times <- ncdf4::ncvar_get( nc = con, varid = "time" ) %>%
    as.POSIXct( origin = "1970-01-01", tz = "UTC" ) %>%
    as.Date()

  ncdf4::nc_close( con )

  datadt <- lapply( X = seq_along( data[1,1,] ),
                    FUN = function( x, data ) { as.vector( data[,,x] ) },
                    data = data ) %>%
    do.call( what = cbind ) %>%
    data.table::data.table() %>%
    data.table::setnames( as.character( times ) )

  output <- cbind( output, datadt )

  return( output )

}
