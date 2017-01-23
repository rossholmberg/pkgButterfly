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

  file.contents <- ncdf4::ncvar_get( nc = con, varid = data.variable )
  ncdf4::nc_close( con )

  output[ , as.character( times ) := lapply( X = seq_along( file.contents[1,1,] ),
                    FUN = function( x ) { as.vector( file.contents[,,x] ) } ) ]

  # clean up
  rm( file.contents )
  gc()

  return( output )

}
