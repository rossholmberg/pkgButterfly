
#' df2Ascii function to convert the columns of a data frame to Ascii files, ready for further processing outside of R
#'
#' @param lat vector or matrix of latitude points
#' @param lon vector or matrix of longitude points
#' @param data vector or matrix, with dimensions matching lat and lon
#' @param file path to write output
#' @param noDataValue value to be used in Ascii file as representing a "no data" point
#'
#' @import data.table
#'
#' @return data.frame
#'
#' @export
#'


df2Ascii <- function( lat, lon, data, file, noDataValue = -9999 ) {

  library( data.table )

  gridlat <- unique( lat )
  gridlon <- unique( lon )

  x.lower.left <- min( lon )
  y.lower.left <- min( lat )

  cell.size <- round( ( max( gridlon ) - min( gridlon ) ) / length( gridlon ),
                      2 )

  dt <- data.table( lat = lat, lon = lon, data = data )

  # make sure any NA values are represented with the `noDataValue`
  dt[ is.na( data ), data := noDataValue ]

  setorder( dt, lon, -lat )

  matrix <- matrix( data = as.vector( dt[,data] ),
                    nrow = length( gridlat ),
                    ncol = length( gridlon )
  )

  # write the ascii header as appropriate
  header <- c( paste( "NCOLS", ncol( matrix ) ),
               paste( "NROWS", nrow( matrix ) ),
               paste( "XLLCORNER", x.lower.left ),
               paste( "YLLCORNER", y.lower.left ),
               paste( "CELLSIZE", cell.size ),
               paste( "NODATA_value", noDataValue ) )
  cat( header, file = file, append = FALSE, sep = "\n" )

  # and write the data as appropriate
  write.table( matrix, file = file, append = TRUE,
               sep = "\t", eol = "\n",
               row.names = FALSE, col.names = FALSE
  )

}
