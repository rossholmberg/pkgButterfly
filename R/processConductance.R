#'
#' processConductance convert files output from circuitscape processing into a data table
#'
#' @param conductance.file file to be processed
#' @param land.mask
#' @param split.quant
#' @param file.identifier
#' @param write.file
#' @param return.dt
#' @param return.data.vector
#'
#' @keywords conductance, ocean currents
#'
#' @export
#'
#' @return none


processConductance <-  function( conductance.file,
                                 land.mask,
                                 split.quant = 0.75,
                                 file.identifier = "_dynamic",
                                 write.file = TRUE,
                                 return.dt = FALSE,
                                 return.data.vector = FALSE ){

  library( data.table )

  header <- readLines( conductance.file, n = 7 )
  num.cols <- as.integer( gsub( "ncols| ", "", header[ grep( "ncols", header ) ] ) )
  num.rows <- as.integer( gsub( "nrows| ", "", header[ grep( "nrows", header ) ] ) )
  x.lower.left <- as.numeric( gsub( "xllcorner| ", "", header[ grep( "xllcorner", header ) ] ) )
  y.lower.left <- as.numeric( gsub( "yllcorner| ", "", header[ grep( "yllcorner", header ) ] ) )
  cell.size <- as.numeric( gsub( "cellsize| ", "", header[ grep( "cellsize", header ) ] ) )
  noDataValue <- as.numeric( gsub( "NODATA_value| ", "", header[ grep( "NODATA_value", header ) ] ) )

  data <- readr::read_delim( conductance.file,
                             delim = " ",
                             trim_ws = TRUE,
                             skip = 6,
                             col_names = FALSE,
                             na = c( "", "NA", noDataValue )
  )

  # for some reason, the last column sometimes comes up as all NA values, if so, ditch it
  if( sum( is.na( data[,ncol(data)] ) ) > nrow( data ) * .75 ) {
    data[,ncol(data)] <- NULL
  }

  # turn this data into a vector, ready to fill a column of the dataframe
  datavec <- as.numeric( as.vector( do.call( cbind, data ) ) )

  if( return.data.vector ) {
    datavec[ land.mask$mask == 1 ] <- NA
    datavec[ datavec == noDataValue ] <- NA
    # split.point <- as.numeric(
    #     stats::quantile( datavec, probs = split.quant, na.rm = TRUE )
    # )
    # output <- vector( mode = "numeric", length = length( datavec ) )
    # output <- rep( as.numeric( NA ), length( datavec ) )
    # output[ datavec >= split.point ] <- 1L
    # output[ datavec < split.point ] <- NA
    return( datavec )
  }

  latlist <- seq( from = y.lower.left,
                  to = y.lower.left + ( num.rows - 1 ) * cell.size,
                  by = cell.size )
  lonlist <- seq( from = x.lower.left,
                  to = x.lower.left + ( num.cols - 1 ) * cell.size,
                  by = cell.size )
  grid <- data.table::data.table(
    lat = rep( sort( latlist, decreasing = TRUE ), length( lonlist ) ),
    lon = sort( rep( lonlist, length( latlist ) ) ),
    data = datavec
  )

  # bring in the land mask, making sure it's ordered the same as grid
  data.table::setorder( land.mask, lon, -lat )
  data.table::setorder( grid, lon, -lat )
  grid[ , land.mask := land.mask$mask ]

  # mask out the land
  grid[ land.mask == 0L, data.masked := data ]

  # find a point upon which to split the data
  split.point <- as.numeric(
    stats::quantile( grid$data.masked, probs = split.quant, na.rm = TRUE )
  )

  # mark points within the upper range of conductance values
  grid[ !is.na( data.masked ) & data.masked >= split.point, reclass.data := 1L ]


  if( write.file ) {
    # and write out the ascii file
    df2Ascii(
      lat = grid[,lat], lon = grid[,lon], data = grid[,reclass.data],
      file = gsub( "\\.asc", paste0( file.identifier, ".asc" ), conductance.file ),
      noDataValue = noDataValue
    )

  }

  if( return.dt ) {
    return( grid[ , .( lat, lon, reclass.data ) ] )
  }



}
