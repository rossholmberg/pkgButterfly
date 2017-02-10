#' extractFromNc get data from a .nc file
#'
#'
#' @param file .nc file containing chlorophyll levels for the entire area of influence
#' @param data.variable character string corresponding to the relevant data subset within the .nc file (defaults to "chlorophyll")
#' @param lat.varname if automatic searching doesn't work, specify a particular variable name here
#' @param lon.varname if automatic searching doesn't work, specify a particular variable name here
#' @param timeseries manually extracted timeseries data as "Date" class, to override automatic extraction from file
#' @param time.varname if automatic searching doesn't work, specify a particular variable name here
#' @param depth.varname name of depth variable in .nc file, auto matching used if NULL
#' @param depth.dimension if a depth dimension is present, specify which dimension it is. auto-matching used if NULL
#' @param depth.touse if a depth dimension is present, specify which value to use. default NULL will use minimum available depth
#' @keywords file import nc
#'
#' @import data.table
#' @import magrittr
#' @import ncdf4
#'
#' @export
#' @return A data.table, containing variables for date and chlorophyll level
#'

extractFromNc <- function( file,
                           data.variable,
                           lat.varname = NULL,
                           lon.varname = NULL,
                           timeseries = NULL,
                           time.varname = NULL,
                           depth.varname = NULL,
                           depth.dimension = NULL,
                           depth.touse = NULL ) {

    con <- ncdf4::nc_open( filename = file )

    variables.available <- nc_variableNames( file )

    if( is.null( lat.varname ) || is.na( lat.varname ) ) {
        if( length( grep( "latitude", variables.available, ignore.case = TRUE ) ) == 1L ) {
            lat.varname <- variables.available[ grep( "latitude", variables.available, ignore.case = TRUE ) ]
        } else if( length( grep( "^lat", variables.available, ignore.case = TRUE ) ) == 1L ) {
            lat.varname <- variables.available[ grep( "^lat", variables.available, ignore.case = TRUE ) ]
        } else {
            stop( "An appropriate latitude variable name could not be found. Please specify one in the function call." )
        }
    }

    if( is.null( lon.varname ) || is.na( lon.varname ) ) {
        if( length( grep( "longitude", variables.available, ignore.case = TRUE ) ) == 1L ) {
            lon.varname <- variables.available[ grep( "longitude", variables.available, ignore.case = TRUE ) ]
        } else if( length( grep( "^lon", variables.available, ignore.case = TRUE ) ) == 1L ) {
            lon.varname <- variables.available[ grep( "^lon", variables.available, ignore.case = TRUE ) ]
        } else {
            stop( "An appropriate longitude variable name could not be found. Please specify one in the function call." )
        }
    }

    if( is.null( timeseries ) && ( is.null( time.varname ) || is.na( time.varname ) ) ) {
        if( length( grep( "time", variables.available, ignore.case = TRUE ) ) == 1L ) {
            time.varname <- variables.available[ grep( "time", variables.available, ignore.case = TRUE ) ]
        } else if( length( grep( "time|date", variables.available, ignore.case = TRUE ) ) == 1L ) {
            time.varname <- variables.available[ grep( "time|date", variables.available, ignore.case = TRUE ) ]
        } else {
            stop( "An appropriate time variable name could not be found. Please specify one in the function call." )
        }
    }

    if( is.null( depth.varname ) || is.na( depth.varname ) ) {
        if( "depth" %in% tolower( variables.available ) ) {
            depth.varname <- "depth"
        } else if( length( grep( "depth", variables.available, ignore.case = TRUE ) ) == 1L ) {
            depth.varname <- variables.available[ grep( "depth", variables.available, ignore.case = TRUE ) ]
        } else {
            depth.varname <- NULL
        }
        # don't stop in this case, since there may not be a depth variable at all
    }


    lon <- ncdf4::ncvar_get( nc = con, varid = lon.varname )
    lat <- ncdf4::ncvar_get( nc = con, varid = lat.varname )

    if( is.null( timeseries ) ) {
        times <- ncdf4::ncvar_get( nc = con, varid = time.varname )
        times <- as.Date( times )
    } else {
        times <- timeseries
        if( class( times ) != "Date" ) {
            times <- as.Date( times )
        }
    }


    file.contents <- ncdf4::ncvar_get( nc = con, varid = data.variable )

    # only retrieve depth data if we have a variable name to use for it
    if( !is.null( depth.varname ) && !is.na( depth.varname ) ) {
        depths <- ncdf4::ncvar_get( nc = con, varid = depth.varname )
        if( is.null( depth.touse ) || is.na( depth.touse ) ) {
            depth.touse <- min( depths )
        }
    }

    # close the input file
    ncdf4::nc_close( con )
    gc()

    # if there's a depth dimension, we need to select only one depth value to analyse here
    if( length( dim( file.contents ) ) > 3L ) {
        # try to find which dimension represents depths by matching dimensions
        if( is.null( depth.dimension ) || is.na( depth.dimension ) ) {
            depth.dimension <- which( dim( file.contents ) == dim( depths ) )
            # we need to stop here if there's more than one match, since we might match the wrong thing
        }
        if( length( depth.dimension ) > 1L ) {
            stop( "Could not confirm which dimension in the data represents depths. Please specify depth.dimension" )
        }
        file.contents <- plyr::alply( file.contents, .margins = 3, .fun = as.array )[[ which( depths == depth.touse ) ]]
    }

    time.dimension <- which( dim( file.contents ) == length( times ) )
    if( length( time.dimension ) != 1L ) {
        stop( "Unable to isolate which dimension of the array represents which spatial dimension (lat, lon)." )
    }

    # now turn the array into a list of matrices, separated along the time dimension
    # this should leave us with a spatial matrix for each time datapoint
    file.contents <- plyr::alply( file.contents, .margins = time.dimension, as.matrix )

    # we need to carefully find the most appropriate way to convert the data into a data frame,
    # to do this, we *may* need to convert the spatial objects "lat" and "lon" to matrices.
    if( length( dim( lat ) ) == 1L ) {
        # now isolate which dimension represents which spatial dimension
        lat.dimension <- which( dim( file.contents[[1]] ) == dim( lat ) )
        lon.dimension <- which( dim( file.contents[[1]] ) == dim( lon ) )
        if( length( lon.dimension ) != 1L || length( lat.dimension ) != 1L ) {
            stop( "Unable to isolate which dimension of the array represents which spatial dimension (lat, lon)." )
        }
        if( lat.dimension == 1L ) {
            lat <- rep_len( lat, length( file.contents[[1]] ) )
            lon <- sort( rep_len( lon, length( file.contents[[1]] ) ), decreasing = FALSE )
        } else if( lon.dimension == 1L ) {
            lat <- sort( rep_len( lat, length( file.contents[[1]] ) ), decreasing = FALSE )
            lon <- rep_len( lon, length( file.contents[[1]] ) )
        }
    }

    # put the spatial coordinates into a data table
    output <- data.table( lat = lat, lon = lon )

    # and add the data column-wise
    output[ , as.character( times ) := lapply( file.contents, as.vector ) ]

    # reorder the data into a logical series
    setorder( output, lon, -lat )

    # clean up
    rm( file.contents )
    gc()

    return( output )

}
