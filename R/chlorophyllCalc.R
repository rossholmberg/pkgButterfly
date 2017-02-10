#' chlorophyllCalc Use the relevant local area of influence (calculated using the butterfly algorithm) to find chlorophyll levels over time
#'
#'
#' @param input.file .nc file containing chlorophyll levels for the entire area of influence
#' @param data.variable character string corresponding to the relevant data subset within the .nc file (defaults to "chlorophyll")
#' @param lat.varname name of latitude variable, automatic searching is used if NULL
#' @param lon.varname name of longitude variable, automatic searching is used if NULL
#' @param timeseries manually extracted timeseries data as "Date" class, to override automatic extraction from file
#' @param time.varname name of time variable, automatic searching is used if NULL
#' @param conductance.data input for calculation
#' @param max.day.diff integer, number of days to allow as a difference between a conductance array
#' @param exclude.nas should NA values be excluded?
#' @param depth.varname name of depth variable in .nc file, auto matching used if NULL
#' @param depth.dimension if a depth dimension is present, specify which dimension it is. auto-matching used if NULL
#' @param depth.touse if a depth dimension is present, specify which value to use. default NULL will use minimum available depth
#' @keywords cost chlorophyll currents
#'
#' @import data.table
#'
#' @export
#' @return A data.table, containing variables for date and chlorophyll level
#'

chlorophyllCalc <- function( input.file,
                             data.variable = "chl",
                             lat.varname = NULL,
                             lon.varname = NULL,
                             timeseries = NULL,
                             time.varname = NULL,
                             conductance.data,
                             max.day.diff = 10,
                             exclude.nas = TRUE,
                             depth.varname = NULL,
                             depth.dimension = NULL,
                             depth.touse = NULL) {

    mean.chlorophyll <- date <- NULL

    variables.available <- nc_variableNames( input.file )

    # find which variable to use
    # search for an exact match first
    if( data.variable %in% variables.available ) {
        # nothing to be done here
        # next try to ignore case
    } else if( tolower( data.variable ) %in% tolower( variables.available ) ) {
        cat( paste0( "Variable name ", data.variable, " was not found, " ) )
        data.variable <- variables.available[ tolower( variables.available ) == tolower( data.variable ) ]
        cat( paste0( data.variable, " used instead.\n" ) )

        # next try regex matching (possibly partial string matching)
        # only accept if there is a single match
    } else if( length( grep( data.variable, variables.available, ignore.case = TRUE ) ) == 1L ) {
        cat( paste0( "Variable name ", data.variable, " was not found, " ) )
        data.variable <- variables.available[ grep( data.variable, variables.available, ignore.case = TRUE ) ]
        cat( paste0( data.variable, " used instead.\n" ) )

        # if nothing has worked so far, give up
    } else {
        stop( paste0( "No appropriate match for data variable ", data.variable, " could be found.\n" ) )
    }



    data.input <- extractFromNc(
        file = input.file,
        data.variable = data.variable,
        lat.varname = lat.varname,
        lon.varname = lon.varname,
        time.varname = time.varname,
        timeseries = timeseries,
        depth.varname = depth.varname,
        depth.dimension = depth.dimension,
        depth.touse = depth.touse
    )
    gc()

    dates.data <- data.table::data.table(
        date = names( data.input )[ 3 : ncol( data.input ) ]
    )

    dates.data[ , mean.chlorophyll := meanChlorophyll( date = date,
                                                       data.chlorophyll = data.input,
                                                       data.conductance = conductance.data,
                                                       max.day.diff = max.day.diff ),
                by = date ]

    rm( data.input )
    gc()

    if( exclude.nas ) {
        dates.data <- dates.data[ !is.na( mean.chlorophyll ), ]
    }

    # convert dates to date format before returning to the user
    # dates.data[ , date := as.Date( date ) ]

    return( dates.data )

}
