#' meanChlorophyll Calculating the relevant metric for a single time
#'
#' @param date date for which to calculate the chlorophyll level
#' @param data.chlorophyll data extracted from .nc file
#' @param data.conductance data calculated using `costCalcMaster`, based on currents
#' @param max.day.diff integer, number of days to allow as a difference between a conductance array
#' and the corresponding data array
#'
#' @import data.table
#'
#' @export
#'
#' @return numeric value for mean chlorophyll
#'

meanChlorophyll <- function( date,
                             data.chlorophyll,
                             data.conductance,
                             max.day.diff = 31 ) {

  lat <- lon <- cond.lat <- cond.lon <- conductance <- NULL

  date <- as.character( date )
  chlorophyll.thisdate <- data.chlorophyll[ , .( lat, lon ) ]
  chlorophyll.thisdate[ , data := data.chlorophyll[, get( date ) ] ]
  setDT( data.conductance )
  conductance.thisdate <- data.conductance[ , .( lat, lon ) ]

  # get a list of dates available for conductance data
  conductance.dates <- names( data.conductance )
  conductance.dates <- conductance.dates[ -c(1,2) ]

  # measure the difference in days from each conductance datapoint to the date we want
  date.diff <- abs( as.integer( as.Date( conductance.dates ) - as.Date( date ) ) )

  # if there are no conductance datapoints close to the date we want, quit now
  if( min( date.diff ) > as.integer(max.day.diff ) ) {
    print( paste( "No data found within", max.day.diff, "days of", date, ". Returning NA." ) )
    return( as.numeric( NA ) )
    break
  }

  # find the nearest appropriate date to use for conductance
  date.to.use <- conductance.dates[ which.min( date.diff ) ]
  conductance.thisdate[ , conductance := data.conductance[ , get( date.to.use ) ] ]

  # because we know the conductance values are gridwise...
  conductance.cell.size <-
    ( max( conductance.thisdate[,lat], na.rm = TRUE ) -
        min( conductance.thisdate[,lat], na.rm = TRUE ) ) /
    ( length( unique( conductance.thisdate[,lat] ) ) - 1 )

  chlorophyll.thisdate[ , cond.lat := round( lat / conductance.cell.size ) * conductance.cell.size ]
  chlorophyll.thisdate[ , cond.lon := round( lon / conductance.cell.size ) * conductance.cell.size ]

  # now merge the two tables
  setkey( conductance.thisdate, lat, lon )
  setkey( chlorophyll.thisdate, cond.lat, cond.lon )
  chlorophyll.thisdate <- conductance.thisdate[ chlorophyll.thisdate ]

  # and extract a mean chlorophyll value for the specified date
  mean.chlorophyll <- mean(
    chlorophyll.thisdate[ !is.na( conductance ), data ],
    na.rm = TRUE
  )

  return( mean.chlorophyll )
}