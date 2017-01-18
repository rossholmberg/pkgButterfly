#' chlorophyllCalc Use the relevant local area of influence (calculated using the butterfly algorithm) to find chlorophyll levels over time
#'
#'
#' @param input.file .nc file containing chlorophyll levels for the entire area of influence
#' @param data.variable character string corresponding to the relevant data subset within the .nc file (defaults to "chlorophyll")
#' @param conductance.data input for calculation
#' @param max.day.diff integer, number of days to allow as a difference between a conductance array
#' @param exclude.nas should NA values be excluded?
#' @keywords cost chlorophyll currents
#'
#' @import data.table
#'
#' @export
#' @return A data.table, containing variables for date and chlorophyll level
#'

chlorophyllCalc <- function( input.file,
                             data.variable = "chlorophyll",
                             conductance.data,
                             max.day.diff = 31,
                             exclude.nas = TRUE ) {

  mean.chlorophyll <- date <- NULL

  data.input <- extractFromNc(
    file = input.file, data.variable = data.variable
  )

  dates.data <- data.table::data.table(
    date = names( data.input )[ 3 : ncol( data.input ) ]
  )

  dates.data[ , mean.chlorophyll := meanChlorophyll( date = date,
                                                     data.chlorophyll = data.input,
                                                     data.conductance = conductance.data,
                                                     max.day.diff = max.day.diff ),
              by = date ]

  if( exclude.nas ) {
    dates.data <- dates.data[ !is.na( mean.chlorophyll ), ]
  }

  # convert dates to date format before returning to the user
  dates.data[ , date := as.Date( date ) ]

  return( dates.data )

}
