#' plotButterfly View a "butterfly"
#'
#'
#' @param conductance.table as output from costCalcMaster
#' @param which either an integer or a date (character string), which date to plot
#' @keywords plot view
#'
#' @import magrittr
#' @importFrom fields image.plot
#'
#' @export
#' @return TRUE (silently)
#'

plotButterfly <- function( conductance.table, which = 1 ) {

  if( class( which ) == "character" ) {
    which <- which( names( conductance.table ) == which )
  } else {
    which <- which + 2L
  }

  rows <- sum( conductance.table[['lon']] == conductance.table[['lon']][1] )

  conductance.table[[ which ]] %>%
    matrix( nrow = rows ) %>%
    apply( 2, rev ) %>%
    t() %>%
    fields::image.plot()

  return( invisible( TRUE ) )

}

