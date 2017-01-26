#'
#' runCircuitscape a function for externally applying the circuitscape algorithms to give data
#'
#' @param i integer, used to iterate over multiple files
#' @param costs.filelist list of ascii files to be processed
#' @param csrun.link path to python file to be used to run circuitscape
#' @param output.folder where to put the processed data
#' @param source.asc.link "source" file for cuircuitscape
#' @param sink.asc.link "sink" file for cuircuitscape
#' @param dates list of dates corresponding to time series datapoints
#' @param python.call specify python call
#'
#' @keywords circuitscape, ascii
#'
#' @export
#'
#' @return none

runCircuitscape <- function( i, costs.filelist, csrun.link, output.folder, source.asc.link, sink.asc.link, dates, python.call = 'python2.7' ) {

  # Make an .ini file
  CS_ini <- paste("[circuitscape options]",
                  "data_type = raster",
                  "scenario = advanced",
                  "write_cur_maps = 1",
                  paste0( "source_file = ", source.asc.link ),
                  paste0( "ground_file = ", sink.asc.link ),
                  paste0( "habitat_file = ", costs.filelist[i] ), # habitat file: cost background. This will change along the time range
                  paste0( c( "output_file =" ), paste( "conductance/cond_", as.character( dates[i], "%Y%m%d" ), ".out" ) ),
                  sep = "\n"
  ) # One conductance surface for each date. Change the path to store your outputs wherever you want

  setwd( output.folder )
  ini.filename <- paste( "cond_", as.character( dates[i], "%Y%m%d" ), ".ini", sep='' )

  # Write it to your working directory
  cat( CS_ini,
       file = ini.filename,
       append = F )

  # Run circuitscape via a python call

  system( paste( 'python2.7', csrun.link, paste0( output.folder, "/", ini.filename ) ),
          intern = TRUE,
          ignore.stdout = TRUE )

}
