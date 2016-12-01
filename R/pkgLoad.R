#' activate the "install.packages" function, only if the package is not already installed,
#' then load that package
#'
#'
#' @param packages A vector of packages to be checked or installed.
#' Defaults to a list of my favourite packages, all worth having.
#' @param quietly TRUE or FALSE, should be suppress helper outputs?
#'
#' @keywords packages
#' @export

pkgLoad <- function( packages = "favourites", quietly = TRUE ) {

  if( length( packages ) == 1L && packages == "favourites" ) {
    packages <- c( "data.table", "chron", "plyr", "dplyr", "shiny",
                   "shinyjs", "parallel", "devtools", "doMC", "utils",
                   "stats", "microbenchmark", "ggplot2", "readxl",
                   "feather", "googlesheets", "readr", "DT", "knitr",
                   "rmarkdown", "Rcpp"
    )
  }

  packagecheck <- match( packages, utils::installed.packages()[,1] )

  packagestoinstall <- packages[ is.na( packagecheck ) ]

  if( length( packagestoinstall ) > 0L ) {
    utils::install.packages( packagestoinstall,
                             repos = "http://cran.csiro.au"
    )
  } else if( !quietly ) {
    print( "All requested packages already installed" )
  }

  for( package in packages ) {
    if( !quietly ) {
      print( paste( "Installing", package ) )
    }
    suppressPackageStartupMessages(
      library( package, character.only = TRUE, quietly = TRUE )
    )
  }

}
