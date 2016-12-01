#'
#'
#' This function takes a datafile of ocean currents, and uses it to process a dynamic area of influence.
#' The process used is as per Afán et al 2015
#'
#' @param currents.file complete link to .nc data file containing ocean currents data
#' @param dates.range range of dates to subset from input data. If NA, all dates in input file are used.
#' @param output.folder complete link to a folder to use for temporary files during processing.
#' A temporary folder will be created within this specified folder, and deleted when computation has completed.
#' @param POI c( [longitude], [latitude] ) marking the central point (eg: colony)
#' @param foraging.distance A distance in km to consider as the area of interest from the POI.
#' @param cell.size Resolution (in degrees) to create for output. Note data will be interpolated from input.
#' @param split.quant The point at which to split for areas of influence (0.75 takes the TOP 25% influence), as per Afán et al 2015
#' @param csrun.link complete link to `csrun.py` file, included as part of the CircuitScape package.
#' @param parallel FALSE will not use multi-threading, TRUE will guess at an optimal number of cores based on processor,
#' integer value specifies a number of cores to use when multi-threading tasks.
#'
#' @keywords currents, chlorophyll, ocean, circuitscape
#'
#' @export
#'
#' @import navto
#' @import coresToUse
#' @import data.table
#' @import doParallel
#' @import foreach
#' @import plyr
#' @import convertCurrents
#' @import createGrid
#' @import angleSink
#' @import costFun
#' @import landMask
#' @import magrittr
#' @import ncdf4
#'



costCalc <- function( currents.file,
                            dates.range = NA,
                            buffer.days = 10L,
                            output.folder,
                            POI,
                            foraging.distance,
                            cell.size = 0.1,
                            split.quant = 0.75,
                            csrun.link,
                            parallel ) {

  print( "Running pre-flight checks and setup." )

  # create a dummy variable to keep the software happy
  . <- NULL

  # set up a temporary folder for holding files
  navto( output.folder )
  if( !dir.exists( "butterflyTemp" ) ) {
    dir.create( "butterflyTemp" )
    setwd( "butterflyTemp" )
    output.folder <- getwd()
  } else {
    # if there's already a temp folder there, create a new one (make sure it's new)
    folder.append <- 1L
    output.folder.try <- paste0( "butterflyTemp", folder.append )
    while( dir.exists( output.folder.try ) ) {
      folder.append <- folder.append + 1L
      output.folder.try <- paste0( "butterflyTemp", folder.append )
    }
    setwd( output.folder.try )
    output.folder <- getwd()
    rm( output.folder.try )
  }

  # set up multi-threading on Unix-alike systems, or disable it on Windows systems
  # if( "Windows" %in% Sys.info() ) {
  #     print( "Sorry, no support for parallel processing on Windows.")
  #     coresToUse <- 1L
  #     parallel <- FALSE
  #     progress <- "text"
  # } else if( !parallel || parallel == 1 ) {
  #     print( "Running in single-thread mode." )
  #     coresToUse <- 1L
  #     parallel <- FALSE
  #     progress <- "text"
  # } else {
  #     if( !is.numeric( parallel ) ) {
  #         coresToUse <- coresToUse()
  #     } else {
  #         coresToUse <- parallel
  #     }
  #     print( paste( "Multi-threading where possible across", coresToUse, "threads." ) )
  #     doMC::registerDoMC( cores = coresToUse )
  #     parallel <- TRUE
  #     progress = "none"
  # }

  if( is.numeric( parallel ) && parallel > 1L ) {
    coresToUse <- parallel
    parallel <- TRUE
  } else if( isTRUE( parallel ) ) {
    coresToUse <- coresToUse()
    parallel <- TRUE
  } else if( is.numeric( parallel ) && as.integer( parallel ) == 1L ) {
    coresToUse <- 1L
    parallel <- FALSE
    paropts <- NULL
  } else {
    coresToUse <- 1L
    parallel <- FALSE
    paropts <- NULL
  }

  # get the currents data
  print( "Importing data from currents file." )
  aa <- ncdf4::nc_open( currents.file )

  # extract the different variables (metadata) from the .nc:
  depth <- ncdf4::ncvar_get( aa, varid = "depth" )
  lat <- ncdf4::ncvar_get( aa, varid = "lat" )
  lon <- ncdf4::ncvar_get( aa, varid = "lon" )

  # extract a spatial range
  latRange <- range( lat )
  lonRange <- range( lon )

  # retrieve and convert dates
  dates <- ncdf4::ncvar_get( aa, varid = "time" ) %>%
    as.Date( origin = "1979-01-01" )

  print( "Converting currents to appropriate format." )
  # retrieve and convert current data
  currents <- convertCurrents( zonal.current = ncdf4::ncvar_get( aa, varid = "ZonalCurrent" ),
                               meridional.current = ncdf4::ncvar_get( aa, varid = "MeridionalCurrent" ) )

  wdir <- currents[["direction"]]
  wspeed <- currents[["speed"]]


  # for each pixel we now have to calculated the angle with respect LP foraging area
  # first, we have to construct the container for this info. This will be an array with two dimensions: lat and lon
  # the number of records will be the same than those for wdir
  angle_sink <- angleSink( POI = POI,
                           lat = lat,
                           lon = lon
  )
  # fields::image.plot( angle_sink )

  # based on the info on angle_sink, wdir and wspeed we will now calculate the the cost layers following the cost function by Afán et al. 2015
  # we now calculate the costs for all the dates. Our cost function consider angles in radians
  cost_sink <- costFun( water.direction = wdir,
                        water.speed = wspeed,
                        angle.sink = angle_sink )
  # fields::image.plot( cost_sink[,,1] )

  # we now have to transform these objects into rasters
  # as long as the grid is not regular, there is no way to produce directly a raster, so we have to make a long way constructing a shape of points and then interpolating
  # we will construct a dataframe with three variables: lon, lat and cost
  # costs will be latter extracted in a loop. We first extract lon and lat

  df_cost <- data.frame( apply( cost_sink, 3, as.vector ) )
  names( df_cost ) <- dates

  # Mask to be used to erase land-masses from raster data
  # land.mask <- createSpatialMap( latRange = latRange,
  #                                lonRange = lonRange
  # )

  # create a base grid to build upon
  grid.df <- createGrid( latRange = latRange,
                         lonRange = lonRange,
                         cell.size = cell.size,
                         convert.to.spatial = FALSE
  ) %>%
    data.table::setDT(.) %>%
    data.table::setorder( lon, -lat )


  # build a mask for points falling on land
  print( "Downloading local area map, and converting to appropriate mask." )
  land.mask <- landMask( lat = grid.df$lat,
                         lon = grid.df$lon,
                         cores = coresToUse
  ) %>%
    data.table::setDT(.) %>%
    data.table::setorder( lon, -lat )

  input.coords <- data.table::data.table( latit = as.vector( lat ),
                                          longi = as.vector( lon )
  )

  # apply the land mask to the grid
  grid.df[ , landmask := land.mask[,mask] ]



  # subset the dates to analyse, based on input parameters.
  if( !is.na( dates.range ) && length( dates.range ) >= 2L ) {

    dates.range <- as.Date( dates.range )
    dates <- dates[ dates >= min( dates.range ) - buffer.days &
                      dates <= max( dates.range ) + buffer.days ]

    # also filter the df_cost data to only the dates we need
    df_cost.dates <- names( df_cost ) %>%
      as.Date()
    df_cost <- df_cost[ , which( df_cost.dates >= min( dates.range ) - buffer.days &
                                   df_cost.dates <= max( dates.range ) + buffer.days ) ]

  }




  # create the cost rasters via inverse distance weighted interpolation,
  # outputting to ascii files. We are utilising multi-threading here for speed
  print( "Interpolating grid-wise current data using inverse distance weighting." )
  print( "PLEASE BE PATIENT. This process may take a long time, depending on data and parameters..." )



  if( parallel && Sys.info()[['sysname']] != "Windows" ) {
    doMC::registerDoMC( cores = coresToUse )
    progress <- "none"
    parallel.forCost <- TRUE
  } else {
    parallel.forCost <- FALSE
    progress <- "text"
    print( "Sorry, this process is being passed to C++ for processing, this cannot be multi-threaded under Windows." )
  }

  cost.matrix <- plyr::llply( .data = seq_len( nrow( grid.df ) ),
                              .fun = idDub,
                              inputlat = input.coords$latit,
                              inputlon = input.coords$longi,
                              inputdata = as.matrix( df_cost ),
                              outputlat = grid.df$lat,
                              outputlon = grid.df$lon,
                              landmask = grid.df$landmask,
                              .parallel = parallel.forCost,
                              .progress = progress ) %>%
    do.call( what = rbind ) %>%
    as.data.frame()


  noDataValue = -9999

  # mask areas of land with the designated noDataValue
  mask.out <- which( land.mask$mask == 1L )
  cost.matrix[ mask.out, ] <- noDataValue

  # output the ascii files, ready for circuitscape
  for( i in seq_len( ncol( cost.matrix ) ) ) {
    df2Ascii( lat = grid.df[,lat], lon = grid.df[,lon], data = cost.matrix[,i],
              file = paste0( output.folder, "/cost_", as.character( dates[i], "%Y%m%d" ), ".asc" ),
              noDataValue = noDataValue )
  }

  print( "Interpolation done, moving on..." )



  # In order to run CIRCUITSCAPE, we now need a SINK (foraging area) and a SOURCE (each pixel within the bounding box)

  # For the SOURCE.
  # measure the distance from the colony at every point on the grid
  grid.df[ , distance.from.poi := mapDistance( lat.1 = POI[2],
                                               lon.1 = POI[1],
                                               lat.2 = lat,
                                               lon.2 = lon,
                                               unit = "km" ) ]

  # we need land and foraging area marked as "nodata", everything else value 1
  # make values within the foraging range equal to zero, and all others equal to 1
  grid.df[ , source := 1L ] %>%
    .[ distance.from.poi <= foraging.distance, source := 0L ]

  # we also mask the land areas, making them equal to 1 for the source
  grid.df[ landmask == 1L, source := 0L ]


  # For the SINK, we need foraging area marked as "nodata", with all other areas marked as 1
  # make values within the foraging range equal to zero, and all others equal to 1
  grid.df[ , sink := 0L ] %>%
    .[ distance.from.poi <= foraging.distance, sink := 1L ]

  # we also mask the land areas, making them equal to 1
  grid.df[ landmask == 1L, sink := 0L ]

  # now we write the source and sink files to Ascii
  grid.df[ source == 0L, source := noDataValue ] %>%
    .[ sink == 0L, sink := noDataValue ]

  df2Ascii( lat = grid.df[,lat], lon = grid.df[,lon], data = grid.df[,source],
            file = paste( output.folder, "source.asc", sep = "/" ), noDataValue = -9999 )

  df2Ascii( lat = grid.df[,lat], lon = grid.df[,lon], data = grid.df[,sink],
            file = paste( output.folder, "sink.asc", sep = "/" ), noDataValue = -9999 )

  # an object containing all the cost files within the directory
  costs.filelist <- list.files( path = paste0( output.folder ),
                                pattern = "cost", full.names = TRUE
  )

  # we'll use them to create conductance arrays, set a folder for those
  navto( c( output.folder, "conductanceFiles" ) )
  conductanceFiles.folder <- getwd()

  # now we run circuitscape. Note that this will be run externally in python, rather than R
  # we will utilise multi-threading here too, if requested in the original function call
  print( "Passing currents data to CircuitScape for processing." )

  if( parallel ) {
    cl <- parallel::makeCluster( coresToUse )
    doParallel::registerDoParallel( cl )
    paropts <- list(
      .export = c( "costs.filelist", "csrun.link", "conductanceFiles.folder",
                   "output.folder", "dates", "runCircuitscape" )
    )
    progress <- "none"
  } else {
    parallel <- FALSE
    paropts <- NULL
    progress <- "text"
  }

  suppressMessages(
    plyr::l_ply( .data = seq_along( costs.filelist ),
                 .fun = runCircuitscape,
                 costs.filelist = costs.filelist,
                 csrun.link = csrun.link,
                 output.folder = output.folder,
                 source.asc.link = paste( output.folder, "source.asc", sep = "/" ),
                 sink.asc.link = paste( output.folder, "sink.asc", sep = "/" ),
                 dates = dates,
                 .parallel = parallel,
                 .paropts = paropts,
                 .progress = progress
    )
  )

  if( parallel ) {
    parallel::stopCluster( cl )
  }

  # list all the files within the temporary directory
  conductance.files <- list.files( path = paste0( output.folder, "/conductance" ),
                                   pattern = ".asc",
                                   full.names = TRUE )
  conductance.files <- conductance.files[ !grepl( "_dynamic", conductance.files ) ]

  # extract used dates from conductance file names
  dates <- conductance.files %>%
    strsplit( split = "/" ) %>%
    sapply( tail, 1L ) %>%
    strsplit( split = "_" ) %>%
    sapply( "[", 2L ) %>%
    trimws() %>%
    as.Date( format = "%Y%m%d" )

  # We will first construct the dynamic areas of influence (based on ocean currents) and used to later extract CHL
  print( "Processing conductance data." )
  conductance.table <- plyr::llply( .data = conductance.files,
                                    .fun = processConductance,
                                    land.mask = land.mask,
                                    return.data.vector = TRUE,
                                    split.quant = split.quant,
                                    file.identifier = "_dynamic",
                                    .parallel = FALSE,
                                    .progress = "text"
  ) %>%
    do.call( what = cbind ) %>%
    data.table::data.table() %>%
    data.table::setnames( as.character( dates ) )

  conductance.table <- cbind( grid.df[ , .( lat, lon ) ], conductance.table )

  # dynamic.conductance.files <- list.files( path = paste0( conductanceFiles.folder, "/conductance" ),
  #                                          pattern = ".asc",
  #                                          full.names = TRUE )
  # dynamic.conductance.files <- dynamic.conductance.files[ grepl( "_dynamic", dynamic.conductance.files ) ]
  #
  # print( "Arranging processed conductance data for output." )
  # conductance.table <- createConductanceTable( conductance.files = dynamic.conductance.files,
  #                                              land.mask = land.mask )

  # now remove the temporary folder we created
  print( "Removing temporary working directory." )
  setwd( output.folder )
  setwd( ".." )
  unlink( output.folder, recursive = TRUE )

  print( "Done!" )
  return( conductance.table )

}
