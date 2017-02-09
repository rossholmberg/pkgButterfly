#'
#' This function takes a datafile of ocean currents, and uses it to process a dynamic area of influence.
#' @description The process used is as per Afán et al 2015
#'
#' @param currents.file complete link to .nc data file containing ocean currents data
#' @param lat.variableName character name of latitude variable in .nc currents.file. Default "lat"
#' @param lon.variableName character name of longitude variable in .nc currents.file. Default "lon"
#' @param depth.variableName character name of depth variable in .nc currents.file. Default "depth"
#' @param time.variableName character name of time variable in .nc currents.file. Default "time"
#' @param current.variableNames character vector (length 2), names of current variables in .nc currents.file.
#' Element 1 is for the zonal current values, element 2 is for the meridional current values.
#' Default c( "ZonalCurrent", "MeridionalCurrent" )
#' @param depth.touse numerical depth value. Which available depth dataset (if more than one) is to be analysed here?
#' @param depth.dimension integer value specifying which dimension of the arrays in the .nc file represents
#' "depth". This will be used to subset only the depth to be analysed here.
#' @param dates.range range of dates to subset from input data. If NA, all dates in input file are used.
#' @param buffer.days buffer around requested date to search for data
#' @param output.folder complete link to a folder to use for temporary files during processing.
#' A temporary folder will be created within this specified folder, and deleted when computation has completed.
#' @param POI c( [longitude], [latitude] ) marking the central point (eg: colony)
#' @param foraging.distance A distance in km to consider as the area of interest from the POI.
#' @param cell.size Resolution (in degrees) to create for output. Note data will be interpolated from input.
#' @param split.quant The point at which to split for areas of influence (0.75 takes the TOP 25\% influence), as per Afán et al 2015
#' @param circuitscape.link complete link to executable file to be used for running Circuitscape.
#' Under Windows, this can be a `.exe` file for Circuitscape itself. Under *nix systems, this can be a link to python,
#' under which Circuitscape will be called.
#' @param parallel FALSE will not use multi-threading, TRUE will guess at an optimal number of cores based on processor,
#' integer value specifies a number of cores to use when multi-threading tasks.
#'
#' @keywords currents, chlorophyll, ocean, circuitscape
#'
#' @export
#'
#' @import data.table
#' @import doParallel
#' @import parallel
#' @import plyr
#' @import magrittr
#' @import ncdf4
#'



costCalcMaster <- function( currents.file,
                            lat.variableName = NA_character_,
                            lon.variableName = NA_character_,
                            depth.variableName = NA_character_,
                            time.variableName = NA_character_,
                            current.variableNames = NA_character_,
                            depth.touse = NULL,
                            depth.dimension = NULL,
                            dates.range = NA,
                            buffer.days = 10L,
                            output.folder,
                            POI,
                            foraging.distance,
                            cell.size = 0.1,
                            split.quant = 0.75,
                            circuitscape.link = ifelse( Sys.info()['sysname'] == "Windows",
                                                        'C:/"Program Files"/Circuitscape/cs_run.exe',
                                                        'python2.7' ),
                            parallel ) {

    start.time <- as.integer( Sys.time() )

    # if any variables were not specified, try to find them automatically
    ncvarnames <- nc_variableNames( currents.file )
    if( is.na( lat.variableName ) ) {
        lat.variableName <- ncvarnames[ grepl( "lat", ncvarnames, ignore.case = TRUE ) ]
        if( length( lat.variableName ) != 2L ) {
            stop( "Latitude variable could not be located automatically. Please specify in function call." )
        }
    }
    if( is.na( lon.variableName ) ) {
        lon.variableName <- ncvarnames[ grepl( "lon", ncvarnames, ignore.case = TRUE ) ]
        if( length( lon.variableName ) != 2L ) {
            stop( "Longitude variable could not be located automatically. Please specify in function call." )
        }
    }
    if( is.na( current.variableNames ) ) {
        current.variableNames <- c( ncvarnames[ grepl( "^zvel|^zonal", ncvarnames, ignore.case = TRUE ) ],
                                   ncvarnames[ grepl( "^mvel|^meridional", ncvarnames, ignore.case = TRUE ) ] )
        if( length( current.variableNames ) != 2L ) {
            stop( "Current variables could not be located automatically. Please specify in function call." )
        }
    }
    if( is.na( depth.variableName ) ) {
        depth.variableName <- ncvarnames[ grepl( "depth", ncvarnames, ignore.case = TRUE ) ]
        if( length( depth.variableName ) != 1L ) {
            stop( "Depth variable could not be located automatically. Please specify in function call." )
        }
    }
    if( is.na( time.variableName ) ) {
        time.variableName <- ncvarnames[ grepl( "time|date", ncvarnames, ignore.case = TRUE ) ]
        if( length( time.variableName ) > 1L ) {
            stop( "Time variable could not be located automatically. Please specify in function call." )
        }
    }

    # one value that may not be needed here is "depth", since some data will come with only one depth value.
    # in that case, change the depth.variableName back to NA
    if( length( depth.variableName ) == 0L ) {
        depth.variableName <- NA_character_
    }

    cat( "Running pre-flight checks and setup.\n" )

    # create a dummy variable to keep the software happy
    . <- landmask <- distance.from.poi <- NULL

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
        dir.create( output.folder.try )
        setwd( output.folder.try )
        output.folder <- getwd()
        rm( output.folder.try )
    }

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
    cat( "Importing data from currents file.\n" )
    aa <- ncdf4::nc_open( currents.file )

    # extract the different variables (metadata) from the .nc:
    depth <- ncdf4::ncvar_get( aa, varid = depth.variableName )
    lat <- ncdf4::ncvar_get( aa, varid = lat.variableName )
    lon <- ncdf4::ncvar_get( aa, varid = lon.variableName )

    # extract a spatial range
    latRange <- range( lat )
    lonRange <- range( lon )

    # check the

    # retrieve date values
    dates <- ncdf4::ncvar_get( aa, varid = time.variableName )
    # dates <- as.Date( dates, origin = "1979-01-01" )
    # dates <- as.POSIXct( dates * 60 * 60, origin = "1950-01-01" )

    zonal.current <- ncdf4::ncvar_get( aa, varid = current.variableNames[1] )
    meridional.current <- ncdf4::ncvar_get( aa, varid = current.variableNames[2] )

    # at this point we need to select a dataset to use. If there is more than one depth available, take only the one specified:
    if( length( depth ) > 1L ) {
        if( !is.null( depth.dimension ) && !is.null( depth.touse ) ) {
            which.depth.touse <- which( depth == depth.touse )
            if( length( which.depth.touse ) == 0L ) {
                stop( "depth.touse parameter does not match any available depth values" )
            }
            zonal.current <- plyr::alply( zonal.current, depth.dimension, as.array )[[ which.depth.touse ]]
            meridional.current <- plyr::alply( meridional.current, depth.dimension, as.array )[[ which.depth.touse ]]
        } else {
            stop( paste( "There is more than one depth available in the dataset.",
                         "please specify paramters depth.dimension and depth.touse in function call.",
                         sep = "\n" )
            )
        }
    }

    # make sure lats and lons are orientated correctly
    # we need the latitude and longitude objects to be in matrix form. Convert them if they're not already
    if( length( dim( lat ) ) == 1L && length( dim( lon ) ) == 1L ) {
        lat <- matrix( data = sort( lat, decreasing = TRUE ), nrow = dim( lat ), ncol = dim( lon ) )
        lon <- matrix( data = sort( lon ), nrow = dim( lat ), ncol = dim( lon ), byrow = TRUE )

        # transpose currents data if necessary
        if( identical( dim( lat )[c(2,1)], dim( zonal.current )[c(1,2)] ) ) {
            zonal.current <- plyr::aaply( zonal.current,
                                          .margins = seq_along( dim( zonal.current ) )[ -c(1,2) ],
                                          .fun = t )
            meridional.current <- plyr::aaply( meridional.current,
                                               .margins = seq_along( dim( meridional.current ) )[ -c(1,2) ],
                                               .fun = t )
        }

    }

    # check the orientation of the latitude matrix, and correct if necessary
    # if rotation is necessary, ALL relevant data matrices need to be rotated in the same way
    if( max( lat[1,], na.rm = TRUE ) - min( lat[1,], na.rm = TRUE ) >
        max( lat[,1], na.rm = TRUE ) - min( lat[,1], na.rm = TRUE ) ) {
        if( slopeIsPositive( lat[,1] ) ) {
            lat <- matrixRotate( lat, -90 )
            lon <- matrixRotate( lon, -90 )
            zonal.current <- plyr::aaply( zonal.current,
                                          .margins = seq_along( dim( zonal.current ) )[ -c(1,2) ],
                                          .fun = matrixRotate,
                                          angle = -90 )
            meridional.current <- plyr::aaply( meridional.current,
                                               .margins = seq_along( dim( zonal.current ) )[ -c(1,2) ],
                                               .fun = matrixRotate,
                                               angle = -90 )
        } else {
            lat <- matrixRotate( lat, 90 )
            lon <- matrixRotate( lon, 90 )
            zonal.current <- plyr::aaply( zonal.current,
                                          .margins = seq_along( dim( zonal.current ) )[ -c(1,2) ],
                                          .fun = matrixRotate,
                                          angle = 90 )
            meridional.current <- plyr::aaply( meridional.current,
                                               .margins = seq_along( dim( zonal.current ) )[ -c(1,2) ],
                                               .fun = matrixRotate,
                                               angle = 90 )
        }
    } else if( max( lat[1,], na.rm = TRUE ) - min( lat[1,], na.rm = TRUE ) <
               max( lat[,1], na.rm = TRUE ) - min( lat[,1], na.rm = TRUE ) ) {
        if( slopeIsPositive( lat[,1] ) ) {
            lat <- matrixRotate( lat, 180 )
            lon <- matrixRotate( lon, 180 )
            zonal.current <- plyr::aaply( zonal.current,
                                          .margins = seq_along( dim( zonal.current ) )[ -c(1,2) ],
                                          .fun = matrixRotate,
                                          angle = 180 )
            meridional.current <- plyr::aaply( meridional.current,
                                               .margins = seq_along( dim( zonal.current ) )[ -c(1,2) ],
                                               .fun = matrixRotate,
                                               angle = 180 )
        }
    }

    cat( "Converting currents to appropriate format.\n" )
    # retrieve and convert current data
    currents <- convertCurrents( zonal.current = zonal.current,
                                 meridional.current = meridional.current )
    rm( meridional.current, zonal.current )
    gc()

    wdir <- currents[["direction"]]
    wspeed <- currents[["speed"]]
    rm( currents )
    gc()

    # for each pixel we now have to calculated the angle with respect LP foraging area
    # first, we have to construct the container for this info. This will be an array with two dimensions: lat and lon
    # the number of records will be the same as those for wdir
    angle_sink <- angleSink( POI = POI,
                             lat = lat,
                             lon = lon
    )
    # fields::image.plot( matrixRotate( angle_sink, 90 ) )


    # based on the info on angle_sink, wdir and wspeed we will now calculate the the cost layers following the cost function by Afán et al. 2015
    # we now calculate the costs for all the dates. Our cost function consider angles in radians
    cost_sink <- costFun( water.direction = wdir,
                          water.speed = wspeed,
                          angle.sink = angle_sink )
    # fields::image.plot( cost_sink[,,1] )
    rm( wdir, wspeed )
    gc()

    # we now have to transform these objects into rasters
    # as long as the grid is not regular, there is no way to produce directly a raster, so we have to make a long way constructing a shape of points and then interpolating
    # we will construct a dataframe with three variables: lon, lat and cost
    # costs will be latter extracted in a loop. We first extract lon and lat

    df_cost <- data.frame( apply( cost_sink, 3, as.vector ) )
    names( df_cost ) <- dates

    rm( cost_sink )
    gc()

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
    cat( "Downloading local area map, and converting to appropriate mask.\n" )
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
    grid.df[ , landmask := land.mask[['mask']] ]



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


    # convert df_cost to matrix class for passing to c++
    df_cost <- as.matrix( df_cost )


    # create the cost rasters via inverse distance weighted interpolation,
    # outputting to ascii files. We are utilising multi-threading here for speed
    cat( "\nInterpolating grid-wise current data using inverse distance weighting.\n" )
    cat( "PLEASE BE PATIENT. This process may take a long time, depending on data and parameters...\n" )

    # if( parallel && Sys.info()[['sysname']] != "Windows" ) {
    #   doMC::registerDoMC( cores = coresToUse )
    #   progress <- "none"
    #   parallel.forCost <- TRUE
    # } else {
    #   parallel.forCost <- FALSE
    #   progress <- "text"
    #   cat( "Sorry, this process is being passed to C++ for processing, this cannot be multi-threaded under Windows.\n" )
    # }

    if( parallel ) {
        cl <- parallel::makeCluster( coresToUse )
        doParallel::registerDoParallel( cl )
        parallel.forCost <- TRUE
        progress <- "none"
        paropts <- list(
            .packages = c( "pkgButterfly" ),
            .export = c( "grid.df", "input.coords", "df_cost" )
        )
    } else {
        parallel.forCost <- FALSE
        progress <- "text"
        paropts <- NULL
    }

    cat( paste( "Started IDW processing at", format( Sys.time(), "%H:%M:%S ____ %Y-%m-%d" ) ) )
    cat( "\n" )

    # using suppress warnings here to prevent a warning described in https://github.com/hadley/plyr/issues/203
    # the warning appears to be caused by a bug in plyr. Output is fine.
    suppressWarnings(
        cost.matrix <- plyr::llply( .data = seq_len( nrow( grid.df ) ),
                                    .fun = idDub,
                                    inputlat = input.coords$latit,
                                    inputlon = input.coords$longi,
                                    inputdata = df_cost,
                                    outputlat = grid.df$lat,
                                    outputlon = grid.df$lon,
                                    landmask = grid.df$landmask,
                                    .parallel = parallel.forCost,
                                    .progress = progress ) %>%
            do.call( what = rbind ) %>%
            as.data.frame()
    )

    if( parallel.forCost ) {
        doParallel::stopImplicitCluster()
        parallel::stopCluster( cl )
    }

    cat( paste( "Completed IDW processing at", format( Sys.time(), "%H:%M:%S ____ %Y-%m-%d" ) ) )
    cat( "\n" )

    rm( df_cost )
    gc()

    # view the cost matrix for a particular date if desired
    # cost.matrix[[1]] %>%
    #   matrix( nrow = sum( cost.matrix$lon == cost.matrix$lon[1] ) ) %>%
    #   apply( 2, rev ) %>%
    #   t() %>%
    #   fields::image.plot()

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

    cat( "Interpolation done, moving on...\n" )



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
    cat( "Passing currents data to CircuitScape for processing.\n" )

    # make a python script file for running circuitscape
    python.commands <- paste( '#!/usr/bin/python',
                              '',
                              'import sys',
                              'from circuitscape.compute import Compute',
                              'configFile = sys.argv[1]',
                              'cs = Compute(configFile, \'Screen\')',
                              'resistances = cs.compute()',
                              'print resistances',
                              '',
                              sep = "\n" )

    python.file <- paste( output.folder, "csrun.py", sep = "/" )

    cat( python.commands, file = python.file, append = FALSE )

    if( is.na( circuitscape.link ) || is.null( circuitscape.link ) ) {
        circuitscape.link <- ifelse( Sys.info()['sysname'] == "Windows",
                                     'C:/"Program Files"/Circuitscape/cs_run.exe',
                                     'python2.7' )
    }

    plyr::l_ply( .data = seq_along( costs.filelist ),
                 .fun = runCircuitscape,
                 costs.filelist = costs.filelist,
                 csrun.link = python.file,
                 output.folder = output.folder,
                 source.asc.link = paste( output.folder, "source.asc", sep = "/" ),
                 sink.asc.link = paste( output.folder, "sink.asc", sep = "/" ),
                 dates = dates,
                 function.call = circuitscape.link,
                 .parallel = FALSE,
                 .progress = ifelse( Sys.info()['sysname'] == "Windows",
                                     'text',
                                     'none' )
    )

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
    cat( "Processing conductance data.\n" )
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

    newcolorder <- c( "lat", "lon", names( conductance.table ) )

    conductance.table[ , lat := grid.df[['lat']] ]
    conductance.table[ , lon := grid.df[['lon']] ]

    setcolorder( conductance.table, newcolorder )

    # dynamic.conductance.files <- list.files( path = paste0( conductanceFiles.folder, "/conductance" ),
    #                                          pattern = ".asc",
    #                                          full.names = TRUE )
    # dynamic.conductance.files <- dynamic.conductance.files[ grepl( "_dynamic", dynamic.conductance.files ) ]
    #
    # print( "Arranging processed conductance data for output." )
    # conductance.table <- createConductanceTable( conductance.files = dynamic.conductance.files,
    #                                              land.mask = land.mask )

    # now remove the temporary folder we created
    cat( "Removing temporary working directory.\n" )
    setwd( output.folder )
    setwd( ".." )
    unlink( output.folder, recursive = TRUE )

    cat( "Done!\n" )

    gc()

    # print a timing summary
    process.duration <- as.integer( Sys.time() ) - start.time
    cat( "castCalcMaster process took " )
    if( process.duration <= 120 ) {
        cat( process.duration )
        cat( " secs." )
    } else if( process.duration <= 120*60 ) {
        cat( round( process.duration / 60, 2 ) )
        cat( " mins." )
    } else {
        cat( round( process.duration / 3600, 2 ) )
        cat( " hours." )
    }
    cat( "\n" )

    return( conductance.table )

}
