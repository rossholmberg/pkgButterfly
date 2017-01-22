
#' landMask function to download a local map, and use it to create a mask of the local land areas
#'
#' @param lat vector or matrix of latitude points
#' @param lon vector or matrix of longitude points
#' @param cores number of cores to use when multi-threading
#'
#' @import rgeos
#' @import maps
#' @import sp
#' @import plyr
#' @import magrittr
#' @import doParallel
#' @import foreach
#'
#' @return data.frame
#'
#' @export
#'


landMask <- function( lat, lon, cores = TRUE ) {

  if( isTRUE( cores ) ) {
    parallel <- TRUE
    cores <- parallel::detectCores( logical = F )
    if( cores == parallel::detectCores( logical = T ) ) {
      cores <- cores - 1L
    }
  } else if( is.numeric( cores ) ) {
    parallel <- TRUE
  } else {
    parallel <- FALSE
    cores <- 1L
  }

  pkgLoad( c( "rgeos", "maps", "sp", "magrittr" ) )

  # get the latitude and longitude ranges
  latRange <- range( lat )
  lonRange <- range( lon )

  # download a map of the area
  mapObject <- maps::map( database = "world",
                          xlim = lonRange,
                          ylim = latRange,
                          fill = TRUE,
                          plot = FALSE
  )

  # also turn the grid into a 3D array
  grid.latlist <- unique( lat )
  grid.lonlist <- unique( lon )
  grid.array <- array( data = NA,
                       dim = c( length( grid.latlist ),
                                length( grid.lonlist ),
                                2 )
  )
  grid.array[,,1] <- sort( rep( grid.lonlist, length( grid.latlist ) ) )
  grid.array[,,2] <- rep( sort( grid.latlist, decreasing = TRUE ), length( grid.lonlist ) )

  # create an empty matrix to fill
  map <- array( data = NA,
                dim = c( dim( grid.array )[1:2], 3 )
  )

  # fill 2 layers for latitude and longitude values
  map[ , , 2 ] <- c( rep( seq.int( from = max( latRange ),
                                   to = min( latRange ),
                                   length.out = dim( map )[ 1 ] ),
                          dim( map )[ 2 ] )
  )
  map[ , , 3 ] <- sort( c( rep( seq.int( from = min( lonRange ),
                                         to = max( lonRange ),
                                         length.out = dim( map )[ 2 ] ),
                                dim( map )[ 1 ] ) )
  )


  # get the latitude and longitude coordinates from the mapObject
  coords.matrix <- matrix( c( mapObject$x, mapObject$y ), ncol = 2 )

  # extract the divisions (between polygon lines) from the mapObject
  div <- c( 0, which( is.na( mapObject$x ) ), length( mapObject$x ) + 1 )

  # separate coordinates by divisions
  coords.list <- sapply( 1:( length( div ) - 1 ),
                         function( x ) {
                           coords.matrix[ ( div[ x ] + 1 ) : ( div[ x + 1 ] - 1 ), ]
                         } )

  ## change sets of coordinates into SpatialPolygons
  map.spatial <- sapply( coords.list, function( x ) { sp::Polygon( x ) } ) %>%
    sp::Polygons( ., ID = "a" ) %>%
    list() %>%
    sp::SpatialPolygons( . )

  # analyse each point to see whether it's land or water
  if( parallel && as.integer( cores ) > 1L ) {
      cl <- parallel::makeCluster( as.integer( cores ) )
      doParallel::registerDoParallel( cl )
      parallel <- TRUE
  } else {
      paropts <- NULL
      progress <- "text"
  }
  if( parallel ) {
      paropts <- list(
          .packages = c( "rgeos", "sp" ),
          .export = c( "map", "map.spatial" )
      )
      progress <- "none"
  }
  # if( parallel && Sys.info()[['sysname']] != "Windows" ) {
  #   doMC::registerDoMC( cores )
  #   progress <- "none"
  # } else if( Sys.info()[['sysname']] == "Windows" ) {
  #   print( "Sorry, cannot multi-thread this process under Windows." )
  #   parallel <- FALSE
  #   progress <- "text"
  # } else {
  #   parallel <- FALSE
  #   progress <- "text"
  # }

  land.water <- plyr::aaply( .data = map,
                             .margins = c( 1, 2 ),
                             .fun = function(x) {
                               rgeos::gContains( map.spatial,
                                                 sp::SpatialPoints(
                                                   matrix( c( x[ 3 ], x[ 2 ] ), ncol = 2 )
                                                 ) )
                             },
                             .parallel = parallel,
                             .progress = progress )


  # change binary data and combine
  map[ , , 1 ] <- as.integer( land.water )

  map.df <- data.frame( lon = as.vector( map[,,3] ),
                        lat = as.vector( map[,,2] ),
                        mask = as.vector( map[,,1] )
  )

  ##
  # plot( coords.matrix, pch = ".", xlim = lonRange, ylim = latRange )
  # points( c( map[ , , 3 ] ), c( map[ , , 2 ] ),
  #         col = c( 0, 1 )[ as.factor( c( map[ , , 1 ] ) ) ],
  #         pch = 19 )

  return( map.df )

}
