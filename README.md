<!-- Do not edit README.md directly, edit README.Rmd instead and re-knit before commit -->
pkgButterfly
============

### Install and load

Install `pkgButterfly` from github if not installed yet.

``` r
devtools::install_github( "rossholmberg/pkgButterfly" )
```

Load the package.

``` r
library( pkgButterfly )
```

### Chlorophyll analysis

The parameters to the `costCalcMaster` function are as follows.

Central point from which foraging occurs (note areas of land will not be taken into account, so a coastline point is suitable here)

``` r
colony <- c( 145.2, -38.8 )
central.foraging.area <- c( 145.014879, -38.752039 )
```

A file containing currents data needs to be input to the calculations. This data will be used to calculate an "area of influence" (a.k.a "butterfly", due to a shape of which this area may take the form).

``` r
currents.file <- paste0( data.input.folder, "/dataset-armor-3d-v4-cmems-v2_1486508039772.nc" )
```

A folder to be used to temporarily store data created during processing.

``` r
temp.output.folder <- paste0( data.output.folder, "/Rtemp/" )
```

If variable names are logical within the .nc file, `costCalcMaster` will find them automatically. If any are strangely names, we may need to specify them in the function call. To get a list of variable names within the file, we can run:

``` r
nc_variableNames( currents.file )
#> [1] "time"        "depth"       "latitude"    "longitude"   "zvelocity"  
#> [6] "height"      "mvelocity"   "salinity"    "temperature"
```

Note the names in this file are fairly logical. We have "lat", "lon", "ZonalCurrent", and "MeridionalCurrent". There is also a "depth" variable here, meaning there may be more than one depth value available. We will only analyse one depth value (multiple values can be analysed by calling `costCalcMaster` within, for example, `lapply` or similar). If more than one depth value is available, it will be necessary to specify which depth to use in the analysis.

Run the main "butterfly" analysis. This can be a very long process. Expect this step to take 10-90mins, depending on the size of the dataset, and the parameters set on input (particularly `dates.range` and `cell.size`).

``` r
conductance.table <- costCalcMaster( output.folder = temp.output.folder,
                                     # dates.range = as.Date( c( "2012-01-01", "2012-08-30" ) ), # uncomment to limit dataset
                                     buffer.days = 10L,
                                     currents.file = currents.file,
                                     depth.touse = 0,
                                     depth.dimension = 3,
                                     POI = central.foraging.area,
                                     foraging.distance = 50,
                                     cell.size = 0.1,
                                     circuitscape.link = ifelse( Sys.info()['sysname'] == "Windows",
                                                                 'C:/"Program Files"/Circuitscape/cs_run.exe',
                                                                 'python2.7' ),
                                     parallel = 6L
)
gc() # collect garbage to clear RAM
```

In some cases, the "dates" values, which now reside in the column names of `conductance.table`, are not in a logical format. It may be a good idea to change these accordingly. Here we use the `ncdf4` package, and discover that the time values are formatted as "hours since 1950-01-01":

``` r
aa <- ncdf4::nc_open( currents.file )
names( aa )
#>  [1] "filename"    "writable"    "id"          "safemode"    "format"     
#>  [6] "is_GMT"      "groups"      "fqgn2Rindex" "ndims"       "natts"      
#> [11] "dim"         "unlimdimid"  "nvars"       "var"
aa$dim$time$units
#> [1] "hours since 1950-01-01"
ncdf4::nc_close( aa )
```

We can use that to replace the current column names to useful date values. Note we're only changing the values for columns starting from 3, since the first 2 columns are "lat" and "lon"

``` r
library( magrittr )
names( conductance.table )[ 3:ncol( conductance.table ) ] %<>%
    as.numeric() %>%
    divide_by( 24 ) %>%
    as.Date( origin = "1950-01-01" ) %>%
    as.character()
```

Have a look at one of the points in the butterfly over time.

``` r
pkgButterfly::plotButterfly( conductance.table, round( ncol( conductance.table ) - 2L ) * 0.5 )
```

![](READMEfigs/plotButterfly-1.png)

We can pass a single file to the `chlorophyllCalc` function at a time, but here we'll demonstrate passing 2 files, each of which will be used against the conductance table we've just produced.

``` r
library( magrittr )
library( data.table )
library( plyr )

chl.files <- paste0( data.input.folder,
                     c( "/erdSWchla8day_5e25_bcf8_7643.nc",
                        "/erdMH1chla8day_4d10_c7b5_5ad5.nc" )
)
```

Then process that list of files to achieve an output chlorophyll measurement for each available date. Note that each dated chlorophyll matrix must have conductance data against which to apply the algorithm. We can specify a buffer time as `max.day.diff` which will allow some leeway here, such that slight mismatches in conductance data and chlorophyll data can be tolerated (either a single match, or extrapolated data between nearest before and after data points will be used).

``` r
dates.chlorophyll <- plyr::ldply( .data = chl.files,
                                  .fun = chlorophyllCalc,
                                  data.variable = "chlorophyll",
                                  conductance.data = conductance.table,
                                  max.day.diff = 10,
                                  .parallel = FALSE ) %>%
  setDT() %>%
  # for duplicates (since we analysed two files) take an average
  .[ , mean( mean.chlorophyll ), by = date ] %>%
  # sort by date
  setorder( date ) %>%
  # make sure the column names are appropriate
  setnames( c( "date", "mean.chlorophyll" ) )
gc() # collect garbage to clear RAM
```

What we're left with is a data frame displaying dates and chlorophyll values.

``` r
head( dates.chlorophyll )
#>          date mean.chlorophyll
#> 1: 2013-12-23        0.4388904
#> 2: 2013-12-29        0.3825137
#> 3: 2014-01-05        0.3940464
#> 4: 2014-01-13        0.4319269
#> 5: 2014-01-21        0.4566485
#> 6: 2014-01-29        0.5189684
```

We can now plot the results:

``` r
library( ggplot2 )
ggplot( data = dates.chlorophyll, mapping = aes( x = date, y = mean.chlorophyll ) ) +
    geom_point() +
    geom_smooth( method = "loess", span = 0.1 )
```

![](READMEfigs/plotChlorophyll-1.png)

The above shows interpolated chlorophyll level over time, for the specified area of interest (a radius of `foraging.distance` km around `POI`).
