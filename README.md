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

chl.file <- paste0( data.input.folder, "/dataset-global-analysis-forecast-bio-001-014_1486686773262.nc" )
```

The functions here will *attempt* to find logical ways in which data has been stored within the given .nc file. However, since there doesn't seem to be standard for how to store data in .nc files (we should standardise this!), we may need to extract certain things from the file manually. In this case, we'll extract the timeseries data maually, convert it to a logical date format, then pass that to the next function `chlorophyllCalc` to override any automated extraction of that.

``` r
variables.available <- nc_variableNames( chl.file )
time.variable.name <- variables.available[ grep( "time|date", variables.available ) ]
library( ncdf4 )
nc <- nc_open( chl.file )
times <- ncvar_get( nc, time.variable.name )
nc_close( nc )
head( times )
#> [1] 543468 543636 543804 543972 544140 544308
```

Note that in this case, as with the previous on (both of these files came from the same data source, the European Union's "Copernicus" repository), we need to convert the times from a numeric "hourse since 1950-01-01 00:00:00":

``` r
library( magrittr )
times %<>%
    as.numeric() %>%
    divide_by( 24 ) %>%
    as.Date( origin = "1950-01-01" )
head( times )
#> [1] "2011-12-31" "2012-01-07" "2012-01-14" "2012-01-21" "2012-01-28"
#> [6] "2012-02-04"
```

Then process that list of files to achieve an output chlorophyll measurement for each available date. Note that each dated chlorophyll matrix must have conductance data against which to apply the algorithm. We can specify a buffer time as `max.day.diff` which will allow some leeway here, such that slight mismatches in conductance data and chlorophyll data can be tolerated (either a single match, or extrapolated data between nearest before and after data points will be used).

``` r
dates.chlorophyll <- plyr::ldply( .data = chl.file,
                                  .fun = chlorophyllCalc,
                                  data.variable = "chl",
                                  depth.dimension = NULL,
                                  depth.touse = NULL,
                                  timeseries = times,
                                  conductance.data = conductance.table,
                                  max.day.diff = 30,
                                  .parallel = FALSE ) %>%
  setDT() %>%
  # for duplicates (useful in cases where more than one file has been analysed) take an average
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
#> 1: 2013-12-07        0.4638718
#> 2: 2013-12-14        0.4022076
#> 3: 2013-12-22        0.3491838
#> 4: 2013-12-28        0.3184212
#> 5: 2014-01-04        0.4334572
#> 6: 2014-01-11        0.3483841
```

We can now plot the results:

``` r
library( ggplot2 )
ggplot( data = dates.chlorophyll, mapping = aes( x = as.Date( date ), y = mean.chlorophyll ) ) +
    geom_point() +
    geom_smooth( method = "loess", span = 0.2 ) +
    ggtitle( "Mean Chlorophyll concentration, based on local dynamic Butterfly" )
```

![](READMEfigs/plotChlorophyll-1.png)

The above shows interpolated chlorophyll level over time, for the specified area of interest (a radius of `foraging.distance` km around `POI`).

#### Analysis of other variables

Note that what we've done so far is to compute a "Butterfly" area, then apply that area to interpolate available Chlorophyll data in the most meaningful way. If we so wish, we can apply the butterfly area to variables other than Chlorophyll. For example, the file referenced here contains data for "net\_primary\_productivity\_of\_carbon" as well:

``` r
dates.PP <- plyr::ldply( .data = chl.file,
                         .fun = chlorophyllCalc,
                         data.variable = "PP",
                         depth.dimension = NULL,
                         depth.touse = NULL,
                         timeseries = times,
                         conductance.data = conductance.table,
                         max.day.diff = 30,
                         .parallel = FALSE ) %>%
    setDT() %>%
    # for duplicates (useful in cases where more than one file has been analysed) take an average
    .[ , mean( mean.chlorophyll ), by = date ] %>%
    # sort by date
    setorder( date ) %>%
    # make sure the column names are appropriate
    setnames( c( "date", "mean.pp" ) )
gc() # collect garbage to clear RAM
```

``` r
library( ggplot2 )
ggplot( data = dates.PP, mapping = aes( x = as.Date( date ), y = mean.pp ) ) +
    geom_point() +
    geom_smooth( method = "loess", span = 0.2 ) +
    ggtitle( "Mean Primary Production, based on local dynamic Butterfly" )
```

![](READMEfigs/plotPrimaryProduction-1.png)

Likewise, we have included in this file the data for "mole\_concentration\_of\_phytoplankton\_expressed\_as\_carbon\_in\_sea\_water":

``` r
dates.Phyto <- plyr::ldply( .data = chl.file,
                         .fun = chlorophyllCalc,
                         data.variable = "PHYC",
                         depth.dimension = NULL,
                         depth.touse = NULL,
                         timeseries = times,
                         conductance.data = conductance.table,
                         max.day.diff = 30,
                         .parallel = FALSE ) %>%
    setDT() %>%
    # for duplicates (useful in cases where more than one file has been analysed) take an average
    .[ , mean( mean.chlorophyll ), by = date ] %>%
    # sort by date
    setorder( date ) %>%
    # make sure the column names are appropriate
    setnames( c( "date", "mean.phyto" ) )
gc() # collect garbage to clear RAM
```

``` r
library( ggplot2 )
ggplot( data = dates.Phyto, mapping = aes( x = as.Date( date ), y = mean.phyto ) ) +
    geom_point() +
    geom_smooth( method = "loess", span = 0.2 ) +
    ggtitle( "Mean Phytoplankton, based on local dynamic Butterfly" )
```

![](READMEfigs/plotPhytoplankton-1.png)
