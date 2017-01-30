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
currents.file <- "~/git/butterfly R script/dataset-global-reanalysis-phys-001-011-ran-it-cglors-monthly-u-v_1463479328704.nc"
```

A folder to be used to temporarily store data created during processing.

``` r
output.folder <- "C:/Users/rholmberg/Desktop/Rtemp/"
```

Run the main "butterfly" analysis. This can be a very long process. Expect this step to take 10-90mins, depending on the size of the dataset, and the parameters set on input (particularly `dates.range` and `cell.size`).

``` r
conductance.table <- costCalcMaster( output.folder = output.folder,
                                     #dates.range = as.Date( c( "2012-01-01", "2012-08-30" ) ), # uncomment to limit dataset
                                     buffer.days = 10L,
                                     currents.file = currents.file,
                                     POI = central.foraging.area,
                                     foraging.distance = 50,
                                     cell.size = 0.1,
                                     split.quant = 0.75,
                                     parallel = 6L
)
gc() # collect garbage to clear RAM
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

input.folder <- "~/git/butterfly R script/"
chl.files <- c( paste0( input.folder, "erdSWchla8day_5e25_bcf8_7643.nc" ),
                paste0( input.folder, "erdMH1chla8day_4d10_c7b5_5ad5.nc" ) )
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
#> 1: 1997-09-10        0.2573754
#> 2: 1997-09-18        0.3184158
#> 3: 1997-10-12        0.4190093
#> 4: 1997-10-20        0.3525433
#> 5: 1997-11-05        0.3534836
#> 6: 1997-11-13        0.3707462
```

We can now plot the results:

``` r
library( ggplot2 )
ggplot( data = dates.chlorophyll, mapping = aes( x = date, y = mean.chlorophyll ) ) +
    geom_point() +
    geom_smooth( method = "loess", span = 0.03 )
```

![](READMEfigs/plotChlorophyll-1.png)

The above shows interpolated chlorophyll level over time, for the specified area of interest (a radius of `foraging.distance` km around `POI`).
