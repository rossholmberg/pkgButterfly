% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createGrid.R
\name{createGrid}
\alias{createGrid}
\title{Converting a given area range and cell size into a grid of reference points.}
\usage{
createGrid(latRange, lonRange, cell.size = 0.1, convert.to.spatial = FALSE)
}
\arguments{
\item{latRange}{numeric vector, range of latitudes to be represented}

\item{lonRange}{numeric vector, range of longitudes to be represented}

\item{cell.size}{numeric value specifying desired size of each grid point in degrees}

\item{convert.to.spatial}{TRUE/FALSE, whether to convert the object to a spatial object before returning}
}
\value{
data.frame
}
\description{
Converting a given area range and cell size into a grid of reference points.
}
\keyword{GPS,}
\keyword{spatial}
