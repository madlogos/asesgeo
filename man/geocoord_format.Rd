% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{lat_coord}
\alias{lat_coord}
\alias{lon_coord}
\alias{coord_format}
\title{Geographic coordinate number formatters}
\usage{
lat_coord(x, accuracy = 1, scale = 1, prefix = c(pos = "", zero = "",
  neg = ""), unit = "°", sep = "", suffix = c(pos = "N", zero = "",
  neg = "S"), big.mark = " ", decimal.mark = ".", trim = TRUE, ...)

lon_coord(x, accuracy = 1, scale = 1, prefix = c(pos = "", zero = "",
  neg = ""), unit = "°", sep = "", suffix = c(pos = "E", zero = "",
  neg = "W"), big.mark = " ", decimal.mark = ".", trim = TRUE, ...)

coord_format(accuracy = 1, scale = 1, prefix = c(pos = "", zero = "",
  neg = ""), unit = "°", sep = " ", suffix = c(pos = "", zero = "",
  neg = ""), big.mark = " ", decimal.mark = ".", trim = TRUE, ...)
}
\arguments{
\item{x}{numeric vector to format}

\item{accuracy}{Number to round to, NULL for automatic guess.}

\item{scale}{A scaling factor: \code{x} will be multiply by scale before formating 
(useful if the underlying data is on another scale, e.g. for computing 
percentages or thousands).}

\item{prefix}{Symbols to display before and after value. By default, it is
a named character vector \code{c(pos="", zero="", neg="")}. If not in such format,
the function will automatically format the prefix.}

\item{unit}{The units to append.}

\item{sep}{The separator between the number and the unit label.}

\item{suffix}{Symbols to display before and after value. By default, it is
a named character vector \code{c(pos="N", zero="", neg="S")} for 'lat' and
\code{c(pos="E", zero="", neg="W")} for 'lon'. If not in such format, 
the function will automatically format the suffix.}

\item{big.mark}{Character used between every 3 digits to separate thousands.}

\item{decimal.mark}{The character to be used to indicate the numeric decimal point.}

\item{trim}{Logical, if FALSE, values are right-justified to a common width 
(see \code{\link{format}}()).}

\item{...}{Other arguments passed on to \code{\link{format}}().}
}
\value{
a character vector
}
\description{
\code{lat_coord} and \code{lon_coord} are wrappers of \code{\link[scales]{number}} 
to format the geographic coordinate values. \code{coord_format} is a function
factory to produce formatter functions.
}
\details{
\code{coord_format} is especially useful when plotting maps with \pkg{ggplot2}.
The \code{\link{scale_x_continuous}()} and \code{\link{scale_y_continuous}} 
function accepts functions as arguments \code{breaks} and \code{labels}. You 
can contruct your own formatter function using \code{coord_format}.
}
\examples{
\dontrun{
lat_coord(c(-10, 0, 20), sep=' ', unit='deg')
# [1] "10 degS" "0 deg"   "20 degN"

lat_coord(c(-10, 0, 20), sep=' ', prefix=c(
    pos='S.lat', zero='', neg='N.lat'), suffix='', unit='deg')
# [1] "N.lat10 deg" "0 deg"       "S.lat20 deg"
}
\dontrun{
lon_coord(c(-10, 0, 20), sep=' ', unit='deg')
# [1] "10 degW" "0掳"   "20 degE"

lon_coord(c(-10, 0, 20), sep=' ', prefix=c(
    pos='E.lon', zero='', neg='W.lon'), suffix='', unit='deg')
# [1] "W.lon10 deg" "0 deg"       "E.lon20 deg"
}
\dontrun{

# --------construct a function factory------------
library(ggplot2)
ggplot(aes(long, lat, group=group), data=map_data('usa')) + geom_path() +
  scale_x_continuous(labels=lon_coord) + scale_y_continuous(labels=lat_coord)

## define new formatter functions
lon_lbl <- function(x, ...) 
   coord_format(prefix=c(pos='East', zero='', neg='West'), 
                unit='deg', sep=' ', suffix='', ...)(x)
lat_lbl <- function(x, ...)
   coord_format(prefix=c(pos='North', zero='', neg='South'), 
                unit='deg', sep=' ', suffix='', ...)(x)
ggplot(aes(long, lat, group=group), data=map_data('usa')) + geom_path() +
  scale_x_continuous(labels=lon_lbl) + scale_y_continuous(labels=lat_lbl)
}
}
\seealso{
\code{\link{number_format}()} is a function factory to produce functions.
}
