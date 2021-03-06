% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gisco_get.R
\name{gisco_get}
\alias{gisco_get}
\alias{gisco_get_coastallines}
\alias{gisco_get_communes}
\alias{gisco_get_countries}
\alias{gisco_get_lau}
\alias{gisco_get_nuts}
\alias{gisco_get_urban_audit}
\title{Get geospatial data from GISCO API}
\source{
\href{https://gisco-services.ec.europa.eu/distribution/v2/}{GISCO API}
}
\usage{
gisco_get_coastallines(
  year = "2016",
  epsg = "4326",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE,
  resolution = "20"
)

gisco_get_communes(
  year = "2016",
  epsg = "4326",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE,
  spatialtype = "RG",
  country = NULL
)

gisco_get_countries(
  year = "2016",
  epsg = "4326",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE,
  resolution = "20",
  spatialtype = "RG",
  country = NULL,
  region = NULL
)

gisco_get_lau(
  year = "2016",
  epsg = "4326",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE,
  country = NULL,
  gisco_id = NULL
)

gisco_get_nuts(
  year = "2016",
  epsg = "4326",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE,
  resolution = "20",
  spatialtype = "RG",
  country = NULL,
  nuts_id = NULL,
  nuts_level = "all"
)

gisco_get_urban_audit(
  year = "2020",
  epsg = "4326",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE,
  spatialtype = "RG",
  country = NULL,
  level = NULL
)
}
\arguments{
\item{year}{Release year. See Details.}

\item{epsg}{projection of the map:
4-digit \href{https://epsg.io/}{EPSG code}. One of:
\itemize{
\item \code{"4326"} - \href{https://epsg.io/4326}{WGS84}
\item \code{"3035"} - \href{https://epsg.io/3035}{ETRS89 / ETRS-LAEA}
\item \code{"3857"} - \href{https://epsg.io/3857}{Pseudo-Mercator}
}}

\item{cache}{a logical whether to do caching. Default is \code{TRUE}.}

\item{update_cache}{a logical whether
to update cache. Default is \code{FALSE}. When set
to \code{TRUE} it would force a fresh download
of the source \code{.geojson} file.}

\item{cache_dir}{a path to a cache directory.
The directory have to exist.  The \code{NULL} (default) uses
and creates \code{/gisco} directory in the temporary directory
from \code{\link{tempdir}}. The directory can also be
set with \code{options(gisco_cache_dir = "path/to/dir")}.}

\item{verbose}{Display information. Useful
for debugging, default if \code{FALSE}.}

\item{resolution}{Resolution of the geospatial data. One of
\itemize{
   \item \code{"60"} (1:60million),
   \item \code{"20"} (1:20million)
   \item \code{"10"} (1:10million)
   \item \code{"03"} (1:3million) or
   \item \code{"01"} (1:1million).
   }}

\item{spatialtype}{Type of geometry to be returned:
\itemize{
 \item \code{"RG"}: Regions - \code{MULTIPOLYGON/POLYGON} object.
 \item \code{"LB"}: Labels - \code{POINT} object.
 \item \code{"BN"}: Boundaries - \code{LINESTRING} object.
 \item \code{"COASTL"}: coastlines - \code{LINESTRING} object.
 \item \code{"INLAND"}: inland boundaries - \code{LINESTRING} object.
}}

\item{country}{Optional. A character vector of country codes. See Details.}

\item{region}{Optional. A character vector of UN M49 region codes.
Possible values are "Africa", "Americas", "Asia", "Europe", "Oceania".
 See Details and \link{gisco_countrycode}}

\item{gisco_id}{Optional. A character vector of GISCO_ID LAU values.}

\item{nuts_id}{Optional. A character vector of NUTS IDs.}

\item{nuts_level}{NUTS level. One of \code{"0"} (Country-level),
\code{"1", "2"} or \code{"3"}.
See \url{https://ec.europa.eu/eurostat/web/nuts/background}.}

\item{level}{Level of Urban Audit. Possible values are
\code{"CITIES", "FUA", "GREATER_CITIES"} or \code{NULL}.
\code{NULL} would download the full dataset.}
}
\value{
\code{gisco_get_coastallines} returns a \code{POLYGON} object.

\code{gisco_get_lau} returns a \code{POLYGON} object.
}
\description{
Loads a simple feature (\code{sf})
object from GISCO API entry point or your local library.
}
\details{
\code{country} only available on
specific datasets. Some \code{spatialtype} options
(as \code{BN, COASTL, INLAND}) may not present country-level identifies.

\code{country} could be either a vector of country names, a vector of ISO3
country codes or a vector of Eurostat country codes. Mixed types
(as \code{c("Turkey","US","FRA")}) would not work.

Sometimes cached files may be corrupt. On that case, try re-downloading
the data setting \code{update_cache = TRUE}.

If you experience any problem on download, try to download
the corresponding \code{.geojson} file by any other method and
set \code{cache_dir = "path/to/dir"} or
\code{options(gisco_cache_dir = "path/to/dir)"}.

For a complete list of files available check
\link{gisco_db}.

\strong{Release years available}

\code{gisco_get_coastallines}: one of
\code{"2006", "2010", "2013"} or \code{"2016"}.

\code{gisco_get_communes}: one of
\code{"2001", "2004", "2006", "2008", "2010", "2013"} or \code{"2016"}.

\code{gisco_get_countries}:
one of \code{"2001", "2006", "2010", "2013", "2016"} or \code{"2020"}.

\code{gisco_get_lau}: one of \code{"2016", "2017", "2018"} or \code{"2019"}.

\code{gisco_get_nuts}: one of
\code{"2003", "2006", "2010", "2013", "2016"} or \code{"2021"}.

\code{gisco_get_urban_audit}:
one of \code{"2001", "2004", "2014", "2018"} or \code{"2020"}.
}
\note{
Please check the download and usage provisions
on \link{gisco_attributions}.
}
\examples{
library(sf)

##################################
# Example - gisco_get_coastallines
##################################

coastlines <- gisco_get_coastallines()
plot(st_geometry(coastlines), col = "palegreen", border = "lightblue3")
title(main = "Coastal Lines",
      sub = gisco_attributions(),
      line = 1)


###############################
# Example - gisco_get_countries
###############################

sf_world <- gisco_get_countries()
plot(st_geometry(sf_world), col = "seagreen2")
title(sub = gisco_attributions(), line = 1)


sf_africa <- gisco_get_countries(region = 'Africa')
plot(st_geometry(sf_africa),
     col = c("springgreen4", "darkgoldenrod1", "red2"))
title(sub = gisco_attributions(), line = 1)

sf_benelux <-
  gisco_get_countries(country = c('Belgium', 'Netherlands', 'Luxembourg'))
plot(st_geometry(sf_benelux),
     col = c("grey10", "orange", "deepskyblue2"))
title(sub = gisco_attributions(), line = 1)

##########################
# Example - gisco_get_nuts
##########################

nuts1 <- gisco_get_nuts(
  resolution = "20",
  year = "2016",
  epsg = "4326",
  nuts_level = "1",
  country = "ITA"
)
nuts2 <- gisco_get_nuts(
  resolution = "20",
  year = "2016",
  epsg = "4326",
  nuts_level = "2",
  country = "ITA"
)
nuts3 <- gisco_get_nuts(
  resolution = "20",
  year = "2016",
  epsg = "4326",
  nuts_level = "3",
  country = "ITA"
)

plot(st_geometry(nuts3),
     border = "grey60",
     lty = 3)

plot(st_geometry(nuts2),
     lwd = 2,
     border = "red2",
     add = TRUE)

plot(st_geometry(nuts1),
     lwd = 3,
     border = "springgreen4",
     add = TRUE)

box()
title(
  main = "NUTS Levels on Italy",
  sub = gisco_attributions(),
  cex.sub = 0.7,
  line = 1
)
legend(
  "topright",
  legend = c("NUTS 1", "NUTS 2", "NUTS 3"),
  col = c("springgreen4", "red2", "grey60"),
  lty = c(1, 1, 3),
  lwd = c(3, 2, 1),
  bty = "n",
  y.intersp = 2
)
}
\seealso{
\link{gisco_db}, \link{gisco_attributions},
\link{gisco_coastallines}

\link{gisco_countrycode}, \link{gisco_countries}

\link{gisco_nuts}
}
\author{
dieghernan, \url{https://github.com/dieghernan/}
}
