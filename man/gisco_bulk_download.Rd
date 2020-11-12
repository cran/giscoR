% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gisco_bulk_download.R
\name{gisco_bulk_download}
\alias{gisco_bulk_download}
\title{Bulk download from GISCO API}
\source{
\href{https://gisco-services.ec.europa.eu/distribution/v2/}{GISCO API}
}
\usage{
gisco_bulk_download(
  id_giscoR = "countries",
  year = "2016",
  cache_dir = NULL,
  update_cache = FALSE,
  verbose = FALSE,
  resolution = "10",
  ext = "geojson",
  recursive = TRUE
)
}
\arguments{
\item{id_giscoR}{Type of dataset to be downloaded. Values supported are
\code{"coastallines","communes","countries","lau","nuts","urban_audit"}.}

\item{year, cache_dir, update_cache, verbose, resolution}{See \link{gisco_get} for details.}

\item{ext}{Extension of the file(s) to be downloaded. Available formats are
\code{"geojson", "shp", "svg", "json", "gdb"}. See Details.}

\item{recursive}{Tries to unzip recursively the zip files (if any) included
in the initial bulk download (case of \code{ext = "shp"}).}
}
\value{
Silent function.
}
\description{
Downloads zipped data from GISCO and extract them on the
\code{cache_dir} folder.
}
\details{
The usual extension used across \pkg{giscoR} is \code{geojson},
however other formats are already available on GISCO.

This function helps building a personal shape library on \code{cache_dir}
(or \code{options(gisco_cache_dir = "path/to/dir")}, if set by the user).
}
\note{
For downloading specific files use \link{gisco_get} functions.
}
\examples{
\dontrun{
# This example would populate your cache_dir with a selection of geojson files
# Set options(gisco_cache_dir = "path/to/dir") first
# It may take a couple of minutes

# Countries 2016
gisco_bulk_download(id_giscoR = "countries", resolution = "60", verbose = TRUE)
gisco_bulk_download(id_giscoR = "countries", resolution = "20")
gisco_bulk_download(id_giscoR = "countries", resolution = "10")
gisco_bulk_download(id_giscoR = "countries", resolution = "03")


# NUTS 2016
gisco_bulk_download(id_giscoR = "nuts", resolution = "60")
gisco_bulk_download(id_giscoR = "nuts", resolution = "20")
gisco_bulk_download(id_giscoR = "nuts", resolution = "10")
gisco_bulk_download(id_giscoR = "nuts", resolution = "03")

# NUTS 2021
gisco_bulk_download(id_giscoR = "nuts", resolution = "60", year = "2021")
gisco_bulk_download(id_giscoR = "nuts", resolution = "20", year = "2021")
gisco_bulk_download(id_giscoR = "nuts", resolution = "10", year = "2021")
gisco_bulk_download(id_giscoR = "nuts", resolution = "03", year = "2021")
 }
}