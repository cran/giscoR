% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gisco_data.R
\docType{data}
\encoding{UTF-8}
\name{gisco_nuts}
\alias{gisco_nuts}
\title{All NUTS \code{POLYGON} object}
\format{
A \code{POLYGON} data frame
(resolution: 1:20million, EPSG:4326) object with 11 variables:
\describe{
  \item{id}{row ID}
  \item{COAST_TYPE}{COAST_TYPE}
  \item{MOUNT_TYPE}{MOUNT_TYPE}
  \item{NAME_LATN}{Name on Latin characters}
  \item{CNTR_CODE}{Eurostat Country code}
  \item{FID}{FID}
  \item{NUTS_ID}{NUTS identifier}
  \item{NUTS_NAME}{NUTS name on local alphabet}
  \item{LEVL_CODE}{NUTS level code (0,1,2,3)}
  \item{URBN_TYPE}{URBN_TYPE}
  \item{geometry}{geometry field}
}
}
\source{
\href{https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_20M_2016_4326.geojson}{GISCO .geojson source}
}
\description{
A \code{sf} object including all
NUTS levels as provided by GISCO (2016 version).
}
\examples{
library(sf)

nuts <- gisco_nuts

italy <- subset(nuts, CNTR_CODE == "IT" & LEVL_CODE == 3)

plot(st_geometry(italy), col = c("springgreen4", "ivory", "red2"))
title(
  sub = gisco_attributions(),
  line = 1,
  cex.sub = 0.7,
  font.sub = 3
)
}
\seealso{
\link{gisco_get_nuts}
}
