% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{gisco_countrycode}
\alias{gisco_countrycode}
\title{Data frame with different country code schemes and world regions}
\format{
A data frame object with 249 rows and 13 variables:
\describe{
\item{ISO3_CODE}{Eurostat code of each country.}
\item{CNTR_CODE}{ISO 3166-1 alpha-2 code of each country.}
\item{iso2c}{ISO 3166-1 alpha-3 code of each country.}
\item{iso.name.en}{ISO English short name.}
\item{cldr.short.en}{English short name as provided by the Unicode Common
Locale Data Repository.}
\item{continent}{As provided by the World Bank.}
\item{un.region.code}{Numeric region code UN (M49).}
\item{un.region.name}{Region name UN (M49).}
\item{un.regionintermediate.code}{Numeric intermediate Region.}
\item{un.regionintermediate.name}{Intermediate Region name UN (M49).}
\item{un.regionsub.code}{Numeric sub-region code UN (M49).}
\item{un.regionsub.name}{Sub-Region name UN (M49).}
\item{eu}{Logical indicating if the country belongs to the European Union.}
}
}
\source{
\link[countrycode:codelist]{countrycode::codelist} \strong{v1.2.0}.
}
\description{
A data frame containing conversions between different country
code schemes (Eurostat/ISO2 and 3) as well as geographic regions as
provided by the World Bank and the UN (M49). This data set
is extracted from \CRANpkg{countrycode} package.
}
\examples{

data("gisco_countrycode")
dplyr::glimpse(gisco_countrycode)

}
\seealso{
\code{\link[=gisco_get_countries]{gisco_get_countries()}} and \link[countrycode:codelist]{countrycode::codelist}, included in
\CRANpkg{countrycode}.

See also the \href{https://cldr.unicode.org/translation/displaynames/countryregion-territory-names}{Unicode Common Locale Data Repository}.

Other dataset: 
\code{\link{gisco_coastallines}},
\code{\link{gisco_countries}},
\code{\link{gisco_db}},
\code{\link{gisco_nuts}}
}
\concept{dataset}
