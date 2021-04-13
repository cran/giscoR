## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 6, 
  fig.height = 4,
  out.width = "100%",
  out.height = "60%"
)


## ----attributions-------------------------------------------------------------
library(giscoR)
gisco_attributions(copyright = TRUE)


## ----country_reg--------------------------------------------------------------

library(sf)
library(tmap) # Use tmap for plotting


countries <- gisco_get_countries(region = "Asia")

tmap_style("classic")

tm_shape(countries) +
  tm_graticules(lines = FALSE) +
  tm_polygons() +
  tm_credits(gisco_attributions(copyright = "FALSE")) +
  tm_layout(main.title = "Countries of Asia",
            attr.outside = TRUE)

tmap_options_reset()


## ----africa_north-------------------------------------------------------------

africa_north <-
  gisco_get_countries(
    country = c("Morocco", "Argelia", "Libia", "Tunisia", "Egypt"),
    resolution = "20",
    epsg = "4326",
    year = "2016"
  )

# Coastal lines

coast <- gisco_get_coastallines(resolution = "20",
                                epsg = "4326",
                                year = "2016")

# Plot

# Coastline
tm_shape(coast, bbox = c(-13, 18.5, 37, 40)) +
  tm_fill(col = "grey80")  +
  # Shape of Africa
  tm_shape(africa_north) +
  tm_polygons(col = "grey30", border.col = "white")  +
  # Facets
  tm_facets(by = "NAME_ENGL") +
  tm_credits(gisco_attributions(copyright = "FALSE"),
             position = c("right", "bottom")) +
  tm_layout(attr.outside = TRUE)


## ----giscoR_eurostat----------------------------------------------------------


nuts2 <- gisco_get_nuts(
  year = "2016",
  epsg = "4326",
  resolution = "20",
  nuts_level = "2"
)

#Borders
borders <- gisco_get_countries(
  epsg = "4326",
  year = "2016",
  resolution = "20",
  region = c("Europe")
)

# Eurostat data - Purchase parity power
pps <- giscoR::tgs00026
pps <- pps[pps$time == 2016, ]

nuts2.sf <- merge(nuts2,
                  pps,
                  by.x = "NUTS_ID",
                  by.y = "geo",
                  all.x = TRUE)
# Plot

tm_shape(nuts2.sf, bbox = c(-1, 47, 10, 54)) +
  tm_polygons(
    "values",
    pal = "-inferno",
    alpha = 0.6,
    style = "headtails",
    style.args = list(thr = 1),
    showNA = FALSE,
    border.alpha = 0.5,
    border.col = "grey50",
    title = "Euro (EUR)"
  ) +
  # Add borders
  tm_shape(borders) +
  tm_borders(col = "black") +
  # Credits and format
  tm_credits(gisco_attributions(),
             position = c("right", "bottom")) +
  tm_layout(
    main.title = "Disposable Incoming Households 2016 (BENELUX Focus)",
    main.title.size = 0.8,
    main.title.fontface = "bold",
    legend.outside = TRUE,
    attr.outside = TRUE
  )


## ----session_info, echo=FALSE-------------------------------------------------
sessionInfo()

