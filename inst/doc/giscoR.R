## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

knitr::knit_hooks$set(margin = function(before, options, envir){
  if (before){
    par(mar=c(0.1,0.1,1.3,0.1))
  } 
})


## ----attributions-------------------------------------------------------------
library(giscoR)
gisco_attributions(copyright = TRUE)


## ----country_reg, fig.width=7, fig.asp=1--------------------------------------

library(sf)

countries <- gisco_get_countries(region = "Asia")

plot(st_geometry(countries), axes = TRUE)
title(sub = gisco_attributions(copyright = "FALSE"), cex.sub = 0.7)

## ----africa_north, fig.width=7, fig.asp=1-------------------------------------

africa_north <-
  gisco_get_countries(
    country = c("Argelia", "Morocco", "Egipto", "Tunisia", "Libia"),
    resolution = "20",
    epsg = "4326",
    year = "2016"
  )

# Coastal lines

coast <- gisco_get_coastallines(resolution = "20",
                                epsg = "4326",
                                year = "2016")

plot(
  st_geometry(africa_north),
  axes = TRUE,
  col = "grey30",
  border = "grey80"
)
plot(st_geometry(coast), lwd = 2, add = TRUE)
title(sub = gisco_attributions(), cex.sub = 0.7)

## ----giscoR_eurostat, fig.width=6.5,fig.asp=0.9, message=FALSE, warning=FALSE----


library(cartography)
library(colorspace)

# Northen Europe

neur <-
  gisco_countrycode[gisco_countrycode$un.regionsub.name == "Northern Europe", ]

iso3 <- neur[!is.na(neur$CNTR_CODE),]$ISO3_CODE

# Add some more countries

iso3 <-
  c(iso3, "POL", "DEU", "AUT", 
  "CZE", "BEL", "NLD", "LUX", 
  "SVK", "HUN", "ROU")

nuts2 <- gisco_get_nuts(
  year = "2016",
  epsg = "4326",
  resolution = "20",
  nuts_level = "2",
  country = iso3
)

#Borders
borders <- gisco_get_countries(
  epsg = "4326",
  year = "2016",
  resolution = "20",
  region = c("Europe", "Africa")
)

# Eurostat data - Purchase parity power
pps <- giscoR::tgs00026
pps <- pps[pps$time == 2016, ]

nuts2.sf <- merge(nuts2,
                  pps,
                  by.x = "NUTS_ID",
                  by.y = "geo",
                  all.x = TRUE)

# Prepare mapping
br <-
  1000 * c(0,
           10,
           12.5,
           15,
           17.5,
           20,
           22.5,
           25,
           max(nuts2.sf$values, na.rm = TRUE) / 1000 + 1)

# Palette
pal <- sequential_hcl(
  n = (length(br) - 1),
  pal = "Inferno",
  alpha = 0.8
)


opar <- par(no.readonly = TRUE)
par(mar = c(2, 2, 2, 2))

# Basemap
plot(st_geometry(nuts2), col = "grey80")

# Surrounding countries
plot(
  st_geometry(borders),
  border = "black",
  lwd = 3,
  col = "grey80",
  add = TRUE
)

choroLayer(
  nuts2.sf,
  var = "values",
  border = "grey50",
  breaks = br,
  col = pal,
  legend.pos = "n",
  colNA = "grey80",
  add = TRUE
)
plot(st_geometry(borders),
     border = "black",
     col = NA,
     add = TRUE)

att <- gisco_attributions()

legendChoro(
  title.txt = NA,
  breaks = paste0(br / 1000, "K EUR"),
  col = pal,
  nodata.col = "grey80",
  frame = TRUE
)
layoutLayer(
  "Purchase Parity Power, NUTS 2 regions of selected countries (2016)",
  scale = FALSE,
  col = pal[3],
  sources = att
)
par(opar)

