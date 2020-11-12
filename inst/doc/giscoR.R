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
pps <- pps[pps$time == 2016,]

nuts2.sf <- merge(nuts2,
                  pps,
                  by.x = "NUTS_ID",
                  by.y = "geo",
                  all.x = TRUE)

# Prepare mapping
br <- getBreaks(nuts2.sf$values, method = "fisher")
pal <-
  hcl.colors(n = (length(br) - 1),
             palette = "inferno",
             alpha = 0.75)

# Plot
opar <- par(no.readonly = TRUE)
par(mar = c(2, 2, 2, 2))

# Basemap - BENELUX
plot(
  st_geometry(nuts2),
  col = "grey80",
  xlim = c(1, 9),
  ylim = c(47, 55)
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

# Add borders
plot(st_geometry(borders),
     border = "black",
     col = NA,
     add = TRUE)


labs <- paste0(round((br / 1000), 1), "K EUR")
labs[1] <- ""
labs[length(labs)] <- ""

legendChoro(
  pos = "bottomright",
  title.txt = NA,
  breaks = labs,
  col = pal,
  nodata.col = "grey80",
  frame = TRUE
)

layoutLayer(
  "Disposable Incoming Households 2016 (BENELUX focus)",
  scale = FALSE,
  col = pal[3],
  sources = gisco_attributions()
)
par(opar)



