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

options(gisco_cache_dir = "./devel/")


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
  border = "grey50"
)
plot(st_geometry(coast), border = "grey50", add = TRUE)
title(sub = gisco_attributions(), cex.sub = 0.7)

## ----giscoR_eurostat, fig.width=6.5,fig.asp=0.9, message=FALSE, warning=FALSE----
library(eurostat)
library(cartography)
library(colorspace)

nuts2 <- gisco_get_nuts(
  year = "2016",
  nuts_level = "2",
  country = "Italy"
)


#Borders
borders <- gisco_get_nuts(
  nuts_level = "0",
  year = "2016"
)

# Eurostat data - Purchase parity power
pps <- eurostat::tgs00026
pps <- pps[grep("2016", pps$time),]

nuts2.sf <- merge(nuts2,
                  pps,
                  by.x = "NUTS_ID",
                  by.y = "geo",
                  all.x = TRUE)

# Prepare mapping
br <- getBreaks(nuts2.sf$values, method = "pretty")

# Palette
pal <- sequential_hcl(n = (length(br) - 1),
                      pal = "Inferno",
                      rev = TRUE)


opar <- par(no.readonly = TRUE)
par(mar = c(2, 2, 2, 2))

choroLayer(
  nuts2.sf,
  var = "values",
  border = "grey60",
  breaks = br,
  col = pal,
  legend.pos = "n",
  colNA = "grey80"
)
plot(st_geometry(borders),
     border = "black",
     col = NA,
     add = TRUE)
     
att <- paste0("Data extracted from package eurostat \n",
              gisco_attributions(copyright = FALSE))

legendChoro(
  title.txt = NA,
  breaks = paste0(br / 1000, "K EUR"),
  col = pal,
  nodata.col = "grey80",
  frame = TRUE
)
layoutLayer("Purchase Parity Power, Italy NUTS 2 regions (2016)",
            scale = FALSE,
            col = pal[3],
            sources = att)
par(opar)


