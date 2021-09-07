# Packages -----------------------------------------------------------

require(raster)
require(dplyr)
require(rgdal)
require(rio)

# Sources_Dependencies ----------------------------------------------------

source("functions.R")
load("DatesUrlsNgdc.Rdata") # Has Polygons

# Code ---------------------------------------------------

## Polygons ----------------------------------------------

PathFile="./Limite_Favelas_2016/Limite_Favelas_2016.shp"

shape=readOGR(PathFile)

polygons=list()

for ( i in 1: length(shape))
{
  polygons[i]=shape[i,]
}

## FinalData -------------------------------------------------

#urlTest=url4[62:63] ## To test

#dataf=rnight_incio_loop(urlTest) ## to test

dataf=rnight_incio_loop(url4)
export(dataf, "PolygonsLights.xlsx")

