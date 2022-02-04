##%######################################################%##
#                                                          #
####            Preparing global crop layers            ####
#                                                          #
##%######################################################%##

# This script reads in a layer of crop data (need to check where this is from,
# think Tim sent this script to me) and creates a binary map of cells that contain 
# cropland  or not. 

# Start with 1km map, aggregae to a 5km map - once using mean, then once using max
# create a binary map, threshold of 200 used to set binary values (what is the unit of this dataset?)


rm(list = ls())

# load required libraries
library(raster)
library(rgdal)

# set directories
dataDir <- "0_data/"
outDir <- "1_PrepareCropLayers/"
dir.create(outDir)

# Behrmann equal area projection (units in metres)
behrCRS <- CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs')

# read in crop map
crp <- readGDAL(paste(dataDir,"crp_1km_int",sep=""), silent = TRUE)

# convert to raster
crp <- raster(crp)

# take a look
# plot(crp)

# reproject to Behrmann (~ 1km at equator)
crp <- projectRaster(from = crp,res = 1000,crs = behrCRS)

# reduce resolution of the map by a factor of 5, taking mean across cells (5km X 5km)
crp10 <- raster::aggregate(crp,fact=5,fun=mean)

# reduce resolution of the map by a factor of 5, taking max across cells (5km X 5km)
crpMax <- raster::aggregate(crp,fact=5,fun=max)

## create a binary map of cropland area
crpBin <- crpMax
values(crpBin) <- NA
values(crpBin)[values(crpMax)>200] <- 1 # threshold of 200 used to set binary values (what is the unit of this dataset?)

# take a look
plot(crpBin)

#save the lower resolution crop map
writeRaster(x = crp10,filename = paste(outDir,"CroplandCover.tif",sep=""),format="GTiff")

# save the binary map
writeRaster(x = crpBin,filename = paste(outDir,"CroplandBinary.tif",sep=""),format="GTiff")
