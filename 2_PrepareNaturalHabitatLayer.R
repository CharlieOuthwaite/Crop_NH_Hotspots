##%######################################################%##
#                                                          #
####           Preparing natural habitat data           ####
#                                                          #
##%######################################################%##

rm(list = ls())

# load libraries
library(raster)

# set directories
dataDir <- "0_data/"
dataDir2 <- "1_PrepareCropLayers/"
outDir <- "2_PrepareNaturalHabitatLayer/"
dir.create(outDir)

# read in teh fractional natural habitat data
NatHab <- raster(paste0(dataDir, "PercentNatural.tif"))

# take a look
plot(NatHab)

# load the binary crop map
CropDist <- raster(paste0(dataDir2, "CroplandBinary.tif"))

# match extents (?)
NatHab <- raster::extend(NatHab, CropDist)

# creates a map of the natural habitat values in just the areas of cropland
NatHabCrop <- raster::mask(x = NatHab, mask = CropDist)

# take a look 
plot(NatHabCrop)

# save the NH in cropland map
writeRaster(x = NatHabCrop,filename = paste(outDir, "NH_Cropland_Area.tif",sep=""), format="GTiff")

