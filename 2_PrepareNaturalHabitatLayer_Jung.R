
##%######################################################%##
#                                                          #
####           Preparing natural habitat data           ####
#                                                          #
##%######################################################%##

# this version of the script is using the Jung et al 2020 paper

rm(list = ls())

# load libraries
library(raster)
library(ggplot2)

# set directories
dataDir <- "0_data/"
dataDir2 <- "1_PrepareCropLayers/"
outDir <- "2_PrepareNaturalHabitatLayer/"
dir.create(outDir)

# take a look at the habitat files
files <- list.files(paste0(dataDir, "/lvl1_frac_1km_ver004/lvl1_frac_1km_ver004"))
files

# create a raster stack of the natural habitat layers
# 12 maps
hab_maps <- stack(paste0(dataDir, "/lvl1_frac_1km_ver004/lvl1_frac_1km_ver004/", files))

# have a little look
plot(hab_maps[[9]]) # wetlands includes large bodies of water so probably don't want to include that


# which are the NH ones in files: 1, 6,7,8
# (10 and 11 = rocky and desert?)
# remove the non-natural habitat ones
hab_maps <- hab_maps[[c(1,6,7,8)]]

names(hab_maps)

# information on Zenodo says the map values are fractional * 1000, 
# so dividing here to get the original values. 
hab_maps <- hab_maps/1000

# sum the rasters to get the total fractional cover of natural habitat
frac_NH <- sum(hab_maps, na.rm = T) 

plot(frac_NH)

# save the fractional NH map
writeRaster(x = frac_NH,filename = paste(outDir, "Fractional_NH_Jung.tif",sep=""), format="GTiff", overwrite = TRUE)


# frac_NH <- raster(paste(outDir, "Fractional_NH_Jung.tif",sep=""))


#### Just look at NH in area where crops grown ####



## use the binary crop map as a mask to just look at NH in areas where crops are grown

# load the binary crop map
CropDist <- raster(paste0(dataDir2, "CroplandBinary.tif"))


# when trying to reproject as below, get an error: 
# Error in if (maxy == miny) { : missing value where TRUE/FALSE needed
# if you cut the edges this error does not occur
# https://gis.stackexchange.com/questions/220589/error-using-projectraster-in-r-error-in-if-maxy-miny-missing-value-whe
extent(CropDist) <- c(xmin= -17372530, xmax= 17372470, ymin= 0.99*(-6357770), ymax= 0.99*(7347230))

# try cropping the extent rather than the above
b <- extent(-17372530, 17372470,  0.99*(-6357770), 0.99*(7347230))

# reproject the binary crop map to match up with the NH map
CropDist_reproj <- projectRaster(CropDist, frac_NH)


# creates a map of the natural habitat values in just the areas of cropland
NatHabCrop <- raster::mask(x = frac_NH, mask = CropDist_reproj)


# save the NH in cropland map
writeRaster(x = NatHabCrop,filename = paste(outDir, "NH_Cropland_Area_Jung.tif",sep=""), format="GTiff")



### create a figure of NH in cropland values that are binned. ###

# need to reproject for mapping with country polygons
crs(NatHabCrop)


### create a plot with binned values for prop NH ###

# aggregating for faster plotting
NatHabCrop2 <- aggregate(NatHabCrop, fact = 10)

# convert to data frame for use with ggplot
plot_data <- as.data.frame(NatHabCrop2, xy = TRUE)

# remove NAs
plot_data <- plot_data[!is.na(plot_data$Fractional_NH_Jung), ]

# multiply by 10 to get actual % values
plot_data$Fractional_NH_Jung <- plot_data$Fractional_NH_Jung*100

# organise the percNH info into bins
plot_data$bin <- NA

plot_data[plot_data$Fractional_NH_Jung >= 0 & plot_data$Fractional_NH_Jung <= 20, 'bin'] <- "0 - 20%"
plot_data[plot_data$Fractional_NH_Jung > 20 & plot_data$Fractional_NH_Jung <= 40, 'bin'] <- "21 - 40%"
plot_data[plot_data$Fractional_NH_Jung > 40 & plot_data$Fractional_NH_Jung <= 60, 'bin'] <- "41 - 60%"
plot_data[plot_data$Fractional_NH_Jung > 60 & plot_data$Fractional_NH_Jung <= 80, 'bin'] <- "61 - 80%"
plot_data[plot_data$Fractional_NH_Jung > 80, 'bin'] <- "81 - 100%"

plot_data$bin <- as.factor(plot_data$bin)


# load world map outline
map.world <- map_data("world")

#library(maptools)
#data(wrld_simpl)


# plot
ggplot() +
  geom_tile(data = plot_data, aes(x = x, y = y, fill = bin), na.rm = TRUE, alpha = 0.7) +
  scale_fill_manual(values = c("#EE0000", "#EE7600", "#EEC900", "#66CD00", "#458B00")) +
  geom_map(data=map.world, map=map.world,
           aes(x=long, y=lat, group=group, map_id=region),
           fill= "transparent", colour="lightgrey", size=0.2) +
  theme_bw() +
  theme(axis.title = element_blank(), 
        plot.background = element_blank(), 
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom")  

ggsave(filename = paste0(outDir, "/Fugure1_Map_crop_NH_Jung.pdf"), height = 6, width = 8)



