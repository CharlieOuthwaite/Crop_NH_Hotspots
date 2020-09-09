##%######################################################%##
#                                                          #
####           Preparing natural habitat data           ####
#                                                          #
##%######################################################%##

rm(list = ls())

# load libraries
library(raster)
library(ggplot2)

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


# create a figure of NH in cropland values that are binned.

# need to reproject for mapping with country polygons
crs(NatHabCrop)

# desireg CRS
wgs84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# cropping slightly as get a reprojection error otherwise
NatHabCrop_proj <- NatHabCrop
extent(NatHabCrop_proj) <- c(xmin= -17372530, xmax= 17372470, ymin= 0.99*(-6357770), ymax= 0.99*(7347230))

# reproject to desired crs
NatHabCrop_proj <- projectRaster(from = NatHabCrop_proj, crs = wgs84, res = 0.0518)

#plot(NatHabCrop_proj)

# aggregating for faster plotting
NatHabCrop2 <- aggregate(NatHabCrop_proj, fact = 2)

# convert to data frame for use with ggplot
plot_data <- as.data.frame(NatHabCrop2, xy = TRUE)

# remove NAs
plot_data <- plot_data[!is.na(plot_data$PercentNatural), ]

# divide by 10 to get actual % values
plot_data$PercentNatural <- plot_data$PercentNatural/10

# organise the percNH info into bins
plot_data$bin <- NA

plot_data[plot_data$PercentNatural >= 0 & plot_data$PercentNatural <= 20, 'bin'] <- "0 - 20%"
plot_data[plot_data$PercentNatural > 20 & plot_data$PercentNatural <= 40, 'bin'] <- "21 - 40%"
plot_data[plot_data$PercentNatural > 40 & plot_data$PercentNatural <= 60, 'bin'] <- "41 - 60%"
plot_data[plot_data$PercentNatural > 60 & plot_data$PercentNatural <= 80, 'bin'] <- "61 - 80%"
plot_data[plot_data$PercentNatural > 80, 'bin'] <- "81 - 100%"

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

ggsave(filename = paste0(outDir, "/Map_crop_NH.pdf"), height = 5, width = 7)


