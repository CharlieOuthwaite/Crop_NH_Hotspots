##%######################################################%##
#                                                          #
####          plotting for projection rasters           ####
#                                                          #
##%######################################################%##

rm(list = ls())

# load libraries
library(ggplot2)
library(maptools)
library(raster)

# directories
datadir <- "6_Projections/"


plotdir <- paste0(datadir, "Plots/")
if(!dir.exists(plotdir)) dir.create(plotdir)

# these are the files for the percentage change created above
files4plot <- list.files(paste0(datadir, "Projection_Rasters/"), pattern = "difference_to_ref")



#### ABUNDANCE PLOTS ####

for(i in 1:2){
  
  plotdat <- raster(paste0(datadir, "Projection_Rasters/", files4plot[i]))  
  
  
  # Need to aggregate to a coarser resolution for plotting, cannot allocate vector error.
  plotdat <- aggregate(plotdat, fact = 10)
  
  
  # # get natural habitat data to explore values
  # Jung2_RS <- raster(paste0(outdir, "/Jung2_NH_map_rescaled.tif"))
  # Jung2data <- as.data.frame(Jung2_RS, xy = T)
  # 
  # convert to dataframe for plotting
  rastdata <- as.data.frame(plotdat, xy = T)
  names(rastdata)[3] <- "layer"
  
  # remove NAs for plotting, makes it smaller
  rastdata <- rastdata[!is.na(rastdata$layer) ,]
  
  # plot map using appropriate axes (seem to be some very high values making it hard to visualise)
  
  brks <- c(-50, -25, 0, 10, 20)
  cols <- c("#CD0000", "#EE5C42", "#FFC125", "#66CD00", "#006400")
  
  
  # need country polygons
  map.world <- map_data('world')
  
  
  p1 <- ggplot() +
    geom_map(data=map.world, map=map.world,
             aes(x=long, y=lat, group=group, map_id=region),
             fill= NA, colour="grey", size=0.2) + 
    geom_tile(data = rastdata, aes(x = x, y = y, fill = layer)) + 
    theme_void() + 
    theme(panel.background = element_blank(), 
          panel.border = element_rect(colour = "grey", fill = NA), 
          axis.title = element_blank(),
          legend.position = "bottom") + 
    scale_fill_gradientn(colours = cols, values = scales::rescale(brks), limits = c(-50, 20)) + 
    labs(fill = "% change \nin abundance") + 
    ylim(c(-55, 90))
  
  
  ggsave(p1, filename = paste0(plotdir, "/PLOT_", sub(".tif", "", files4plot[i]), ".pdf"))
  
  
}




#### RICHNESS PLOTS ####

for(i in 3:4){
  
  plotdat <- raster(paste0(datadir, "Projection_Rasters/", files4plot[i]))  
  
  
  # Need to aggregate to a coarser resolution for plotting, cannot allocate vector error.
  plotdat <- aggregate(plotdat, fact = 10)
  
  
  
  # crop to country polygons?
  
  # crop to harvested area using the binary map?
  # use the cropland area mask as the base
  #crop_bin <- raster("1_PrepareCropLayers/CroplandBinary.tif")
  # crop map is at a 5x5km grid
  
  rastdata <- as.data.frame(plotdat, xy = T)
  names(rastdata)[3] <- "layer"
  rastdata <- rastdata[!is.na(rastdata$layer) ,]
  
  # plot map using appropriate axes (seem to be some very high values making it hard to visualise)
  
  brks <- c(-50, -25, 0, 10, 20)
  cols <- c("#CD0000", "#EE5C42", "#FFC125", "#66CD00", "#006400")
  
  
  # need country polygons
  map.world <- map_data('world')
  
  
  p1 <- ggplot() +
    geom_map(data=map.world, map=map.world,
             aes(x=long, y=lat, group=group, map_id=region),
             fill= NA, colour="grey", size=0.2) + 
    geom_tile(data = rastdata, aes(x = x, y = y, fill = layer)) + 
    theme_void() + 
    theme(panel.background = element_blank(), 
          panel.border = element_rect(colour = "grey", fill = NA), 
          axis.title = element_blank(),
          legend.position = "bottom") + 
    scale_fill_gradientn(colours = cols, values = scales::rescale(brks), limits = c(-50, 20)) + 
    labs(fill = "% change in\nspecies richness") + 
    ylim(c(-55, 90))
  
  
  ggsave(p1, filename = paste0(plotdir, "/PLOT_", sub(".tif", "", files4plot[i]), ".pdf"))
  
  
}


