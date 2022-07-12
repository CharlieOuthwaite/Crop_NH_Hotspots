##%######################################################%##
#                                                          #
####                    Projections                     ####
#                                                          #
##%######################################################%##

# here, create a projection matrix covering global cropland area
# include Land use and use intensity and natural habitat availability.
# test using Jung2 and Jung4 versions. 


rm(list = ls())

# libraries
library(raster)
library(predictsFunctions)
library(StatisticalModels)
library(ggplot2)
library(cowplot)


# directories
datadir <- "2_PrepareNaturalHabitatLayer/"
outdir <- "6_Projections/"
if(!dir.exists(outdir)) dir.create(outdir)

# files from Tim for land use use intensity combo in here
proj_data <- "Z:/Datasets/LandUseIntensity_globalmap_fromTim/02Projections_02ProjectLUIntensity"

# first 3 files are cropland Int Lt Min, are these proportions of area?
NH_projs <- list.files(proj_data, pattern = "raster.tif$")


# read in the rasters
cr_int <- raster(paste0(proj_data, "/", NH_projs[1]))
cr_lt <- raster(paste0(proj_data, "/", NH_projs[2]))
cr_min <- raster(paste0(proj_data, "/", NH_projs[3]))

# take a look
# plot(cr_int)
# plot(cr__lt)
# plot(cr_min)


# read in the fractional natural habitat data
Jung4 <- raster(paste0(datadir, "/NH_Cropland_Area_Jung_four.tif"))
Jung2 <- raster(paste0(datadir, "/NH_Cropland_Area_Jung_two.tif"))

# use the cropland area mask as the base
crop_bin <- raster("1_PrepareCropLayers/CroplandBinary.tif")
# crop map is at a 5x5km grid


# I'm not sure what the best approach is for projecting this. Use the binary map for cropland
# area and then aggregate the Jung data up to that resolution, or use Tim's cropland maps and
# try to match the resolutions (they are slightly off)

# try to use the finer resolution maps (Tim's cropland and Jung) first.

# only need presence of cropland, so try to make the crp map the same res as the Jung data

# resample to get them in the same resolution and extent
cr_int_resamp <- projectRaster(cr_int, Jung4, method = 'bilinear')
cr_lt_resamp <- projectRaster(cr_lt, Jung4, method = 'bilinear')
cr_min_resamp <- projectRaster(cr_min, Jung4, method = 'bilinear')


# save the resampled versions
writeRaster(cr_int_resamp, filename = paste0(outdir, "/resampled_crp_int_layer.tif"), format = "GTiff")
writeRaster(cr_lt_resamp, filename = paste0(outdir, "/resampled_crp_lt_layer.tif"), format = "GTiff")
writeRaster(cr_min_resamp, filename = paste0(outdir, "/resampled_crp_min_layer.tif"), format = "GTiff")

# load in if not already
# cr_int_resamp <- raster(paste0(outdir, "/resampled_crp_int_layer.tif"))
# cr_lt_resamp <- raster(paste0(outdir, "/resampled_crp_lt_layer.tif"))
# cr_min_resamp <- raster(paste0(outdir, "/resampled_crp_min_layer.tif"))
# 





##%######################################################%##
#                                                          #
####                     ALL MODELS                     ####
#                                                          #
##%######################################################%##


# read in models
# load("5_Models/Richness_Jung2_Tropical.rdata")
# load("5_Models/Richness_Jung4_Tropical.rdata")
# load("5_Models/Richness_Jung2_Temperate.rdata")
# load("5_Models/Richness_Jung4_Temperate.rdata")
# load("5_Models/Abundance_Jung2_Tropical.rdata")
# load("5_Models/Abundance_Jung4_Tropical.rdata")
# load("5_Models/Abundance_Jung2_Temperate.rdata")
# load("5_Models/Abundance_Jung4_Temperate.rdata")


# result <- intercept + 
#   cropland_min * coeef LU:UI cropmin +
#   cropland_lt * coeff LU:UI croplt +
#   cropland_int * coeff LU:UI cropIn +
#   NH * coeff NH +
#   coeff LU:NH *  cropland (binary map?) * NH + 
#   UI interaction with NH?

# also realm?
  
#### Rescale the NH data ####

#### Projections 1: Abundance, Jung4 ####

## tropical model
load("5_Models/Abundance_Jung4_Tropical.rdata") # ab1.trop
summary(ab1.trop$model)
#LogAbun ~ Predominant_land_use + Use_intensity + poly(percNH_Jung4_RS, 1) +
# Predominant_land_use:poly(percNH_Jung4_RS, 1) + Use_intensity:poly(percNH_Jung4_RS, 1) + Predominant_land_use:Use_intensity + 
# (1 | SS) + (1 | SSB)

ab1.trop_coefs <- fixef(ab1.trop$model)


proj_ab_trop <- ab1.trop_coefs['(Intercept)'] + 




## temperate model







# remove wrong realm results

# combine with other realm



# crop to harvest area using the binary map?














#### create a projection matrix ####

# create a table with a row for each cell within cropland area (use binary map)
# then extract the NH available within each cell


# load in the cropped Jung data based on the crop binary map
NH_crp <- raster(paste0(datadir, "/NH_Cropland_Area_Jung_four.tif"))

#plot(NH_crp)

# extract the data as a dataframe
proj_mat <- as.data.frame(NH_crp, xy = T)

# remove NAs
proj_mat <- proj_mat[!is.na(proj_mat$NH_Cropland_Area),]

#head(proj_mat)

# add in the column for cropland
proj_mat$Predominant_land_use <- "Cropland"

# edit NH column name
colnames(proj_mat)[3] <- "percNH_Jung4"

# add biodiversity columns
proj_mat$Species_richness <- 0
proj_mat$LogAbun <- 0


# read in data file
load("5_Models/PREDICTS_dataset_incNH.rdata") # sites.sub

# read in models
load("5_Models/Richness_Jung4_finalmod.rdata")
load("5_Models/Abundance_Jung4_finalmod.rdata")

# set levels
proj_mat$Predominant_land_use <- factor(proj_mat$Predominant_land_use, levels = levels(sr1$data$Predominant_land_use))

# just take the columns needed
model_data <- proj_mat[, 3:6]

# try to reduce the size of the model_data object by just taking unique values of percNH_jung4
model_data <- unique(model_data)

# set a reference row- here where NH is 100%, but is this the best comparison, perhaps 50%?
ref_row <- which(model_data$percNH_Jung4 == 1)

# predict the values for both the abundance and richness 
Pred_abun <- PredictGLMERRandIter(model = ab1$model, data = model_data)
Pred_rich <- PredictGLMERRandIter(model = sr1$model, data = model_data)


# back transform the values
Pred_abun <- exp(Pred_abun)-1
Pred_rich <- exp(Pred_rich)

# convert to relative to reference
Pred_abun <- sweep(x = Pred_abun,MARGIN = 2,STATS = Pred_abun[ref_row,],FUN = '/')
Pred_rich <- sweep(x = Pred_rich,MARGIN = 2,STATS = Pred_rich[ref_row,],FUN = '/')


# Get the median, upper and lower quants 
model_data$PredMedian_AB <- ((apply(X = Pred_abun,MARGIN = 1,
                         FUN = median,na.rm=TRUE))*100)-100
model_data$PredUpper_AB <- ((apply(X = Pred_abun,MARGIN = 1,
                        FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
model_data$PredLower_AB <- ((apply(X = Pred_abun,MARGIN = 1,
                        FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


model_data$PredMedian_SR <- ((apply(X = Pred_rich,MARGIN = 1,
                         FUN = median,na.rm=TRUE))*100)-100
model_data$PredUpper_SR <- ((apply(X = Pred_rich,MARGIN = 1,
                        FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
model_data$PredLower_SR <- ((apply(X = Pred_rich,MARGIN = 1,
                        FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


# fill in predictions in complete matrix table proj_mat alongside lat/lon info for mapping
# match by Jung4 value

map_data <- merge(model_data, proj_mat)

# save as Rdata file
save(map_data, file = paste0(outdir, "Map_data_perchange.rdata"))

#load(paste0(outdir, "Map_data_perchange.rdata"))


#### create the maps ####

# turn the dataframe into a raster
Ab_med <- rasterFromXYZ(map_data[, c(11,12,5)])
Rich_med <- rasterFromXYZ(map_data[, c(11,12,8)])

# aggregate to a coarser scale?
Ab_med_agg <- aggregate(Ab_med, fact = 10)
Rich_med_agg <- aggregate(Rich_med, fact = 10)

# convert back to dataframe for use in ggplot
plot_data_ab <- as.data.frame(Ab_med_agg$layer, xy = T)
plot_data_sr <- as.data.frame(Rich_med_agg$layer, xy = T)

# remove the NAs
plot_data_ab <- plot_data_ab[!is.na(plot_data_ab$layer), ]
plot_data_sr <- plot_data_sr[!is.na(plot_data_sr$layer), ]

# load world map outline
map.world <- map_data("world")


# plot for abundance
p1 <- ggplot() +
          #scale_fill_manual(values = c("#EE0000", "#EE7600", "#EEC900", "#66CD00", "#458B00")) +
          geom_map(data=map.world, map=map.world,
                   aes(x=long, y=lat, group=group, map_id=region),
                   fill= "darkgrey", colour="darkgrey", size=0.1) +
          geom_tile(data = plot_data_ab, aes(x = x, y = y, fill = layer), na.rm = TRUE, alpha = 0.7) +
            scale_fill_gradient2(low = c("#EE0000"), mid =  c("#EEAD0E"), high = c("#458B00"), midpoint = median(plot_data_ab$layer),
                               name = "% change in \ntotal abundance") +
          theme_bw() +
          theme(axis.title = element_blank(), 
                plot.background = element_blank(), 
                panel.background = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(), 
                legend.position = "bottom", 
                legend.title = element_text(size = 10),
                legend.text = element_text(size = 10),
                panel.border = element_blank()) +
            guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))


# plot for richness
p2 <- ggplot() +
  #scale_fill_manual(values = c("#EE0000", "#EE7600", "#EEC900", "#66CD00", "#458B00")) +
  geom_map(data=map.world, map=map.world,
           aes(x=long, y=lat, group=group, map_id=region),
           fill= "darkgrey", colour="darkgrey", size=0.1) +
  geom_tile(data = plot_data_sr, aes(x = x, y = y, fill = layer), na.rm = TRUE, alpha = 0.7) +
  scale_fill_gradient2(low = c("#EE0000"), mid =  c("#EEAD0E"), high = c("#458B00"), midpoint = median(plot_data_sr$layer),
                       name = "% change in \nspecies richness") +
  theme_bw() +
  theme(axis.title = element_blank(), 
        plot.background = element_blank(), 
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        panel.border = element_blank()) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))

# organise plots into panels
p3 <- plot_grid(p1, p2, nrow = 2)

# save plot (A5 size)
ggsave2(p3, filename = paste0(outdir, "All_biodiv_ab_sr_maps.pdf"), height = 8.27, width = 5.83, units = "in")




##%######################################################%##
#                                                          #
####              Including use intensity               ####
#                                                          #
##%######################################################%##

# Here projections are done a little differentlys o that use intesity can be used. 

# following example code for Arc/python analyses used aaages ago. 

ref_value <- 

