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


# determine total area of cropland in a cell
total_crp <- sum(cr_min_resamp, cr_lt_resamp, cr_int_resamp)

writeRaster(total_crp, filename = paste0(outdir, "/resampled_total_cropland_layer.tif"), format = "GTiff")
# total_crp <- raster(paste0(outdir, "/resampled_total_cropland_layer.tif"))

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


#### Rescale the NH data ####

Jung2_RS <- scale(Jung2) # actually, do I need to scale using values used in predicts dataset? 
#But all proportions so max/min values should be similar. 
rm(Jung2)

#### Projections 1: Abundance, Jung2 ####

#### tropical model####


load("5_Models/Abundance_Jung2_Tropical.rdata") # ab1.trop
summary(ab1.trop$model)
#LogAbun ~ Predominant_land_use + Use_intensity + poly(percNH_Jung2_RS, 1) +
# Predominant_land_use:poly(percNH_Jung2_RS, 1) + Use_intensity:poly(percNH_Jung2_RS, 1) + Predominant_land_use:Use_intensity + 
# (1 | SS) + (1 | SSB)

# get the model coefficients
ab1.trop_coefs <- fixef(ab1.trop$model)
rm(ab1.trop)

# set the reference values
load(file ="5_MOdels/PREDICTS_dataset_incNH.rdata") # sites.sub

scalers <- c(attr(sites.sub$percNH_Jung2_RS, "scaled:scale"), attr(sites.sub$percNH_Jung2_RS, "scaled:center"))

rm(sites.sub)

# the reference value of cropland
NH_ref <- (0.4 - scalers[2])/scalers[1]
# what is a useful baseline? try various? 40?
# reference: all cropland is minimal with X% natural habitat surrounding it. 
# need to determine an appropriate and feasible amount of NH. 40%?


# reference: cropland, minimal, NH = X
refval_trop <- (ab1.trop_coefs["(Intercept)"] + 
                  (ab1.trop_coefs["Predominant_land_useCropland"] * total_crp) +
                  (ab1.trop_coefs["poly(percNH_Jung2_RS, 1)"] * NH_ref) + 
                  (ab1.trop_coefs["Predominant_land_useCropland:poly(percNH_Jung2_RS, 1)"] * NH_ref))
summary(refval_trop)

# possibly need another * total_crp in the final line?

#notes from meeting with Tim, see also photo of whiteboard
# intercept + 
# Coef(crop) (* total cropland) + # areas cancel out
# NHref (0.4) * NH coef
# + coefLUCrop:NH x 0.4


# test with and without total area of cropland in the ref and predictions, to see if they cancel out


pred_trop <- exp(
  
  # intercept +
  # coef cropland + (assuming not needing * total crop area map)
  # Nh map * NHCoef +
  # coef UIlight * fracUIlight +
  # coef UIint * fract UI int +
  # NHmap * coef NH:LUcrop +
  # NHmap * coef NH:uilight * fract lgiht
  # NHmap * coef NH:int * frac int
  
  (ab1.trop_coefs["(Intercept)"]) + 
    
    # cropland area  
    (ab1.trop_coefs["Predominant_land_useCropland"] * total_crp) +
    
    # amount of NH    
    (ab1.trop_coefs["poly(percNH_Jung2_RS, 1)"] * Jung2_RS) + 
    
    # use intensity  
    (ab1.trop_coefs["Use_intensityLight use"] * cr_lt_resamp) +
    (ab1.trop_coefs["Use_intensityIntense use"] * cr_int_resamp) + 
    
    # use intensity natural habitat interactions  
    (Jung2_RS * ab1.trop_coefs["Predominant_land_useCropland:poly(percNH_Jung2_RS, 1)"] * total_crp) + 
    (Jung2_RS * ab1.trop_coefs["Use_intensityLight use:poly(percNH_Jung2_RS, 1)"] * cr_lt_resamp) +
    (Jung2_RS * ab1.trop_coefs["Use_intensityIntense use:poly(percNH_Jung2_RS, 1)"] * cr_int_resamp)
  
  # need to add in LU:UI interaction?
  
)/(exp(refval_trop))

writeRaster(pred_trop, filename = paste0(outdir, "projection_raster_all_Jung2_abundance.tif"), format = "GTiff")
pred_trop <- raster(paste0(outdir, "projection_raster_all_Jung2_abundance.tif"))


#plot(pred_trop)
summary(pred_trop)
quantile(pred_trop, 0.90)

# set extents that need to be converted to NA as outside tropical region
extent_NA_trop1 <- extent(matrix(c(-180,  23.44, 180, 90), nrow=2))
extent_NA_trop2 <- extent(matrix(c(-180, -90, 180, -23.44), nrow=2))

# set values out side of the trop/temp extent to NA

# assess number of NAs to check if NA assignment is being completed
summary(pred_trop) #NA's    6.121259e+08

pred_trop[extent_NA_trop1] <- NA
pred_trop[extent_NA_trop2] <- NA

plot(pred_trop)

#### temperate model ####



load("5_Models/Abundance_Jung2_Temperate.rdata") # ab1.trop
summary(ab1.temp$model)
#LogAbun ~ Predominant_land_use + Use_intensity + poly(percNH_Jung2_RS, 1) +
# Predominant_land_use:poly(percNH_Jung2_RS, 1) + Use_intensity:poly(percNH_Jung2_RS, 1) + Predominant_land_use:Use_intensity + 
# (1 | SS) + (1 | SSB)

# get the model coefficients
ab1.temp_coefs <- fixef(ab1.temp$model)

# the reference value of cropland
NH_ref <- (0.4 - scalers[2])/scalers[1]
# what is a useful baseline? try various? 40?
# reference: all cropland is minimal with X% natural habitat surrounding it. 
# need to determine an appropriate and feasible amount of NH. 40%?


# reference: cropland, minimal, NH = X
refval_temp <- (ab1.temp_coefs["(Intercept)"] + 
                  (ab1.temp_coefs["Predominant_land_useCropland"] * total_crp) +
                  (ab1.temp_coefs["poly(percNH_Jung2_RS, 1)"] * NH_ref) + 
                  (ab1.temp_coefs["Predominant_land_useCropland:poly(percNH_Jung2_RS, 1)"] * NH_ref))



pred_temp <- exp(
  
  # intercept +
  # coef cropland + (assuming not needing * total crop area map)
  # Nh map * NHCoef +
  # coef UIlight * fracUIlight +
  # coef UIint * fract UI int +
  # NHmap * coef NH:LUcrop +
  # NHmap * coef NH:uilight * fract lgiht
  # NHmap * coef NH:int * frac int
  
  ab1.temp_coefs["(Intercept)"] + 
    
    # cropland area  
    (ab1.temp_coefs["Predominant_land_useCropland"] * total_crp) +
    
    # amount of NH    
    (ab1.temp_coefs["poly(percNH_Jung2_RS, 1)"] * Jung2_RS) + 
    
    # use intensity  
    (ab1.temp_coefs["Use_intensityLight use"] * cr_lt_resamp) +
    (ab1.temp_coefs["Use_intensityIntense use"] * cr_int_resamp) + 
    
    # use intensity natural habitat interactions  
    (Jung2_RS * ab1.temp_coefs["Predominant_land_useCropland:poly(percNH_Jung2_RS, 1)"] * Jung2_RS) + 
    (Jung2_RS * ab1.temp_coefs["Use_intensityLight use:poly(percNH_Jung2_RS, 1)"] * cr_lt_resamp) +
    (Jung2_RS * ab1.temp_coefs["Use_intensityIntense use:poly(percNH_Jung2_RS, 1)"] * cr_int_resamp)
  
  # need to add in LU:UI interaction?
  
)/exp(refval_temp)


# set values within a certain extent to NA

extent_NA_temp <- extent(matrix(c(-180, -23.44,  180, 23.44), nrow=2))

extent_NA_trop1 <- extent(matrix(c(-180,  23.44, 180, 90), nrow=2))
extent_NA_trop2 <- extent(matrix(c(-180, -90, 180, -23.44), nrow=2))

# set values out side of the trop/temp extent to NA

# assess number of NAs to check if NA assignment is being completed
summary(pred_temp) #NA's    6.121259e+08
summary(pred_trop) #NA's    6.121259e+08

pred_temp[extent_NA_temp] <- NA
pred_trop[extent_NA_trop1] <- NA
pred_trop[extent_NA_trop2] <- NA

# warnings 
# 34: In writeBin(v, x@file@con, size = x@file@dsize) :
# problem writing to connection
# 35: In .rasterFromRasterFile(grdfile, band = band, objecttype,  ... :
#                                size of values file does not match the number of cells (given the data type)

summary(pred_temp) #NA's    
summary(pred_trop) #NA's    

# combine the trop and temp maps to 
final_abun_Jung2 <- sum(pred_temp, pred_trop
                        
)

# crop to harvested area using the binary map?

# use the cropland area mask as the base
crop_bin <- raster("1_PrepareCropLayers/CroplandBinary.tif")
# crop map is at a 5x5km grid



# plot map using appropriate axes (seem to be some very high values making it hard to visualise)












