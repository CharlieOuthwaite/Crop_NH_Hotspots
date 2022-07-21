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



# determine total area of cropland in a cell
total_crp <- sum(cr_min_resamp, cr_lt_resamp, cr_int_resamp)

writeRaster(total_crp, filename = paste0(outdir, "/resampled_total_cropland_layer.tif"), format = "GTiff")

##%######################################################%##
#                                                          #
####                     PROJECTIONS                    ####
#                                                          #
##%######################################################%##

# list the models that predictions needed from
abun_models <- list.files(path = "5_Models/", pattern = "Abundance")
rich_models <- list.files(path = "5_Models/", pattern = "Richness")


#### Rescale the NH data ####

Jung2_RS <- scale(Jung2) # actually, do I need to scale using values used in predicts dataset? 
#But all proportions so max/min values should be similar.
writeRaster(Jung2_RS, filename = paste0(outdir, "/Jung2_NH_map_rescaled.tif"), format = "GTiff")

# save space by removing files not needed anymore
rm(Jung2)


# load in datasets to avoid rerunning above
total_crp <- raster(paste0(outdir, "/resampled_total_cropland_layer.tif"))
Jung2_RS <- raster(paste0(outdir, "/Jung2_NH_map_rescaled.tif"))
cr_int_resamp <- raster(paste0(outdir, "/resampled_crp_int_layer.tif"))
cr_lt_resamp <- raster(paste0(outdir, "/resampled_crp_lt_layer.tif"))
cr_min_resamp <- raster(paste0(outdir, "/resampled_crp_min_layer.tif"))
 


##%######################################################%##
#                                                          #
####              Loop through each model               ####
#                                                          #
##%######################################################%##

#### Abundance all biodiv ####

# loop through Jung2 then Jung4
abun_models
   
job_list <- abun_models[grep("POLL", abun_models, invert = T)]
   
   
 # loop through for Jung2 then Jung4
 for(i in c("2", "4")){
   
  models <-  job_list[grep(paste0("Jung", i), job_list)]
   
  # load the models
  load(paste0("5_Models/", models[1])) 
  load(paste0("5_Models/", models[2])) 
  
  
  #### tropical ####
  
  # get the model coefficients
  ab1.trop_coefs <- fixef(ab1.trop$model)
  #rm(ab1.trop)
  
  # get the scalers
  load(file ="5_MOdels/PREDICTS_dataset_incNH.rdata") # sites.sub
    if(i == "2"){
  scalers <- c(attr(sites.sub$percNH_Jung2_RS, "scaled:scale"), attr(sites.sub$percNH_Jung2_RS, "scaled:center"))
  }else{
    scalers <- c(attr(sites.sub$percNH_Jung4_RS, "scaled:scale"), attr(sites.sub$percNH_Jung2_RS, "scaled:center"))
  }
  
  # the reference value of cropland
  NH_ref <- (0.4 - scalers[2])/scalers[1]
  # what is a useful baseline? try various? 40?
  # reference: all cropland is minimal with X% natural habitat surrounding it. 
  # need to determine an appropriate and feasible amount of NH. 40%?
  
  # reference map: cropland, minimal, NH = X
  refval_trop <- (# intercept
                  ab1.trop_coefs["(Intercept)"] + 
                  # cropland area
                    (ab1.trop_coefs["Predominant_land_useCropland"] * total_crp) +
                  # Amount of NH
                    (ab1.trop_coefs["poly(percNH_Jung2_RS, 1)"] * NH_ref) + 
                  # interaction: LU:NH
                    (ab1.trop_coefs["Predominant_land_useCropland:poly(percNH_Jung2_RS, 1)"] * NH_ref * total_crp))
  #summary(refval_trop)
  
  # test with and without total area of cropland in the ref and predictions, to see if they cancel out
  
  ### test map relative to reference map ###
  pred_trop <- (exp(
    
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
      
      # natural habitat interactions  
      (Jung2_RS * ab1.trop_coefs["Predominant_land_useCropland:poly(percNH_Jung2_RS, 1)"] * total_crp) + 
      (Jung2_RS * ab1.trop_coefs["Use_intensityLight use:poly(percNH_Jung2_RS, 1)"] * cr_lt_resamp) +
      (Jung2_RS * ab1.trop_coefs["Use_intensityIntense use:poly(percNH_Jung2_RS, 1)"] * cr_int_resamp) +
    
       # added LU:UI interaction
      (ab1.trop_coefs["Predominant_land_useCropland:Use_intensityLight use"] * cr_lt_resamp) +
      (ab1.trop_coefs["Predominant_land_useCropland:Use_intensityIntense use"] * cr_int_resamp)
    
    
  )/(exp(refval_trop))) *100 -100
  
  writeRaster(pred_trop, filename = paste0(outdir, "projection_raster_all_Jung", i, "_abundance_tropical.tif"), format = "GTiff", overwrite = T)
  #pred_trop <- raster(paste0(outdir, "projection_raster_all_Jung2_abundance_tropical.tif"))
  
  # set extents that need to be converted to NA as outside tropical region
  extent_NA_trop1 <- extent(matrix(c(-180,  23.44, 180, 90), nrow=2))
  extent_NA_trop2 <- extent(matrix(c(-180, -90, 180, -23.44), nrow=2))
  
  # set values out side of the trop/temp extent to NA
  
  # assess number of NAs to check if NA assignment is being completed
  #summary(pred_trop) #NA's    6.121259e+08
  
  pred_trop[extent_NA_trop1] <- NA
  pred_trop[extent_NA_trop2] <- NA
  
  #plot(pred_trop)
  #summary(pred_trop)  #NA's    6.440544e+08
  
  writeRaster(pred_trop, filename = paste0(outdir, "projection_raster_all_Jung", i, "_abundance.tif"), format = "GTiff", overwrite = T)
  
  # remove large objects
  rm(refval_trop, extent_NA_trop1, extent_NA_trop2)
  

  #### temperate ####
  
  # get the model coefficients
  ab1.temp_coefs <- fixef(ab1.temp$model)
  
  
  # reference: cropland, minimal, NH = X
  refval_temp <- (ab1.temp_coefs["(Intercept)"] + 
                    (ab1.temp_coefs["Predominant_land_useCropland"] * total_crp) +
                    (ab1.temp_coefs["poly(percNH_Jung2_RS, 1)"] * NH_ref) + 
                    (ab1.temp_coefs["Predominant_land_useCropland:poly(percNH_Jung2_RS, 1)"] * NH_ref))
  
    # test map
  pred_temp <- (exp(
    
    # intercept +
    # coef cropland + (assuming not needing * total crop area map)
    # Nh map * NHCoef +
    # coef UIlight * fracUIlight +
    # coef UIint * fract UI int +
    # NHmap * coef NH:LUcrop +
    # NHmap * coef NH:uilight * fract lgiht
    # NHmap * coef NH:int * frac int
    
      (ab1.temp_coefs["(Intercept)"]) + 
      
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
    
    # added LU:UI interaction
      (ab1.temp_coefs["Predominant_land_useCropland:Use_intensityLight use"] * cr_lt_resamp) +
      (ab1.temp_coefs["Predominant_land_useCropland:Use_intensityIntense use"] * cr_int_resamp)
    
    
  )/(exp(refval_temp))) *100 -100
  
  writeRaster(pred_temp, filename = paste0(outdir, "projection_raster_all_Jung", i, "_abundance_temp.tif"), format = "GTiff", overwrite = T)
  #pred_temp <- raster(paste0(outdir, "projection_raster_all_Jung2_abundance_temp.tif"))
  
  # set values within a certain extent to NA
  extent_NA_temp <- extent(matrix(c(-180, -23.44,  180, 23.44), nrow=2))
  
  # set values out side of the trop/temp extent to NA
  
  # assess number of NAs to check if NA assignment is being completed
  #summary(pred_temp) #NA's    6.121259e+08
  
  # set cells outside temperate range to NA
  pred_temp[extent_NA_temp] <- NA 
  
  
  # summary(pred_temp)   
  # NA's    6.251043e+08
  #plot(pred_temp)
  
  writeRaster(pred_temp, filename = paste0(outdir, "projection_raster_all_Jung", i, "_abundance_temp.tif"), format = "GTiff", overwrite = T)
  
  # remove objects
  rm(extent_NA_temp, refval_temp)
  
  
  # load tropical map back in
  #pred_trop <- raster(paste0(outdir, "projection_raster_all_Jung2_abundance.tif"))
  
  
  # try to check if the maps match up without overlap
  # test1 <- pred_temp
  # test1[!is.na(test1)] <- 1
  # test2 <- pred_trop
  # test2[!is.na(test2)] <- 1
  # 
  # combined <- sum(test1, test2, na.rm = T)
  # summary(combined)
  # 
  # seemed to work!
  
  # combine the trop and temp maps to 
  final_abun_Jung2 <- sum(pred_temp, pred_trop, na.rm = T)
  
  # plot(final_abun_Jung2)
  # summary(final_abun_Jung2)
  
  # save it
  writeRaster(final_abun_Jung2, filename = paste0(outdir, "Abundance_Jung", i, "_difference_to_ref.tif"), format = "GTiff", overwrite = T)
  
  
  
  rm()
 }  
   



#### Create nicer plot ####

final_abun_Jung2 <- raster(paste0(outdir, "Abundance_Jung", i, "_difference_to_ref.tif"))


library(ggplot2)





# crop to harvested area using the binary map?

# use the cropland area mask as the base
crop_bin <- raster("1_PrepareCropLayers/CroplandBinary.tif")
# crop map is at a 5x5km grid



# plot map using appropriate axes (seem to be some very high values making it hard to visualise)












