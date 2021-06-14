##%######################################################%##
#                                                          #
####                    Projections                     ####
#                                                          #
##%######################################################%##

# here, create a projection matrix covering global cropland area
# include Land use/Use intensity and Jung4 percNH


rm(list = ls())

# libraries


# directories
datadir <- "4_Country_NH_summaries/"
outdir <- "6_Projections/"
dir.create(outdir)

# files from Tim for land use use intensity combo in here
proj_data <- "Z:\Datasets\LandUseIntensity_globalmap_fromTim\02Projections_02ProjectLUIntensity"

# read in teh fractional natural habitat data
NatHabCrop_proj <- raster(paste0(datadir, "NH_Cropland_Area_Jung_four.tif"))


proj_mat <- as.data.frame(NatHabCrop_proj, xy = T)3

proj_mat <- proj_mat[!is.na(proj_mat$NatHabCrop_wgs84), ]

# read in values for rescaling

# rescale
proj_mat$percNH_rs <-(proj_mat$NatHabCrop_wgs84-values[, "centre"])/values[, "scale"]



# add information on landuse and use intensity

# load in models


proj_mat$Predominant_land_use <- "Primary_vegetation"
proj_mat$Use_intensity <- "Minimal_use"

proj_mat$Species_richness <- 0

proj_mat$Predominant_land_use <- as.factor(proj_mat$Predominant_land_use)
proj_mat$Use_intensity <- as.factor(proj_mat$Use_intensity)

levels(proj_mat$Predominant_land_use) <- levels(sr1$data$Predominant_land_use)
levels(proj_mat$Use_intensity) <- levels(sr1$data$Use_intensity) 

model_data <- proj_mat[, 4:7]

# predict the result


results <- NULL



result1 <- PredictGLMER(model = sr1$model, data = model_data[1:50000,], se.fit = TRUE, seMultiplier = 1.96)
result2 <- PredictGLMER(model = sr1$model, data = model_data[50001:100000,], se.fit = TRUE, seMultiplier = 1.96)
result3 <- PredictGLMER(model = sr1$model, data = model_data[100001:150000,], se.fit = TRUE, seMultiplier = 1.96)
result4 <- PredictGLMER(model = sr1$model, data = model_data[150001:200000,], se.fit = TRUE, seMultiplier = 1.96)
result5 <- PredictGLMER(model = sr1$model, data = model_data[200001:250000,], se.fit = TRUE, seMultiplier = 1.96)
result6 <- PredictGLMER(model = sr1$model, data = model_data[250001:300000,], se.fit = TRUE, seMultiplier = 1.96)
result7 <- PredictGLMER(model = sr1$model, data = model_data[300001:350000,], se.fit = TRUE, seMultiplier = 1.96)
result8 <- PredictGLMER(model = sr1$model, data = model_data[350001:400000,], se.fit = TRUE, seMultiplier = 1.96)
result9 <- PredictGLMER(model = sr1$model, data = model_data[400001:450000,], se.fit = TRUE, seMultiplier = 1.96)
result10 <- PredictGLMER(model = sr1$model, data = model_data[450001:500000,], se.fit = TRUE, seMultiplier = 1.96)

results <- rbind(result1, result2, result3, result4, result5, result6, result7, result8, result9, result10)

rm(result1, result2, result3, result4, result5, result6, result7, result8, result9, result10)

result1 <- PredictGLMER(model = sr1$model, data = model_data[500001:550000,], se.fit = TRUE, seMultiplier = 1.96)
result2 <- PredictGLMER(model = sr1$model, data = model_data[550001:600000,], se.fit = TRUE, seMultiplier = 1.96)
result3 <- PredictGLMER(model = sr1$model, data = model_data[600001:650000,], se.fit = TRUE, seMultiplier = 1.96)
result4 <- PredictGLMER(model = sr1$model, data = model_data[650001:700000,], se.fit = TRUE, seMultiplier = 1.96)
result5 <- PredictGLMER(model = sr1$model, data = model_data[700001:750000,], se.fit = TRUE, seMultiplier = 1.96)
result6 <- PredictGLMER(model = sr1$model, data = model_data[750001:800000,], se.fit = TRUE, seMultiplier = 1.96)
result7 <- PredictGLMER(model = sr1$model, data = model_data[800001:850000,], se.fit = TRUE, seMultiplier = 1.96)
result8 <- PredictGLMER(model = sr1$model, data = model_data[850001:900000,], se.fit = TRUE, seMultiplier = 1.96)
result9 <- PredictGLMER(model = sr1$model, data = model_data[900001:950000,], se.fit = TRUE, seMultiplier = 1.96)
result10 <- PredictGLMER(model = sr1$model, data = model_data[950001:1000000,], se.fit = TRUE, seMultiplier = 1.96)

results <- rbind(results, result1, result2, result3, result4, result5, result6, result7, result8, result9, result10)

rm(result1, result2, result3, result4, result5, result6, result7, result8, result9, result10)


result1 <- PredictGLMER(model = sr1$model, data = model_data[1000001:150000,], se.fit = TRUE, seMultiplier = 1.96)
result2 <- PredictGLMER(model = sr1$model, data = model_data[150001:1100000,], se.fit = TRUE, seMultiplier = 1.96)
result3 <- PredictGLMER(model = sr1$model, data = model_data[1100001:1150000,], se.fit = TRUE, seMultiplier = 1.96)
result4 <- PredictGLMER(model = sr1$model, data = model_data[1150001:1200000,], se.fit = TRUE, seMultiplier = 1.96)
result5 <- PredictGLMER(model = sr1$model, data = model_data[1200001:1250000,], se.fit = TRUE, seMultiplier = 1.96)
result6 <- PredictGLMER(model = sr1$model, data = model_data[1250001:1300000,], se.fit = TRUE, seMultiplier = 1.96)
result7 <- PredictGLMER(model = sr1$model, data = model_data[1300001:1350000,], se.fit = TRUE, seMultiplier = 1.96)
result8 <- PredictGLMER(model = sr1$model, data = model_data[1350001:1400000,], se.fit = TRUE, seMultiplier = 1.96)
result9 <- PredictGLMER(model = sr1$model, data = model_data[1400001:1450000,], se.fit = TRUE, seMultiplier = 1.96)
result10 <- PredictGLMER(model = sr1$model, data = model_data[1450001:1500000,], se.fit = TRUE, seMultiplier = 1.96)

results <- rbind(results, result1, result2, result3, result4, result5, result6, result7, result8, result9, result10)


result1 <- PredictGLMER(model = sr1$model, data = model_data[1500001:1550000,], se.fit = TRUE, seMultiplier = 1.96)
result2 <- PredictGLMER(model = sr1$model, data = model_data[1550001:1600000,], se.fit = TRUE, seMultiplier = 1.96)
result3 <- PredictGLMER(model = sr1$model, data = model_data[1600001:1650000,], se.fit = TRUE, seMultiplier = 1.96)
result4 <- PredictGLMER(model = sr1$model, data = model_data[1650001:1700000,], se.fit = TRUE, seMultiplier = 1.96)
result5 <- PredictGLMER(model = sr1$model, data = model_data[1700001:1750000,], se.fit = TRUE, seMultiplier = 1.96)
result6 <- PredictGLMER(model = sr1$model, data = model_data[1750001:1800000,], se.fit = TRUE, seMultiplier = 1.96)
result7 <- PredictGLMER(model = sr1$model, data = model_data[1800001:1850000,], se.fit = TRUE, seMultiplier = 1.96)
result8 <- PredictGLMER(model = sr1$model, data = model_data[1850001:1900000,], se.fit = TRUE, seMultiplier = 1.96)
result9 <- PredictGLMER(model = sr1$model, data = model_data[1900001:1950000,], se.fit = TRUE, seMultiplier = 1.96)
result10 <- PredictGLMER(model = sr1$model, data = model_data[1950001:2000000,], se.fit = TRUE, seMultiplier = 1.96)


result1 <- PredictGLMER(model = sr1$model, data = model_data[2000001:2500000,], se.fit = TRUE, seMultiplier = 1.96)
result2 <- PredictGLMER(model = sr1$model, data = model_data[150001:1100000,], se.fit = TRUE, seMultiplier = 1.96)
result3 <- PredictGLMER(model = sr1$model, data = model_data[1100001:1150000,], se.fit = TRUE, seMultiplier = 1.96)
result4 <- PredictGLMER(model = sr1$model, data = model_data[1150001:1200000,], se.fit = TRUE, seMultiplier = 1.96)
result5 <- PredictGLMER(model = sr1$model, data = model_data[1200001:1250000,], se.fit = TRUE, seMultiplier = 1.96)
result6 <- PredictGLMER(model = sr1$model, data = model_data[1250001:1300000,], se.fit = TRUE, seMultiplier = 1.96)
result7 <- PredictGLMER(model = sr1$model, data = model_data[1300001:1350000,], se.fit = TRUE, seMultiplier = 1.96)
result8 <- PredictGLMER(model = sr1$model, data = model_data[1350001:1400000,], se.fit = TRUE, seMultiplier = 1.96)
result9 <- PredictGLMER(model = sr1$model, data = model_data[1400001:1450000,], se.fit = TRUE, seMultiplier = 1.96)
result10 <- PredictGLMER(model = sr1$model, data = model_data[1450001:1500000,], se.fit = TRUE, seMultiplier = 1.96)

result <- PredictGLMER(model = sr1$model, data = model_data, se.fit = TRUE, seMultiplier = 1.96)

result <- PredictGLMER(model = sr1$model, data = model_data, se.fit = TRUE, seMultiplier = 1.96)

result <- PredictGLMER(model = sr1$model, data = model_data, se.fit = TRUE, seMultiplier = 1.96)

# transform the results
result <- exp(result)



