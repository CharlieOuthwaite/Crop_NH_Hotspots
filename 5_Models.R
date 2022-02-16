##%######################################################%##
#                                                          #
####        Models and biodiversity projections         ####
#                                                          #
##%######################################################%##

#  TO DO:

# finalise this script when decided upon whether to include UI or not. 




# run simple models of biodiversity response to LU, UI and NH

# use the effects from the models to project SR and abundance in space

rm(list = ls())

# load libraries
library(predictsFunctions)
library(StatisticalModels)
library(raster)
library(sf)
library(ggplot2)
library(cowplot)
library(car)

# directories
datadir <- "0_data"
outdir <- "5_Models"
dir.create(outdir)

# read in the complete PREDICTS dataset
pred.data <- readRDS(paste0(datadir, "/database.rds")) # 3250404 rows

### organise using functions from predictsFunctions package ###

# correct sampling effort 
predicts <- CorrectSamplingEffort(pred.data)

# merge sites: this combines potential subsamples within one site
predicts <- MergeSites(predicts) # 2906994 rows

# Calculate site level metrics
pred.sites.metrics <- SiteMetrics(predicts, extra.cols = c("Predominant_land_use", "SSB", "SSBS", "Biome"), 
                                  srEstimators = NULL) # 22678 rows


### only interested in natural habitats plus cropland, drop other land uses ###

# site level data primary, secondary and cropland only
sites.sub <- pred.sites.metrics[!pred.sites.metrics$Predominant_land_use %in% c("Urban", "Pasture", "Cannot decide", "Plantation forest"), ]

# remove sites with NA in lat/long columns
sites.sub <- sites.sub[!is.na(sites.sub$Longitude),  ] # 15612 rows



# get the tropical values
sites.sub$Tropical <- NA

sites.sub[sites.sub$Latitude > -23.44 & sites.sub$Latitude < 23.44, 'Tropical'] <- "Tropical"

# label the remaining as temperate
sites.sub[is.na(sites.sub$Tropical), 'Tropical'] <- "Temperate"

# set as a factor
sites.sub$Tropical <- as.factor(sites.sub$Tropical)
# levels: Temperate Tropical


table(sites.sub$Tropical)

# Temperate  Tropical 
#      8677      6935


table(sites.sub$Biome)

# Tundra                                                      Boreal Forests/Taiga 
# 13                                                          799 
# Temperate Conifer Forests                                   Temperate Broadleaf & Mixed Forests 
# 294                                                         4166 
# Montane Grasslands & Shrublands                             Temperate Grasslands, Savannas & Shrublands 
# 563                                                         801 
# Mediterranean Forests, Woodlands & Scrub                    Deserts & Xeric Shrublands 
# 1541                                                        157 
# Tropical & Subtropical Grasslands, Savannas & Shrublands    Tropical & Subtropical Coniferous Forests 
# 1807                                                        266 
# Flooded Grasslands & Savannas                               Tropical & Subtropical Dry Broadleaf Forests 
# 0                                                           333 
# Tropical & Subtropical Moist Broadleaf Forests              Mangroves 
# 4842                                                        30 


save(sites.sub, file = paste0(outdir, "/Predicts_site_level.rdata"))
#load(file = paste0(outdir, "/Predicts_site_level.rdata"))

##%######################################################%##
#                                                          #
####             Percentage natural habitat             ####
#                                                          #
##%######################################################%##

# use Jung habitat data, combining natural habitat types 

# # read in the fractional natural habitat data
NatHabCrop4 <- raster(paste0("2_PrepareNaturalHabitatLayer/Fractional_NH_Jung_four.tif"))
NatHabCrop2 <- raster(paste0("2_PrepareNaturalHabitatLayer/Fractional_NH_Jung_two.tif"))

# convert to actual % values
#NatHabCrop <- NatHabCrop*100

plot(NatHabCrop4)
plot(NatHabCrop2)

# convert the PREDICTS lat/longs into spatial points
sites.sub_xy <- sites.sub[, c("Longitude", "Latitude")]
sites.sub_xy <- SpatialPoints(sites.sub_xy, proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84"))


# extract the dataset info for the PREDICTS sites
sites.sub$percNH_Jung4 <- extract(NatHabCrop4, sites.sub_xy, na.rm = FALSE)
sites.sub$percNH_Jung2 <- extract(NatHabCrop2, sites.sub_xy, na.rm = FALSE)

# # how many NAs
# nrow(sites.sub[is.na(sites.sub$percNH_Jung4),]) #0
# nrow(sites.sub[is.na(sites.sub$percNH_Jung2),]) #0


### add in Hoskins data for comparison ###

# read in the raster
#percNH <- raster(paste0(datadir,"/PercentNatural.tif"))

# ensure both types are proportions
#percNH <- percNH/1000

# extract the dataset info for the PREDICTS sites
#sites.sub$percNH_hosk <- extract(percNH, sites.sub_xy, na.rm = FALSE)

# how many NAs
#nrow(sites.sub[is.na(sites.sub$percNH_hosk),]) #32


# nrows of dataset
nrow(sites.sub) # 15612

# remove those sites that have "Cannot decide" as a use intensity
nrow(sites.sub[sites.sub$Use_intensity == "Cannot decide", ]) # 1946
sites.sub <- sites.sub[!sites.sub$Use_intensity == "Cannot decide", ] 

nrow(sites.sub) # 13666

# remove any rows that have NA in the variable columns
summary(is.na(sites.sub))
#sites.sub <- sites.sub[!is.na(sites.sub$percNH_hosk), ]

# drop unused levels of factors
sites.sub <- droplevels(sites.sub)

##%######################################################%##
#                                                          #
####                 Assess the dataset                 ####
#                                                          #
##%######################################################%##

# nsites per use intensity
table(sites.sub$Use_intensity)
table(sites.sub$Predominant_land_use)

# Minimal use   Light use Intense use 
# 6978        4668        2020 

# set land use as character variable
sites.sub$Predominant_land_use <- as.character(sites.sub$Predominant_land_use)


# combine secondary land uses
sites.sub$Predominant_land_use <- sub("Mature secondary vegetation", "Secondary vegetation", sites.sub$Predominant_land_use)
sites.sub$Predominant_land_use <- sub("Intermediate secondary vegetation", "Secondary vegetation", sites.sub$Predominant_land_use)
sites.sub$Predominant_land_use <- sub("Young secondary vegetation", "Secondary vegetation", sites.sub$Predominant_land_use)
sites.sub[sites.sub$Predominant_land_use == "Secondary vegetation (indeterminate age)", 'Predominant_land_use'] <- "Secondary vegetation"

table(sites.sub$Predominant_land_use)

# Cropland   Primary vegetation Secondary vegetation 
# 2561                 6556                 4549  

# set factor levels of predominant land use
sites.sub$Predominant_land_use <- factor(sites.sub$Predominant_land_use,
                                                levels=c("Primary vegetation","Secondary vegetation", "Cropland"))


# nsites per land use
table(sites.sub$Predominant_land_use)

# transform abundance
sites.sub$LogAbun <- log(sites.sub$Total_abundance + 1)

# save transformed dataset
save(sites.sub, file = paste0(outdir, "/PREDICTS_dataset_incNH.rdata"))

# load(file = paste0(outdir, "/PREDICTS_dataset_incNH.rdata"))

##%######################################################%##
#                                                          #
####              Assess spread of NH data              ####
#                                                          #
##%######################################################%##

p1 <- ggplot(sites.sub) +
  geom_histogram(aes(percNH_Jung4), fill = c("#1874CD")) + 
  theme_bw() +
  xlab("Proportion of NH (Jung4)")

p2 <- ggplot(sites.sub) +
  geom_histogram(aes(percNH_Jung2), fill = c("#66CD00")) + 
  theme_bw() +
  xlab("Proportion of NH (Jung2)")


p3 <- plot_grid(p1, p2)

ggsave2(filename = paste0(outdir, "/Jung_data_histograms.pdf"), p3, height = 3, width = 6)


# should these be transformed for analysis? Not sure there is a transformation to 
# suit these u-shaped distributions. 

# They don't need to be rescaled as they are the only continuous variable


##%######################################################%##
#                                                          #
####                    Run  models                     ####
#                                                          #
##%######################################################%##


#### SPECIES RICHNESS MODELS ####

# remove NAs in the specified columns
model_data <- na.omit(sites.sub[,c('Species_richness','Predominant_land_use', 'percNH_Jung2', "Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])
#model_data <- na.omit(sites.sub[,c('Species_richness','Predominant_land_use', 'percNH_Jung4', 'Tropical', 'Biome', 'SS','SSB','SSBS')])

# summaries
length(unique(model_data$SS)) # 577
length(unique(model_data$SSBS)) # 13666


# run set of simple models with different fixed effects structures
# see comparison markdown/PDF
# adding in use intensity here

# using Jung2 data, currently untransformed
# using Jung4 data, currently untransformed

srmod <- GLMERSelect(modelData = model_data, 
                     responseVar = "Species_richness",
                     fitFamily = "poisson", 
                     fixedFactors = c("Predominant_land_use", "Use_intensity", "Tropical"),
                     fixedTerms = list(percNH_Jung2 = 1),
                     randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)", 
                     fixedInteractions = c("Predominant_land_use:poly(percNH_Jung2,1)",
                                           "Use_intensity:poly(percNH_Jung2,1)",
                                           "Predominant_land_use:Use_intensity",
                                           "Tropical:poly(percNH_Jung2,1)",
                                           "Tropical:Predominant_land_use:poly(percNH_Jung2,1)"),  verbose = F)



summary(srmod$model)
vif(srmod)
# model selected not inc use intensity:
# Species_richness ~ Predominant_land_use + poly(percNH_Jung4, 1) + (1 | SS) + (1 | SSB) + (1 | SSBS)

#Warning message:
#  In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                 Model failed to converge with max|grad| = 0.00109362 (tol = 0.001, component 1)

# model selected inc use intensity:
# Species_richness ~ Predominant_land_use + Use_intensity + poly(percNH_Jung4, 1) + Predominant_land_use:Use_intensity + Use_intensity:poly(percNH_Jung4, 1) + (1 | SS) + (1 | SSB) + (1 | SSBS)

#7: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge with max|grad| = 0.00200249 (tol = 0.001, component 1)

mod_struc <- "Predominant_land_use + Use_intensity + Tropical + poly(percNH_Jung2, 1) + Predominant_land_use:poly(percNH_Jung2,1) + Use_intensity:poly(percNH_Jung2,1) + Predominant_land_use:Use_intensity + Tropical:poly(percNH_Jung2,1) + Tropical:Predominant_land_use:poly(percNH_Jung2,1)"

sr1 <- GLMER(modelData = model_data,responseVar = "Species_richness",fitFamily = "poisson",
             fixedStruct = mod_struc,
             randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)",
             REML = TRUE, maxIters = 20000)

summary(sr1$model)

# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.00321917 (tol = 0.002, component 1)
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model is nearly unidentifiable: large eigenvalue ratio- Rescale variables?

# save model outputs for both models and stats table
save(srmod, file = paste0(outdir, "/Richness_Jung4_modelselection.rdata"))
save(sr1, file = paste0(outdir, "/Richness_Jung4_finalmod.rdata"))

write.csv(srmod$stats, file = paste0(outdir, "/Richness_Jung4_stats.csv"))




#load(file = paste0(outdir, "/Richness_Jung4_modelselection_incUI.rdata"))
#load(file = paste0(outdir, "/Richness_Jung4_finalmod_incUI.rdata"))
#load(file = paste0(outdir, "/Richness_Jung4_modelselection.rdata"))
#load(file = paste0(outdir, "/Richness_Jung4_finalmod.rdata"))

#### ABUNDANCE MODELS ####

model_data <- na.omit(sites.sub[,c('LogAbun','Predominant_land_use','percNH_Jung2', "Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])

# summaries
length(unique(model_data$SS)) # 510
length(unique(model_data$SSBS)) # 11396


abmod <- GLMERSelect(modelData = model_data, 
                     responseVar = "LogAbun",
                     fitFamily = "gaussian", 
                     fixedFactors = c("Predominant_land_use", "Use_intensity", "Tropical"),
                     fixedTerms = list(percNH_Jung2 = 1),
                     randomStruct = "(1|SS)+(1|SSB)", 
                     fixedInteractions = c("Predominant_land_use:poly(percNH_Jung2,1)",
                                           "Use_intensity:poly(percNH_Jung2,1)",
                                           "Predominant_land_use:Use_intensity",
                                           "Tropical:poly(percNH_Jung2,1)",
                                           "Tropical:Predominant_land_use:poly(percNH_Jung2,1)"), verbose = F)


summary(abmod$model)
vif(abmod$model)
# model selected:
# LogAbun ~ Predominant_land_use + Predominant_land_use:poly(percNH_Jung4, 1) + poly(percNH_Jung4, 1) + (1 | SS) + (1 | SSB)

# model selected inc use intensity
# LogAbun ~ Predominant_land_use + Use_intensity + Predominant_land_use:poly(percNH_Jung4, 1) + Predominant_land_use:Use_intensity + Use_intensity:poly(percNH_Jung4,1) + poly(percNH_Jung4, 1) + (1 | SS) + (1 | SSB)

# more complicated model
# LogAbun ~ Predominant_land_use + Use_intensity + poly(percNH_Jung2, 1) + 
# Predominant_land_use:poly(percNH_Jung2, 1) + Use_intensity:poly(percNH_Jung2, 1) +
# Predominant_land_use:Use_intensity + Tropical:poly(percNH_Jung2, 1) + 
# Tropical:Predominant_land_use:poly(percNH_Jung2, 1) +  Tropical + (1 | SS) + (1 | SSB)

# GVIF Df GVIF^(1/(2*Df))
# Predominant_land_use                                 4.627042  2        1.466648
# Use_intensity                                        5.384701  2        1.523317
# poly(percNH_Jung2, 1)                                3.102611  1        1.761423
# Tropical                                             1.007021  1        1.003504
# Predominant_land_use:poly(percNH_Jung2, 1)           9.794847  2        1.769088
# Use_intensity:poly(percNH_Jung2, 1)                  2.532461  2        1.261495
# Predominant_land_use:Use_intensity                  20.596088  4        1.459564
# poly(percNH_Jung2, 1):Tropical                       2.634295  1        1.623051
# Predominant_land_use:poly(percNH_Jung2, 1):Tropical  8.939302  2        1.729123

ab1 <- GLMER(modelData = model_data,responseVar = "LogAbun",fitFamily = "gaussian",
             fixedStruct = "Predominant_land_use + poly(percNH_Jung4, 1) + Predominant_land_use:poly(percNH_Jung4, 1)",
             randomStruct = "(1|SS)+(1|SSB)",
             REML = TRUE, 
             saveVars = "SSBS")

summary(ab1$model)


# save model outputs for both models and stats table

save(abmod, file = paste0(outdir, "/Abundance_Jung4_modelselection.rdata"))
save(ab1, file = paste0(outdir, "/Abundance_Jung4_finalmod.rdata"))

write.csv(abmod$stats, file = paste0(outdir, "/Abundance_Jung4_stats.csv"))

# load(file = paste0(outdir, "/Abundance_Jung4_modelselection_incUI.rdata"))
# load(file = paste0(outdir, "/Abundance_Jung4_finalmod_incUI.rdata"))
# load(file = paste0(outdir, "/Abundance_Jung4_modelselection.rdata"))
# load(file = paste0(outdir, "/Abundance_Jung4_finalmod.rdata"))


