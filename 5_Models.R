##%######################################################%##
#                                                          #
####        Models and biodiversity projections         ####
#                                                          #
##%######################################################%##

# This script runs models looking at land use, use intensity and 
# NH effects on richness and abundance. 

rm(list = ls())

# load libraries
library(predictsFunctions)
library(StatisticalModels)
library(raster)
library(sf)
library(ggplot2)
library(cowplot)
library(car)
library(optimx)

# directories
datadir <- "0_data"
outdir <- "5_Models"
if(!dir.exists(outdir)) dir.create(outdir)

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



##%######################################################%##
#                                                          #
####                    Run  models                     ####
#                                                          #
##%######################################################%##


# try rescaling the NH data
sites.sub$percNH_Jung2_RS <- scale(sites.sub$percNH_Jung2)
sites.sub$percNH_Jung2_log_RS <- scale(log(sites.sub$percNH_Jung2+1))

sites.sub$percNH_Jung4_RS <- scale(sites.sub$percNH_Jung4)
sites.sub$percNH_Jung4_log_RS <- scale(log(sites.sub$percNH_Jung4+1))

save(sites.sub, file = paste0(outdir, "/PREDICTS_dataset_incNH.rdata"))

# check factor levels, ensure primary vegetation is the baseline
levels(sites.sub$Predominant_land_use)

#### SPECIES RICHNESS MODELS ####

# remove NAs in the specified columns
#model_data <- na.omit(sites.sub[,c('Species_richness','Predominant_land_use', 'percNH_Jung2', 'percNH_Jung2_RS', "percNH_Jung2_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])
model_data <- na.omit(sites.sub[,c('Species_richness','Predominant_land_use', 'percNH_Jung4','percNH_Jung4_RS', "percNH_Jung4_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])

# summaries
length(unique(model_data$SS)) # 577
length(unique(model_data$SSBS)) # 13666


# run set of simple models with different fixed effects structures
# see comparison markdown/PDF
# adding in use intensity here

# using Jung2 data, currently untransformed, but rescaled
# using Jung4 data, currently untransformed, but rescaled


# test alternative model structures
# 1. including all variables and rescaled NH
mod_struc <- "Predominant_land_use + Use_intensity + Tropical + poly(percNH_Jung4_RS, 1) + Predominant_land_use:poly(percNH_Jung4_RS,1) + Use_intensity:poly(percNH_Jung4_RS,1) + Predominant_land_use:Use_intensity + Tropical:poly(percNH_Jung4_RS,1) + Tropical:Predominant_land_use:poly(percNH_Jung4_RS,1)"

# 2. removing realm and interactions with realm
mod_struc2 <- "Predominant_land_use + Use_intensity + poly(percNH_Jung4_RS, 1) + Predominant_land_use:poly(percNH_Jung4_RS,1) + Use_intensity:poly(percNH_Jung4_RS,1) + Predominant_land_use:Use_intensity"

sr1.trop <- GLMER(modelData = model_data,responseVar = "Species_richness",fitFamily = "poisson",
             fixedStruct = mod_struc,
             randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)",
             REML = TRUE, maxIters = 60000
             #, optimizer = "Nelder_Mead"
             )

# summary(sr1.trop$model)
# vif(sr1.trop$model)

# Warning messages: Jung2 data
#   1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model failed to converge with max|grad| = 0.00363711 (tol = 0.002, component 1)
#   2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model is nearly unidentifiable: large eigenvalue ratio
#   - Rescale variables?


# Warning message: Jung4 data
#   In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model is nearly unidentifiable: large eigenvalue ratio
#                - Rescale variables?
                 
                 
srmod <- GLMER(modelData = model_data,responseVar = "Species_richness",fitFamily = "poisson",
                  fixedStruct = mod_struc2,
                  randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)

# Warning message: Jung2 data
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.00216804 (tol = 0.002, component 1)

# Warning message: Jung4 data
#   In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge with max|grad| = 0.00276697 (tol = 0.002, component 1)

save(srmod, file = paste0(outdir, "/Richness_Jung4_finalmod.rdata"))


#
table(model_data$Predominant_land_use, model_data$Use_intensity)
plot(model_data$Predominant_land_use, model_data$percNH_Jung2)
plot(model_data$Predominant_land_use, model_data$percNH_Jung4)

# rerun model using lme4 so that allFit function can be used to test all optimizer options.
mod_struc <- Species_richness ~ Predominant_land_use + Use_intensity + poly(percNH_Jung4_RS, 1) + Predominant_land_use:poly(percNH_Jung4_RS,1) + Use_intensity:poly(percNH_Jung4_RS,1) + Predominant_land_use:Use_intensity +(1|SS)+(1|SSB)+(1|SSBS)

sr1 <- glmer(formula = mod_struc, data = model_data, family = poisson)

# Warning message: Jung2 data
#   In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge with max|grad| = 0.00216804 (tol = 0.002, component 1)

# Warning message: Jung4 data
#   In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge with max|grad| = 0.00810945 (tol = 0.002, component 1)
               
               
# allFit function runs model will all available optimizers so can see if convergence
# warning occurs with all, and if the results are broadly similar across models.
output <- allFit(sr1)

summary(output)
# Jung2 model failed to converge in 5 out of 7 models
# However, estimates for fixed effects across each model all very similar

save(output, file = paste0(outdir, "/allFit_output_Richness_Jung4.rdata"))

# use this function to get model stats
srmod <- GLMERSelect(modelData = model_data,
                     responseVar = "Species_richness",
                     fitFamily = "poisson",
                     fixedFactors = c("Predominant_land_use", "Use_intensity"),
                     fixedTerms = list(percNH_Jung4_RS = 1),
                     randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)",
                     fixedInteractions = c("Predominant_land_use:poly(percNH_Jung4_RS,1)",
                                           "Use_intensity:poly(percNH_Jung4_RS,1)",
                                           "Predominant_land_use:Use_intensity"),  verbose = F)



summary(srmod$model)
vif(srmod$model)
write.csv(srmod$stats, file = paste0(outdir, "/Richness_Jung4_stats.csv"))

# for jung4, Predominant_land_use:poly(percNH_Jung4_RS,1) not selected


##%######################################################%##
#                                                          #
####                 ABUNDANCE MODELS                   ####
#                                                          #
##%######################################################%##

# remove NAs in the specified columns
#model_data <- na.omit(sites.sub[,c('Species_richness','Predominant_land_use', 'percNH_Jung2', 'percNH_Jung2_RS', "percNH_Jung2_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])
model_data <- na.omit(sites.sub[,c('LogAbun','Predominant_land_use', 'percNH_Jung2','percNH_Jung2_RS', "percNH_Jung2_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB', 'SSBS')])

# summaries
length(unique(model_data$SS)) # 510
length(unique(model_data$SSBS)) # 11396


# test alternative model structures
# 1. including all variables and rescaled NH
mod_struc <- "Predominant_land_use + Use_intensity + Tropical + poly(percNH_Jung2_RS, 1) + Predominant_land_use:poly(percNH_Jung2_RS,1) + Use_intensity:poly(percNH_Jung2_RS,1) + Predominant_land_use:Use_intensity + Tropical:poly(percNH_Jung2_RS,1) + Tropical:Predominant_land_use:poly(percNH_Jung2_RS,1)"

# 2. removing realm and interactions with realm
mod_struc2 <- "Predominant_land_use + Use_intensity + poly(percNH_Jung2_RS, 1) + Predominant_land_use:poly(percNH_Jung2_RS,1) + Use_intensity:poly(percNH_Jung2_RS,1) + Predominant_land_use:Use_intensity"

ab1.trop <- GLMER(modelData = model_data,responseVar = "LogAbun",fitFamily = "gaussian",
                  fixedStruct = mod_struc,
                  randomStruct = "(1|SS)+(1|SSB)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)


summary(ab1.trop$model)
vif(ab1.trop$model)
save(ab1.trop, file = paste0(outdir, "/Abundance_model_Jung2_incRealm.rdata"))

# no warnings

ab1 <- GLMER(modelData = model_data,responseVar = "LogAbun",fitFamily = "gaussian",
                  fixedStruct = mod_struc2,
                  randomStruct = "(1|SS)+(1|SSB)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)


summary(ab1$model)
vif(ab1$model)
save(ab1, file = paste0(outdir, "/Abundance_model_Jung2.rdata"))

# no warnings


abmod <- GLMERSelect(modelData = model_data, 
                     responseVar = "LogAbun",
                     fitFamily = "gaussian", 
                     fixedFactors = c("Predominant_land_use", "Use_intensity", "Tropical"),
                     fixedTerms = list(percNH_Jung2_RS = 1),
                     randomStruct = "(1|SS)+(1|SSB)", 
                     fixedInteractions = c("Predominant_land_use:poly(percNH_Jung2_RS,1)",
                                           "Use_intensity:poly(percNH_Jung2_RS,1)",
                                           "Predominant_land_use:Use_intensity",
                                           "Tropical:poly(percNH_Jung2_RS,1)",
                                           "Tropical:Predominant_land_use:poly(percNH_Jung2_RS,1)"), verbose = F)


summary(abmod$model)
vif(abmod$model)
# model selected:
# LogAbun ~ Predominant_land_use + Use_intensity + Tropical + poly(percNH_Jung2_RS,1) +
# Predominant_land_use:poly(percNH_Jung2_RS, 1) + Use_intensity:poly(percNH_Jung2_RS, 1) +
# Predominant_land_use:Use_intensity + Tropical:poly(percNH_Jung2_RS, 1) + 
# Tropical:Predominant_land_use:poly(percNH_Jung2_RS,1) + (1 | SS) + (1 | SSB)


write.csv(abmod$stats, file = paste0(outdir, "/Abundance_Jung4_stats_incRealm.csv"))


##%######################################################%##
#                                                          #
####           Separate models for each realm           ####
#                                                          #
##%######################################################%##


#### Richness Realm ####

model_data <- na.omit(sites.sub[,c('Species_richness','Predominant_land_use', 'percNH_Jung2', 'percNH_Jung2_RS', "percNH_Jung2_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])
#model_data <- na.omit(sites.sub[,c('Species_richness','Predominant_land_use', 'percNH_Jung4', 'percNH_Jung4_RS', "percNH_Jung4_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])

model_data_trop <- model_data[model_data$Tropical == "Tropical", ] # 5519 rows
model_data_temp <- model_data[model_data$Tropical == "Temperate", ] # 8147 rows


# 2. removing realm and interactions with realm
mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung2_RS, 1) + Predominant_land_use:poly(percNH_Jung2_RS,1) + Use_intensity:poly(percNH_Jung2_RS,1) + Predominant_land_use:Use_intensity"
#mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung4_RS, 1) + Predominant_land_use:poly(percNH_Jung4_RS,1) + Use_intensity:poly(percNH_Jung4_RS,1) + Predominant_land_use:Use_intensity"

sr1.trop <- GLMER(modelData = model_data_trop,responseVar = "Species_richness",fitFamily = "poisson",
                  fixedStruct = mod_struc,
                  randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)

summary(sr1.trop$model)

# no warnings

sr1.temp <- GLMER(modelData = model_data_temp,responseVar = "Species_richness",fitFamily = "poisson",
                  fixedStruct = mod_struc,
                  randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)

summary(sr1.temp$model)

# no warnings

save(sr1.trop, file = paste0(outdir, "/Richness_Jung2_Tropical.rdata"))
save(sr1.temp, file = paste0(outdir, "/Richness_Jung2_Temperate.rdata"))
#save(sr1.trop, file = paste0(outdir, "/Richness_Jung4_Tropical.rdata"))
#save(sr1.temp, file = paste0(outdir, "/Richness_Jung4_Temperate.rdata"))


#### Abundance Realm ####

model_data <- na.omit(sites.sub[,c('LogAbun','Predominant_land_use', 'percNH_Jung2', 'percNH_Jung2_RS', "percNH_Jung2_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])
#model_data <- na.omit(sites.sub[,c('LogAbun','Predominant_land_use', 'percNH_Jung4', 'percNH_Jung4_RS', "percNH_Jung4_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])

model_data_trop <- model_data[model_data$Tropical == "Tropical", ] # 4708 rows
model_data_temp <- model_data[model_data$Tropical == "Temperate", ] # 6688 rows


# 2. removing realm and interactions with realm
mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung2_RS, 1) + Predominant_land_use:poly(percNH_Jung2_RS,1) + Use_intensity:poly(percNH_Jung2_RS,1) + Predominant_land_use:Use_intensity"
#mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung4_RS, 1) + Predominant_land_use:poly(percNH_Jung4_RS,1) + Use_intensity:poly(percNH_Jung4_RS,1) + Predominant_land_use:Use_intensity"

ab1.trop <- GLMER(modelData = model_data_trop,responseVar = "LogAbun",fitFamily = "gaussian",
                  fixedStruct = mod_struc,
                  randomStruct = "(1|SS)+(1|SSB)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)

summary(ab1.trop$model)

# no warnings

ab1.temp <- GLMER(modelData = model_data_temp,responseVar = "LogAbun",fitFamily = "gaussian",
                  fixedStruct = mod_struc,
                  randomStruct = "(1|SS)+(1|SSB)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)

summary(ab1.temp$model)

# no warnings

save(ab1.trop, file = paste0(outdir, "/Abundance_Jung2_Tropical.rdata"))
save(ab1.temp, file = paste0(outdir, "/Abundance_Jung2_Temperate.rdata"))
#save(ab1.trop, file = paste0(outdir, "/Abundance_Jung4_Tropical.rdata"))
#save(ab1.temp, file = paste0(outdir, "/Abundance_Jung4_Temperate.rdata"))



##%######################################################%##
#                                                          #
####                  Pollinator Data                   ####
#                                                          #
##%######################################################%##

# read in pollinator dataset
pols.data <- readRDS(paste0(datadir, "/PREDICTS_pollinators_8_exp.rds"))

# correct sampling effort 
pols.data <- CorrectSamplingEffort(pols.data)

# merge sites: this combines potential subsamples within one site
pols.data <- MergeSites(pols.data[, 1:67]) # 344555 rows

# Calculate site level metrics
pols.data.sites.metrics <- SiteMetrics(pols.data, extra.cols = c("Predominant_land_use", "SSB", "SSBS", "Biome"), 
                                  srEstimators = NULL) # 11352 rows


### only interested in natural habitats plus cropland, drop other land uses ###

# site level data primary, secondary and cropland only
sites.sub.pols <- pols.data.sites.metrics[!pols.data.sites.metrics$Predominant_land_use %in% c("Urban", "Pasture", "Cannot decide", "Plantation forest"), ]
# 7845 rows

# remove sites with NA in lat/long columns
sites.sub.pols <- sites.sub.pols[!is.na(sites.sub.pols$Longitude),  ] # 7839 rows

# get the tropical values
sites.sub.pols$Tropical <- NA

sites.sub.pols[sites.sub.pols$Latitude > -23.44 & sites.sub.pols$Latitude < 23.44, 'Tropical'] <- "Tropical"

# label the remaining as temperate
sites.sub.pols[is.na(sites.sub.pols$Tropical), 'Tropical'] <- "Temperate"

# set as a factor
sites.sub.pols$Tropical <- as.factor(sites.sub.pols$Tropical)

table(sites.sub.pols$Tropical)
# Temperate  Tropical 
# 4396       3443 

# convert the PREDICTS lat/longs into spatial points
sites.sub_xy.pol <- sites.sub.pols[, c("Longitude", "Latitude")]
sites.sub_xy.pol <- SpatialPoints(sites.sub_xy.pol, proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84"))


# extract the dataset info for the PREDICTS sites
sites.sub.pols$percNH_Jung4 <- extract(NatHabCrop4, sites.sub_xy.pol, na.rm = FALSE)
sites.sub.pols$percNH_Jung2 <- extract(NatHabCrop2, sites.sub_xy.pol, na.rm = FALSE)


# nrows of dataset
nrow(sites.sub.pols) # 7839

# remove those sites that have "Cannot decide" as a use intensity
nrow(sites.sub.pols[sites.sub.pols$Use_intensity == "Cannot decide", ]) # 1266
sites.sub.pols <- sites.sub.pols[!sites.sub.pols$Use_intensity == "Cannot decide", ] 

nrow(sites.sub.pols) # 6573

# remove any rows that have NA in the variable columns
summary(is.na(sites.sub.pols))
#sites.sub <- sites.sub[!is.na(sites.sub$percNH_hosk), ]

# drop unused levels of factors
sites.sub.pols <- droplevels(sites.sub.pols)

# nsites per use intensity
table(sites.sub.pols$Use_intensity)
table(sites.sub.pols$Predominant_land_use)

# Minimal use   Light use Intense use 
# 3076        2124        1373

# set land use as character variable
sites.sub.pols$Predominant_land_use <- as.character(sites.sub.pols$Predominant_land_use)


# combine secondary land uses
sites.sub.pols$Predominant_land_use <- sub("Mature secondary vegetation", "Secondary vegetation", sites.sub.pols$Predominant_land_use)
sites.sub.pols$Predominant_land_use <- sub("Intermediate secondary vegetation", "Secondary vegetation", sites.sub.pols$Predominant_land_use)
sites.sub.pols$Predominant_land_use <- sub("Young secondary vegetation", "Secondary vegetation", sites.sub.pols$Predominant_land_use)
sites.sub.pols[sites.sub.pols$Predominant_land_use == "Secondary vegetation (indeterminate age)", 'Predominant_land_use'] <- "Secondary vegetation"

table(sites.sub.pols$Predominant_land_use)

# Cropland   Primary vegetation Secondary vegetation 
# 1680                 2813                 2080 

# set factor levels of predominant land use
sites.sub.pols$Predominant_land_use <- factor(sites.sub.pols$Predominant_land_use,
                                         levels=c("Primary vegetation","Secondary vegetation", "Cropland"))


# nsites per land use
table(sites.sub.pols$Predominant_land_use)

# transform abundance
sites.sub.pols$LogAbun <- log(sites.sub.pols$Total_abundance + 1)



# try rescaling the NH data
sites.sub.pols$percNH_Jung2_RS <- scale(sites.sub.pols$percNH_Jung2)
sites.sub.pols$percNH_Jung2_log_RS <- scale(log(sites.sub.pols$percNH_Jung2+1))

sites.sub.pols$percNH_Jung4_RS <- scale(sites.sub.pols$percNH_Jung4)
sites.sub.pols$percNH_Jung4_log_RS <- scale(log(sites.sub.pols$percNH_Jung4+1))




# save transformed dataset
save(sites.sub.pols, file = paste0(outdir, "/PREDICTS_dataset_incNH_POLLINATORS.rdata"))

# load(file = paste0(outdir, "/PREDICTS_dataset_incNH_POLLINATORS.rdata"))



##%######################################################%##
#                                                          #
####                 Pollinator models                  ####
#                                                          #
##%######################################################%##

# Run the above models on the pollinator subset of PREDICTS

#### all pollinators richness ####

#model_data <- na.omit(sites.sub.pols[,c('Species_richness','Predominant_land_use', 'percNH_Jung2','percNH_Jung2_RS', "percNH_Jung2_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])
model_data <- na.omit(sites.sub.pols[,c('Species_richness','Predominant_land_use', 'percNH_Jung4','percNH_Jung4_RS', "percNH_Jung4_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])

# 6573 rows

# 2. removing realm and interactions with realm
#mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung2_RS, 1) + Predominant_land_use:poly(percNH_Jung2_RS,1) + Use_intensity:poly(percNH_Jung2_RS,1) + Predominant_land_use:Use_intensity"
mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung4_RS, 1) + Predominant_land_use:poly(percNH_Jung4_RS,1) + Use_intensity:poly(percNH_Jung4_RS,1) + Predominant_land_use:Use_intensity"


srmod_pols <- GLMER(modelData = model_data,responseVar = "Species_richness",fitFamily = "poisson",
               fixedStruct = mod_struc,
               randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)",
               REML = TRUE, maxIters = 60000
               #, optimizer = "Nelder_Mead"
)

summary(srmod_pols$model)
#save(srmod_pols, file = paste0(outdir, "/Richness_Jung2_pollinators.rdata"))
save(srmod_pols, file = paste0(outdir, "/Richness_Jung4_pollinators.rdata"))

# no warnings


#### all pollinators abundance ####


# remove NAs in the specified columns
#model_data <- na.omit(sites.sub.pols[,c('LogAbun','Predominant_land_use', 'percNH_Jung2','percNH_Jung2_RS', "percNH_Jung2_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB', 'SSBS')])
model_data <- na.omit(sites.sub.pols[,c('LogAbun','Predominant_land_use', 'percNH_Jung4','percNH_Jung4_RS', "percNH_Jung4_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB', 'SSBS')])

# 2. removing realm and interactions with realm
#mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung2_RS, 1) + Predominant_land_use:poly(percNH_Jung2_RS,1) + Use_intensity:poly(percNH_Jung2_RS,1) + Predominant_land_use:Use_intensity"
mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung4_RS, 1) + Predominant_land_use:poly(percNH_Jung4_RS,1) + Use_intensity:poly(percNH_Jung4_RS,1) + Predominant_land_use:Use_intensity"

abmod_pols <- GLMER(modelData = model_data,responseVar = "LogAbun",fitFamily = "gaussian",
                  fixedStruct = mod_struc,
                  randomStruct = "(1|SS)+(1|SSB)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)


summary(abmod_pols$model)

#save(abmod_pols, file = paste0(outdir, "/Abundance_Jung2_pollinators.rdata"))
save(abmod_pols, file = paste0(outdir, "/Abundance_Jung4_pollinators.rdata"))




##%######################################################%##
#                                                          #
####                Pollinators by realm                ####
#                                                          #
##%######################################################%##

# run a separate model for each realm for the pollinators 


#### Richness Realm ####

model_data <- na.omit(sites.sub.pols[,c('Species_richness','Predominant_land_use', 'percNH_Jung2', 'percNH_Jung2_RS', "percNH_Jung2_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])
#model_data <- na.omit(sites.sub.pols[,c('Species_richness','Predominant_land_use', 'percNH_Jung4', 'percNH_Jung4_RS', "percNH_Jung4_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])

model_data_trop <- model_data[model_data$Tropical == "Tropical", ] # 2524 rows
model_data_temp <- model_data[model_data$Tropical == "Temperate", ] # 4049 rows


# 2. removing realm and interactions with realm
mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung2_RS, 1) + Predominant_land_use:poly(percNH_Jung2_RS,1) + Use_intensity:poly(percNH_Jung2_RS,1) + Predominant_land_use:Use_intensity"
#mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung4_RS, 1) + Predominant_land_use:poly(percNH_Jung4_RS,1) + Use_intensity:poly(percNH_Jung4_RS,1) + Predominant_land_use:Use_intensity"

sr1.trop <- GLMER(modelData = model_data_trop,responseVar = "Species_richness",fitFamily = "poisson",
                  fixedStruct = mod_struc,
                  randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)

summary(sr1.trop$model)

# no warnings

sr1.temp <- GLMER(modelData = model_data_temp,responseVar = "Species_richness",fitFamily = "poisson",
                  fixedStruct = mod_struc,
                  randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)

summary(sr1.temp$model)

# no warnings

save(sr1.trop, file = paste0(outdir, "/Richness_Jung2_Tropical_POLLINATORS.rdata"))
save(sr1.temp, file = paste0(outdir, "/Richness_Jung2_Temperate_POLLINATORS.rdata"))
#save(sr1.trop, file = paste0(outdir, "/Richness_Jung4_Tropical_POLLINATORS.rdata"))
#save(sr1.temp, file = paste0(outdir, "/Richness_Jung4_Temperate_POLLINATORS.rdata"))


#### Abundance Realm ####

model_data <- na.omit(sites.sub.pols[,c('LogAbun','Predominant_land_use', 'percNH_Jung2', 'percNH_Jung2_RS', "percNH_Jung2_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])
#model_data <- na.omit(sites.sub.pols[,c('LogAbun','Predominant_land_use', 'percNH_Jung4', 'percNH_Jung4_RS', "percNH_Jung4_log_RS","Use_intensity", 'Tropical', 'Biome', 'SS','SSB','SSBS')])

model_data_trop <- model_data[model_data$Tropical == "Tropical", ] # 4708 rows
model_data_temp <- model_data[model_data$Tropical == "Temperate", ] # 6688 rows


# 2. removing realm and interactions with realm
mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung2_RS, 1) + Predominant_land_use:poly(percNH_Jung2_RS,1) + Use_intensity:poly(percNH_Jung2_RS,1) + Predominant_land_use:Use_intensity"
#mod_struc <- "Predominant_land_use + Use_intensity + poly(percNH_Jung4_RS, 1) + Predominant_land_use:poly(percNH_Jung4_RS,1) + Use_intensity:poly(percNH_Jung4_RS,1) + Predominant_land_use:Use_intensity"

ab1.trop <- GLMER(modelData = model_data_trop,responseVar = "LogAbun",fitFamily = "gaussian",
                  fixedStruct = mod_struc,
                  randomStruct = "(1|SS)+(1|SSB)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)

summary(ab1.trop$model)

# no warnings

ab1.temp <- GLMER(modelData = model_data_temp,responseVar = "LogAbun",fitFamily = "gaussian",
                  fixedStruct = mod_struc,
                  randomStruct = "(1|SS)+(1|SSB)",
                  REML = TRUE, maxIters = 60000
                  #, optimizer = "Nelder_Mead"
)

summary(ab1.temp$model)

# no warnings

save(ab1.trop, file = paste0(outdir, "/Abundance_Jung2_Tropical_POLLINATORS.rdata"))
save(ab1.temp, file = paste0(outdir, "/Abundance_Jung2_Temperate_POLLINATORS.rdata"))
#save(ab1.trop, file = paste0(outdir, "/Abundance_Jung4_Tropical_POLLINATORS.rdata"))
#save(ab1.temp, file = paste0(outdir, "/Abundance_Jung4_Temperate_POLLINATORS.rdata"))




