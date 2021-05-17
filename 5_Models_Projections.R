##%######################################################%##
#                                                          #
####        Models and biodiversity projections         ####
#                                                          #
##%######################################################%##

# run simple models of biodiversity response to LU, UI and NH

# use the effects from the models to project SR and abundance in space

rm(list = ls())

# load libraries
library(predictsFunctions)
library(StatisticalModels)

# directories
datadir <- "0_data"
outdir <- "5_Models_Projections"
dir.create(outdir)

# read in the complete PREDICTS dataset
pred.data <- readRDS(paste0(datadir, "/database.rds")) # 3250404 rows

### organise using functions from predictsFunctions package ###

# correct sampling effort 
predicts <- CorrectSamplingEffort(pred.data)

# merge sites: this combines potential subsamples within one site
predicts <- MergeSites(predicts) # 2906994 rows

# Calculate site level metrics
pred.sites.metrics <- SiteMetrics(predicts, extra.cols = c("Predominant_land_use", "SSB", "SSBS")) # 22678 rows

### only interested in natural habitats plus cropland, drop other land uses ###

# site level data primary, secondary and cropland only
sites.sub <- pred.sites.metrics[!pred.sites.metrics$Predominant_land_use %in% c("Urban", "Pasture", "Cannot decide", "Plantation forest"), ]

# remove sites with NA in lat/long columns
sites.sub <- sites.sub[!is.na(sites.sub$Longitude),  ] # 15612 rows




##%######################################################%##
#                                                          #
####             Percentage natural habitat             ####
#                                                          #
##%######################################################%##

# use Jung habitat data, combining natural habitat types 

# # read in teh fractional natural habitat data
NatHabCrop <- raster(paste0("2_PrepareNaturalHabitatLayer/Fractional_NH_Jung.tif"))

# convert to actual % values
NatHabCrop <- NatHabCrop*100

plot(NatHabCrop)

# convert the PREDICTS lat/longs into spatial points
sites.sub_xy <- sites.sub[, c("Longitude", "Latitude")]
sites.sub_xy <- SpatialPoints(sites.sub_xy, proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84"))


# extract the dataset info for the PREDICTS sites
sites.sub$percNH <- extract(NatHabCrop, sites.sub_xy, na.rm = FALSE)

# how many NAs
nrow(sites.sub[is.na(sites.sub$percNH),]) #0


# remove any rows that have NA in the variable columns
summary(is.na(sites.sub))

# nrows of dataset
nrow(sites.sub) # 15612

# remove those sites that have "Cannot decide" as a use intensity
nrow(sites.sub[sites.sub$Use_intensity == "Cannot decide", ]) # 1946
sites.sub <- sites.sub[!sites.sub$Use_intensity == "Cannot decide", ] 

nrow(sites.sub) # 13666


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


# set land use as character variable
sites.sub$Predominant_land_use <- as.character(sites.sub$Predominant_land_use)


# combine secondary land uses
sites.sub$Predominant_land_use <- sub("Mature secondary vegetation", "Secondary vegetation", sites.sub$Predominant_land_use)
sites.sub$Predominant_land_use <- sub("Intermediate secondary vegetation", "Secondary vegetation", sites.sub$Predominant_land_use)
sites.sub$Predominant_land_use <- sub("Young secondary vegetation", "Secondary vegetation", sites.sub$Predominant_land_use)
sites.sub[sites.sub$Predominant_land_use == "Secondary vegetation (indeterminate age)", 'Predominant_land_use'] <- "Secondary vegetation"

table(sites.sub$Predominant_land_use)


# set factor levels of predominant land use
sites.sub$Predominant_land_use <- factor(sites.sub$Predominant_land_use,
                                                levels=c("Primary vegetation","Secondary vegetation", "Cropland"))


# nsites per land use
table(sites.sub$Predominant_land_use)


sites.sub$LogAbun <- log(sites.sub$Total_abundance + 1)

# save transformed dataset
save(sites.sub, file = paste0(outdir, "PREDICTS_subset.rdata"))





##%######################################################%##
#                                                          #
####                 Run simple models                  ####
#                                                          #
##%######################################################%##



# remove NAs in the specified columns
model_data <- na.omit(sites.sub[,c('Species_richness','Predominant_land_use','Use_intensity','percNH','SS','SSB','SSBS')])

# summaries
length(unique(model_data$SS)) # 577
length(unique(model_data$SSBS)) # 13666


# run set of simple models with different fixed effects structures

srmod <- GLMERSelect(modelData = model_data, 
                     responseVar = "Species_richness",
                     fitFamily = "poisson", 
                     fixedFactors = c("Predominant_land_use", "Use_intensity"),
                     fixedTerms = list(percNH = 1),
                     randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)", 
                     fixedInteractions = c("Predominant_land_use:poly(percNH,1)",
                                           "Use_intensity:poly(percNH,1)",
                                           "Predominant_land_use:Use_intensity"), verbose = F)

summary(srmod$model)
# model selected:
# Species_richness ~ Predominant_land_use + Use_intensity + poly(percNH, 1) + Use_intensity:poly(percNH, 1) + Predominant_land_use:Use_intensity +  
# (1 | SS) + (1 | SSB) + (1 | SSBS)

sr1 <- GLMER(modelData = model_data,responseVar = "Species_richness",fitFamily = "poisson",
             fixedStruct = "Predominant_land_use + Use_intensity + poly(percNH, 1) + Use_intensity:poly(percNH, 1) + Predominant_land_use:Use_intensity",randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)",REML = TRUE)

summary(sr1$model)

#Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#               Model failed to converge with max|grad| = 0.0041052 (tol = 0.001, component 1)



#### ABUNDANCE MODELS ####

model_data <- na.omit(sites.sub[,c('LogAbun','Predominant_land_use','Use_intensity','percNH','SS','SSB','SSBS')])

# summaries
length(unique(model_data$SS)) # 510
length(unique(model_data$SSBS)) # 11396


abmod <- GLMERSelect(modelData = model_data, 
                     responseVar = "LogAbun",
                     fitFamily = "gaussian", 
                     fixedFactors = c("Predominant_land_use", "Use_intensity"),
                     fixedTerms = list(percNH = 1),
                     randomStruct = "(1|SS)+(1|SSB)", 
                     fixedInteractions = c("Predominant_land_use:poly(percNH,1)",
                                           "Use_intensity:poly(percNH,1)",
                                           "Predominant_land_use:Use_intensity"), verbose = F)

summary(abmod$model)
# model selected:
# LogAbun ~ Predominant_land_use + Use_intensity + Predominant_land_use:Use_intensity + (1 | SS) + (1 | SSB)


## no significant effect of percNH or interactions

# run set of simple models with different fixed effects structures
#ab1 <- GLMER(modelData = model_data,responseVar = "LogAbun",fitFamily = "gaussian",
#             fixedStruct = "Predominant_land_use + Use_intensity + percNH_rs + Predominant_land_use*Use_intensity",randomStruct = "(1|SS)+(1|SSB)",REML = TRUE)


#summary(ab1$model)



### Predict responses for plotting ###



# basic table of median values and reference factors
pred_tab <- data.frame(percNH = NA,
                       Use_intensity = "Minimal use",
                       Predominant_land_use = "Primary vegetation",
                       Species_richness = 0,
                       LogAbun = 0)

levels(pred_tab$Predominant_land_use) <- levels(sr1$data$Predominant_land_use)
levels(pred_tab$Use_intensity) <- levels(sr1$data$Use_intensity) 

vals <- 1000

pred_tab2 <- do.call("rbind", replicate(vals, pred_tab, simplify = FALSE))

pred_tab2$percNH_rs <- seq(from = 0, to = 100, length.out = 1000)

# rescale the values
pred_tab2$percNH_rs <-(pred_tab2$percNH_rs-values[, "centre"])/values[, "scale"]


# predict the result
result <- PredictGLMER(model = sr1$model, data = pred_tab2, se.fit = TRUE, seMultiplier = 1.96)

# transform the results
result <- exp(result)

vals <- seq(from = 0, to = 100, length.out = 1000)



# SR plot = full range
ggplot(data = result) +
  geom_line(aes(x = vals, y = y), col = c("#66CD00")) +
  geom_ribbon(aes(x = seq(from = 0, to = 100, length.out = 1000), ymin= yminus, ymax = yplus), fill = c("#66CD00"), alpha = 0.3) +
  geom_rug(data = final.data.trans, aes(x = percNH), size = 0.1) +
  ylim(c(0,30)) +
  xlim(c(0, 100)) +
  xlab("Percentage of Natural Habitat") +
  ylab("Species Richness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.8,0.8), legend.title = element_blank(),
        aspect.ratio = 1) 

ggsave(filename = paste0(outdir, "/SR_simplemod_percNH_plot.png"))

### abundance model ###

# predict the result
result2 <- PredictGLMER(model = ab1$model, data = pred_tab2[,c(1:3,5)], se.fit = TRUE, seMultiplier = 1.96)

# transform the results
result2 <- exp(result2)-1

vals <- seq(from = 0, to = 100, length.out = 1000)

# SR plot = full range
ggplot(data = result2) +
  geom_line(aes(x = vals, y = y), col = c("#66CD00")) +
  geom_ribbon(aes(x = vals, ymin= yminus, ymax = yplus), fill = c("#66CD00"), alpha = 0.3) +
  geom_rug(data = final.data.trans, aes(x = percNH), size = 0.1) +
  ylim(c(0,300)) +
  xlim(c(0, 100)) +
  xlab("Percentage of Natural Habitat") +
  ylab("Total abundance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.8,0.8), legend.title = element_blank(),
        aspect.ratio = 1) 

ggsave(filename = paste0(outdir, "/AB_simplemod_percNH_plot.png"))





##%######################################################%##
#                                                          #
####                    Model checks                    ####
#                                                          #
##%######################################################%##


# check out the performance package
library(performance)
