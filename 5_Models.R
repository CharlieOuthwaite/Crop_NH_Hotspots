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
pred.sites.metrics <- SiteMetrics(predicts, extra.cols = c("Predominant_land_use", "SSB", "SSBS", "Biome")) # 22678 rows


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


#save(sites.sub, file = paste0(outdir, "/Predicts_site_level.rdata"))
#load(file = paste0(outdir, "/Predicts_site_level.rdata"))

##%######################################################%##
#                                                          #
####             Percentage natural habitat             ####
#                                                          #
##%######################################################%##

# use Jung habitat data, combining natural habitat types 

# # read in teh fractional natural habitat data
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

# how many NAs
nrow(sites.sub[is.na(sites.sub$percNH_Jung4),]) #0
nrow(sites.sub[is.na(sites.sub$percNH_Jung2),]) #0


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
# 6964        4659        2019 

# set land use as character variable
sites.sub$Predominant_land_use <- as.character(sites.sub$Predominant_land_use)


# combine secondary land uses
sites.sub$Predominant_land_use <- sub("Mature secondary vegetation", "Secondary vegetation", sites.sub$Predominant_land_use)
sites.sub$Predominant_land_use <- sub("Intermediate secondary vegetation", "Secondary vegetation", sites.sub$Predominant_land_use)
sites.sub$Predominant_land_use <- sub("Young secondary vegetation", "Secondary vegetation", sites.sub$Predominant_land_use)
sites.sub[sites.sub$Predominant_land_use == "Secondary vegetation (indeterminate age)", 'Predominant_land_use'] <- "Secondary vegetation"

table(sites.sub$Predominant_land_use)

# Cropland   Primary vegetation Secondary vegetation 
# 2555                 6547                4540 

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


# should these be transformed for analysis?


##%######################################################%##
#                                                          #
####                 Run simple models                  ####
#                                                          #
##%######################################################%##


#### SPECIES RICHNESS MODELS ####

# remove NAs in the specified columns
model_data <- na.omit(sites.sub[,c('Species_richness','Predominant_land_use','Use_intensity','percNH_Jung2', 'percNH_Jung4', 'Tropical', 'Biome', 'SS','SSB','SSBS')])

# summaries
length(unique(model_data$SS)) # 577
length(unique(model_data$SSBS)) # 13666


# run set of simple models with different fixed effects structures
# see comparison markdown/PDF
# realm only 

# using Jung4 data, currently untransformed

srmod <- GLMERSelect(modelData = model_data, 
                     responseVar = "Species_richness",
                     fitFamily = "poisson", 
                     fixedFactors = c("Predominant_land_use", "Use_intensity"),
                     fixedTerms = list(percNH_Jung4 = 1),
                     randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)", 
                     fixedInteractions = c("Predominant_land_use:poly(percNH_Jung4,1)", "Predominant_land_use:Use_intensity", "Use_intensity:poly(percNH_Jung4,1)"),  verbose = F)

summary(srmod$model)
# model selected not inc use intensity:
# Species_richness ~ Predominant_land_use + poly(percNH_Jung4, 1) + (1 | SS) + (1 | SSB) + (1 | SSBS)

#Warning message:
#  In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                 Model failed to converge with max|grad| = 0.00109362 (tol = 0.001, component 1)

# model selected inc use intensity:
# Species_richness ~ Predominant_land_use + Use_intensity + poly(percNH_Jung4, 1) + Predominant_land_use:Use_intensity + Use_intensity:poly(percNH_Jung4, 1) + (1 | SS) + (1 | SSB) + (1 | SSBS)

#7: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge with max|grad| = 0.00200249 (tol = 0.001, component 1)


sr1 <- GLMER(modelData = model_data,responseVar = "Species_richness",fitFamily = "poisson",
             fixedStruct = "Predominant_land_use + Use_intensity + poly(percNH_Jung4, 1) + Predominant_land_use:Use_intensity + Use_intensity:poly(percNH_Jung4, 1)",randomStruct = "(1|SS)+(1|SSB)+(1|SSBS)",
             REML = TRUE)

summary(sr1$model)

# tried a different optimizer but still got the warning below.

# save model outputs for both models and stats table
save(srmod, file = paste0(outdir, "/Richness_Jung4_modelselection_incUI.rdata"))
save(sr1, file = paste0(outdir, "/Richness_Jung4_finalmod_incUI.rdata"))

write.csv(srmod$stats, file = paste0(outdir, "/Richness_Jung4_stats_incUI.csv"))

# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#               Model failed to converge with max|grad| = 0.00200249 (tol = 0.001, component 1)



#load(file = paste0(outdir, "/Richness_Jung4_modelselection_incUI.rdata"))
#load(file = paste0(outdir, "/Richness_Jung4_finalmod_incUI.rdata"))
#load(file = paste0(outdir, "/Richness_Jung4_modelselection.rdata"))
#load(file = paste0(outdir, "/Richness_Jung4_finalmod.rdata"))

#### ABUNDANCE MODELS ####

model_data <- na.omit(sites.sub[,c('LogAbun','Predominant_land_use','Use_intensity','percNH_Jung4', 'Tropical', 'Biome', 'SS','SSB','SSBS')])

# summaries
length(unique(model_data$SS)) # 510
length(unique(model_data$SSBS)) # 11396


abmod <- GLMERSelect(modelData = model_data, 
                     responseVar = "LogAbun",
                     fitFamily = "gaussian", 
                     fixedFactors = c("Predominant_land_use", "Use_intensity"),
                     fixedTerms = list(percNH_Jung4 = 1),
                     randomStruct = "(1|SS)+(1|SSB)", 
                     fixedInteractions = c("Predominant_land_use:poly(percNH_Jung4,1)", "Predominant_land_use:Use_intensity", "Use_intensity:poly(percNH_Jung4,1)"), verbose = F)


summary(abmod$model)
# model selected:
# LogAbun ~ Predominant_land_use + Predominant_land_use:poly(percNH_Jung4, 1) + poly(percNH_Jung4, 1) + (1 | SS) + (1 | SSB)

# model selected inc use intensity
# LogAbun ~ Predominant_land_use + Use_intensity + Predominant_land_use:poly(percNH_Jung4, 1) + Predominant_land_use:Use_intensity + Use_intensity:poly(percNH_Jung4,1) + poly(percNH_Jung4, 1) + (1 | SS) + (1 | SSB)

ab1 <- GLMER(modelData = model_data,responseVar = "LogAbun",fitFamily = "gaussian",
             fixedStruct = "Predominant_land_use + poly(percNH_Jung4, 1) + Predominant_land_use:poly(percNH_Jung4, 1)",
             randomStruct = "(1|SS)+(1|SSB)",
             REML = TRUE, 
             saveVars = "SSBS")

summary(ab1$model)


# save model outputs for both models and stats table

save(abmod, file = paste0(outdir, "/Abundance_Jung4_modelselection_incUI.rdata"))
save(ab1, file = paste0(outdir, "/Abundance_Jung4_finalmod.rdata"))

write.csv(abmod$stats, file = paste0(outdir, "/Abundance_Jung4_stats_incUI.csv"))

# load(file = paste0(outdir, "/Abundance_Jung4_modelselection_incUI.rdata"))
# load(file = paste0(outdir, "/Abundance_Jung4_finalmod_incUI.rdata"))
# load(file = paste0(outdir, "/Abundance_Jung4_modelselection.rdata"))
# load(file = paste0(outdir, "/Abundance_Jung4_finalmod.rdata"))

##%######################################################%##
#                                                          #
####                       Plots                        ####
#                                                          #
##%######################################################%##


#### Species Richness ####

PlotGLMERFactor(model = srmod$model,data = srmod$data,
                responseVar = "Species Richness",seMultiplier = 1.96,
                logLink = "e",catEffects = "Predominant_land_use", params = list(las = 2, cex = 0.8, mar = c(9,5,1,4), adj = 1), xtext.srt = 50)


# plot of percNH
pred_tab <- expand.grid(percNH_Jung4 = seq(from = min(sites.sub$percNH_Jung4),
                                           to = max(sites.sub$percNH_Jung4),
                                           length.out = 500),
                        Predominant_land_use = factor(c("Primary vegetation"),
                                                      levels = levels(sites.sub$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"), 
                                               levels = levels(sites.sub$Use_intensity)),
                        Species_richness = 0)

pred.sr <- PredictGLMERRandIter(model = srmod$model,data = pred_tab)

pred.sr <- exp(pred.sr)

refRow <- which((pred_tab$Predominant_land_use=="Primary vegetation") & (pred_tab$percNH_Jung4==min(abs(pred_tab$percNH_Jung4))))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100

# SR plot = full range
ggplot(data = pred_tab) +
  geom_line(aes(x = percNH_Jung4, y = PredMedian), col = c("#006400")) +
  geom_ribbon(aes(x = percNH_Jung4, ymin= PredLower, ymax = PredUpper), fill = c("#006400"), alpha = 0.3) +
  #geom_rug(data = sites.sub, aes(x = percNH_Jung4, col = Predominant_land_use), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylim(c(-10, 20)) +
  xlim(c(0, 1)) +
  #scale_colour_manual(values = c("#006400"))+
  #scale_fill_manual(values = c("#006400")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in species richness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) +
  ggtitle("Jung4 dataset - forest, grassland, shrubland, savanna")


ggsave(filename = paste0(outdir, "/Plot_richness_percNH_jung4_incUI.pdf"))



#### plot of interaction between UI and percNH ####

pred_tab <- expand.grid(percNH_Jung4 = seq(from = min(sites.sub$percNH_Jung4),
                                           to = max(sites.sub$percNH_Jung4),
                                           length.out = 1000),
                        Predominant_land_use = factor(c("Primary vegetation"),
                                                      levels = levels(sites.sub$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"), 
                                               levels = levels(sites.sub$Use_intensity)),
                        Species_richness = 0)

pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))
pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub$Use_intensity))

pred.sr <- PredictGLMERRandIter(model = srmod$model,data = pred_tab, nIters = 3000)

pred.sr <- exp(pred.sr)

refRow <- which((pred_tab$Predominant_land_use=="Primary vegetation") & (pred_tab$percNH_Jung4==min(abs(pred_tab$percNH_Jung4))) &(pred_tab$Use_intensity=="Minimal use"))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100

# SR plot = full range
ggplot(data = pred_tab) +
  geom_line(aes(x = percNH_Jung4, y = PredMedian, col = Use_intensity)) +
  geom_ribbon(aes(x = percNH_Jung4, ymin= PredLower, ymax = PredUpper, fill = Use_intensity), alpha = 0.3) +
  geom_rug(data = sites.sub, aes(x = percNH_Jung4, col = Use_intensity), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylim(c(-10, 50)) +
  xlim(c(0, 1)) +
  scale_colour_manual(values = c("#66CD00", "#FFB90F", "#EE0000"))+
  scale_fill_manual(values = c("#66CD00", "#FFB90F", "#EE0000")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in species richness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) +
  ggtitle("Jung4 dataset - forest, grassland, shrubland, savanna")


ggsave(filename = paste0(outdir, "/Plot_richness_percNHUI_jung4_incUI.pdf"))

## LU:UI

pred_tab <- expand.grid(percNH_Jung4 = median(sites.sub$percNH_Jung4),
                        Predominant_land_use = factor(c("Primary vegetation"),
                                                      levels = levels(sites.sub$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"), 
                                               levels = levels(sites.sub$Use_intensity)),
                        Species_richness = 0)


pred_tab <- do.call("rbind", replicate(9, pred_tab, simplify = FALSE))

pred_tab[4:6, 'Predominant_land_use'] <- "Secondary vegetation"
pred_tab[7:9, 'Predominant_land_use'] <- "Cropland"

pred_tab[c(2,5,8), 'Use_intensity'] <- "Light use"
pred_tab[c(3,6,9), 'Use_intensity'] <- "Intense use"


# predict the result
resulta <- PredictGLMERRandIter(model = srmod$model, data = pred_tab)

# transform the results
resulta <- exp(resulta)

resulta <- sweep(x = resulta, MARGIN = 2, STATS = resulta[1,], FUN = '/')

resulta.median <- ((apply(X = resulta, MARGIN = 1, FUN = median))*100)-100
resulta.upper <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
resulta.lower <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100


errbar.cols <- c(rep("#006400",3),rep("#8B0000", 3), rep("#EEAD0E", 3))


pdf(file = paste0(outdir, "/Plot_Richness_LUUI.pdf"))
par(mar=c(5,5,1,1))

errbar(x = 1:9,y = resulta.median,yplus = resulta.upper,yminus = resulta.lower,
       col=errbar.cols,errbar.col = errbar.cols,
       ylim=c(min(resulta.lower),max(resulta.upper)),xaxt="n",
       pch =rep(c(16,17,18), 3), 
       ylab="Species Richness (%)",xlab="",bty="l", cex.lab =1.6, cex.axis = 1.6, cex = 1.5)

axis(side = 1,at = c(2,5,8),
     labels = c("Primary \nvegetation","Secondary\nvegetation", "Cropland"),
     padj = 0.5, cex.axis =1.6)

abline(h=0,col="#00000077",lty=2)

legend("topright", 
       legend = c("Minimal Use", "Light Use", "Intense Use"),
       pch = c(16,17,18), bty = "n", inset=c(0,0), cex =1.8)

dev.off()



#### Abundance ####


PlotGLMERFactor(model = abmod$model,data = abmod$data,
                responseVar = "Total Abundance",seMultiplier = 1.96,
                logLink = "e",catEffects = "Predominant_land_use", params = list(las = 2, cex = 0.8, mar = c(9,5,1,4), adj = 1), xtext.srt = 50)



# plot of percNH interaction with LU
pred_tab <- expand.grid(percNH_Jung4 = seq(from = min(sites.sub$percNH_Jung4),
                                           to = max(sites.sub$percNH_Jung4),
                                           length.out = 500),
                        Predominant_land_use = factor(c("Primary vegetation","Secondary vegetation","Cropland"),
                                                      levels = levels(sites.sub$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"), 
                                               levels = levels(sites.sub$Use_intensity)),
                        LogAbun = 0)

pred.abun <- PredictGLMERRandIter(model = abmod$model,data = pred_tab)

pred.abun <- exp(pred.abun)-1

refRow <- which((pred_tab$Predominant_land_use=="Primary vegetation") & (pred_tab$percNH_Jung4==min(abs(pred_tab$percNH_Jung4))) & (pred_tab$Use_intensity == "Minimal use"))

pred.abun <- sweep(x = pred.abun,MARGIN = 2,STATS = pred.abun[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.abun,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.abun,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.abun,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100

# SR plot = full range
ggplot(data = pred_tab) +
  geom_line(aes(x = percNH_Jung4, y = PredMedian, col = Predominant_land_use)) +
  geom_ribbon(aes(x = percNH_Jung4, ymin= PredLower, ymax = PredUpper, fill = Predominant_land_use), alpha = 0.3) +
  geom_rug(data = sites.sub, aes(x = percNH_Jung4, col = Predominant_land_use), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylim(c(-50, 20)) +
  xlim(c(0, 1)) +
  scale_colour_manual(values = c("#006400", "#8B0000", "#EEAD0E"))+
  scale_fill_manual(values = c("#006400", "#8B0000", "#EEAD0E")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in total abundance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.25,0.85), legend.title = element_blank(),
        aspect.ratio = 1) +
  ggtitle("Jung4 dataset - forest, grassland, shrubland, savanna")


ggsave(filename = paste0(outdir, "/Plot_abundance_percNHLU_jung4_incUI.pdf"))


#### plot of interaction between UI and percNH ####

pred_tab <- expand.grid(percNH_Jung4 = seq(from = min(sites.sub$percNH_Jung4),
                                           to = max(sites.sub$percNH_Jung4),
                                           length.out = 1000),
                        Predominant_land_use = factor(c("Primary vegetation"),
                                                      levels = levels(sites.sub$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"), 
                                               levels = levels(sites.sub$Use_intensity)),
                        LogAbun = 0)

pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))
pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub$Use_intensity))

pred.sr <- PredictGLMERRandIter(model = abmod$model,data = pred_tab)

pred.sr <- exp(pred.sr)-1

refRow <- which((pred_tab$Predominant_land_use=="Primary vegetation") & (pred_tab$percNH_Jung4==min(abs(pred_tab$percNH_Jung4))) &(pred_tab$Use_intensity=="Minimal use"))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100

# SR plot = full range
ggplot(data = pred_tab) +
  geom_line(aes(x = percNH_Jung4, y = PredMedian, col = Use_intensity)) +
  geom_ribbon(aes(x = percNH_Jung4, ymin= PredLower, ymax = PredUpper, fill = Use_intensity), alpha = 0.3) +
  geom_rug(data = sites.sub, aes(x = percNH_Jung4, col = Use_intensity), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylim(c(-30, 50)) +
  xlim(c(0, 1)) +
  scale_colour_manual(values = c("#66CD00", "#FFB90F", "#EE0000"))+
  scale_fill_manual(values = c("#66CD00", "#FFB90F", "#EE0000")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in total abundance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) +
  ggtitle("Jung4 dataset - forest, grassland, shrubland, savanna")


ggsave(filename = paste0(outdir, "/Plot_abundance_percNHUI_jung4_incUI.pdf"))

## LU:UI

pred_tab <- expand.grid(percNH_Jung4 = median(sites.sub$percNH_Jung4),
                        Predominant_land_use = factor(c("Primary vegetation"),
                                                      levels = levels(sites.sub$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"), 
                                               levels = levels(sites.sub$Use_intensity)),
                        LogAbun = 0)


pred_tab <- do.call("rbind", replicate(9, pred_tab, simplify = FALSE))

pred_tab[4:6, 'Predominant_land_use'] <- "Secondary vegetation"
pred_tab[7:9, 'Predominant_land_use'] <- "Cropland"

pred_tab[c(2,5,8), 'Use_intensity'] <- "Light use"
pred_tab[c(3,6,9), 'Use_intensity'] <- "Intense use"


# predict the result
resulta <- PredictGLMERRandIter(model = abmod$model, data = pred_tab)

# transform the results
resulta <- exp(resulta)-1

resulta <- sweep(x = resulta, MARGIN = 2, STATS = resulta[1,], FUN = '/')

resulta.median <- ((apply(X = resulta, MARGIN = 1, FUN = median))*100)-100
resulta.upper <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
resulta.lower <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100


errbar.cols <- c(rep("#006400",3),rep("#8B0000", 3), rep("#EEAD0E", 3))


pdf(file = paste0(outdir, "/Plot_Abundance_LUUI.pdf"))
par(mar=c(5,5,1,1))

errbar(x = 1:9,y = resulta.median,yplus = resulta.upper,yminus = resulta.lower,
       col=errbar.cols,errbar.col = errbar.cols,
       ylim=c(min(resulta.lower),max(resulta.upper)),xaxt="n",
       pch =rep(c(16,17,18), 3), 
       ylab="Total Abundance (%)",xlab="",bty="l", cex.lab =1.6, cex.axis = 1.6, cex = 1.5)

axis(side = 1,at = c(2,5,8),
     labels = c("Primary \nvegetation","Secondary\nvegetation", "Cropland"),
     padj = 0.5, cex.axis =1.6)

abline(h=0,col="#00000077",lty=2)

legend("topright", 
       legend = c("Minimal Use", "Light Use", "Intense Use"),
       pch = c(16,17,18), bty = "n", inset=c(0,0), cex =1.8)

dev.off()


##%######################################################%##
#                                                          #
####                    Model checks                    ####
#                                                          #
##%######################################################%##

# https://easystats.github.io/performance/

# check out the performance package
library(performance)

#### checks: richness ####

# 1. check overdispersion

check_overdispersion(sr1$model)

# Overdispersion test

# dispersion ratio =    0.469
# Pearson's Chi-Squared = 6412.834
#                p-value =        1

# No overdispersion detected.


# 2. check zero inflation

check_zeroinflation(sr1$model)

# Check for zero-inflation

# Observed zeros: 525
# Predicted zeros: 661
# Ratio: 1.26

# Model is overfitting zeros.


# 3. check for singular model fits

check_singularity(sr1$model)

# FALSE


# 4. check for heteroskedasticity

check_heteroscedasticity(sr1$model)

# Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.000).


# 5. visualisations of model checks

check_model(sr1$model)

# save the plots


# 6. Double checking the collinearity


check_collinearity(sr1$model)

# Check for Multicollinearity

# Low Correlation

# Parameter  VIF Increased SE
# Predominant_land_use 3.93         1.98
# poly(percNH_Jung4, 1) 1.59         1.26
# Use_intensity:poly(percNH_Jung4, 1) 2.70         1.64

# Moderate Correlation

# Parameter  VIF Increased SE
# Use_intensity 5.27         2.30

# High Correlation

# Parameter   VIF Increased SE
# Predominant_land_use:Use_intensity 22.22         4.71

## 1. Checking the fitted vs residuals relationship
p1 <- plot(sr1$model)


## 2. Normality of Residuals
pdf(NULL)
dev.control(displaylist="enable")
qqnorm(resid(sr1$model), main = "")
qqline(resid(sr1$model))
p2 <- recordPlot()
invisible(dev.off())


## 3. Check for spatial autocorrelation

sa_test<-roquefort::SpatialAutocorrelationTest(model=sr1, all.data=sites.sub)

#summary(sa_test)

# percentage of studies that show spatial autocorrelation?
perc_auto <- (length(which(sa_test$P<0.05))/length(sa_test$P))*100


sa_test_vals <- as.data.frame(sa_test$P)
sa_test_vals$`sa_test$P` <- round(sa_test_vals$`sa_test$P`, digits = 4)

label1 <- paste0("P < 0.05 \nin ", round(perc_auto, 1), "% \nof studies")

p3 <- ggplot(data = sa_test_vals ) +
  geom_histogram(aes(x = sa_test_vals$`sa_test$P`)) +
  geom_vline(xintercept = 0.05, col = "red") +
  geom_text(aes(x = 0.9, y = 90, label = label1), size = 4, check_overlap = T) +
  theme_bw() +
  ylim(c(0, 100)) +
  xlab("P-value") +
  ylab("Frequency") +
  theme(panel.grid = element_blank(), 
        aspect.ratio = 1)


plot_grid(p2,p3,
          labels = c("A.", "B."))
ggsave(file = paste0(outdir, "/Model_checks_additional_Richness.pdf"), height = 5, width = 10)


#### checks: abundance ####

# 1. check overdispersion 

# GLMMs only, not valid here where family = gaussian

# 2. check zero inflation

# GLMMs only, not valid here where family = gaussian

# 3. check for singular model fits

check_singularity(ab1$model)

# FALSE

# 4. check for heteroskedasticity

check_heteroscedasticity(ab1$model)

# OK: Error variance appears to be homoscedastic (p = 0.455).


# 5. visualisations of model checks

check_model(ab1$model)

# 6. double check collinarity

check_collinearity(ab1$model)

# 7. Check outliers
check_outliers(ab1$model)

# OK: No outliers detected.
# Warning messages:
#  1: Some model terms could not be found in model data. You probably need to load the data into the environment. 
#  2: In hatvalues.merMod(model) :
#  the hat matrix may not make sense for GLMMs

# Check for Multicollinearity

# Low Correlation

# Parameter  VIF Increased SE
# Predominant_land_use 4.05         2.01
# poly(percNH_Jung4, 1) 2.86         1.69
# Predominant_land_use:poly(percNH_Jung4, 1) 2.30         1.52
# Use_intensity:poly(percNH_Jung4, 1) 3.23         1.80

# Moderate Correlation

# Parameter  VIF Increased SE
# Use_intensity 5.59         2.36

# High Correlation

# Parameter   VIF Increased SE
# Predominant_land_use:Use_intensity 20.34         4.51


# save the plots


## 1. Checking the fitted vs residuals relationship
p1 <- plot(ab1$model)


## 2. Normality of Residuals
pdf(NULL)
dev.control(displaylist="enable")
qqnorm(resid(ab1$model), main = "")
qqline(resid(ab1$model))
p2 <- recordPlot()
invisible(dev.off())


## 3. Check for spatial autocorrelation

sa_test<-roquefort::SpatialAutocorrelationTest(model=ab1, all.data=sites.sub)

#summary(sa_test)

# percentage of studies that show spatial autocorrelation?
perc_auto <- (length(which(sa_test$P<0.05))/length(sa_test$P))*100


sa_test_vals <- as.data.frame(sa_test$P)
sa_test_vals$`sa_test$P` <- round(sa_test_vals$`sa_test$P`, digits = 4)

label1 <- paste0("P < 0.05 \nin ", round(perc_auto, 1), "% \nof studies")

p3 <- ggplot(data = sa_test_vals ) +
  geom_histogram(aes(x = sa_test_vals$`sa_test$P`)) +
  geom_vline(xintercept = 0.05, col = "red") +
  geom_text(aes(x = 0.9, y = 90, label = label1), size = 4, check_overlap = T) +
  theme_bw() +
  ylim(c(0, 100)) +
  xlab("P-value") +
  ylab("Frequency") +
  theme(panel.grid = element_blank(), 
        aspect.ratio = 1)


plot_grid(p1,p2,p3,
          labels = c("A.", "B.", "C."))
ggsave(file = paste0(outdir, "/Model_checks_additional_Abundance.pdf"), height = 10, width = 10)

