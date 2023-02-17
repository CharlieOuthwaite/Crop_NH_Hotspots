##%######################################################%##
#                                                          #
####                       Plots                        ####
#                                                          #
##%######################################################%##

rm(list = ls())


# load libraries
library(StatisticalModels)
library(ggplot2)
library(cowplot)

# directories
datadir <- "5_Models"
outdir <- "5b_Model_plots"
if(!dir.exists(outdir)) dir.create(outdir)


# load in the models
# all biodiversity
load(paste0(datadir, "/Richness_Jung2_Tropical_nopoly.rdata")) # sr1.trop
load(paste0(datadir, "/Richness_Jung2_Temperate_nopoly.rdata")) # sr1.temp
load(paste0(datadir, "/Abundance_Jung2_Tropical_nopoly.rdata")) # ab1.trop
load(paste0(datadir, "/Abundance_Jung2_Temperate_nopoly.rdata")) # ab1.temp


## load in the dataset ##
load(file = paste0(datadir, "/PREDICTS_dataset_incNH.rdata")) # sites.sub

levels(sites.sub$Predominant_land_use)
levels(sites.sub$Use_intensity)

scalers <- c(attr(sites.sub$percNH_Jung2_RS, "scaled:scale"), attr(sites.sub$percNH_Jung2_RS, "scaled:center"))

#### create custom theme to be applied to all plots ####

theme_custom <- theme(panel.grid = element_blank(),
                      legend.position = c(0.8,0.8), 
                      legend.title = element_blank(),
                      legend.text = element_text(size = 8),
                      aspect.ratio = 1, legend.background = element_blank(),
                      #text = element_text(size = 8), 
                      axis.text = element_text(size = 8), 
                      line = element_line(size = 0.2), 
                      panel.border = element_rect(size = 0.2),
                      axis.title = element_text(size = 8))


#### Species Richness ####

#### LU UI ####

# basic table of median values and reference factors
pred_tab3 <- data.frame(percNH_Jung2_RS = median(sites.sub$percNH_Jung2_RS),
                        Use_intensity = "Minimal use",
                        Predominant_land_use = "Primary vegetation",
                        #Tropical = "Temperate",
                        Species_richness = 0,
                        logAbun = 0)


# organise factor levels
# check levels of factor variables
pred_tab3$Predominant_land_use <- factor(pred_tab3$Predominant_land_use, levels = levels(sites.sub$Predominant_land_use))
pred_tab3$Use_intensity <- factor(pred_tab3$Use_intensity, levels = levels(sites.sub$Use_intensity)) 


# add and change factor levels of land use and intensity
pred_tab3 <- do.call("rbind", replicate(9, pred_tab3, simplify = FALSE))

pred_tab3[4:6, 'Predominant_land_use'] <- "Secondary vegetation"
pred_tab3[7:9, 'Predominant_land_use'] <- "Cropland"

pred_tab3[c(2,5,8), 'Use_intensity'] <- "Light use"
pred_tab3[c(3,6,9), 'Use_intensity'] <- "Intense use"


### Tropical predictions ###

# predict the result
resulta <- PredictGLMERRandIter(model = sr1.trop$model, data = pred_tab3)

# transform the results
resulta <- exp(resulta)

resulta <- sweep(x = resulta, MARGIN = 2, STATS = resulta[1,], FUN = '/')

pred_tab3$median <- ((apply(X = resulta, MARGIN = 1, FUN = median))*100)-100
pred_tab3$upper <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
pred_tab3$lower <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100

pred_tab3$realm <- "Tropical"

# temperate

## Richness, Temperate ##

pred_tab4 <- pred_tab3[, 1:5]

# predict the result
resulta2 <- PredictGLMERRandIter(model = sr1.temp$model, data = pred_tab4)

# transform the results
resulta2 <- exp(resulta2)

resulta2 <- sweep(x = resulta2, MARGIN = 2, STATS = resulta2[1,], FUN = '/')

pred_tab4$median <- ((apply(X = resulta2, MARGIN = 1, FUN = median))*100)-100
pred_tab4$upper <- ((apply(X = resulta2, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
pred_tab4$lower <- ((apply(X = resulta2, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100

pred_tab4$realm <- "Non-tropical"

plot_data2 <- rbind(pred_tab3, pred_tab4)

plot_data2[plot_data2$upper == 0, c("upper", "lower")] <- NA

plot_data2$realm <- factor(plot_data2$realm, levels = c("Tropical", "Non-tropical"))

plot_data2$Predominant_land_use <- sub("Primary vegetation", "Primary\nvegetation", plot_data2$Predominant_land_use)
plot_data2$Predominant_land_use <- sub("Secondary vegetation", "Secondary\nvegetation", plot_data2$Predominant_land_use)

plot_data2$Predominant_land_use <- factor(plot_data2$Predominant_land_use, levels = c("Primary\nvegetation", "Secondary\nvegetation", "Cropland"))

ggplot(data = plot_data2)+
  geom_point(aes(x = Predominant_land_use, y = median, col = Predominant_land_use, shape = Use_intensity),
             position = position_dodge(width = 0.9), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper, y = median, x = Predominant_land_use, col = Predominant_land_use),
                position = position_dodge2(padding = 0.5)) +
  facet_grid(~ realm) +
  scale_colour_manual(values = c("#006400", "#8B0000", "#EEAD0E"), guide = F)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2)+
  xlab("") +
  ylab("Species Richness (%)") +
  theme_bw() +
  theme_custom +
  theme(legend.position = c(0.1,0.9), strip.background = element_rect(fill = NA, size = 0.2))

ggsave(filename = paste0(outdir, "/Rich_LUUI_Jung2_nopoly.pdf"), width = 6, height = 3, uni = "in")


rm(pred_tab3, pred_tab4, resulta, resulta2)



#### PERCNH ####

## tropical ##
pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub$percNH_Jung2_RS),
                                           to = max(sites.sub$percNH_Jung2_RS),
                                           length.out = 500),
                        Predominant_land_use = factor(c("Cropland"),
                                                      levels = levels(sites.sub$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"), 
                                               levels = levels(sites.sub$Use_intensity)),
                        Species_richness = 0)

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]

pred.sr <- PredictGLMERRandIter(model = sr1.trop$model,data = pred_tab)

pred.sr <- exp(pred.sr)

refRow <- which((pred_tab$Predominant_land_use=="Cropland") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100

## non-tropical ##
pred_tab2 <- pred_tab[, 1:5]

pred.sr.te <- PredictGLMERRandIter(model = sr1.temp$model,data = pred_tab2)

pred.sr.te <- exp(pred.sr.te)

refRow <- which((pred_tab2$Predominant_land_use=="Cropland") & (pred_tab2$percNH_Jung2 == 0))

pred.sr.te <- sweep(x = pred.sr.te,MARGIN = 2,STATS = pred.sr.te[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr.te,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr.te,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr.te,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)
  

# SR plot = full range
ggplot(data = plot_data) +
  geom_line(aes(x = percNH_Jung2, y = PredMedian), col = c("#006400")) +
  geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper), fill = c("#006400"), alpha = 0.3) +
  facet_wrap(~ realm) +
  #geom_rug(data = sites.sub, aes(x = percNH_Jung4, col = Predominant_land_use), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylim(c(-50, 100)) +
  xlim(c(0, 1)) +
  #scale_colour_manual(values = c("#006400"))+
  #scale_fill_manual(values = c("#006400")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in species richness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_richness_percNH_jung2_nopoly.pdf"))

rm(pred_tab, pred_tab2, pred.sr, pred.sr.te, plot_data, plot_data2)

#### plot of interaction between LU and percNH ####

pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub$percNH_Jung2_RS),
                                          to = max(sites.sub$percNH_Jung2_RS),
                                          length.out = 1000),
                       Predominant_land_use = factor(c("Cropland"),
                                                     levels = levels(sites.sub$Predominant_land_use)),
                       Use_intensity = factor(c("Minimal use"),
                                              levels = levels(sites.sub$Use_intensity)),
                       Species_richness = 0)

pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))
# pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
# pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub$Use_intensity))

pred_tab$Predominant_land_use <- c(rep("Primary vegetation", 1000), rep("Secondary vegetation", 1000), rep("Cropland", 1000))
pred_tab$Predominant_land_use <- factor(pred_tab$Predominant_land_use, levels = levels(sites.sub$Predominant_land_use))

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]


## tropical ##

# predict values
pred.sr <- PredictGLMERRandIter(model = sr1.trop$model,data = pred_tab, nIters = 3000)

pred.sr <- exp(pred.sr)

refRow <- which((pred_tab$Predominant_land_use=="Cropland") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                             FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                             FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


## non-tropical ##

pred_tab2 <- pred_tab[, 1:5]

# predict values
pred.sr2 <- PredictGLMERRandIter(model = sr1.temp$model,data = pred_tab2, nIters = 3000)

pred.sr2 <- exp(pred.sr2)

refRow <- which((pred_tab2$Predominant_land_use=="Cropland") & (pred_tab2$percNH_Jung2 == 0))

pred.sr2 <- sweep(x = pred.sr2,MARGIN = 2,STATS = pred.sr2[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr2,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr2,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)


# SR plot = full range
ggplot(data = plot_data) +
 geom_line(aes(x = percNH_Jung2, y = PredMedian, col = Predominant_land_use)) +
 geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper, fill = Predominant_land_use), alpha = 0.3) +
 geom_rug(data = sites.sub, aes(x = percNH_Jung2, col = Predominant_land_use), size = 0.1) +
 geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~realm) +
 ylim(c(-50, 100)) +
 xlim(c(0, 1)) +
 scale_colour_manual(values = c("#006400", "#8B0000", "#EEAD0E"))+
 scale_fill_manual(values = c("#006400", "#8B0000", "#EEAD0E")) +
 xlab("Proportion Natural Habitat") +
 ylab("% Change in species richness") +
 theme_bw() +
 theme(panel.grid = element_blank(),
       aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_richness_percNH_LU_jung2_nopoly.pdf"))

rm(pred_tab, pred_tab2, pred.sr, pred.sr2, plot_data)


#### plot of interaction between UI and percNH ####

pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub$percNH_Jung2_RS),
                                              to = max(sites.sub$percNH_Jung2_RS),
                                              length.out = 1000),
                        Predominant_land_use = factor(c("Cropland"),
                                                      levels = levels(sites.sub$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"),
                                               levels = levels(sites.sub$Use_intensity)),
                        Species_richness = 0)

pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))

pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub$Use_intensity))

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]


## tropical ##

# predict values
pred.sr <- PredictGLMERRandIter(model = sr1.trop$model,data = pred_tab, nIters = 3000)

pred.sr <- exp(pred.sr)

refRow <- which((pred_tab$Use_intensity=="Minimal use") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


## non-tropical ##

pred_tab2 <- pred_tab[, 1:5]

# predict values
pred.sr2 <- PredictGLMERRandIter(model = sr1.temp$model,data = pred_tab2, nIters = 3000)

pred.sr2 <- exp(pred.sr2)

refRow <- which((pred_tab2$Use_intensity=="Minimal use") & (pred_tab2$percNH_Jung2 == 0))

pred.sr2 <- sweep(x = pred.sr2,MARGIN = 2,STATS = pred.sr2[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr2,MARGIN = 1,
                                FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)


# SR plot = full range
ggplot(data = plot_data) +
  geom_line(aes(x = percNH_Jung2, y = PredMedian, col = Use_intensity)) +
  geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper, fill = Use_intensity), alpha = 0.3) +
  geom_rug(data = sites.sub, aes(x = percNH_Jung2, col = Use_intensity), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~realm) +
  ylim(c(-50, 100)) +
  xlim(c(0, 1)) +
  scale_colour_manual(values = c("#66CD00", "#FFB90F", "#EE0000"))+
  scale_fill_manual(values = c("#66CD00", "#FFB90F", "#EE0000")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in species richness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_richness_percNH_UI_jung2_nopoly.pdf"))


rm(pred_tab, pred_tab2, pred.sr, pred.sr2, plot_data)






#### ABUNDANCE ####


#### LU UI ####

# basic table of median values and reference factors
pred_tab3 <- data.frame(percNH_Jung2_RS = median(sites.sub$percNH_Jung2_RS),
                        Use_intensity = "Minimal use",
                        Predominant_land_use = "Primary vegetation",
                        #Tropical = "Temperate",
                        Species_richness = 0,
                        LogAbun = 0)


# organise factor levels
# check levels of factor variables
pred_tab3$Predominant_land_use <- factor(pred_tab3$Predominant_land_use, levels = levels(sites.sub$Predominant_land_use))
pred_tab3$Use_intensity <- factor(pred_tab3$Use_intensity, levels = levels(sites.sub$Use_intensity)) 


# add and change factor levels of land use and intensity
pred_tab3 <- do.call("rbind", replicate(9, pred_tab3, simplify = FALSE))

pred_tab3[4:6, 'Predominant_land_use'] <- "Secondary vegetation"
pred_tab3[7:9, 'Predominant_land_use'] <- "Cropland"

pred_tab3[c(2,5,8), 'Use_intensity'] <- "Light use"
pred_tab3[c(3,6,9), 'Use_intensity'] <- "Intense use"


### Tropical predictions ###

# predict the result
resulta <- PredictGLMERRandIter(model = ab1.trop$model, data = pred_tab3)

# transform the results
resulta <- exp(resulta)-1

resulta <- sweep(x = resulta, MARGIN = 2, STATS = resulta[1,], FUN = '/')

pred_tab3$median <- ((apply(X = resulta, MARGIN = 1, FUN = median))*100)-100
pred_tab3$upper <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
pred_tab3$lower <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100

pred_tab3$realm <- "Tropical"

# temperate

## Richness, Temperate ##

pred_tab4 <- pred_tab3[, 1:5]

# predict the result
resulta2 <- PredictGLMERRandIter(model = ab1.temp$model, data = pred_tab4)

# transform the results
resulta2 <- exp(resulta2)-1

resulta2 <- sweep(x = resulta2, MARGIN = 2, STATS = resulta2[1,], FUN = '/')

pred_tab4$median <- ((apply(X = resulta2, MARGIN = 1, FUN = median))*100)-100
pred_tab4$upper <- ((apply(X = resulta2, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
pred_tab4$lower <- ((apply(X = resulta2, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100

pred_tab4$realm <- "Non-tropical"

plot_data2 <- rbind(pred_tab3, pred_tab4)

plot_data2[plot_data2$upper == 0, c("upper", "lower")] <- NA

plot_data2$realm <- factor(plot_data2$realm, levels = c("Tropical", "Non-tropical"))

plot_data2$Predominant_land_use <- sub("Primary vegetation", "Primary\nvegetation", plot_data2$Predominant_land_use)
plot_data2$Predominant_land_use <- sub("Secondary vegetation", "Secondary\nvegetation", plot_data2$Predominant_land_use)

plot_data2$Predominant_land_use <- factor(plot_data2$Predominant_land_use, levels = c("Primary\nvegetation", "Secondary\nvegetation", "Cropland"))

ggplot(data = plot_data2)+
  geom_point(aes(x = Predominant_land_use, y = median, col = Predominant_land_use, shape = Use_intensity),
             position = position_dodge(width = 0.9), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper, y = median, x = Predominant_land_use, col = Predominant_land_use),
                position = position_dodge2(padding = 0.5)) +
  facet_grid(~ realm) +
  scale_colour_manual(values = c("#006400", "#8B0000", "#EEAD0E"), guide = F)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2)+
  xlab("") +
  ylab("Abundance (%)") +
  theme_bw() +
  theme_custom +
  theme(legend.position = "bottom", strip.background = element_rect(fill = NA, size = 0.2))


ggsave(filename = paste0(outdir, "/Abundance_LUUI_Jung2_nopoly.pdf"), width = 6, height = 3, uni = "in")

rm(pred_tab3, pred_tab4, resulta, resulta2, plot_data2)

#### PERCNH ####

## tropical ##
pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub$percNH_Jung2_RS),
                                              to = max(sites.sub$percNH_Jung2_RS),
                                              length.out = 500),
                        Predominant_land_use = factor(c("Cropland"),
                                                      levels = levels(sites.sub$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"), 
                                               levels = levels(sites.sub$Use_intensity)),
                        LogAbun = 0)

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]

pred.sr <- PredictGLMERRandIter(model = ab1.trop$model,data = pred_tab)

pred.sr <- exp(pred.sr)-1

refRow <- which((pred_tab$Predominant_land_use=="Cropland") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100

## non-tropical ##
pred_tab2 <- pred_tab[, 1:5]

pred.sr.te <- PredictGLMERRandIter(model = ab1.temp$model,data = pred_tab2)

pred.sr.te <- exp(pred.sr.te)-1

refRow <- which((pred_tab2$Predominant_land_use=="Cropland") & (pred_tab2$percNH_Jung2 == 0))

pred.sr.te <- sweep(x = pred.sr.te,MARGIN = 2,STATS = pred.sr.te[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr.te,MARGIN = 1,
                                FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr.te,MARGIN = 1,
                               FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr.te,MARGIN = 1,
                               FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)


# SR plot = full range
ggplot(data = plot_data) +
  geom_line(aes(x = percNH_Jung2, y = PredMedian), col = c("#006400")) +
  geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper), fill = c("#006400"), alpha = 0.3) +
  facet_wrap(~ realm) +
  #geom_rug(data = sites.sub, aes(x = percNH_Jung4, col = Predominant_land_use), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylim(c(-100, 100)) +
  xlim(c(0, 1)) +
  #scale_colour_manual(values = c("#006400"))+
  #scale_fill_manual(values = c("#006400")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in Total Abundance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_Abundance_percNH_jung2_nopoly.pdf"))


rm(pred_tab, pred_tab2, pred.sr, pred.sr.te, plot_data)



#### plot of interaction between LU and percNH ####

pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub$percNH_Jung2_RS),
                                              to = max(sites.sub$percNH_Jung2_RS),
                                              length.out = 1000),
                        Predominant_land_use = factor(c("Cropland"),
                                                      levels = levels(sites.sub$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"),
                                               levels = levels(sites.sub$Use_intensity)),
                        LogAbun = 0)

pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))
# pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
# pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub$Use_intensity))

pred_tab$Predominant_land_use <- c(rep("Primary vegetation", 1000), rep("Secondary vegetation", 1000), rep("Cropland", 1000))
pred_tab$Predominant_land_use <- factor(pred_tab$Predominant_land_use, levels = levels(sites.sub$Predominant_land_use))

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]


## tropical ##

# predict values
pred.sr <- PredictGLMERRandIter(model = ab1.trop$model,data = pred_tab, nIters = 3000)

pred.sr <- exp(pred.sr)-1

refRow <- which((pred_tab$Predominant_land_use=="Cropland") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


## non-tropical ##

pred_tab2 <- pred_tab[, 1:5]

# predict values
pred.sr2 <- PredictGLMERRandIter(model = ab1.temp$model,data = pred_tab2, nIters = 3000)

pred.sr2 <- exp(pred.sr2)-1

refRow <- which((pred_tab2$Predominant_land_use=="Cropland") & (pred_tab2$percNH_Jung2 == 0))

pred.sr2 <- sweep(x = pred.sr2,MARGIN = 2,STATS = pred.sr2[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr2,MARGIN = 1,
                                FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)


# SR plot = full range
ggplot(data = plot_data) +
  geom_line(aes(x = percNH_Jung2, y = PredMedian, col = Predominant_land_use)) +
  geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper, fill = Predominant_land_use), alpha = 0.3) +
  geom_rug(data = sites.sub, aes(x = percNH_Jung2, col = Predominant_land_use), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~realm) +
  ylim(c(-75, 100)) +
  xlim(c(0, 1)) +
  scale_colour_manual(values = c("#006400", "#8B0000", "#EEAD0E"))+
  scale_fill_manual(values = c("#006400", "#8B0000", "#EEAD0E")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in Total Abundance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_Abundance_percNH_LU_jung2_nopoly.pdf"))

rm(pred_tab, pred_tab2, pred.sr, pred.sr2, plot_data)


#### plot of interaction between UI and percNH ####

pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub$percNH_Jung2_RS),
                                              to = max(sites.sub$percNH_Jung2_RS),
                                              length.out = 1000),
                        Predominant_land_use = factor(c("Cropland"),
                                                      levels = levels(sites.sub$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"),
                                               levels = levels(sites.sub$Use_intensity)),
                        LogAbun = 0)

pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))

pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub$Use_intensity))

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]


## tropical ##

# predict values
pred.sr <- PredictGLMERRandIter(model = ab1.trop$model,data = pred_tab, nIters = 3000)

pred.sr <- exp(pred.sr)-1

refRow <- which((pred_tab$Use_intensity=="Minimal use") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


## non-tropical ##

pred_tab2 <- pred_tab[, 1:5]

# predict values
pred.sr2 <- PredictGLMERRandIter(model = ab1.temp$model,data = pred_tab2, nIters = 3000)

pred.sr2 <- exp(pred.sr2)-1

refRow <- which((pred_tab2$Use_intensity=="Minimal use") & (pred_tab2$percNH_Jung2 == 0))

pred.sr2 <- sweep(x = pred.sr2,MARGIN = 2,STATS = pred.sr2[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr2,MARGIN = 1,
                                FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)


# SR plot = full range
ggplot(data = plot_data) +
  geom_line(aes(x = percNH_Jung2, y = PredMedian, col = Use_intensity)) +
  geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper, fill = Use_intensity), alpha = 0.3) +
  geom_rug(data = sites.sub, aes(x = percNH_Jung2, col = Use_intensity), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~realm) +
  ylim(c(-75, 100)) +
  xlim(c(0, 1)) +
  scale_colour_manual(values = c("#66CD00", "#FFB90F", "#EE0000"))+
  scale_fill_manual(values = c("#66CD00", "#FFB90F", "#EE0000")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in Total Abundance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_Abundance_percNH_UI_jung2_nopoly.pdf"))


rm(pred_tab, pred_tab2, pred.sr, pred.sr2, plot_data)


##%######################################################%##
#                                                          #
####                    POLLINATORS                     ####
#                                                          #
##%######################################################%##

rm(ab1.temp, ab1.trop, sr1.temp, sr1.trop, sites.sub, scalers)

# pollinators
load(paste0(datadir, "/Richness_Jung2_Tropical_POLLINATORS.rdata")) # sr1.trop
load(paste0(datadir, "/Richness_Jung2_Temperate_POLLINATORS.rdata")) # sr1.temp
load(paste0(datadir, "/Abundance_Jung2_Tropical_POLLINATORS.rdata")) # ab1.trop
load(paste0(datadir, "/Abundance_Jung2_Temperate_POLLINATORS.rdata")) # ab1.temp


load(file = paste0(datadir, "/PREDICTS_dataset_incNH_POLLINATORS.rdata"))


scalers <- c(attr(sites.sub.pols$percNH_Jung2_RS, "scaled:scale"), attr(sites.sub.pols$percNH_Jung2_RS, "scaled:center"))

#### LU UI ####

# basic table of median values and reference factors
pred_tab3 <- data.frame(percNH_Jung2_RS = median(sites.sub.pols$percNH_Jung2_RS),
                        Use_intensity = "Minimal use",
                        Predominant_land_use = "Primary vegetation",
                        #Tropical = "Temperate",
                        Species_richness = 0,
                        logAbun = 0)


# organise factor levels
# check levels of factor variables
pred_tab3$Predominant_land_use <- factor(pred_tab3$Predominant_land_use, levels = levels(sites.sub.pols$Predominant_land_use))
pred_tab3$Use_intensity <- factor(pred_tab3$Use_intensity, levels = levels(sites.sub.pols$Use_intensity)) 


# add and change factor levels of land use and intensity
pred_tab3 <- do.call("rbind", replicate(9, pred_tab3, simplify = FALSE))

pred_tab3[4:6, 'Predominant_land_use'] <- "Secondary vegetation"
pred_tab3[7:9, 'Predominant_land_use'] <- "Cropland"

pred_tab3[c(2,5,8), 'Use_intensity'] <- "Light use"
pred_tab3[c(3,6,9), 'Use_intensity'] <- "Intense use"


### Tropical predictions ###

# predict the result
resulta <- PredictGLMERRandIter(model = sr1.trop$model, data = pred_tab3)

# transform the results
resulta <- exp(resulta)

resulta <- sweep(x = resulta, MARGIN = 2, STATS = resulta[1,], FUN = '/')

pred_tab3$median <- ((apply(X = resulta, MARGIN = 1, FUN = median))*100)-100
pred_tab3$upper <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
pred_tab3$lower <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100

pred_tab3$realm <- "Tropical"

# temperate

## Richness, Temperate ##

pred_tab4 <- pred_tab3[, 1:5]

# predict the result
resulta2 <- PredictGLMERRandIter(model = sr1.temp$model, data = pred_tab4)

# transform the results
resulta2 <- exp(resulta2)

resulta2 <- sweep(x = resulta2, MARGIN = 2, STATS = resulta2[1,], FUN = '/')

pred_tab4$median <- ((apply(X = resulta2, MARGIN = 1, FUN = median))*100)-100
pred_tab4$upper <- ((apply(X = resulta2, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
pred_tab4$lower <- ((apply(X = resulta2, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100

pred_tab4$realm <- "Non-tropical"

plot_data2 <- rbind(pred_tab3, pred_tab4)

plot_data2[plot_data2$upper == 0, c("upper", "lower")] <- NA

plot_data2$realm <- factor(plot_data2$realm, levels = c("Tropical", "Non-tropical"))

plot_data2$Predominant_land_use <- sub("Primary vegetation", "Primary\nvegetation", plot_data2$Predominant_land_use)
plot_data2$Predominant_land_use <- sub("Secondary vegetation", "Secondary\nvegetation", plot_data2$Predominant_land_use)

plot_data2$Predominant_land_use <- factor(plot_data2$Predominant_land_use, levels = c("Primary\nvegetation", "Secondary\nvegetation", "Cropland"))

ggplot(data = plot_data2)+
  geom_point(aes(x = Predominant_land_use, y = median, col = Predominant_land_use, shape = Use_intensity),
             position = position_dodge(width = 0.9), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper, y = median, x = Predominant_land_use, col = Predominant_land_use),
                position = position_dodge2(padding = 0.5)) +
  facet_grid(~ realm) +
  scale_colour_manual(values = c("#006400", "#8B0000", "#EEAD0E"), guide = F)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2)+
  xlab("") +
  ylab("Species Richness (%)") +
  theme_bw() +
  theme_custom +
  theme(legend.position = c(0.1,0.9), strip.background = element_rect(fill = NA, size = 0.2))

ggsave(filename = paste0(outdir, "/Supp_Rich_LUUI_POLLINATORS_Jung2.pdf"), width = 6, height = 3, uni = "in")


rm(pred_tab3, pred_tab4, resulta, resulta2, plot_data2)


#### PERCNH ####

## tropical ##
pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub.pols$percNH_Jung2_RS),
                                              to = max(sites.sub.pols$percNH_Jung2_RS),
                                              length.out = 500),
                        Predominant_land_use = factor(c("Cropland"),
                                                      levels = levels(sites.sub.pols$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"), 
                                               levels = levels(sites.sub.pols$Use_intensity)),
                        Species_richness = 0)

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]

pred.sr <- PredictGLMERRandIter(model = sr1.trop$model,data = pred_tab)

pred.sr <- exp(pred.sr)

refRow <- which((pred_tab$Predominant_land_use=="Cropland") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100

## non-tropical ##
pred_tab2 <- pred_tab[, 1:5]

pred.sr.te <- PredictGLMERRandIter(model = sr1.temp$model,data = pred_tab2)

pred.sr.te <- exp(pred.sr.te)

refRow <- which((pred_tab2$Predominant_land_use=="Cropland") & (pred_tab2$percNH_Jung2 == 0))

pred.sr.te <- sweep(x = pred.sr.te,MARGIN = 2,STATS = pred.sr.te[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr.te,MARGIN = 1,
                                FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr.te,MARGIN = 1,
                               FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr.te,MARGIN = 1,
                               FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)


# SR plot = full range
ggplot(data = plot_data) +
  geom_line(aes(x = percNH_Jung2, y = PredMedian), col = c("#006400")) +
  geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper), fill = c("#006400"), alpha = 0.3) +
  facet_wrap(~ realm) +
  #geom_rug(data = sites.sub.pols, aes(x = percNH_Jung4, col = Predominant_land_use), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylim(c(-50, 100)) +
  xlim(c(0, 1)) +
  #scale_colour_manual(values = c("#006400"))+
  #scale_fill_manual(values = c("#006400")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in species richness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_richness_percNH_jung2_POLLINATORS.pdf"))

rm(pred_tab, pred_tab2, pred.sr, pred.sr.te, plot_data)


#### plot of interaction between LU and percNH ####

pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub.pols$percNH_Jung2_RS),
                                              to = max(sites.sub.pols$percNH_Jung2_RS),
                                              length.out = 1000),
                        Predominant_land_use = factor(c("Cropland"),
                                                      levels = levels(sites.sub.pols$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"),
                                               levels = levels(sites.sub.pols$Use_intensity)),
                        Species_richness = 0)

pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))
# pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
# pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub.pols$Use_intensity))

pred_tab$Predominant_land_use <- c(rep("Primary vegetation", 1000), rep("Secondary vegetation", 1000), rep("Cropland", 1000))
pred_tab$Predominant_land_use <- factor(pred_tab$Predominant_land_use, levels = levels(sites.sub.pols$Predominant_land_use))

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]


## tropical ##

# predict values
pred.sr <- PredictGLMERRandIter(model = sr1.trop$model,data = pred_tab, nIters = 3000)

pred.sr <- exp(pred.sr)

refRow <- which((pred_tab$Predominant_land_use=="Cropland") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


## non-tropical ##

pred_tab2 <- pred_tab[, 1:5]

# predict values
pred.sr2 <- PredictGLMERRandIter(model = sr1.temp$model,data = pred_tab2, nIters = 3000)

pred.sr2 <- exp(pred.sr2)

refRow <- which((pred_tab2$Predominant_land_use=="Cropland") & (pred_tab2$percNH_Jung2 == 0))

pred.sr2 <- sweep(x = pred.sr2,MARGIN = 2,STATS = pred.sr2[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr2,MARGIN = 1,
                                FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)


# SR plot = full range
ggplot(data = plot_data) +
  geom_line(aes(x = percNH_Jung2, y = PredMedian, col = Predominant_land_use)) +
  geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper, fill = Predominant_land_use), alpha = 0.3) +
  geom_rug(data = sites.sub.pols, aes(x = percNH_Jung2, col = Predominant_land_use), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~realm) +
  ylim(c(-50, 100)) +
  xlim(c(0, 1)) +
  scale_colour_manual(values = c("#006400", "#8B0000", "#EEAD0E"))+
  scale_fill_manual(values = c("#006400", "#8B0000", "#EEAD0E")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in species richness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_richness_percNH_LU_jung2_POLLINATORS.pdf"))

rm(pred_tab, pred_tab2, pred.sr, pred.sr2, plot_data)


#### plot of interaction between UI and percNH ####

pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub.pols$percNH_Jung2_RS),
                                              to = max(sites.sub.pols$percNH_Jung2_RS),
                                              length.out = 1000),
                        Predominant_land_use = factor(c("Cropland"),
                                                      levels = levels(sites.sub.pols$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"),
                                               levels = levels(sites.sub.pols$Use_intensity)),
                        Species_richness = 0)

pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))

pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub.pols$Use_intensity))

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]


## tropical ##

# predict values
pred.sr <- PredictGLMERRandIter(model = sr1.trop$model,data = pred_tab, nIters = 3000)

pred.sr <- exp(pred.sr)

refRow <- which((pred_tab$Use_intensity=="Minimal use") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


## non-tropical ##

pred_tab2 <- pred_tab[, 1:5]

# predict values
pred.sr2 <- PredictGLMERRandIter(model = sr1.temp$model,data = pred_tab2, nIters = 3000)

pred.sr2 <- exp(pred.sr2)

refRow <- which((pred_tab2$Use_intensity=="Minimal use") & (pred_tab2$percNH_Jung2 == 0))

pred.sr2 <- sweep(x = pred.sr2,MARGIN = 2,STATS = pred.sr2[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr2,MARGIN = 1,
                                FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)


# SR plot = full range
ggplot(data = plot_data) +
  geom_line(aes(x = percNH_Jung2, y = PredMedian, col = Use_intensity)) +
  geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper, fill = Use_intensity), alpha = 0.3) +
  geom_rug(data = sites.sub.pols, aes(x = percNH_Jung2, col = Use_intensity), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~realm) +
  ylim(c(-50, 100)) +
  xlim(c(0, 1)) +
  scale_colour_manual(values = c("#66CD00", "#FFB90F", "#EE0000"))+
  scale_fill_manual(values = c("#66CD00", "#FFB90F", "#EE0000")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in species richness") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_richness_percNH_UI_jung2_POLLINATORS.pdf"))


rm(pred_tab, pred_tab2, pred.sr, pred.sr2, plot_data)





#### Abundance ####


#### LU UI ####

# basic table of median values and reference factors
pred_tab3 <- data.frame(percNH_Jung2_RS = median(sites.sub.pols$percNH_Jung2_RS),
                        Use_intensity = "Minimal use",
                        Predominant_land_use = "Primary vegetation",
                        #Tropical = "Temperate",
                        Species_richness = 0,
                        LogAbun = 0)


# organise factor levels
# check levels of factor variables
pred_tab3$Predominant_land_use <- factor(pred_tab3$Predominant_land_use, levels = levels(sites.sub.pols$Predominant_land_use))
pred_tab3$Use_intensity <- factor(pred_tab3$Use_intensity, levels = levels(sites.sub.pols$Use_intensity)) 


# add and change factor levels of land use and intensity
pred_tab3 <- do.call("rbind", replicate(9, pred_tab3, simplify = FALSE))

pred_tab3[4:6, 'Predominant_land_use'] <- "Secondary vegetation"
pred_tab3[7:9, 'Predominant_land_use'] <- "Cropland"

pred_tab3[c(2,5,8), 'Use_intensity'] <- "Light use"
pred_tab3[c(3,6,9), 'Use_intensity'] <- "Intense use"


### Tropical predictions ###

# predict the result
resulta <- PredictGLMERRandIter(model = ab1.trop$model, data = pred_tab3)

# transform the results
resulta <- exp(resulta)-1

resulta <- sweep(x = resulta, MARGIN = 2, STATS = resulta[1,], FUN = '/')

pred_tab3$median <- ((apply(X = resulta, MARGIN = 1, FUN = median))*100)-100
pred_tab3$upper <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
pred_tab3$lower <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100

pred_tab3$realm <- "Tropical"

# temperate

## Richness, Temperate ##

pred_tab4 <- pred_tab3[, 1:5]

# predict the result
resulta2 <- PredictGLMERRandIter(model = ab1.temp$model, data = pred_tab4)

# transform the results
resulta2 <- exp(resulta2)

resulta2 <- sweep(x = resulta2, MARGIN = 2, STATS = resulta2[1,], FUN = '/')

pred_tab4$median <- ((apply(X = resulta2, MARGIN = 1, FUN = median))*100)-100
pred_tab4$upper <- ((apply(X = resulta2, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
pred_tab4$lower <- ((apply(X = resulta2, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100

pred_tab4$realm <- "Non-tropical"

plot_data2 <- rbind(pred_tab3, pred_tab4)

plot_data2[plot_data2$upper == 0, c("upper", "lower")] <- NA

plot_data2$realm <- factor(plot_data2$realm, levels = c("Tropical", "Non-tropical"))

plot_data2$Predominant_land_use <- sub("Primary vegetation", "Primary\nvegetation", plot_data2$Predominant_land_use)
plot_data2$Predominant_land_use <- sub("Secondary vegetation", "Secondary\nvegetation", plot_data2$Predominant_land_use)

plot_data2$Predominant_land_use <- factor(plot_data2$Predominant_land_use, levels = c("Primary\nvegetation", "Secondary\nvegetation", "Cropland"))

ggplot(data = plot_data2)+
  geom_point(aes(x = Predominant_land_use, y = median, col = Predominant_land_use, shape = Use_intensity),
             position = position_dodge(width = 0.9), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper, y = median, x = Predominant_land_use, col = Predominant_land_use),
                position = position_dodge2(padding = 0.5)) +
  facet_grid(~ realm) +
  scale_colour_manual(values = c("#006400", "#8B0000", "#EEAD0E"), guide = F)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2)+
  xlab("") +
  ylab("Abundance (%)") +
  theme_bw() +
  theme_custom +
  theme(legend.position = "bottom", strip.background = element_rect(fill = NA, size = 0.2))


ggsave(filename = paste0(outdir, "/Abundance_LUUI_Jung2_POLLINATORS.pdf"), width = 6, height = 3, uni = "in")

rm(pred_tab3, pred_tab4, resulta, resulta2, plot_data2)



#### PERCNH ####

## tropical ##
pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub.pols$percNH_Jung2_RS),
                                              to = max(sites.sub.pols$percNH_Jung2_RS),
                                              length.out = 500),
                        Predominant_land_use = factor(c("Cropland"),
                                                      levels = levels(sites.sub.pols$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"), 
                                               levels = levels(sites.sub.pols$Use_intensity)),
                        LogAbun = 0)

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]

pred.sr <- PredictGLMERRandIter(model = ab1.trop$model,data = pred_tab)

pred.sr <- exp(pred.sr)-1

refRow <- which((pred_tab$Predominant_land_use=="Cropland") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100

## non-tropical ##
pred_tab2 <- pred_tab[, 1:5]

pred.sr.te <- PredictGLMERRandIter(model = ab1.temp$model,data = pred_tab2)

pred.sr.te <- exp(pred.sr.te)-1

refRow <- which((pred_tab2$Predominant_land_use=="Cropland") & (pred_tab2$percNH_Jung2 == 0))

pred.sr.te <- sweep(x = pred.sr.te,MARGIN = 2,STATS = pred.sr.te[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr.te,MARGIN = 1,
                                FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr.te,MARGIN = 1,
                               FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr.te,MARGIN = 1,
                               FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)


# SR plot = full range
ggplot(data = plot_data) +
  geom_line(aes(x = percNH_Jung2, y = PredMedian), col = c("#006400")) +
  geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper), fill = c("#006400"), alpha = 0.3) +
  facet_wrap(~ realm) +
  #geom_rug(data = sites.sub.pols, aes(x = percNH_Jung4, col = Predominant_land_use), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylim(c(-100, 100)) +
  xlim(c(0, 1)) +
  #scale_colour_manual(values = c("#006400"))+
  #scale_fill_manual(values = c("#006400")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in Total Abundance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_Abundance_percNH_jung2_POLLINATORS.pdf"))

rm(pred_tab, pred_tab2, pred.sr, pred.sr.te, plot_data)




#### plot of interaction between LU and percNH ####

pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub.pols$percNH_Jung2_RS),
                                              to = max(sites.sub.pols$percNH_Jung2_RS),
                                              length.out = 1000),
                        Predominant_land_use = factor(c("Cropland"),
                                                      levels = levels(sites.sub.pols$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"),
                                               levels = levels(sites.sub.pols$Use_intensity)),
                        LogAbun = 0)

pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))
# pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
# pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub.pols$Use_intensity))

pred_tab$Predominant_land_use <- c(rep("Primary vegetation", 1000), rep("Secondary vegetation", 1000), rep("Cropland", 1000))
pred_tab$Predominant_land_use <- factor(pred_tab$Predominant_land_use, levels = levels(sites.sub.pols$Predominant_land_use))

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]


## tropical ##

# predict values
pred.sr <- PredictGLMERRandIter(model = ab1.trop$model,data = pred_tab, nIters = 3000)

pred.sr <- exp(pred.sr)-1

refRow <- which((pred_tab$Predominant_land_use=="Cropland") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


## non-tropical ##

pred_tab2 <- pred_tab[, 1:5]

# predict values
pred.sr2 <- PredictGLMERRandIter(model = ab1.temp$model,data = pred_tab2, nIters = 3000)

pred.sr2 <- exp(pred.sr2)-1

refRow <- which((pred_tab2$Predominant_land_use=="Cropland") & (pred_tab2$percNH_Jung2 == 0))

pred.sr2 <- sweep(x = pred.sr2,MARGIN = 2,STATS = pred.sr2[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr2,MARGIN = 1,
                                FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)


# SR plot = full range
ggplot(data = plot_data) +
  geom_line(aes(x = percNH_Jung2, y = PredMedian, col = Predominant_land_use)) +
  geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper, fill = Predominant_land_use), alpha = 0.3) +
  geom_rug(data = sites.sub.pols, aes(x = percNH_Jung2, col = Predominant_land_use), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~realm) +
  ylim(c(-75, 120)) +
  xlim(c(0, 1)) +
  scale_colour_manual(values = c("#006400", "#8B0000", "#EEAD0E"))+
  scale_fill_manual(values = c("#006400", "#8B0000", "#EEAD0E")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in Total Abundance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_Abundance_percNH_LU_jung2_POLLINATORS.pdf"))

rm(pred_tab, pred_tab2, pred.sr, pred.sr2, plot_data)




#### plot of interaction between UI and percNH ####

pred_tab <- expand.grid(percNH_Jung2_RS = seq(from = min(sites.sub.pols$percNH_Jung2_RS),
                                              to = max(sites.sub.pols$percNH_Jung2_RS),
                                              length.out = 1000),
                        Predominant_land_use = factor(c("Cropland"),
                                                      levels = levels(sites.sub.pols$Predominant_land_use)),
                        Use_intensity = factor(c("Minimal use"),
                                               levels = levels(sites.sub.pols$Use_intensity)),
                        LogAbun = 0)

pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))

pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub.pols$Use_intensity))

# add unscaled version
pred_tab$percNH_Jung2 <- (pred_tab$percNH_Jung2_RS *scalers[1]) + scalers[2]


## tropical ##

# predict values
pred.sr <- PredictGLMERRandIter(model = ab1.trop$model,data = pred_tab, nIters = 3000)

pred.sr <- exp(pred.sr)-1

refRow <- which((pred_tab$Use_intensity=="Minimal use") & (pred_tab$percNH_Jung2 == 0))

pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
                               FUN = median,na.rm=TRUE))*100)-100
pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


## non-tropical ##

pred_tab2 <- pred_tab[, 1:5]

# predict values
pred.sr2 <- PredictGLMERRandIter(model = ab1.temp$model,data = pred_tab2, nIters = 3000)

pred.sr2 <- exp(pred.sr2)-1

refRow <- which((pred_tab2$Use_intensity=="Minimal use") & (pred_tab2$percNH_Jung2 == 0))

pred.sr2 <- sweep(x = pred.sr2,MARGIN = 2,STATS = pred.sr2[refRow,],FUN = '/')


#Get the median, upper and lower quants for the plot
pred_tab2$PredMedian <- ((apply(X = pred.sr2,MARGIN = 1,
                                FUN = median,na.rm=TRUE))*100)-100
pred_tab2$PredUpper <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
pred_tab2$PredLower <- ((apply(X = pred.sr2,MARGIN = 1,
                               FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100


pred_tab$realm <- "tropical"
pred_tab2$realm <- "non-tropical"

plot_data <- rbind(pred_tab, pred_tab2)


# SR plot = full range
ggplot(data = plot_data) +
  geom_line(aes(x = percNH_Jung2, y = PredMedian, col = Use_intensity)) +
  geom_ribbon(aes(x = percNH_Jung2, ymin= PredLower, ymax = PredUpper, fill = Use_intensity), alpha = 0.3) +
  geom_rug(data = sites.sub.pols, aes(x = percNH_Jung2, col = Use_intensity), size = 0.1) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~realm) +
  ylim(c(-75, 155)) +
  xlim(c(0, 1)) +
  scale_colour_manual(values = c("#66CD00", "#FFB90F", "#EE0000"))+
  scale_fill_manual(values = c("#66CD00", "#FFB90F", "#EE0000")) +
  xlab("Proportion Natural Habitat") +
  ylab("% Change in Total Abundance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 1) 


ggsave(filename = paste0(outdir, "/Plot_Abundance_percNH_UI_jung2_POLLINATORS.pdf"))

rm(pred_tab, pred_tab2, pred.sr, pred.sr2, plot_data)

