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

#pred_tab <- expand.grid(percNH_Jung4 = seq(from = min(sites.sub$percNH_Jung4),
#                                           to = max(sites.sub$percNH_Jung4),
#                                           length.out = 1000),
#                        Predominant_land_use = factor(c("Primary vegetation"),
#                                                      levels = levels(sites.sub$Predominant_land_use)),
#                        Use_intensity = factor(c("Minimal use"), 
#                                               levels = levels(sites.sub$Use_intensity)),
#                        Species_richness = 0)
#
#pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))
#pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
#pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub$Use_intensity))
#
#pred.sr <- PredictGLMERRandIter(model = srmod$model,data = pred_tab, nIters = 3000)
#
#pred.sr <- exp(pred.sr)
#
#refRow <- which((pred_tab$Predominant_land_use=="Primary vegetation") & (pred_tab$percNH_Jung4==min(abs(pred_tab$percNH_Jung4))) &(pred_tab$Use_intensity=="Minimal use"))
#
#pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')


# Get the median, upper and lower quants for the plot
#pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
#                               FUN = median,na.rm=TRUE))*100)-100
#pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
#                              FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
#pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
#                              FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100

# SR plot = full range
#ggplot(data = pred_tab) +
#  geom_line(aes(x = percNH_Jung4, y = PredMedian, col = Use_intensity)) +
#  geom_ribbon(aes(x = percNH_Jung4, ymin= PredLower, ymax = PredUpper, fill = Use_intensity), alpha = 0.3) +
#  geom_rug(data = sites.sub, aes(x = percNH_Jung4, col = Use_intensity), size = 0.1) +
#  geom_hline(yintercept = 0, linetype = 'dashed') +
#  ylim(c(-10, 50)) +
#  xlim(c(0, 1)) +
#  scale_colour_manual(values = c("#66CD00", "#FFB90F", "#EE0000"))+
#  scale_fill_manual(values = c("#66CD00", "#FFB90F", "#EE0000")) +
#  xlab("Proportion Natural Habitat") +
#  ylab("% Change in species richness") +
#  theme_bw() +
#  theme(panel.grid = element_blank(),
#        aspect.ratio = 1) +
#  ggtitle("Jung4 dataset - forest, grassland, shrubland, savanna")


#ggsave(filename = paste0(outdir, "/Plot_richness_percNHUI_jung4_incUI.pdf"))

## LU:UI

#pred_tab <- expand.grid(percNH_Jung4 = median(sites.sub$percNH_Jung4),
#                        Predominant_land_use = factor(c("Primary vegetation"),
#                                                      levels = levels(sites.sub$Predominant_land_use)),
#                        Use_intensity = factor(c("Minimal use"), 
#                                               levels = levels(sites.sub$Use_intensity)),
#                        Species_richness = 0)
#
#
#pred_tab <- do.call("rbind", replicate(9, pred_tab, simplify = FALSE))
#
#pred_tab[4:6, 'Predominant_land_use'] <- "Secondary vegetation"
#pred_tab[7:9, 'Predominant_land_use'] <- "Cropland"
#
#pred_tab[c(2,5,8), 'Use_intensity'] <- "Light use"
#pred_tab[c(3,6,9), 'Use_intensity'] <- "Intense use"
#
#
# predict the result
#resulta <- PredictGLMERRandIter(model = srmod$model, data = pred_tab)

# transform the results
#resulta <- exp(resulta)
#
#resulta <- sweep(x = resulta, MARGIN = 2, STATS = resulta[1,], FUN = '/')
#
#resulta.median <- ((apply(X = resulta, MARGIN = 1, FUN = median))*100)-100
#resulta.upper <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
#resulta.lower <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100
#

#errbar.cols <- c(rep("#006400",3),rep("#8B0000", 3), rep("#EEAD0E", 3))
#
#
#pdf(file = paste0(outdir, "/Plot_Richness_LUUI.pdf"))
#par(mar=c(5,5,1,1))
#
#errbar(x = 1:9,y = resulta.median,yplus = resulta.upper,yminus = resulta.lower,
#       col=errbar.cols,errbar.col = errbar.cols,
#       ylim=c(min(resulta.lower),max(resulta.upper)),xaxt="n",
#       pch =rep(c(16,17,18), 3), 
#       ylab="Species Richness (%)",xlab="",bty="l", cex.lab =1.6, cex.axis = 1.6, cex = 1.5)
#
#axis(side = 1,at = c(2,5,8),
#     labels = c("Primary \nvegetation","Secondary\nvegetation", "Cropland"),
#     padj = 0.5, cex.axis =1.6)
#
#abline(h=0,col="#00000077",lty=2)

#legend("topright", 
#       legend = c("Minimal Use", "Light Use", "Intense Use"),
#       pch = c(16,17,18), bty = "n", inset=c(0,0), cex =1.8)
#
#dev.off()



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


ggsave(filename = paste0(outdir, "/Plot_abundance_percNHLU_jung4.pdf"))


# #### plot of interaction between UI and percNH ####
# 
# pred_tab <- expand.grid(percNH_Jung4 = seq(from = min(sites.sub$percNH_Jung4),
#                                            to = max(sites.sub$percNH_Jung4),
#                                            length.out = 1000),
#                         Predominant_land_use = factor(c("Primary vegetation"),
#                                                       levels = levels(sites.sub$Predominant_land_use)),
#                         Use_intensity = factor(c("Minimal use"), 
#                                                levels = levels(sites.sub$Use_intensity)),
#                         LogAbun = 0)
# 
# pred_tab <- do.call("rbind", replicate(3, pred_tab, simplify = FALSE))
# pred_tab$Use_intensity <- c(rep("Minimal use", 1000), rep("Light use", 1000), rep("Intense use", 1000))
# pred_tab$Use_intensity <- factor(pred_tab$Use_intensity, levels = levels(sites.sub$Use_intensity))
# 
# pred.sr <- PredictGLMERRandIter(model = abmod$model,data = pred_tab)
# 
# pred.sr <- exp(pred.sr)-1
# 
# refRow <- which((pred_tab$Predominant_land_use=="Primary vegetation") & (pred_tab$percNH_Jung4==min(abs(pred_tab$percNH_Jung4))) &(pred_tab$Use_intensity=="Minimal use"))
# 
# pred.sr <- sweep(x = pred.sr,MARGIN = 2,STATS = pred.sr[refRow,],FUN = '/')
# 
# 
# # Get the median, upper and lower quants for the plot
# pred_tab$PredMedian <- ((apply(X = pred.sr,MARGIN = 1,
#                                FUN = median,na.rm=TRUE))*100)-100
# pred_tab$PredUpper <- ((apply(X = pred.sr,MARGIN = 1,
#                               FUN = quantile,probs = 0.975,na.rm=TRUE))*100)-100
# pred_tab$PredLower <- ((apply(X = pred.sr,MARGIN = 1,
#                               FUN = quantile,probs = 0.025,na.rm=TRUE))*100)-100
# 
# # SR plot = full range
# ggplot(data = pred_tab) +
#   geom_line(aes(x = percNH_Jung4, y = PredMedian, col = Use_intensity)) +
#   geom_ribbon(aes(x = percNH_Jung4, ymin= PredLower, ymax = PredUpper, fill = Use_intensity), alpha = 0.3) +
#   geom_rug(data = sites.sub, aes(x = percNH_Jung4, col = Use_intensity), size = 0.1) +
#   geom_hline(yintercept = 0, linetype = 'dashed') +
#   ylim(c(-30, 50)) +
#   xlim(c(0, 1)) +
#   scale_colour_manual(values = c("#66CD00", "#FFB90F", "#EE0000"))+
#   scale_fill_manual(values = c("#66CD00", "#FFB90F", "#EE0000")) +
#   xlab("Proportion Natural Habitat") +
#   ylab("% Change in total abundance") +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         aspect.ratio = 1) +
#   ggtitle("Jung4 dataset - forest, grassland, shrubland, savanna")
# 
# 
# ggsave(filename = paste0(outdir, "/Plot_abundance_percNHUI_jung4_incUI.pdf"))
# 
# ## LU:UI
# 
# pred_tab <- expand.grid(percNH_Jung4 = median(sites.sub$percNH_Jung4),
#                         Predominant_land_use = factor(c("Primary vegetation"),
#                                                       levels = levels(sites.sub$Predominant_land_use)),
#                         Use_intensity = factor(c("Minimal use"), 
#                                                levels = levels(sites.sub$Use_intensity)),
#                         LogAbun = 0)
# 
# 
# pred_tab <- do.call("rbind", replicate(9, pred_tab, simplify = FALSE))
# 
# pred_tab[4:6, 'Predominant_land_use'] <- "Secondary vegetation"
# pred_tab[7:9, 'Predominant_land_use'] <- "Cropland"
# 
# pred_tab[c(2,5,8), 'Use_intensity'] <- "Light use"
# pred_tab[c(3,6,9), 'Use_intensity'] <- "Intense use"
# 
# 
# # predict the result
# resulta <- PredictGLMERRandIter(model = abmod$model, data = pred_tab)
# 
# # transform the results
# resulta <- exp(resulta)-1
# 
# resulta <- sweep(x = resulta, MARGIN = 2, STATS = resulta[1,], FUN = '/')
# 
# resulta.median <- ((apply(X = resulta, MARGIN = 1, FUN = median))*100)-100
# resulta.upper <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.975))*100)-100
# resulta.lower <- ((apply(X = resulta, MARGIN = 1, FUN = quantile,probs = 0.025))*100)-100
# 
# 
# errbar.cols <- c(rep("#006400",3),rep("#8B0000", 3), rep("#EEAD0E", 3))
# 
# 
# pdf(file = paste0(outdir, "/Plot_Abundance_LUUI.pdf"))
# par(mar=c(5,5,1,1))
# 
# errbar(x = 1:9,y = resulta.median,yplus = resulta.upper,yminus = resulta.lower,
#        col=errbar.cols,errbar.col = errbar.cols,
#        ylim=c(min(resulta.lower),max(resulta.upper)),xaxt="n",
#        pch =rep(c(16,17,18), 3), 
#        ylab="Total Abundance (%)",xlab="",bty="l", cex.lab =1.6, cex.axis = 1.6, cex = 1.5)
# 
# axis(side = 1,at = c(2,5,8),
#      labels = c("Primary \nvegetation","Secondary\nvegetation", "Cropland"),
#      padj = 0.5, cex.axis =1.6)
# 
# abline(h=0,col="#00000077",lty=2)
# 
# legend("topright", 
#        legend = c("Minimal Use", "Light Use", "Intense Use"),
#        pch = c(16,17,18), bty = "n", inset=c(0,0), cex =1.8)
# 
# dev.off()
