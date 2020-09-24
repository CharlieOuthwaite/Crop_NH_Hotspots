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

# site level data cropland only
sites.sub <- pred.sites.metrics[!pred.sites.metrics$Predominant_land_use %in% c("Urban", "Pasture", "Cannot decide", "Plantation forest"), ]

# remove sites with NA in lat/long columns
sites.sub <- sites.sub[!is.na(sites.sub$Longitude),  ] # 15612 rows





##%######################################################%##
#                                                          #
####             Percentage natural habitat             ####
#                                                          #
##%######################################################%##

# using reprojected percentage natural habitat raster based on Hoskins et al 2016:

# Hoskins, A.J., Bush, A., Gilmore, J., Harwood, T., Hudson, L.N., Ware, C., Williams, 
# K.J., and Ferrier, S. (2016). Downscaling land-use data to provide global estimates 
# of five land-use classes. Ecol. Evol.

# read in the raster
percNH <- raster(paste0(datadir,"/PercentNatural.tif"))

# 
percNH <- percNH/10

# extract the dataset info for the PREDICTS sites
sites.sub$percNH <- extract(percNH, sites.sub_xy, na.rm = FALSE)

# how many NAs
nrow(sites.sub[is.na(sites.sub$percNH),]) #29





##%######################################################%##
#                                                          #
####                 Assess the dataset                 ####
#                                                          #
##%######################################################%##



# remove any rows that have NA in the variable columns
summary(is.na(sites.sub))

# remove rows with NAs for any variable of interest
sites.sub <- sites.sub[!is.na(sites.sub$homogen), ] # 11934
sites.sub <- sites.sub[!is.na(sites.sub$Hansen_mindist), ] # 11796
sites.sub <- sites.sub[!is.na(sites.sub$percNH), ] # 11782

# nrows of dataset
nrow(sites.sub) # 11782

# remove those sites that have "Cannot decide" as a use intensity
nrow(sites.sub[sites.sub$Use_intensity == "Cannot decide", ]) # 1389
sites.sub <- sites.sub[!sites.sub$Use_intensity == "Cannot decide", ] 

nrow(sites.sub) # 10393



##%######################################################%##
#                                                          #
####                Data transformations                ####
#                                                          #
##%######################################################%##

final.data.trans <- final.data

# standardise all continuous variables
final.data.trans$percNH <-scale(final.data.trans$percNH)

# get data sections for the scaling info for plotting later
percNH <- final.data.trans$percNH

p <- c("percNH", attr(percNH, "scaled:scale"), attr(percNH, "scaled:center"))

values <- rbind(d, f, l, h, p)
colnames(values) <- c("variable", "scale", "centre")

#save
write.csv(values, paste0(outdir, "/Scaling_values.csv"), row.names = FALSE)


### organise factor levels ###

# drop unused levels of factors
final.data.trans <- droplevels(final.data.trans)

# set factor levels in the best order
final.data.trans$Use_intensity <- relevel(final.data.trans$Use_intensity, ref = "Minimal use")

# nsites per use intensity
table(final.data.trans$Use_intensity)


# set land use as character variable
final.data.trans$Predominant_land_use <- as.character(final.data.trans$Predominant_land_use)


# combine secondary land uses
final.data.trans$Predominant_land_use <- sub("Mature secondary vegetation", "Secondary vegetation", final.data.trans$Predominant_land_use)
final.data.trans$Predominant_land_use <- sub("Intermediate secondary vegetation", "Secondary vegetation", final.data.trans$Predominant_land_use)
final.data.trans$Predominant_land_use <- sub("Young secondary vegetation", "Secondary vegetation", final.data.trans$Predominant_land_use)
final.data.trans[final.data.trans$Predominant_land_use == "Secondary vegetation (indeterminate age)", 'Predominant_land_use'] <- "Secondary vegetation"

table(final.data.trans$Predominant_land_use)


# set factor levels of predominant land use
final.data.trans$Predominant_land_use <- factor(final.data.trans$Predominant_land_use,
                                                levels=c("Primary vegetation","Secondary vegetation", "Cropland"))


# nsites per land use
table(final.data.trans$Predominant_land_use)




# save transformed dataset
save(final.data.trans, file = paste0(outdir, "/PREDICTS_dataset_inc_variables_TRANS.rdata"))






