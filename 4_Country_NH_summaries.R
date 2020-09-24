##%######################################################%##
#                                                          #
####    Generate country level summaries of NH data     ####
#                                                          #
##%######################################################%##

# This script takes the global map of area of NH where crops are grown and
# summaries, for each country, how much NH is available and other stats. 

rm(list = ls())

# load libraries
library(raster)
library(maptools)
library(dplyr)


# directories
datadir <- "2_PrepareNaturalHabitatLayer/"
outdir <- "4_Country_NH_summaries/"
dir.create(outdir)

# read in teh fractional natural habitat data
NatHabCrop <- raster(paste0(datadir, "NH_Cropland_Area.tif"))

# convert to actual % values
NatHabCrop <- NatHabCrop/10

# load country polygons
data(wrld_simpl)

# ensure both in same crs
crs(NatHabCrop)
crs(wrld_simpl)

# reproject
wgs84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
NatHabCrop_proj <- NatHabCrop

# edit the extent or get an error when projecting
extent(NatHabCrop_proj) 

# define new extent, slightly smaller than original extent
new_extent <- extent(-17372530, 17372470, 0.999*(-6357770), 0.999*(7347230))

# crop the raster to the smaller extent
NatHabCrop_proj  <- crop(x = NatHabCrop_proj, y = new_extent)
extent(NatHabCrop_proj) 

# reproject 
NatHabCrop_proj2 <- projectRaster(from = NatHabCrop_proj, crs = wgs84, res = 0.04166667) # resolution taken from another 5km grid map

# save the reprojected version
writeRaster(x = NatHabCrop_proj2,filename = paste(outdir, "NatHabCrop_wgs84.tif",sep=""), format="GTiff", overwrite = T)


crs(NatHabCrop_proj2) 
crs(wrld_simpl)

plot(NatHabCrop_proj2)
plot(wrld_simpl, add = TRUE)



# extracting country level summaries

# 1. mean proportion of natural habitat in an area with cropland
# 2. maximum proportion of NH 
# 3. minimum proportion of NH
# 4. median proportion of NH
# 5. standard deviation of proportion of NH
# 6. proportion of cells with cropland with 40% or less NH
# 7. proportion of cells with cropland with 60% or more NH
# 8. area of cropland 
# 9. total crop production 



# read in the fractional natural habitat data if not already in environment
NatHabCrop_proj <- raster(paste0(outdir, "NatHabCrop_wgs84.tif"))

plot(NatHabCrop_proj2)
plot(wrld_simpl, add = TRUE)


# use the country polygons in wrld_simpl to get data summaries

# create dataframe to save results in
results <- data.frame(country = wrld_simpl@data$NAME)


### 1. mean proportion NH  ###
NH_mean <- extract(x = NatHabCrop_proj, y = wrld_simpl, fun = mean, na.rm = TRUE, df = TRUE)

results$mean_prop_NH <- NH_mean$NatHabCrop_wgs84


### 2. Max proportion of NH ###
NH_min <- extract(x = NatHabCrop_proj, y = wrld_simpl, fun = min, na.rm = TRUE, df = TRUE)

results$min_prop_NH <- NH_min$NatHabCrop_wgs84


### 3. Min proportion of NH ###
NH_max <- extract(x = NatHabCrop_proj, y = wrld_simpl, fun = max, na.rm = TRUE, df = TRUE)

results$max_prop_NH <- NH_max$NatHabCrop_wgs84


### 4. median proportion of NH ###
NH_median <- extract(x = NatHabCrop_proj, y = wrld_simpl, fun = median, na.rm = TRUE, df = TRUE)

results$median_prop_NH <- NH_median$NatHabCrop_wgs84


### 5. sd of prop of NH ###
NH_sd <- extract(x = NatHabCrop_proj, y = wrld_simpl, fun = sd, na.rm = TRUE, df = TRUE)

results$sd_prop_NH <- NH_sd$NatHabCrop_wgs84


### 6. proportion of cropland with 40% or less NH ###

prop_less_40 <- NULL

for(i in 1:nrow(wrld_simpl)){
  
  x <- wrld_simpl[i,]
  
  all <- extract(x = NatHabCrop_proj, y = x, df = TRUE)
  
  thres <- 40
  
  prop <- length(all$NatHabCrop_wgs84[all$NatHabCrop_wgs84 <= thres])/ length(all$NatHabCrop_wgs84)
  
  result <- c(as.character(x$NAME), prop)
  
  prop_less_40 <- rbind(prop_less_40, result)
  
}

prop_less_40 <- as.data.frame(prop_less_40)

results$prop_less_40 <- as.numeric(as.character(prop_less_40$V2))



### 7. proportion of cells with cropland with 60% or more NH ###

prop_more_60 <- NULL

for(i in 1:nrow(wrld_simpl)){
  
  x <- wrld_simpl[i,]
  
  all <- extract(x = NatHabCrop_proj, y = x, df = TRUE)
  
  thres <- 60
  
  prop <- length(all$NatHabCrop_wgs84[all$NatHabCrop_wgs84 >= thres])/ length(all$NatHabCrop_wgs84)
  
  result <- c(as.character(x$NAME), prop)
  
  prop_more_60 <- rbind(prop_more_60, result)
  
}

prop_more_60 <- as.data.frame(prop_more_60)

results$prop_more_60 <- as.numeric(as.character(prop_more_60$V2))


write.csv(results, file = paste0(outdir, "Country_level_summaries_PARTIAL.csv"))



# 8. area of cropland

# this info was downloaded from http://www.fao.org/faostat/en/#data/QC

dir <- "0_data"

# read in the data, 2005 data but also downloaded 2018 data, which to use?
proddata <- read.csv(paste0(dir, "/FAOSTAT_data_2005_9-24-2020.csv"))

length(unique(proddata$Area)) # 214 countries

# getarea totals for each country

# subset the data
areaharv <- proddata[proddata$Element == "Area harvested", ]

# get country totals
area_ctry <- areaharv %>%
  group_by(Area) %>%
  summarize(total_area = sum(Value, na.rm = TRUE))


results$country <- as.character(results$country)

colnames(area_ctry)[1] <- "country"


# add into results table
results <- merge(results, area_ctry, by = "country", all.x = TRUE)

results[is.na(results$total_area), ]


# fill in the gaps where country names differ
results[results$country == "Bolivia", "total_area"] <- area_ctry[area_ctry$country == "Bolivia (Plurinational State of)", "total_area"]
results[results$country == "Cote d'Ivoire", "total_area"] <- area_ctry[area_ctry$country == "CÃ´te d'Ivoire", "total_area"]
results[results$country == "Cape Verde", "total_area"] <- area_ctry[area_ctry$country == "Cabo Verde", "total_area"]
results[results$country == "Czech Republic", "total_area"] <- area_ctry[area_ctry$country == "Czechia", "total_area"]
results[results$country == "French Guiana", "total_area"] <- area_ctry[area_ctry$country == "French Guyana", "total_area"]
results[results$country == "Hong Kong", "total_area"] <- area_ctry[area_ctry$country == "China, Hong Kong SAR", "total_area"]
results[results$country == "Korea, Democratic People's Republic of", "total_area"] <- area_ctry[area_ctry$country == "Democratic People's Republic of Korea", "total_area"]
results[results$country == "Korea, Republic of", "total_area"] <- area_ctry[area_ctry$country == "Republic of Korea", "total_area"]
results[results$country == "Micronesia, Federated States of", "total_area"] <- area_ctry[area_ctry$country == "Micronesia (Federated States of)", "total_area"]
results[results$country == "Reunion", "total_area"] <- area_ctry[area_ctry$country == "RÃ©union", "total_area"]
results[results$country == "Russia", "total_area"] <- area_ctry[area_ctry$country == "Russian Federation", "total_area"]
results[results$country == "Taiwan", "total_area"] <- area_ctry[area_ctry$country == "China, Taiwan Province of", "total_area"]
results[results$country == "United Kingdom", "total_area"] <- area_ctry[area_ctry$country == "United Kingdom of Great Britain and Northern Ireland", "total_area"]
results[results$country == "United States", "total_area"] <- area_ctry[area_ctry$country == "United States of America", "total_area"]
results[results$country == "Venezuela", "total_area"] <- area_ctry[area_ctry$country == "Venezuela (Bolivarian Republic of)", "total_area"]
results[results$country == "Sudan", "total_area"] <- area_ctry[area_ctry$country == "Sudan (former)", "total_area"]


nrow(results[is.na(results$total_area), ]) # 41




# 9. total crop production - earthstat

# subset the data
produc <- proddata[proddata$Element == "Production", ]

# get country totals
prod_ctry <- produc %>%
  group_by(Area) %>%
  summarize(total_prod = sum(Value, na.rm = TRUE))

# match colnames with results table
colnames(prod_ctry)[1] <- "country"


# add into results table
results <- merge(results, prod_ctry, by = "country", all.x = TRUE)

results[is.na(results$total_prod), ]


# fill in the gaps where country names differ
results[results$country == "Bolivia", "total_prod"] <- prod_ctry[prod_ctry$country == "Bolivia (Plurinational State of)", "total_prod"]
results[results$country == "Cote d'Ivoire", "total_prod"] <- prod_ctry[prod_ctry$country == "CÃ´te d'Ivoire", "total_prod"]
results[results$country == "Cape Verde", "total_prod"] <- prod_ctry[prod_ctry$country == "Cabo Verde", "total_prod"]
results[results$country == "Czech Republic", "total_prod"] <- prod_ctry[prod_ctry$country == "Czechia", "total_prod"]
results[results$country == "French Guiana", "total_prod"] <- prod_ctry[prod_ctry$country == "French Guyana", "total_prod"]
results[results$country == "Hong Kong", "total_prod"] <- prod_ctry[prod_ctry$country == "China, Hong Kong SAR", "total_prod"]
results[results$country == "Korea, Democratic People's Republic of", "total_prod"] <- prod_ctry[prod_ctry$country == "Democratic People's Republic of Korea", "total_prod"]
results[results$country == "Korea, Republic of", "total_prod"] <- prod_ctry[prod_ctry$country == "Republic of Korea", "total_prod"]
results[results$country == "Micronesia, Federated States of", "total_prod"] <- prod_ctry[prod_ctry$country == "Micronesia (Federated States of)", "total_prod"]
results[results$country == "Reunion", "total_prod"] <- prod_ctry[prod_ctry$country == "RÃ©union", "total_prod"]
results[results$country == "Russia", "total_prod"] <- prod_ctry[prod_ctry$country == "Russian Federation", "total_prod"]
results[results$country == "Taiwan", "total_prod"] <- prod_ctry[prod_ctry$country == "China, Taiwan Province of", "total_prod"]
results[results$country == "United Kingdom", "total_prod"] <- prod_ctry[prod_ctry$country == "United Kingdom of Great Britain and Northern Ireland", "total_prod"]
results[results$country == "United States", "total_prod"] <- prod_ctry[prod_ctry$country == "United States of America", "total_prod"]
results[results$country == "Venezuela", "total_prod"] <- prod_ctry[prod_ctry$country == "Venezuela (Bolivarian Republic of)", "total_prod"]
results[results$country == "Sudan", "total_prod"] <- prod_ctry[prod_ctry$country == "Sudan (former)", "total_prod"]


results[is.na(results$total_prod), ]

