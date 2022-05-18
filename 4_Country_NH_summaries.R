##%######################################################%##
#                                                          #
####    Generate country level summaries of NH data     ####
#                                                          #
##%######################################################%##

# This script takes the global map of area of NH where crops are grown and
# summarises, for each country, how much NH is available and other stats. 

rm(list = ls())

# load libraries
library(raster)
library(maptools)
library(dplyr)
library(devtools)
#devtools::install_github("Pakillo/rgis")
library(rgis) # fast_extract function from this package
library(velox)
library(parallel)
library(sf)
library(exactextractr) # exact_extract function from here



# directories
datadir <- "2_PrepareNaturalHabitatLayer/"
outdir <- "4_Country_NH_summaries/"
dir.create(outdir)

# read in the fractional natural habitat data
#NatHabCrop <- raster(paste0(datadir, "NH_Cropland_Area_Jung_two.tif"))
NatHabCrop <- raster(paste0(datadir, "NH_Cropland_Area_Jung_four.tif"))

# convert to actual % values
#NatHabCrop <- NatHabCrop*100

# load country polygons
data(wrld_simpl)

# ensure both in same crs
crs(NatHabCrop)
crs(wrld_simpl)

plot(NatHabCrop)
plot(wrld_simpl, add = TRUE)


# subset the country list in wrld_simpl that has cropland data in the polygon


#### extracting country level summaries ####

# 1. mean proportion of natural habitat in an area with cropland
# 2. maximum proportion of NH 
# 3. minimum proportion of NH
# 4. median proportion of NH
# 5. standard deviation of proportion of NH
# 6. proportion of cells with cropland with 40% or less NH
# 7. proportion of cells with cropland with 60% or more NH
# 8. area of cropland 
# 9. total crop production 


# use the country polygons in wrld_simpl to get data summaries

# create dataframe to save results in
results <- wrld_simpl@data$NAME

# remove Antarctica
results <- results[c(1:144, 146:246)]

results <- data.frame(country = droplevels(results))


# convert to sf object
wrld_simpl_sf <- st_as_sf(wrld_simpl)

# just take the features with country name to make it smaller
wrld_simpl_sf <- wrld_simpl_sf[5]

# take a look
plot(wrld_simpl_sf)

# remove antarctica 145
wrld_simpl_sf <- wrld_simpl_sf[c(1:144, 146:246), ]



#### 1. Mean, Min, Max, Median and SD of natural habitat availability ####
 
NH_sums <- exact_extract(x = NatHabCrop, y = wrld_simpl_sf, fun = c('mean', 'min', 'max', 'median', 'stdev'))


# add into the results table
results$mean_prop_NH <- NH_sums$mean
results$min_prop_NH <- NH_sums$min
results$max_prop_NH <- NH_sums$max
results$median_prop_NH <- NH_sums$median
results$sd_prop_NH <- NH_sums$stdev

#### 2. proportion of cropland with 20% or less NH ####

countries <- wrld_simpl$NAME

assess_less20 <- function(i){
  
  x <- wrld_simpl[wrld_simpl$NAME == i, ]
  
  all <- extract(x = NatHabCrop, y = x, df = TRUE)
  
  all <- all[!is.na(all$NH_Cropland_Area_Jung_two),]
  
  
  if(!is.null(nrow(all))){
    
    
    thres <- 0.2
    
    prop <- length(all$NH_Cropland_Area_Jung_two[all$NH_Cropland_Area_Jung_two <= thres])/length(all$NH_Cropland_Area_Jung_two)
    
    result <- c(as.character(x$NAME), prop)
    
  } else result <- c(as.character(x$NAME), NA)
  
  return(result) 
  
}


# run in parallel as it takes a really long time
numCores <- detectCores() 

cl <- makeCluster(numCores)
clusterExport(cl, c("wrld_simpl", "NatHabCrop"))
clusterEvalQ(cl, {
  library(raster)
})


start <- Sys.time()

prop_less_20 <- parLapplyLB(cl, countries, assess_less20)

stopCluster(cl)

end <- Sys.time()
runtime <- end-start
runtime # 2.761624 hours for all countries, 1.139936 hours on desktop

# organise the outputs

prop_less_20 <- t(as.data.frame(prop_less_20, row.names = NULL))

colnames(prop_less_20) <- c("country", "prop_less_20")
rownames(prop_less_20) <- NULL

results <- merge(results, prop_less_20, by = "country", all.x = TRUE)



#### 3. proportion of cells with cropland with 20% or more NH ####


assess_more20 <- function(i){
  
  x <- wrld_simpl[wrld_simpl$NAME == i, ]
  
  all <- extract(x = NatHabCrop, y = x, df = TRUE)
  
  all <- all[!is.na(all$NH_Cropland_Area_Jung_two),]
  
  if(!is.null(nrow(all))){
    
    
    thres <- 0.2
    
    prop <- length(all$NH_Cropland_Area_Jung_two[all$NH_Cropland_Area_Jung_two > thres])/ length(all$NH_Cropland_Area_Jung_two)
    
    result <- c(as.character(x$NAME), prop)
    
  } else result <- c(as.character(x$NAME), NA)
  
  return(result)
  
}

# run function across all countries in parallel

cl <- makeCluster(numCores)
clusterExport(cl, c("wrld_simpl", "NatHabCrop"))
clusterEvalQ(cl, {
  library(raster)
})


start <- Sys.time()

prop_more_20 <- parLapplyLB(cl, countries, assess_more20)

stopCluster(cl)

end <- Sys.time()
runtime <- end-start
runtime # 2.4 hours for all countries, 1.1554 hours on desktop


# organise the outputs

prop_more_20 <- t(as.data.frame(prop_more_20, row.names = NULL))

colnames(prop_more_20) <- c("country", "prop_more_20")
rownames(prop_more_20) <- NULL

results <- merge(results, prop_more_20, by = "country", all.x = TRUE)



# proportion of cells with more than 40%

assess_more40 <- function(i){
  
  x <- wrld_simpl[wrld_simpl$NAME == i, ]
  
  all <- extract(x = NatHabCrop, y = x, df = TRUE)
  
  all <- all[!is.na(all$NH_Cropland_Area_Jung_two),]
  
  if(!is.null(nrow(all))){
    
    
    thres <- 0.4
    
    prop <- length(all$NH_Cropland_Area_Jung_two[all$NH_Cropland_Area_Jung_two > thres])/ length(all$NH_Cropland_Area_Jung_two)
    
    result <- c(as.character(x$NAME), prop)
    
  } else result <- c(as.character(x$NAME), NA)
  
  return(result)
  
}

# run function across all countries in parallel

cl <- makeCluster(numCores)
clusterExport(cl, c("wrld_simpl", "NatHabCrop"))
clusterEvalQ(cl, {
  library(raster)
})


start <- Sys.time()

prop_more_40 <- parLapplyLB(cl, countries, assess_more40)

stopCluster(cl)

end <- Sys.time()
runtime <- end-start
runtime # 2.4 hours for all countries


# organise the outputs

prop_more_40 <- t(as.data.frame(prop_more_40, row.names = NULL))

colnames(prop_more_40) <- c("country", "prop_more_40")
rownames(prop_more_40) <- NULL

results <- merge(results, prop_more_40, by = "country", all.x = TRUE)




# convert factors
results$prop_less_20 <- as.numeric(as.character(results$prop_less_20))
results$prop_more_20 <- as.numeric(as.character(results$prop_more_20))
results$prop_more_40 <- as.numeric(as.character(results$prop_more_40))


# save the table
write.csv(results, file = paste0(outdir, "/Country_summaries_Jung2.csv"), row.names = F)
#write.csv(results, file = paste0(outdir, "/Country_summaries_Jung4.csv"), row.names = F)

# results <- read.csv(file = paste0(outdir, "/Country_summaries_Jung2.csv"))




#### 4. area of cropland ####

# this info was downloaded from http://www.fao.org/faostat/en/#data/QC

alldata <- "0_data"

# which year should I be using for the production data?
proddata <- read.csv(paste0(alldata, "/FAOSTAT_data_2015_4-30-2021.csv"))

length(unique(proddata$Area)) # 200 countries


#### subset crops to just those represented in the Eartstat data used to get the crop area map?


# get area totals for each country

# subset the data
areaharv <- proddata[proddata$Element == "Area harvested", ]

# get country totals
area_ctry <- areaharv %>%
  group_by(Area) %>%
  summarize(total_area = sum(Value, na.rm = TRUE))

# convert to character so results can be merged
results$country <- as.character(results$country)

# match up column names
colnames(area_ctry)[1] <- "country"


# add into results table
results <- merge(results, area_ctry, by = "country", all.x = TRUE)

# some countries will be labelled differently so take a look at NAs
View(results[is.na(results$total_area), ])


# fill in the gaps where country names differ
results[results$country == "Bolivia", "total_area"] <- area_ctry[area_ctry$country == "Bolivia (Plurinational State of)", "total_area"]
results[results$country == "Cote d'Ivoire", "total_area"] <- area_ctry[area_ctry$country == "CÃ´te d'Ivoire", "total_area"]
results[results$country == "Cape Verde", "total_area"] <- area_ctry[area_ctry$country == "Cabo Verde", "total_area"]
results[results$country == "Czech Republic", "total_area"] <- area_ctry[area_ctry$country == "Czechia", "total_area"]
#results[results$country == "French Guiana", "total_area"] <- area_ctry[area_ctry$country == "French Guyana", "total_area"]
results[results$country == "Hong Kong", "total_area"] <- area_ctry[area_ctry$country == "China, Hong Kong SAR", "total_area"]
results[results$country == "Korea, Democratic People's Republic of", "total_area"] <- area_ctry[area_ctry$country == "Democratic People's Republic of Korea", "total_area"]
results[results$country == "Korea, Republic of", "total_area"] <- area_ctry[area_ctry$country == "Republic of Korea", "total_area"]
results[results$country == "Micronesia, Federated States of", "total_area"] <- area_ctry[area_ctry$country == "Micronesia (Federated States of)", "total_area"]
#results[results$country == "Reunion", "total_area"] <- area_ctry[area_ctry$country == "RÃ©union", "total_area"]
results[results$country == "Russia", "total_area"] <- area_ctry[area_ctry$country == "Russian Federation", "total_area"]
results[results$country == "Taiwan", "total_area"] <- area_ctry[area_ctry$country == "China, Taiwan Province of", "total_area"]
results[results$country == "United Kingdom", "total_area"] <- area_ctry[area_ctry$country == "United Kingdom of Great Britain and Northern Ireland", "total_area"]
results[results$country == "United States", "total_area"] <- area_ctry[area_ctry$country == "United States of America", "total_area"]
results[results$country == "Venezuela", "total_area"] <- area_ctry[area_ctry$country == "Venezuela (Bolivarian Republic of)", "total_area"]
#results[results$country == "Sudan", "total_area"] <- area_ctry[area_ctry$country == "Sudan (former)", "total_area"]
results[results$country == "Libyan Arab Jamahiriya", "total_area"] <- area_ctry[area_ctry$country == "Libya", "total_area"]
results[results$country == "Macau", "total_area"] <- area_ctry[area_ctry$country == "China, Macao SAR", "total_area"]
results[results$country == "Swaziland", "total_area"] <- area_ctry[area_ctry$country == "Eswatini", "total_area"]
results[results$country == "The former Yugoslav Republic of Macedonia", "total_area"] <- area_ctry[area_ctry$country == "North Macedonia", "total_area"]



nrow(results[is.na(results$total_area), ]) # 48

View(results[is.na(results$total_area) & !is.na(results$mean_prop_NH), ]) # 39 countries with values for NH but no production data from FAO



#### 5. total crop production ####

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

nrow(results[is.na(results$total_prod), ]) # 65


# fill in the gaps where country names differ
results[results$country == "Bolivia", "total_prod"] <- prod_ctry[prod_ctry$country == "Bolivia (Plurinational State of)", "total_prod"]
results[results$country == "Cote d'Ivoire", "total_prod"] <- prod_ctry[prod_ctry$country == "CÃ´te d'Ivoire", "total_prod"]
results[results$country == "Cape Verde", "total_prod"] <- prod_ctry[prod_ctry$country == "Cabo Verde", "total_prod"]
results[results$country == "Czech Republic", "total_prod"] <- prod_ctry[prod_ctry$country == "Czechia", "total_prod"]
#results[results$country == "French Guiana", "total_prod"] <- prod_ctry[prod_ctry$country == "French Guyana", "total_prod"]
results[results$country == "Hong Kong", "total_prod"] <- prod_ctry[prod_ctry$country == "China, Hong Kong SAR", "total_prod"]
results[results$country == "Korea, Democratic People's Republic of", "total_prod"] <- prod_ctry[prod_ctry$country == "Democratic People's Republic of Korea", "total_prod"]
results[results$country == "Korea, Republic of", "total_prod"] <- prod_ctry[prod_ctry$country == "Republic of Korea", "total_prod"]
results[results$country == "Micronesia, Federated States of", "total_prod"] <- prod_ctry[prod_ctry$country == "Micronesia (Federated States of)", "total_prod"]
#results[results$country == "Reunion", "total_prod"] <- prod_ctry[prod_ctry$country == "RÃ©union", "total_prod"]
results[results$country == "Russia", "total_prod"] <- prod_ctry[prod_ctry$country == "Russian Federation", "total_prod"]
results[results$country == "Taiwan", "total_prod"] <- prod_ctry[prod_ctry$country == "China, Taiwan Province of", "total_prod"]
results[results$country == "United Kingdom", "total_prod"] <- prod_ctry[prod_ctry$country == "United Kingdom of Great Britain and Northern Ireland", "total_prod"]
results[results$country == "United States", "total_prod"] <- prod_ctry[prod_ctry$country == "United States of America", "total_prod"]
results[results$country == "Venezuela", "total_prod"] <- prod_ctry[prod_ctry$country == "Venezuela (Bolivarian Republic of)", "total_prod"]
#results[results$country == "Sudan", "total_prod"] <- prod_ctry[prod_ctry$country == "Sudan (former)", "total_prod"]
results[results$country == "Libyan Arab Jamahiriya", "total_prod"] <- prod_ctry[prod_ctry$country == "Libya", "total_prod"]
results[results$country == "Macau", "total_prod"] <- prod_ctry[prod_ctry$country == "China, Macao SAR", "total_prod"]
results[results$country == "Swaziland", "total_prod"] <- prod_ctry[prod_ctry$country == "Eswatini", "total_prod"]
results[results$country == "The former Yugoslav Republic of Macedonia", "total_prod"] <- prod_ctry[prod_ctry$country == "North Macedonia", "total_prod"]


nrow(results[is.na(results$total_prod), ]) # 48


# save data
write.csv(results, file = paste0(outdir, "Country_level_summaries_Jung2.csv"), row.names = F)
#write.csv(results, file = paste0(outdir, "Country_level_summaries_Jung4.csv"), row.names = F)

#results <- read.csv(file = paste0(outdir, "Country_level_summaries_Jung2.csv"))





##%######################################################%##
#                                                          #
####            Assessing countries at risk             ####
#                                                          #
##%######################################################%##


#### Table 1: top 50 producers only ####

# order countries table by production values

results_prod <- results[order(results$total_prod, decreasing = T),]

# subset the highest producers?
top <- 50
results_prod <- results_prod[1:top, ]

# order by median level of NH around croplands
results_prod_gd <- results_prod[order(results_prod$median_prop_NH, decreasing = T),] 
results_prod_bd <- results_prod[order(results_prod$median_prop_NH, decreasing = F),]

# save the ordered tables
write.csv(results_prod_gd, file = paste0(outdir, "/Top_50_producers_ordered_medianNH_Jung2.csv"), row.names = F)
#write.csv(results_prod_gd, file = paste0(outdir, "/Top_50_producers_ordered_medianNH_Jung4.csv"), row.names = F)






#### table 2: based on prop GDP from Agriculture - incomplete

# gdpdat <- read.csv(paste0(alldata, "/Macro-Statistics_Key_Indicators_E_All_Data_17-05-2021.csv"))
# 
# 
# # NEXT: not all countries have gross output (agriculture) data, calc these and then use something else for others?
# 
# 
# gdpdat_sub <- gdpdat[gdpdat$Item == "Gross Output (Agriculture)" & gdpdat$Element %in% c("Value US$") | gdpdat$Item == "Gross Domestic Product" & gdpdat$Element %in% c("Value US$"), ]
# 
# results$
# 
# # get country totals
# prod_ctry <- produc %>%
#   group_by(Area) %>%
#   summarize(total_prod = sum(Value, na.rm = TRUE))





##%######################################################%##
#                                                          #
####         Maps of countries that are top 50          ####
####     producers coloured by median NH and SD NH      ####
#                                                          #
##%######################################################%##

library(ggplot2)
library(plyr)
library(cowplot)

results <- read.csv(file = paste0(outdir, "/Top_50_producers_ordered_medianNH_Jung2.csv"))

# load country polygons
data(wrld_simpl)

wrld_tab <- wrld_simpl@data

results$country <- factor(results$country, levels = levels(wrld_tab$NAME))

wrld_tab2 <- merge(x = wrld_tab, y = results, by.x = "NAME", by.y = "country", all.x = T)

wrld_simpl@data <- wrld_tab2

wrld_simpl@data$id <- wrld_simpl@data$ISO3

# need to do this to plot wrld_simpl using ggplot
shape_df <- fortify(wrld_simpl)

shape_df <- join(shape_df,wrld_simpl@data, by="id")                   
                    

p1 <- ggplot() +
  geom_polygon(data = shape_df , aes(x = long, y = lat, group = group, fill = median_prop_NH), color = 'lightgrey', size = 0.2) +
  scale_fill_continuous(low = "red", high = "green", na.value = NA) +
  theme_bw() +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(), 
        panel.border = element_rect(size = 0.2)) + 
  labs(fill="Median")


p2 <- ggplot() +
  geom_polygon(data = shape_df , aes(x = long, y = lat, group = group, fill = sd_prop_NH), color = 'lightgrey', size = 0.2) +
  scale_fill_continuous(low = "red", high = "green", na.value = NA) +
  theme_bw() +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(), 
        panel.border = element_rect(size = 0.2)) + 
  labs(fill="Sd")


plot_grid(p1, p2, nrow = 2)

ggsave2(file = paste0(outdir, "Figure_2_median_sd_top50.pdf"), height = 6, width = 6)
