##%######################################################%##
#                                                          #
####            FAO stat country level data             ####
#                                                          #
##%######################################################%##

# This code from Tim organises country level FAO data on crop production
# area, yield and irrigation to model how yield stability is affected by
# crop diversity and percentage of crop area irrigated. 

# This country level data will be useful later to look at which countries 
# produce which crops and how much is produced.

# assessing country level yield, area and production data

rm(list = ls())

# set directories
dataDir <- "0_data/"
outDir <- "3_PrepareFAOData"
dir.create(outDir)

# read in the FAO production etc dataset
fao.crop <- read.csv(paste0(dataDir,"Production_Crops_E_All_Data.csv"))

# TODO: convert yield from tonnes/ha to kcal/ha

# separate out the yield, area and production data
fao.yield <- fao.crop[(fao.crop$Element=="Yield"),]
fao.area <- fao.crop[(fao.crop$Element=="Area harvested"),]
fao.prod <- fao.crop[(fao.crop$Element=="Production"),]

# join croptype and country info
fao.prod$Item_Area <- paste0(fao.prod$Item,'_',fao.prod$Area.Code)
fao.area$Item_Area <- paste0(fao.area$Item,'_',fao.area$Area.Code)

# just select data with area and production info
fao.area <- fao.area[(fao.area$Item_Area %in% fao.prod$Item_Area),]
fao.prod <- fao.prod[(fao.prod$Item_Area %in% fao.area$Item_Area),]

# calculates shannon diversity metric for crops per country
crop.div <- unlist(lapply(X = split(x = fao.area,f = fao.area$Area.Code),
                          FUN = function(country){
                            stopifnot(length(unique(country$Item))==nrow(country))
                            
                            # mean crop area across years for each row (crop)
                            crop.area.means <- apply(
                              X = country[,paste0('Y',1996:2005)],
                              MARGIN = 1,FUN = mean,na.rm=TRUE)
                            
                            crop.area.means <- crop.area.means/sum(crop.area.means,na.rm=TRUE)
                            
                            crop.area.means <- na.omit(crop.area.means)
                            
                            shannon.index <- -sum(crop.area.means * log(crop.area.means))
                            
                            crop.div <- exp(shannon.index)
                            
                            return(crop.div)
                          }))


# for each year, get the total crop area
country.totals.area <- sapply(X = 1996:2005,FUN = function(yr){
  
  total.area <- tapply(X = fao.area[[paste0('Y',yr)]],
                       INDEX = fao.area$Area.Code,
                       FUN = sum,na.rm=TRUE)
  
})

# for each year get the total production
country.totals.prod <- sapply(X = 1996:2005,FUN = function(yr){
  
  total.prod <- tapply(X = fao.prod[[paste0('Y',yr)]],
                       INDEX = fao.prod$Area.Code,
                       FUN = sum,na.rm=TRUE)
  
})

# get the total country yield per hectare (is that the unit?) for year year
country.yields <- country.totals.prod/country.totals.area

# calc mean and sd of yield across years for each country
mean.yields <- apply(X = country.yields,MARGIN = 1,FUN = mean)
sd.yields <- apply(X = country.yields,MARGIN = 1,FUN = sd)


stopifnot(all(names(crop.div)==names(mean.yields)))
stopifnot(all(names(crop.div)==names(sd.yields)))

# calculate yield stability (average yield divided by sd of yield)
yield.stability <- mean.yields/sd.yields

# organise the data
model.data <- data.frame(CountryCode = names(crop.div),
                         CropDiversity = crop.div,
                         YieldStability = yield.stability)

model.data$Country <- fao.area$Area[match(model.data$CountryCode,fao.area$Area.Code)]


# read in irrigation data from FAO
irrig.data <- read.csv(file = paste0(dataDir,"FAOSTAT_data_6-12-2020.csv"))

# calculate percentage irrigated crop area (?)
irrig.data$IrrigPercent <- NA

for(i in 1:nrow(irrig.data)){
  
  if(paste(irrig.data$Area.Code[i]) %in% 
     row.names(country.totals.area)){
    
    total.crop.area <- country.totals.area[
      paste(irrig.data$Area.Code[i]),][(irrig.data$Year[i]-1995)]
    if(is.na(total.crop.area)) total.crop.area <- 0
    
    if(total.crop.area>0){
      irrig.data$IrrigPercent[i] <- min(1,(irrig.data$Value[i]*1000)/total.crop.area)
    } else {
      irrig.data$IrrigPercent[i] <- NA
    }
    
  }
  
}

# average percentage of crop irrigated (?)
country.irrig.means <- tapply(X = irrig.data$IrrigPercent,
                              INDEX = irrig.data$Area.Code,
                              FUN = mean,na.rm=TRUE)

# add irrigation info to the model data table
model.data$IrrigPercent <- country.irrig.means[
  match(model.data$CountryCode,names(country.irrig.means))]

# crop.div[crop.div<10] <- NA

# remove values where stability is greater than 70
model.data$YieldStability[model.data$YieldStability>70] <- NA

# plot the data
plot(model.data$CropDiversity,model.data$YieldStability,log="y")
plot(sqrt(model.data$IrrigPercent),model.data$YieldStability,log="y")

# model yield stabilty response to crop diveristy and percentage irrigated
mod <- lm(log(YieldStability)~CropDiversity+IrrigPercent,data=model.data)

# look at the results
print(summary(mod))
