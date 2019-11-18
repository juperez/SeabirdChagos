rm(list = ls())
library(sp) #for the spatial points
library(geosphere) #for calculating distance
library(fossil) #for function eart.dist
library(plyr) #for ldply
library(raster)
require(ape) #for the Moran Is test
require(geoR) # for the variograms 
library(lattice)
require(mgcv)
library(tools)
library(rgdal)
library(grDevices)
library(RColorBrewer)

setwd("/Users/ASUS/Documents/Master_CMEE/Chagos/")

#Get the data

Bobbies_Abundance <- read.csv("Variables/Data_Birds_Separate_Final/F_Sulidae_Data.csV")
names(Bobbies_Abundance)


#calculation of offset is the area for every kind of transect


Bobbies_Abundance$offset_time <- NA
levels(Bobbies_Abundance$Bird_Recording_Activity)
for(i in 1:length(Bobbies_Abundance$Bird_Recording_Activity)){
  if (Bobbies_Abundance$Bird_Recording_Activity[i] == "Aggregate"){
    Bobbies_Abundance$offset_time[i] <- log((pi*300^2)/2)
  }
  if (Bobbies_Abundance$Bird_Recording_Activity[i] == "PointCount"){
    Bobbies_Abundance$offset_time[i] <- log(pi*300^2)
  }
  if (Bobbies_Abundance$Bird_Recording_Activity[i] == "Transect"){
    Bobbies_Abundance$offset_time[i] <- log((pi*300^2) + 300^2)
  }
  }

#Getting only variables that are going to be used to model
Data_GamBob <- data.frame(Bobbies_Abundance$Sulidae,Bobbies_Abundance$Bath, Bobbies_Abundance$Slope,
                       Bobbies_Abundance$SST_Climonth, Bobbies_Abundance$Chl_Climonth, Bobbies_Abundance$SLA_Climonth,
                       Bobbies_Abundance$Year, Bobbies_Abundance$offset_time, Bobbies_Abundance$Bird_Recording_Activity,
                       as.factor(Bobbies_Abundance$Month))
names(Data_GamBob) <- c("Bobbies_Abundance","Depth", "Slope", 
                     "SST", "CHL", "SLA",
                     "Year", "offset.dat", "BRA", "Month")

head(Data_GamBob)
boxplot(Data_GamBob$Bobbies_Abundance ~ Data_GamBob$Year, ylim = c(0, 10))


#Get correlation pairwaise test for all variables 

panel.cor <- function(x, y, digits=1, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}

#Correlation of Total 
pdf("/home/cme006/Documents/Chagos/Models/Final_Models/Climonth_Pair_Graph.pdf")
Names <- c("Abundance", "Bath", "Slope", "SST", "NPP", 
           "SLA", "Year")
pairs(Data_GamBob[, c(1:7)],
      lower.panel = panel.cor,
      cex.labels=1.3,
      labels=Names, main = "Correlation of Oceanographic Variables at Climathological Resolution")
dev.off()


#Getting all conbinations of variables, exluding those that are correlates

Model_com <- c()
Com_factors <- combn(c(1:6), m = 4) #this is to get the number of the combinations
formulas <- c()
name_var <- c("Depth", "Slope",
              "SST", "CHL", "SLA", "Year")
#, "Year"

all_var <- c()

all_aic <- c()


for(j in 1:ncol(Com_factors)){
  
  variables <- Com_factors[,j]
  
  # the3 <- '3' %in% variables
  # the4 <- '4' %in% variables
  # 
  # 
  # if(the3 == TRUE & the4 == TRUE) {
  #   next
  # }
  
  
  get_formula <- paste(names(Data_GamBob)[1], " ~ ", 
                       " s(",name_var[variables[1]],",k=3)", "+", 
                       " s(",name_var[variables[2]],",k=3)", "+",
                       " s(",name_var[variables[3]],",k=3)", "+", 
                       " s(",name_var[variables[4]],",k=3)", "+", 
                       " s(BRA, bs = 're', k = 4)", sep = "")
                      #" + s(BRA, bs = 're', k = 4) + Month", sep = "")

  Gam_model <- gam(as.formula(get_formula), offset(offset.dat),
                   method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamBob)
  
  the_var <- cbind(name_var[variables[1]], name_var[variables[2]], 
                   name_var[variables[3]], name_var[variables[4]])
  
  
  gam_sum <- summary(Gam_model)
  
  SumaryGam <- summary(Gam_model)
  
  aic_dev <- cbind(Gam_model$gcv.ubre, gam_sum$dev.expl, 
                   SumaryGam$s.pv[1], SumaryGam$s.pv[2], 
                   SumaryGam$s.pv[3], SumaryGam$s.pv[4])
  
  
  all_aic <- rbind(all_aic, aic_dev)
  all_var <- rbind(all_var, the_var)
  
}


# Getting from all models the one that has the lowest GCV score
max(all_aic[,2])
min_aic <- min(all_aic[,1])
match(all_aic[,1], min_aic)
summary(Gam_model)
formulas[35]

#True GAM
gam_selected <- gam(Bobbies_Abundance ~ s(Depth, k = 3) + s(Slope, k = 3) + 
                      s(SLA, k = 3) + s(Year, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
                    method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamBob)
summary(gam_selected)

# Extract values using year

GAM_Values <- predict(gam_selected, type="terms",
                       se.fit = TRUE)
GAM_Values$fit[,4]

names(GAM_Values)

gamresults <- data.frame(GAM_Values$fit[,4], GAM_Values$fit[,4] + GAM_Values$se.fit[,4], GAM_Values$fit[,4] - GAM_Values$se.fit[,4])
names(gamresults) <- c("fit", "upper", "lower")

gamresults$month <- Bobbies_Abundance$Month
gamresults$year <- Bobbies_Abundance$Year

write.csv(gamresults, "Paper/Data_Birds_Separate_Final/Gam_Boobies.csv", row.names = F)


gam_selected <- gam(Bobbies_Abundance ~ s(Depth, k = 3) + s(Slope, k = 3) + 
                      s(SLA, k = 3) + s(Year, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
                    method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamBob)

summary(gam_selected)
#######################################

names(gamresults)
boxplot(fit ~ as.factor(month) + as.factor(year), data =  gamresults)


plot(GAM_Values$fit[,4] ~ Data_GamBob$Year, type = "o", ylim = c(-0.3,0.5))
upper <- GAM_Values$fit[,4] + GAM_Values$se.fit[,4]
lower <- GAM_Values$fit[,4] - GAM_Values$se.fit[,4]

length(upper)
lines(upper ~ Data_GamBob$Year, lty = "dashed")
lines(lower ~ Data_GamBob$Year, lty = "dashed")

head(GAM_Values$se.fit)

summary(gam_selected)
plot(gam_selected, pages = 1, scale = 0)
# Saving PDF of covariates

pdf("Paper/Graph_GAM_FINAL/Sulidae_Depth_Smooth.pdf")
plot(gam_selected, select = 1, scale = 0, se = TRUE,ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

pdf("Paper/Graph_GAM_FINAL/Sulidae_Slope_Smooth.pdf")
plot(gam_selected, select = 2, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

pdf("Paper/Graph_GAM_FINAL/Sulidae_SLA_Smooth.pdf")
plot(gam_selected, select = 3, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

pdf("Paper/Graph_GAM_FINAL/Sulidae_Year_Smooth.pdf")
plot(gam_selected, select = 4, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()


#### Saving TIFF of covariates

summary(gam_selected)
tiff("Paper/Graph_GAM_FINAL/Sulidae_Depth_Smooth.tiff")
plot(gam_selected, select = 1, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     cex.lab = 1.5, shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

  tiff("Paper/Graph_GAM_FINAL/Sulidae_Slope_Smooth.tiff")
plot(gam_selected, select = 2, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

tiff("Paper/Graph_GAM_FINAL/Sulidae_SLA_Smooth.tiff")
plot(gam_selected, select = 3, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

tiff("Paper/Graph_GAM_FINAL/Sulidae_Year_Smooth.tiff")
plot(gam_selected, select = 4, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()



#####################################################################################

#Getting Deviance explained

gam_selected <- gam(Bobbies_Abundance ~ s(Depth, k = 3) + s(Slope, k = 3) + 
                      s(SLA, k = 3) + s(Year, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
                    method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamBob)

summary(gam_selected)
Gam0 <- gam(Bobbies_Abundance  ~  1, offset(offset.dat),
            method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamBob)


Gam1 <- gam(Bobbies_Abundance ~  s(Slope, k = 3) + 
                      s(SLA, k = 3) + s(Year, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
                    method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamBob,
                    sp = c(gam_selected$sp[2], gam_selected$sp[3], gam_selected$sp[4], gam_selected$sp[5]))


Gam2 <-  gam(Bobbies_Abundance ~ s(Depth, k = 3) + 
               s(SLA, k = 3) + s(Year, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamBob,
             sp = c(gam_selected$sp[1], gam_selected$sp[3], gam_selected$sp[4], gam_selected$sp[5]))


Gam3 <-  gam(Bobbies_Abundance ~ s(Depth, k = 3) + s(Slope, k = 3) + 
               s(Year, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamBob,
             sp = c(gam_selected$sp[1], gam_selected$sp[2], gam_selected$sp[4], gam_selected$sp[5]))

Gam4 <-  gam(Bobbies_Abundance ~ s(Depth, k = 3) + s(Slope, k = 3) + 
               s(SLA, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamBob,
             sp = c(gam_selected$sp[1], gam_selected$sp[2], gam_selected$sp[3], gam_selected$sp[5]))

Gam5 <-  gam(Bobbies_Abundance ~ s(Depth, k = 3) + s(Slope, k = 3) + 
               s(SLA, k = 3) + s(Year, k = 3) , offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamBob,
             sp = c(gam_selected$sp[1], gam_selected$sp[2], gam_selected$sp[3], gam_selected$sp[4]))

# Calculating % of deviance considering only the amount explained by environmental variables
DevGam1 <- (deviance(Gam1) - deviance(gam_selected))/deviance(Gam0)
DevGam2 <- (deviance(Gam2) - deviance(gam_selected))/deviance(Gam0)
DevGam3 <- (deviance(Gam3) - deviance(gam_selected))/deviance(Gam0)
DevGam4 <- (deviance(Gam4) - deviance(gam_selected))/deviance(Gam0)
DevGam5 <- (deviance(Gam5) - deviance(gam_selected))/deviance(Gam0)

DevTot <- DevGam1 + DevGam2 + DevGam3 + DevGam4 + DevGam5
DevNo5 <- DevGam1 + DevGam2 + DevGam3 + DevGam4


Deviance_ex <- data.frame(rbind("Depth", "Slope", "SLA", "Year"),
                          rbind(c(DevGam1/DevNo5), c(DevGam2/DevNo5), 
                          c(DevGam3/DevNo5), c(DevGam4/DevNo5)))

names(Deviance_ex) <- c("Variables", "% Deviance")


write.csv(Deviance_ex, "/Chagos/Paper/Graph_GAM_FINAL/Deviance_data_Sulidae.csv", row.names = F)


##################################################
############# PREDICTING ###############
##################################################

summary(gam_selected)
plot(gam_selected, pages = 1, scale = 0)
gam.check(gam_selected)

# bird recording activities: "PointCount", "Transect", "Aggregate"

rasterextend <- raster("/Chagos/Variables/Chlorophyll/Clima-Month/Rasters/A20023052016335.L3m_MC_CHL_chlor_a_4km.grd")

setwd("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Covariates/")

get_list_pred <- list.files(pattern=".csv")
#Dist_Data <- read.csv("/Chagos/Variables/New_Variables/Dist_Coast_YN_Rats.csv")
#Dist_Coast <- Dist_Data$Dist_Coast
#names(Predictors)
#head(Dist_Coast)

Bird_Act <- c("Transect")

#rep this on the different ranges of covariates
rangeDepth <- range(Bobbies_Abundance$Bath)
rangeSlope <- range(Bobbies_Abundance$Slope)
rangeSLA <- range(Bobbies_Abundance$SLA_Climonth)


file <- get_list_pred[1]

for(file in get_list_pred){
  #summary(gam_selected)
  Predictors <- read.csv(file)
  
  Predictors <- data.frame(Predictors$LongDec, Predictors$LatDec, 
                           Predictors$Bath, Predictors$Slope, 
                           Predictors$SLA_Climonth)
  names(Predictors) <- c("LongDec", "LatDec","Depth","Slope", "SLA")
  
  Predictors <- subset(Predictors, 
                       Predictors$Depth > rangeDepth[1] & Predictors$Depth < rangeDepth[2])
  names(Predictors) <- c("LongDec", "LatDec","Depth","Slope", "SLA")
  
  
  Predictors <- subset(Predictors, 
                       Predictors$Slope > rangeSlope[1] & Predictors$Slope < rangeSlope[2])
  names(Predictors) <- c("LongDec", "LatDec","Depth","Slope", "SLA")
  
  
  Predictors <- subset(Predictors, 
                       Predictors$SLA > rangeSLA[1] & Predictors$SLA < rangeSLA[2])
  names(Predictors) <- c("LongDec", "LatDec","Depth","Slope", "SLA")
  
  
  Predictors$BRA <- (rep(Bird_Act, nrow(Predictors)))
  Predictors$Year <- (rep(2017, nrow(Predictors)))
  
  Values_pred <- predict(gam_selected,newdata=Predictors,type="response", exclude = "Year",
                         se.fit = TRUE)
    
  New_Values <- data.frame(Predictors$LongDec, Predictors$LatDec, Values_pred)
    
  New_Values$CoVa <- New_Values$se.fit/New_Values$fit
    
  names(New_Values) <- c("LongDec", "LatDec", "Predictions", "SE", "CoVa")
    
  #geting maps of fitted values
  Values_fit <- data.frame(New_Values$LongDec, New_Values$LatDec, New_Values$Predictions)
    
  New_Raster_Val <- rasterFromXYZ(Values_fit)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  
  writeRaster(ras2_resized,paste("../Rasters/Sulidae_ab_Fitted/",as.factor(Bird_Act),"_fitted_",
                                   file_path_sans_ext(file),sep = ""), overwrite = T)
    
  # getting maps of variance
  values_CV <- data.frame(New_Values$LongDec, New_Values$LatDec, New_Values$CoVa)
    
  New_Raster_CV <- rasterFromXYZ(values_CV)
  ras2_resized_cv <- resample(New_Raster_CV, rasterextend)
  proj4string(ras2_resized_cv) <- CRS("+proj=longlat +ellps=WGS84") 
    
  writeRaster(ras2_resized_cv,paste("../Rasters/Sulidae_ab_COVA/",as.factor(Bird_Act),"_CoVa_",
                                      file_path_sans_ext(file),sep = ""), overwrite = T)
    
}



##################################################
############# MAPPING ###############
##################################################


#function to get the mean of the raster but without including 0s
meanIgnoringna <- function(x) {
  mean(x,na.rm=T)
}

#Function for scalebar downloaded from internt




#coordinates
e_coord <- extent(67.5,76, -11,-2)
e_chagos <- extent(70.5,73, -7.5,-5)

#chagos maps
ChagosMap <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Maps_Project/Chagos_Land.shp")
proj4string(ChagosMap) <- CRS("+proj=longlat +ellps=WGS84")

ChagosEEZ <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Maps_Project/ChagosEEZ_Reprojected.shp")
proj4string(ChagosEEZ) <- CRS("+proj=longlat +ellps=WGS84")

ChagosBanks <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Mapping/Chagos_v6.shp")
proj4string(ChagosBanks) <- CRS("+proj=longlat +ellps=WGS84")


#get all the files from the fitted values

File_Raster <- list.files("../Rasters/Sulidae_ab_Fitted/", full.names = T, recursive=TRUE)

Stack_Files <- stack(File_Raster)


Rasters_Total <- overlay(Stack_Files, fun = meanIgnoringna)
proj4string(Rasters_Total) <- CRS("+proj=longlat +ellps=WGS84")
Rasters_Total <- mask(Rasters_Total, ChagosEEZ)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(log(Rasters_Total), min), cellStats(log(Rasters_Total), max))))
color_sequence <- seq(0,3.5,length.out=color_levels+1)

#function adapted to this raster
myScalebar = function(units_label, yadj=1.5) {
  
  # Get plot coordinates
  pc = par("usr") 
  
  # Position scale line between last two major x-axis tick marks
  # and 1/10th of the total y-range above the lower y-axis coordinate
  lines(c(floor(pc[1]+2),floor(pc[1]+1)),     
        rep(pc[3] + 0.1*(pc[4] - pc[3]), 2))
  
  # Place the units label at the midpoint of and just below the scale line
  text(x=min(c(floor(pc[1]+2), floor(pc[1]+1))), 
       y=pc[3] + 0.1*(pc[4] - pc[3]),
       label=units_label, adj=c(0.1, yadj))
}


pdf("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Maps/GAM_4knots/Sulidae_Abundance.pdf")
#par(mgp=c(2,1,0))
plot(log(Rasters_Total), col = my_palette(n=color_levels), breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies Abundance", line = 0.5, cex.main = 1.5)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
myScalebar("100 km")
dev.off()

plot(log(Rasters_Total))

writeRaster(Rasters_Total, "../Final_Rasters/Sulidae", overwrite = T)
writeRaster(log(Rasters_Total), "../Final_Rasters/SulidaeLog", overwrite = T)

RaSul <- raster("../Final_Rasters/Sulidae.gri")

mean(values(log(RaSul)), na.rm = T)
max(values(log(RaSul)), na.rm = T)
min(values(log(RaSul)), na.rm = T)
median(values(log(RaSul)), na.rm = T)


Suldt <- as.data.frame(RaSul, xy = T)
write.csv(Suldt,"../Final_Rasters/SulidaePred.csv" , row.names = F)

tiff("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Maps/GAM_4knots/Sulidae_Abundance.tiff")
#par(mgp=c(2,1,0))
plot(log(Rasters_Total), col = my_palette(n=color_levels), breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies Abundance", line = 0.5, cex.main = 1.5)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
myScalebar("100 km")
dev.off()


raster_total_crop <- crop(Rasters_Total,e_chagos)




#function adapted to this crop raster
myScalebar = function(units_label, yadj=1.5) {
  
  # Get plot coordinates
  pc = par("usr") 
  
  # Position scale line between last two major x-axis tick marks
  # and 1/10th of the total y-range above the lower y-axis coordinate
  lines(c(floor(pc[1]+2)-0.5,floor(pc[1]+1)-0.5),     
        rep(pc[3] + 0.90*(pc[4] - pc[3]), 2))
  
  # Place the units label at the midpoint of and just below the scale line
  text(x=min(c(floor(pc[1]+2)-0.5, floor(pc[1]+1)-0.5)), 
       y=pc[3] + 0.90*(pc[4] - pc[3]),
       label=units_label, adj=c(-0.3, yadj))
}

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(log(raster_total_crop), min), cellStats(log(raster_total_crop), max))))
color_sequence <- seq(0,3.5,length.out=color_levels+1)

pdf("/Chagos/Paper/Maps/GAM_4knots/Chagos_Sulidae_Abundance.pdf")
plot(log(raster_total_crop), col = my_palette(n=color_levels), breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies Abundance", line = 0.5, cex.main = 2)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
myScalebar("100 km")
dev.off()




tiff("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Maps/GAM_4knots/Chagos_Sulidae_Abundance.tiff")
plot(log(raster_total_crop), col = my_palette(n=color_levels), breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies Abundance", line = 0.5, cex.main = 2)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
myScalebar("100 km")
dev.off()

#get all the files from the variance matrix
File_Raster <- list.files("../Rasters/Sulidae_ab_COVA/", full.names = T, recursive=TRUE)

Stack_Files <- stack(File_Raster)

Rasters_Total <- overlay(Stack_Files, fun = meanIgnoringna)
proj4string(Rasters_Total) <- CRS("+proj=longlat +ellps=WGS84")
Rasters_Total <- mask(Rasters_Total, ChagosEEZ)
Rasters_Total <- crop(Rasters_Total,e_coord)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(Rasters_Total, min), cellStats(Rasters_Total, max))))
color_sequence <- seq(0,0.7,length.out=color_levels+1)


pdf("/Chagos/Paper/Maps/GAM_4knots/Sulidae_Abundance_VA.pdf")
plot(Rasters_Total, col = my_palette(n=color_levels), breaks = color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=0.7,
     #legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies", line = 0.5, cex.main = 3)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


tiff("/Chagos/Paper/Maps/GAM_4knots/Sulidae_Abundance_VA.tiff")
plot(Rasters_Total, col = my_palette(n=color_levels), breaks = color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=0.7,
     #legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies", line = 0.5, cex.main = 3)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


raster_total_crop <- crop(Rasters_Total,e_chagos)


my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(raster_total_crop, min), cellStats(raster_total_crop, max))))
color_sequence <- seq(0,0.7,length.out=color_levels+1)

pdf("/Chagos/Paper/Maps/GAM_4knots/Chagos_Sulidae_Abundance_VA.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels), breaks =  color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=0.75,
     #legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies", line = 0.5, cex.main = 2)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


tiff("/Chagos/Paper/Maps/GAM_4knots/Chagos_Sulidae_Abundance_VA.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels), breaks =  color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=0.75,
     #legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies", line = 0.5, cex.main = 2)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

