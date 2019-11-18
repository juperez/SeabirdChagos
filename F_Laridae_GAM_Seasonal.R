
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
library(mgcViz)  #visualization of GAM..interesting

setwd("/Users/ASUS/Documents/Master_CMEE/Chagos/")

#Get the data

Laridae_Abundance <- read.csv("Variables/Data_Birds_Separate_Final/F_Laridae_Data.csv", sep = ";")
#Laridae_Abundance <- read.csv("D:/Chagos/Variables/Data_Birds_Separate_Final/Noddies_Abundance_Data.csv", sep = ";")

names(Laridae_Abundance)

table(Laridae_Abundance$Year)


#calculation of offset is the area for every kind of transect


Laridae_Abundance$offset_time <- NA
levels(Laridae_Abundance$Bird_Recording_Activity)
for(i in 1:length(Laridae_Abundance$Bird_Recording_Activity)){
  if (Laridae_Abundance$Bird_Recording_Activity[i] == "Aggregate"){
    Laridae_Abundance$offset_time[i] <- log((pi*300^2)/2)
  }
  if (Laridae_Abundance$Bird_Recording_Activity[i] == "PointCount"){
    Laridae_Abundance$offset_time[i] <- log(pi*300^2)
  }
  if (Laridae_Abundance$Bird_Recording_Activity[i] == "Transect"){
    Laridae_Abundance$offset_time[i] <- log((pi*300^2) + 300^2)
  }
}

#Getting only variables that are going to be used to model
Data_GamLar <- data.frame(Laridae_Abundance$Laridae,Laridae_Abundance$Bath, Laridae_Abundance$Slope,
                          Laridae_Abundance$SST_Climonth, Laridae_Abundance$Chl_Climonth, Laridae_Abundance$SLA_Climonth,
                          Laridae_Abundance$Year, Laridae_Abundance$offset_time, Laridae_Abundance$Bird_Recording_Activity)
names(Data_GamLar) <- c("Laridae_Abundance","Depth", "Slope", 
                        "SST", "CHL", "SLA",
                        "Year", "offset.dat", "BRA")


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
  

  get_formula <- paste(names(Data_GamLar)[1], " ~ ", 
                       " s(",name_var[variables[1]],",k=3)", "+", 
                       " s(",name_var[variables[2]],",k=3)", "+",
                       " s(",name_var[variables[3]],",k=3)", "+", 
                       " s(",name_var[variables[4]],",k=3)", "+", 
                       " s(BRA, bs = 're', k = 3)", sep = "")
  
  Gam_model <- gam(as.formula(get_formula), offset(offset.dat),
                   method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamLar)
  
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


gam_selected <- gam(Laridae_Abundance ~ s(Depth, k = 3) + s(SST, k = 3) + 
                      s(CHL, k = 3) + s(SLA, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
                    method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamLar)


gam_selected$weights/cv(Data_GamLar$Laridae_Abundance)
summary(gam_selected)
plot(gam_selected, pages = 1, scale = 0, se = T)
# Saving PDF of covariates

pdf("Paper/Graph_GAM_FINAL/Laridae_Depth_Smooth.pdf")
plot(gam_selected, select = 1, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

pdf("Paper/Graph_GAM_FINAL/Laridae_SST_Smooth.pdf")
plot(gam_selected, select = 2, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

pdf("Paper/Graph_GAM_FINAL/Laridae_CHL_Smooth.pdf")
plot(gam_selected, select = 3, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

pdf("Paper/Graph_GAM_FINAL/Laridae_SLA_Smooth.pdf")
plot(gam_selected, select = 4, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()


#### Saving TIFF of covariates


tiff("Paper/Graph_GAM_FINAL/Laridae_Depth_Smooth.tiff")
plot(gam_selected, select = 1, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

tiff("Paper/Graph_GAM_FINAL/Laridae_SST_Smooth.tiff")
plot(gam_selected, select = 2, scale = 0, se = TRUE,ylab = "",cex.axis = 2, 
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

tiff("Paper/Graph_GAM_FINAL/Laridae_CHL_Smooth.tiff")
plot(gam_selected, select = 3, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

tiff("Paper/Graph_GAM_FINAL/Laridae_SLA_Smooth.tiff")
plot(gam_selected, select = 4, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

summary(gam_selected)

######TESTING FOR MGCvIZ

gamViz <- getViz(gam_selected)

o <- plot( sm(gamViz, 1) )

o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=log(y)), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()

o +  l_fitRaster() + l_fitContour() + l_points()

#####################################################################################

#Getting Deviance explained

gam_selected <- gam(Laridae_Abundance ~ s(Depth, k = 3) + s(SST, k = 3) + 
                      s(CHL, k = 3) + s(SLA, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
                    method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamLar)
summary(gam_selected)
Gam0 <- gam(Laridae_Abundance  ~  1, offset(offset.dat),
            method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamLar)


Gam1 <- gam(Laridae_Abundance ~  s(SST, k = 3) + 
              s(CHL, k = 3) + s(SLA, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
            method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamLar,
            sp = c(gam_selected$sp[2], gam_selected$sp[3], gam_selected$sp[4], gam_selected$sp[5]))


Gam2 <-  gam(Laridae_Abundance ~ s(Depth, k = 3) + 
               s(CHL, k = 3) + s(SLA, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamLar,
             sp = c(gam_selected$sp[1], gam_selected$sp[3], gam_selected$sp[4], gam_selected$sp[5]))


Gam3 <-  gam(Laridae_Abundance ~ s(Depth, k = 3) + s(SST, k = 3) + 
               s(SLA, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamLar,
             sp = c(gam_selected$sp[1], gam_selected$sp[2], gam_selected$sp[4], gam_selected$sp[5]))

Gam4 <-  gam(Laridae_Abundance ~ s(Depth, k = 3) + s(SST, k = 3) + 
               s(CHL, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamLar,
             sp = c(gam_selected$sp[1], gam_selected$sp[2], gam_selected$sp[3], gam_selected$sp[5]))

Gam5 <-  gam(Laridae_Abundance ~ s(Depth, k = 3) + s(SST, k = 3) + 
               s(CHL, k = 3) + s(SLA, k = 3) , offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamLar,
             sp = c(gam_selected$sp[1], gam_selected$sp[2], gam_selected$sp[3], gam_selected$sp[4]))

# Calculating % of deviance considering only the amount explained by environmental variables
DevGam1 <- (deviance(Gam1) - deviance(gam_selected))/deviance(Gam0)
DevGam2 <- (deviance(Gam2) - deviance(gam_selected))/deviance(Gam0)
DevGam3 <- (deviance(Gam3) - deviance(gam_selected))/deviance(Gam0)
DevGam4 <- (deviance(Gam4) - deviance(gam_selected))/deviance(Gam0)
DevGam5 <- (deviance(Gam5) - deviance(gam_selected))/deviance(Gam0)

DevNo5 <- DevGam1 + DevGam2 + DevGam3 + DevGam4


Deviance_ex <- data.frame(rbind("Depth", "SST", "CHL", "SLA"),
                          rbind(c(DevGam1/DevNo5), c(DevGam2/DevNo5), 
                                c(DevGam3/DevNo5), c(DevGam4/DevNo5)))

names(Deviance_ex) <- c("Variables", "% Deviance")

write.csv(Deviance_ex, "/Chagos/Paper/Graph_GAM_FINAL/Deviance_data_Laridae.csv", row.names = F)


##################################################
############# PREDICTING ###############
##################################################

#get the extent of the raster that is going to be used
rasterextend <- raster("Chagos/Variables/Chlorophyll/Clima-Month/Rasters/A20023052016335.L3m_MC_CHL_chlor_a_4km.grd")


#change directory
setwd("Paper/Covariates/")
Bird_Act <- c("Transect")

get_list_pred <- list.files(pattern=".csv")
head(read.csv(get_list_pred[1]))

#rep this on the different ranges of covariates
rangeDepth <- range(Data_GamLar$Depth)
rangeSST <- range(Data_GamLar$SST)
rangeCHL <- range(Data_GamLar$CHL)
rangeSLA <- range(Data_GamLar$SLA)

file <- get_list_pred[1]

for(file in get_list_pred){

  Predictors <- read.csv(file)
  
  Predictors <- data.frame(Predictors$LongDec, Predictors$LatDec, 
                           Predictors$Bath, Predictors$SST_Climonth, 
                           Predictors$Chl_Climonth, Predictors$SLA_Climonth)
  names(Predictors) <- c("LongDec", "LatDec", "Depth", "SST", "CHL",
                         "SLA")
  
  Predictors <- subset(Predictors, 
                       Predictors$Depth > rangeDepth[1] & Predictors$Depth < rangeDepth[2])
  names(Predictors) <- c("LongDec", "LatDec", "Depth", "SST", "CHL",
                         "SLA")
  
  Predictors <- subset(Predictors, 
                       Predictors$SST > rangeSST[1] & Predictors$SST < rangeSST[2])
  names(Predictors) <- c("LongDec", "LatDec", "Depth", "SST", "CHL",
                           "SLA")
  
  Predictors <- subset(Predictors, 
                         Predictors$CHL > rangeCHL[1] & Predictors$CHL < rangeCHL[2])
  names(Predictors) <- c("LongDec", "LatDec", "Depth", "SST", "CHL",
                           "SLA")  
    
  Predictors <- subset(Predictors, 
                       Predictors$SLA > rangeSLA[1] & Predictors$SLA < rangeSLA[2])
  names(Predictors) <- c("LongDec", "LatDec", "Depth", "SST", "CHL",
                         "SLA")
  
  Predictors$BRA <- rep(Bird_Act, nrow(Predictors))
  
  
  #Values_pred <- predict(gam_selected,newdata=Predictors, exclude = "Year", type="response", se.fit = TRUE)
  Values_pred <- predict(gam_selected,newdata=Predictors, type="response", se.fit = TRUE)
  
  New_Values <- data.frame(Predictors$LongDec, Predictors$LatDec, Values_pred)
  
  New_Values$CoVa <- New_Values$se.fit/New_Values$fit
  
  names(New_Values) <- c("LongDec", "LatDec", "Predictions", "SE", "CoVa")
  
  head(New_Values)
  #geting maps of fitted values
  Values_fit <- data.frame(New_Values$LongDec, New_Values$LatDec, New_Values$Predictions)
  
  New_Raster_Val <- rasterFromXYZ(Values_fit)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  
  writeRaster(ras2_resized,paste("../Rasters/Laridae_ab_Fitted/",as.factor(Bird_Act),"_fitted_",
                                 file_path_sans_ext(file),sep = ""), overwrite = T)
  
  # getting maps of variance
  values_CV <- data.frame(New_Values$LongDec, New_Values$LatDec, New_Values$CoVa)
  
  New_Raster_CV <- rasterFromXYZ(values_CV)
  ras2_resized_cv <- resample(New_Raster_CV, rasterextend)
  proj4string(ras2_resized_cv) <- CRS("+proj=longlat +ellps=WGS84") 
  
  writeRaster(ras2_resized_cv,paste("../Rasters/Laridae_ab_COVA/",as.factor(Bird_Act),"_CoVa_",
                                    file_path_sans_ext(file),sep = ""), overwrite = T)
  
}




##################################################
############# MAPPING ###############
##################################################


#GET THE COLORS OF THE MAPS

#function to get the mean of the raster but without including 0s
meanIgnoringna <- function(x) {
  mean(x,na.rm=T)
}

#coordinates
e_coord <- extent(67.5,76, -11,-2)
e_chagos <- extent(70.5,73, -7.5,-5)

#chagos maps
ChagosMap <- shapefile("~/Master_CMEE/Chagos/Maps_Project/Chagos_Land.shp")
proj4string(ChagosMap) <- CRS("+proj=longlat +ellps=WGS84")

ChagosEEZ <- shapefile("~/Master_CMEE/Chagos/Maps_Project/ChagosEEZ_Reprojected.shp")
proj4string(ChagosEEZ) <- CRS("+proj=longlat +ellps=WGS84")

ChagosBanks <- shapefile("~/Master_CMEE/Chagos/Mapping/Chagos_v6.shp")
proj4string(ChagosBanks) <- CRS("+proj=longlat +ellps=WGS84")



#get all the files from the fitted values

File_Raster <- list.files("../Rasters/Laridae_ab_Fitted/", full.names = T, recursive=TRUE)

Stack_Files <- stack(File_Raster)

Rasters_Total <- overlay(Stack_Files, fun = meanIgnoringna)
proj4string(Rasters_Total) <- CRS("+proj=longlat +ellps=WGS84")
Rasters_Total <- mask(Rasters_Total, ChagosEEZ)
Rasters_Total <- crop(Rasters_Total,e_coord)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(log(Rasters_Total), min), cellStats(log(Rasters_Total), max))))
color_sequence <- seq(round(max_absolute_value,1)*-1,round(max_absolute_value,1),length.out=color_levels+1)


pdf("/Chagos/Paper/Maps/GAM_4knots/Laridae_Abundance.pdf")
plot(log(Rasters_Total), col = my_palette(n=color_levels), breaks = color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

writeRaster(Rasters_Total, "../Final_Rasters/Laridae", overwrite = T)
writeRaster(log(Rasters_Total), "../Final_Rasters/LaridaeLog", overwrite = T)

test1 <- raster(File_Raster[1])
extract(Rasters_Total, xy, df = T)

Ralar <- raster("../Final_Rasters/Laridae.gri")

mean(values(log(Ralar)), na.rm = T)
max(values(log(Ralar)), na.rm = T)
min(values(log(Ralar)), na.rm = T)
median(values(log(Ralar)), na.rm = T)

Lardt <- as.data.frame(Ralar, xy = T)
write.csv(Lardt,"../Final_Rasters/LaridaePred.csv" , row.names = F)

tiff("/Chagos/Paper/Maps/GAM_4knots/Laridae_Abundance.tiff")
plot(log(Rasters_Total), col = my_palette(n=color_levels), breaks = color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

raster_total_crop <- crop(Rasters_Total,e_chagos)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(log(raster_total_crop), min), cellStats(log(raster_total_crop), max))))
color_sequence <- seq(round(max_absolute_value,1)*-1,round(max_absolute_value,1),length.out=color_levels+1)


pdf("/Chagos/Paper/Maps/GAM_4knots/Chagos_Laridae_Abundance.pdf")
plot(log(raster_total_crop), col = my_palette(n=color_levels), breaks = color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

tiff("/Chagos/Paper/Maps/GAM_4knots/Chagos_Laridae_Abundance.tiff")
plot(log(raster_total_crop), col = my_palette(n=color_levels), breaks = color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


#get all the files from the variance matrix
File_Raster <- list.files("Paper/Rasters/Laridae_ab_COVA/", full.names = T, recursive=TRUE)

Stack_Files <- stack(File_Raster)

Rasters_Total <- overlay(Stack_Files, fun = meanIgnoringna)
proj4string(Rasters_Total) <- CRS("+proj=longlat +ellps=WGS84")
Rasters_Total <- mask(Rasters_Total, ChagosEEZ)
Rasters_Total <- crop(Rasters_Total,e_coord)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(Rasters_Total, min), cellStats(Rasters_Total, max))))
color_sequence <- seq(0,round(max_absolute_value,2),length.out=color_levels+1)

pdf("/Chagos/Paper/Maps/GAM_4knots/Laridae_Abundance_VA.pdf")
plot(Rasters_Total, col = my_palette(n=color_levels), breaks = color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=0.7,
     #legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies", line = 0.5, cex.main = 3)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


tiff("/Chagos/Paper/Maps/GAM_4knots/Laridae_Abundance_VA.tiff")
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
color_sequence <- seq(0,round(max_absolute_value),length.out=color_levels+1)


pdf("/Chagos/Paper/Maps/GAM_4knots/Chagos_Laridae_Abundance_VA.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels), breaks =  color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=0.75,
     #legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies", line = 0.5, cex.main = 2)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


tiff("/Chagos/Paper/Maps/GAM_4knots/Chagos_Laridae_Abundance_VA.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels), breaks =  color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=0.75,
     #legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies", line = 0.5, cex.main = 2)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

