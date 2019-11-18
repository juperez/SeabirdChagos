
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

Procelaridae_Abundance <- read.csv("C:/Users/ASUS/Documents/Master_CMEE/Chagos/Variables/Data_Birds_Separate_Final/F_Procellaridae_Data.csv", sep = ";")


boxplot(Procelaridae_Abundance$Procellaridae)

#we exclude outliers that 
Procelaridae_Abundance <- subset(Procelaridae_Abundance, Procelaridae_Abundance$Procellaridae < 50)

hist(Procelaridae_Abundance$Procellaridae)
#calculation of offset is the area for every kind of transect


Procelaridae_Abundance$offset_time <- NA
levels(Procelaridae_Abundance$Bird_Recording_Activity)
for(i in 1:length(Procelaridae_Abundance$Bird_Recording_Activity)){
  if (Procelaridae_Abundance$Bird_Recording_Activity[i] == "Aggregate"){
    Procelaridae_Abundance$offset_time[i] <- log((pi*300^2)/2)
  }
  if (Procelaridae_Abundance$Bird_Recording_Activity[i] == "PointCount"){
    Procelaridae_Abundance$offset_time[i] <- log(pi*300^2)
  }
  if (Procelaridae_Abundance$Bird_Recording_Activity[i] == "Transect"){
    Procelaridae_Abundance$offset_time[i] <- log((pi*300^2) + 300^2)
  }
}

#Getting only variables that are going to be used to model
Data_GamPro <- data.frame(Procelaridae_Abundance$Procellaridae,Procelaridae_Abundance$Bath, Procelaridae_Abundance$Slope,
                          Procelaridae_Abundance$SST_Climonth, Procelaridae_Abundance$Chl_Climonth, Procelaridae_Abundance$SLA_Climonth,
                          Procelaridae_Abundance$Year, Procelaridae_Abundance$offset_time, Procelaridae_Abundance$Bird_Recording_Activity)
names(Data_GamPro) <- c("Procelaridae_Abundance","Depth", "Slope", 
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
pdf("../Final_Graphs/Climonth_Pair_Graph.pdf")
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

all_var <- c()

all_aic <- c()


for(j in 1:ncol(Com_factors)){
  
  variables <- Com_factors[,j]
  
  get_formula <- paste(names(Data_GamPro)[1], " ~ ", 
                       " s(",name_var[variables[1]],",k=3)", "+", 
                       " s(",name_var[variables[2]],",k=3)", "+",
                       " s(",name_var[variables[3]],",k=3)", "+", 
                       " s(",name_var[variables[4]],",k=3)", "+", 
                       " s(BRA, bs = 're', k = 3)", sep = "")
  
  Gam_model <- gam(as.formula(get_formula), offset(offset.dat),
                   method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamPro)
  
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


gam_selected <- gam(Procelaridae_Abundance ~ s(SST, k = 3) + s(CHL, k = 3) + 
                      s(SLA, k = 3) + s(Year, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
                    method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamPro)

summary(gam_selected)

# Extract values using year for procellaridae

GAM_Values <- predict(gam_selected, type="terms",
                      se.fit = TRUE)
GAM_Values$fit[,4]

gamresults <- data.frame(GAM_Values$fit[,4], GAM_Values$fit[,4] + GAM_Values$se.fit[,4], GAM_Values$fit[,4] - GAM_Values$se.fit[,4])
names(gamresults) <- c("fit", "upper", "lower")

gamresults$month <- Procelaridae_Abundance$Month
gamresults$year <- Procelaridae_Abundance$Year

write.csv(gamresults, "Paper/Data_Birds_Separate_Final/Gam_Procellaridae.csv", row.names = F)

#######################################

summary(gam_selected)
plot(gam_selected, pages = 1, scale = 0)
# Saving PDF of covariates

pdf("Paper/Graph_GAM_FINAL/Procellaridae_SST_Smooth.pdf")
plot(gam_selected, select = 1, scale = 0, se = TRUE, ylab = "", cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = "SST", scheme = 1)
dev.off()

pdf("Paper/Graph_GAM_FINAL/Procellaridae_CHL_Smooth.pdf")
plot(gam_selected, select = 2, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

pdf("Paper/Graph_GAM_FINAL/Procellaridae_SLA_Smooth.pdf")
plot(gam_selected, select = 3, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

pdf("Paper/Graph_GAM_FINAL/Procellaridae_Year_Smooth.pdf")
plot(gam_selected, select = 4, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()


#### Saving TIFF of covariates


tiff("Paper/Graph_GAM_FINAL/Procellaridae_SST_Smooth.tiff")
plot(gam_selected, select = 1, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

tiff("Paper/Graph_GAM_FINAL/Procellaridae_CHL_Smooth.tiff")
plot(gam_selected, select = 2, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

tiff("Paper/Graph_GAM_FINAL/Procellaridae_SLA_Smooth.tiff")
plot(gam_selected, select = 3, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()

tiff("Paper/Graph_GAM_FINAL/Procellaridae_Year_Smooth.tiff")
plot(gam_selected, select = 4, scale = 0, se = TRUE, ylab = "",cex.axis = 2,
     shade = TRUE, shade.col = "gray70", col = "darkblue", xlab = NULL, scheme = 1)
dev.off()






#####################################################################################

#Getting Deviance explained

gam_selected <- gam(Procelaridae_Abundance ~ s(SST, k = 3) + s(CHL, k = 3) + 
                      s(SLA, k = 3) + s(Year, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
                    method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamPro)

summary(gam_selected)

Gam0 <- gam(Procelaridae_Abundance  ~  1, offset(offset.dat),
            method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamPro)


Gam1 <- gam(Procelaridae_Abundance ~  s(CHL, k = 3) + 
              s(SLA, k = 3) + s(Year, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
            method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamPro,
            sp = c(gam_selected$sp[2], gam_selected$sp[3], gam_selected$sp[4], gam_selected$sp[5]))


Gam2 <-  gam(Procelaridae_Abundance ~ s(SST, k = 3) + 
               s(SLA, k = 3) + s(Year, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamPro,
             sp = c(gam_selected$sp[1], gam_selected$sp[3], gam_selected$sp[4], gam_selected$sp[5]))


Gam3 <-  gam(Procelaridae_Abundance ~ s(SST, k = 3) + s(CHL, k = 3) + 
               s(Year, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamPro,
             sp = c(gam_selected$sp[1], gam_selected$sp[2], gam_selected$sp[4], gam_selected$sp[5]))

Gam4 <-  gam(Procelaridae_Abundance ~ s(SST, k = 3) + s(CHL, k = 3) + 
               s(SLA, k = 3) + s(BRA, bs = 're', k = 3), offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamPro,
             sp = c(gam_selected$sp[1], gam_selected$sp[2], gam_selected$sp[3], gam_selected$sp[5]))

Gam5 <-  gam(Procelaridae_Abundance ~ s(SST, k = 3) + s(CHL, k = 3) + 
               s(SLA, k = 3) + s(Year, k = 3) , offset(offset.dat),
             method = "GCV.Cp", family = quasipoisson(link = "log"), data = Data_GamPro,
             sp = c(gam_selected$sp[1], gam_selected$sp[2], gam_selected$sp[3], gam_selected$sp[4]))

# Calculating % of deviance considering only the amount explained by environmental variables
DevGam1 <- (deviance(Gam1) - deviance(gam_selected))/deviance(Gam0)
DevGam2 <- (deviance(Gam2) - deviance(gam_selected))/deviance(Gam0)
DevGam3 <- (deviance(Gam3) - deviance(gam_selected))/deviance(Gam0)
DevGam4 <- (deviance(Gam4) - deviance(gam_selected))/deviance(Gam0)
DevGam5 <- (deviance(Gam5) - deviance(gam_selected))/deviance(Gam0)

DevTot <- DevGam1 + DevGam2 + DevGam3 + DevGam4 + DevGam5
DevNo5 <- DevGam1 + DevGam2 + DevGam3 + DevGam4


Deviance_ex <- data.frame(rbind("SST", "CHL", "SLA", "Year"),
                          rbind(c(DevGam1/DevNo5), c(DevGam2/DevNo5), 
                                c(DevGam3/DevNo5), c(DevGam4/DevNo5)))

names(Deviance_ex) <- c("Variables", "% Deviance")

summary(gam_selected)
#write.csv(Deviance_ex, "Paper/Graph_GAM_FINAL/Deviance_data_Procellaridae.csv", row.names = F)

#this is for procellaridae model withour taking out the outliers
write.csv(Deviance_ex, "Paper/Graph_GAM_FINAL/Deviance_data_Procellaridae2.csv", row.names = F)

##################################################
############# PREDICTING ###############
##################################################

#get the extent of the raster that is going to be used
rasterextend <- raster("C:/Users/ASUS/Documents/Master_CMEE/Chagos/Variables/Chlorophyll/Clima-Month/Rasters/A20023052016335.L3m_MC_CHL_chlor_a_4km.grd")


#change directory
setwd("C:/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Covariates/")

get_list_pred <- list.files(pattern=".csv")

Bird_Act <- c("Transect")

#rep this on the different ranges of covariates
rangeSlope<- range(Procelaridae_Abundance$Slope)
rangeSST<- range(Procelaridae_Abundance$SST_Climonth)
rangeChl <- range(Procelaridae_Abundance$Chl_Climonth)
rangeSLA <- range(Procelaridae_Abundance$SLA_Climonth)


file <- get_list_pred[1]

for(file in get_list_pred){
  #summary(gam_selected)
  Predictors <- read.csv(file)
  
  Predictors <- data.frame(Predictors$LongDec, Predictors$LatDec, Predictors$SST_Climonth,
                           Predictors$Chl_Climonth, Predictors$SLA_Climonth)
  names(Predictors) <- c("LongDec", "LatDec","SST","CHL", "SLA")
  
  Predictors <- subset(Predictors, 
                       Predictors$SST > rangeSST[1] & Predictors$SST < rangeSST[2])
  names(Predictors) <- c("LongDec", "LatDec","SST","CHL", "SLA")
  
  
  Predictors <- subset(Predictors, 
                       Predictors$CHL > rangeChl[1] & Predictors$CHL < rangeChl[2])
  names(Predictors) <- c("LongDec", "LatDec","SST","CHL", "SLA")

  
  Predictors <- subset(Predictors, 
                       Predictors$SLA > rangeSLA[1] & Predictors$SLA < rangeSLA[2])
  names(Predictors) <- c("LongDec", "LatDec","SST","CHL", "SLA")
  
  
  
  Predictors$BRA <- rep(Bird_Act, nrow(Predictors))
  Predictors$Year <- rep(2017, nrow(Predictors))
  
  Values_pred <- predict(gam_selected,newdata=Predictors, exclude = "Year", type="response", se.fit = TRUE)
  
  New_Values <- data.frame(Predictors$LongDec, Predictors$LatDec, Values_pred)
  
  New_Values$CoVa <- New_Values$se.fit/New_Values$fit
  
  names(New_Values) <- c("LongDec", "LatDec", "Predictions", "SE", "CoVa")
  
  #geting maps of fitted values
  Values_fit <- data.frame(New_Values$LongDec, New_Values$LatDec, New_Values$Predictions)
  
  New_Raster_Val <- rasterFromXYZ(Values_fit)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  
  plot(ras2_resized)
  
  writeRaster(ras2_resized,paste("../Rasters/Procellaridae_ab_Fitted/",as.factor(Bird_Act),"_fitted_",
                                 file_path_sans_ext(file),"_noout",sep = ""), overwrite = T)
  
  # getting maps of variance
  values_CV <- data.frame(New_Values$LongDec, New_Values$LatDec, New_Values$CoVa)
  
  New_Raster_CV <- rasterFromXYZ(values_CV)
  ras2_resized_cv <- resample(New_Raster_CV, rasterextend)
  proj4string(ras2_resized_cv) <- CRS("+proj=longlat +ellps=WGS84") 
  
  writeRaster(ras2_resized_cv,paste("../Rasters/Procellaridae_ab_COVA/",as.factor(Bird_Act),"_CoVa_",
                                    file_path_sans_ext(file),"_noout",sep = ""), overwrite = T)
  
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
ChagosMap <- shapefile("C:/Users/ASUS/Documents/Master_CMEE/Chagos/Maps_Project/Chagos_Land.shp")
proj4string(ChagosMap) <- CRS("+proj=longlat +ellps=WGS84")

ChagosEEZ <- shapefile("C:/Users/ASUS/Documents/Master_CMEE/Chagos/Maps_Project/ChagosEEZ_Reprojected.shp")
proj4string(ChagosEEZ) <- CRS("+proj=longlat +ellps=WGS84")

ChagosBanks <- shapefile("C:/Users/ASUS/Documents/Master_CMEE/Chagos/Mapping/Chagos_v6.shp")
proj4string(ChagosBanks) <- CRS("+proj=longlat +ellps=WGS84")



#get all the files from the fitted values

#File_Raster <- list.files("../Rasters/Procellaridae_ab_Fitted/", full.names = T, recursive=TRUE, pattern = "noout")

File_Raster <- list.files("../Rasters/Procellaridae_ab_Fitters_out/", full.names = T, recursive=TRUE)

Stack_Files <- stack(File_Raster)


Rasters_Total <- overlay(Stack_Files, fun = meanIgnoringna)
proj4string(Rasters_Total) <- CRS("+proj=longlat +ellps=WGS84")
Rasters_Total <- mask(Rasters_Total, ChagosEEZ)
Rasters_Total <- crop(Rasters_Total,e_coord)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(log(Rasters_Total), min), cellStats(log(Rasters_Total), max))))
color_sequence <- seq(round(max_absolute_value,1)*-1,round(max_absolute_value,1),length.out=color_levels+1)
color_sequence <- seq(-6,6,length.out=color_levels+1)

pdf("/Chagos/Paper/Maps/GAM_4knots/Procellaridae_Abundance.pdf")
plot(log(Rasters_Total), col = my_palette(n=color_levels), breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies Abundance", line = 0.5, cex.main = 1.5)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

writeRaster(Rasters_Total, "../Final_Rasters/Procellariidae", overwrite = T)
writeRaster(log(Rasters_Total), "../Final_Rasters/ProcellariidaeLog", overwrite = T)

RaPro <- raster("../Final_Rasters/Procellariidae.gri")

mean(values(log(RaPro)), na.rm = T)
max(values(log(RaPro)), na.rm = T)
min(values(log(RaPro)), na.rm = T)
median(values(log(RaPro)), na.rm = T)

Prodt <- as.data.frame(RaPro, xy = T)
write.csv(Prodt,"../Final_Rasters/ProcellariidaePred.csv" , row.names = F)


tiff("/Chagos/Paper/Maps/GAM_4knots/Procellaridae_Abundance.tiff")
plot(log(Rasters_Total), col = my_palette(n=color_levels), breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies Abundance", line = 0.5, cex.main = 1.5)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()



raster_total_crop <- crop(Rasters_Total,e_chagos)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(log(Rasters_Total), min), cellStats(log(Rasters_Total), max))))
color_sequence <- seq(round(max_absolute_value,1)*-1,round(max_absolute_value,1),length.out=color_levels+1)


pdf("/Chagos/Paper/Maps/GAM_4knots/Chagos_Procellaridae_Abundance.pdf")
plot(log(raster_total_crop), col = my_palette(n=color_levels), breaks =  color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies Abundance", line = 0.5, cex.main = 2)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


tiff("/Chagos/Paper/Maps/GAM_4knots/Chagos_Procellaridae_Abundance.tiff")
plot(log(raster_total_crop), col = my_palette(n=color_levels), breaks =  color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=0.75, legend.width=0.75,
     legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies Abundance", line = 0.5, cex.main = 2)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

#get all the files from the variance matrix
File_Raster <- list.files("../Rasters/Procellaridae_ab_COVA/", full.names = T, recursive=TRUE)

Stack_Files <- stack(File_Raster)

Rasters_Total <- overlay(Stack_Files, fun = meanIgnoringna)
proj4string(Rasters_Total) <- CRS("+proj=longlat +ellps=WGS84")
Rasters_Total <- mask(Rasters_Total, ChagosEEZ)
Rasters_Total <- crop(Rasters_Total,e_coord)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(Rasters_Total, min), cellStats(Rasters_Total, max))))
color_sequence <- seq(0,round(max_absolute_value,1),length.out=color_levels+1)



pdf("/Chagos/Paper/Maps/GAM_4knots/Procellaridae_Abundance_VA.pdf")
plot(Rasters_Total, col = my_palette(n=color_levels), breaks = color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=0.7,
     #legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
#title("Boobies", line = 0.5, cex.main = 3)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


tiff("/Chagos/Paper/Maps/GAM_4knots/Procellaridae_Abundance_VA.tiff")
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
color_sequence <- seq(0,round(max_absolute_value,1),length.out=color_levels+1)


pdf("/Chagos/Paper/Maps/GAM_4knots/Chagos_Procellaridae_Abundance_VA.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels), breaks =  color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=0.75,
     #legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


tiff("/Chagos/Paper/Maps/GAM_4knots/Chagos_Procellaridae_Abundance_VA.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels), breaks =  color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=0.75,
     #legend.args=list(text='log(abundance)', side=4, font=10, line=-2, cex=1.5),
     axis.args=list(cex.axis=1))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()



