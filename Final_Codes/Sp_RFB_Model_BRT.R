
rm (list = ls())

setwd("C:/Users/ASUS/Documents/Master_CMEE/")

#load(file = "Chagos/Paper/RData_BRTModels/RFB_BRT.rda") #requerir los datos guardados
source("Elith_el_all_BRT/brt.functions.R") #Sourcing BRT functions from Elith et al


library(broom)
library(gbm)
library(segmented)
library(ggplot2)
library(dplyr)
library(ggmap)
library(raster)
library(tools)
library(boot)
library(RColorBrewer)


model.data <- read.csv("Chagos/Paper/Data_Birds_Separate_Final/Species_Data.csv")

model.data <- subset(model.data, model.data$Bird_Recording_Activity == "Transect")
model.data <- subset(model.data, model.data$Year != 2017)

model.data$Year

###########################################################
################ LETS DO SOME PREDICTIONS WITH BRT! #######
###########################################################

#Load Red-footed Bobby BRTs

load("Chagos/Paper/RData_BRTModels/SpeciesBRT_RFB.rda")


int.null.deviance = Model_RFB2$self.statistics$mean.null
int.residual.deviance = Model_RFB2$cv.statistics$deviance.mean
int.dev = (int.residual.deviance/int.null.deviance) * 100

(int.null.deviance - int.residual.deviance)/int.null.deviance

#First we plot the data from the models


RFB_Contribution <- Model_RFB2$contributions


RFB_Cont <- RFB_Contribution$rel.inf
names(RFB_Cont) <- RFB_Contribution$var
RFB_Cont <- RFB_Cont[1:6]
names(RFB_Cont) <- c("Slope", "SST", "CHL", "DC", "SLA", "DMI")

Model_RFB2$cv.statistics

pdf("Chagos/Paper/Final_Graphs/BRT_Contributions/RFB_ModelContribution.pdf")
x <- barplot(RFB_Cont, col = c("darkgreen"), 
             ylim=c(0,60), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2)  
y<-as.matrix(RFB_Cont)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()


tiff("Chagos/Paper/Final_Graphs/BRT_Contributions/RFB_ModelContribution.tiff")
x <- barplot(RFB_Cont, col = c("darkgreen"), 
             ylim=c(0,60), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2)  
y<-as.matrix(RFB_Cont)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()



names(model.data[,c(20, 21, 24, 22, 25, 37)])

RFB_Cont

RFB_1 <- plot.gbm(Model_RFB2,i.var= 5,return.grid=T)
RFB_2 <- plot.gbm(Model_RFB2,i.var= 1,return.grid=T)
RFB_3 <- plot.gbm(Model_RFB2,i.var= 4,return.grid=T)
RFB_4 <- plot.gbm(Model_RFB2,i.var= 2,return.grid=T)
RFB_5 <- plot.gbm(Model_RFB2,i.var= 3,return.grid=T)
RFB_6 <- plot.gbm(Model_RFB2,i.var= 6,return.grid=T)


gbm.plot(Model_RFB2, plot.layout = c(3,2), write.title = F, smooth = TRUE, common.scale = F)


RFB_Slope <- 
  ggplot(RFB_2, aes(x=Slope, y)) + 
  geom_line(aes(y = y), size = 0.5, color = "darkgreen") + theme_classic() +
  labs(y="", x = "Slope (m)") +
  annotate("text", x= 29, y= 1.6 , label= "Cont.: 22.3%", angle = 0,
           size = 8) + stat_smooth() +
  theme(axis.text.x = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, face = "plain"))


RFB_SST <- 
  ggplot(RFB_3, aes(x=SST_Climonth)) + 
  geom_line(aes(y = y), size = 0.5,  color = "darkgreen") + theme_classic() +
  labs(y="", x = "Sea Surface Temperature (°C)") +
  annotate("text", x= 29.5, y= 1.75 , label= "Cont.: 20.7%", angle = 0,
           size = 8) +
  theme(axis.text.x = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, face = "plain"))


RFB_CHL <- 
  ggplot(RFB_4, aes(x=Chl_Climonth)) + 
  geom_line(aes(y = y), size = 0.5,  color = "darkgreen") + theme_classic() +
  labs(y="", x = "Chlorophyll-a (mg/m3)") +
  annotate("text", x= 0.3, y= 1.6 , label= "Cont.: 18.8%", angle = 0,
           size = 8) +
  theme(axis.text.x = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, face = "plain"))


RFB_DC <- 
  ggplot(RFB_1, aes(x=Dist_Coast)) + 
  geom_line(aes(y = y),size = 0.5, color = "darkgreen") + theme_classic() +
  labs(y="", x = "Distance to Coast (km)") +
  annotate("text", x= 100, y= 1.5 , label= "Cont.: 17.6%", angle = 0,
           size = 8) +
  theme(axis.text.x = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, face = "plain"))


RFB_SLA <- 
  ggplot(RFB_5, aes(x=SLA_Climonth, y)) + 
  geom_line(aes(y = y), size = 0.5,  color = "darkgreen") + theme_classic() + 
  labs(y="", x = "Sea Level Anomalies (m)") +
  annotate("text", x= 0.12, y= 1.5 , label= "Cont.: 13.9%", angle = 0,
           size = 8) + stat_smooth() +
  theme(axis.text.x = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, face = "plain"))

RFB_DMI <- 
  ggplot(RFB_6, aes(x=DMI)) + 
  geom_line(aes(y = y), size = 0.5,  color = "darkgreen") + theme_classic() + 
  labs(y="", x = "Dipole Mode Index") +
  annotate("text", x= 0.1, y= 1.7 , label= "Cont.: 6.7%", angle = 0,
           size = 8) +
  theme(axis.text.x = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, face = "plain"))


ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_DC.pdf", RFB_DC)
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_Slope.pdf", RFB_Slope)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_SST.pdf", RFB_SST)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_CHL.pdf", RFB_CHL)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_SLA.pdf", RFB_SLA)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_DMI.pdf", RFB_DMI)  


ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_DC.tiff", RFB_DC)
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_Slope.tiff", RFB_Slope)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_SST.tiff", RFB_SST)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_CHL.tiff", RFB_CHL)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_SLA.tiff", RFB_SLA)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/RFB_Model_DMI.tiff", RFB_DMI)



#now lets plot

rasterextend <- raster("~/Master_CMEE/Chagos/Variables/Chlorophyll/Clima-Month/Rasters/A20023052016335.L3m_MC_CHL_chlor_a_4km.grd")



setwd("Chagos/Paper/Covariates/")

get_list_pred <- list.files(pattern=".csv")

Dist_Data <- read.csv("~/Master_CMEE/Chagos/Variables/New_Variables/Dist_Coast_YN_Rats.csv", sep = ";")


Model_RFB2$contributions
#rep this on the different ranges of covariates

rangeSlope <- range(model.data$Slope)
rangeCHL <- range(model.data$Chl_Climonth)
rangeSST <- range(model.data$SST_Climonth)
rangeSLA <- range(model.data$SLA_Climonth)
rangeDC <- range(model.data$Dist_Coast)

DNIValues <- c(0.43, -0.098, -0.074, 0.066, 0.161, 0.341) #DNI in the order of the predictors appeareance in the loop


file <- get_list_pred[1]

i <- 0 #index controlled by the loop

nrow(Predictors)

for(file in get_list_pred){
  
  Predictors <- read.csv(file)
  
  #we repear discost and area just to include 
  #what would happend (the effect) on rats and withour rats
  Predictors <- data.frame(Predictors$LongDec, Predictors$LatDec, 
                           Predictors$SST_Climonth, Predictors$SLA_Climonth, 
                           Predictors$Chl_Climonth, Predictors$Slope, 
                           Dist_Data$Dist_Coast)
  
  names(Predictors) <- c("LongDec", "LatDec","SST_Climonth",
                         "SLA_Climonth", "Chl_Climonth", "Slope", 
                         "Dist_Coast")
  
  Predictors <- subset(Predictors, 
                       Predictors$SST_Climonth > rangeSST[1] & Predictors$SST_Climonth < rangeSST[2])
  names(Predictors) <- c("LongDec", "LatDec","SST_Climonth",
                         "SLA_Climonth", "Chl_Climonth", "Slope", 
                         "Dist_Coast")
  
  Predictors <- subset(Predictors, 
                       Predictors$SLA_Climonth > rangeSLA[1] & Predictors$SLA_Climonth < rangeSLA[2])
  names(Predictors) <- c("LongDec", "LatDec","SST_Climonth",
                         "SLA_Climonth", "Chl_Climonth", "Slope", 
                         "Dist_Coast")
  
  Predictors <- subset(Predictors, 
                       Predictors$Chl_Climonth > rangeCHL[1] & Predictors$Chl_Climonth < rangeCHL[2])
  names(Predictors) <- c("LongDec", "LatDec","SST_Climonth",
                         "SLA_Climonth", "Chl_Climonth", "Slope", 
                         "Dist_Coast")
  
  Predictors <- subset(Predictors, 
                       Predictors$Slope > rangeSlope[1] & Predictors$Slope < rangeSlope[2])
  names(Predictors) <- c("LongDec", "LatDec","SST_Climonth",
                         "SLA_Climonth", "Chl_Climonth", "Slope", 
                         "Dist_Coast")
  
  Predictors <- subset(Predictors, 
                       Predictors$Dist_Coast > rangeDC[1] & Predictors$Dist_Coast < rangeDC[2])
  names(Predictors) <- c("LongDec", "LatDec","SST_Climonth",
                         "SLA_Climonth", "Chl_Climonth", "Slope", 
                         "Dist_Coast")
  
  i <- i + 1
  
  Predictors$DMI <- rep(DNIValues[i], nrow(Predictors))
  
  
  Values_pred <- predict.gbm(Model_RFB2, Predictors, 
                             n.trees=Model_RFB2$gbm.call$best.trees, 
                             type="response")
  
  #new values from predictions
  New_Values <- data.frame(Predictors$LongDec, Predictors$LatDec, Values_pred)
  names(New_Values) <- c("LongDec", "LatDec", "Predictions")
  
  
  #geting maps of fitted values
  Values_fit <- data.frame(New_Values$LongDec, New_Values$LatDec, New_Values$Predictions)
  
  New_Raster_Val <- rasterFromXYZ(Values_fit)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  writeRaster(ras2_resized,paste("../Graphs_BRT_MAPS/Raster_BRT_SP_Models/RFB_BRT_Fitted", 
                                 file_path_sans_ext(file),sep = ""), overwrite = T)
  #plot(ras2_resized)
  
}



################################################################
################################################################

#GET THE COLORS OF THE MAPS
my_palette <- colorRampPalette(c("blue", "cyan", "yellow", "red"))(n = 100)

#Function to get the means but ignoring the NA in each pixel, it is better
meanIgnoringna <- function(x) {
  mean(x,na.rm=T)
}


#coordinates
e_coord <- extent(67.5,76, -11,-2)
e_chagos <- extent(69.5,74, -8.5,-4)

#chagos maps
ChagosMap <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Maps_Project/Chagos_Land.shp")
proj4string(ChagosMap) <- CRS("+proj=longlat +ellps=WGS84")

ChagosEEZ <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Maps_Project/ChagosEEZ_Reprojected.shp")
proj4string(ChagosEEZ) <- CRS("+proj=longlat +ellps=WGS84")

ChagosBanks <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Mapping/Chagos_v6.shp")
proj4string(ChagosBanks) <- CRS("+proj=longlat +ellps=WGS84")

#Raster_NA <- raster("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT/NA_MAP.grd")


#get all the files from the fitted values for RAT FREE ISLANDS
#get all the rasters you need for rat-free islands
File_Raster <- list.files("../Graphs_BRT_MAPS/Raster_BRT_SP_Models/", full.names = T, recursive=TRUE, pattern = "RFB")

#stack all the files
Stack_Files <- stack(File_Raster)

#this is for overlaying various raster and do operations. I created a function for avoid 0s in the overlay
Rasters <- overlay(Stack_Files, fun = meanIgnoringna) 
proj4string(Rasters) <- CRS("+proj=longlat +ellps=WGS84") #this is for reprojecting into the CRS
Rasters <- mask(Rasters, ChagosEEZ) #here I mask the the map into the BIOT Shape
Rasters <- crop(Rasters,e_coord) #finally here I crop into the chagos extent
raster_total_crop <- crop(Rasters,e_chagos)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(raster_total_crop, min), cellStats(raster_total_crop, max))))
color_sequence <- seq(0,17,length.out=color_levels+1)


myScalebar2 = function(units_label, yadj=1.5) {
  
  # Get plot coordinates
  pc = par("usr") 
  
  # Position scale line between last two major x-axis tick marks
  # and 1/10th of the total y-range above the lower y-axis coordinate
  lines(c(floor(pc[1]+2)-0.5,floor(pc[1]+1)-0.5),     
        rep(pc[3] + 0.70*(pc[4] - pc[3]), 2))
  
  # Place the units label at the midpoint of and just below the scale line
  text(x=min(c(floor(pc[1]+2)-0.5, floor(pc[1]+1)-0.5)), 
       y=pc[3] + 0.70*(pc[4] - pc[3]),
       label=units_label, adj=c(-0.3, yadj))
}

myScalebar2("100km")

pdf("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/RFB_BRT_Models.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5), useRaster= T)
plot(ChagosEEZ, add=TRUE)
plot(ChagosBanks, add = T)
scalebar(d = 100, type = "bar",  xy = c(70,-6), below = "Km", divs = 4)
dev.off()



tiff("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/RFB_BRT_Models.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5), useRaster= T)
plot(ChagosEEZ, add=TRUE)
plot(ChagosBanks, add = T)
scalebar(d = 100, type = "bar",  xy = c(70,-8.2), below = "Km", divs = 4)
dev.off()



myScalebar = function(units_label, yadj=1.5) {
  
  # Get plot coordinates
  pc = par("usr") 
  
  # Position scale line between last two major x-axis tick marks
  # and 1/10th of the total y-range above the lower y-axis coordinate
  lines(c(floor(pc[1]+6),floor(pc[1]+5)),     
        rep(pc[3] + 0.3*(pc[4] - pc[3]), 2))
  
  # Place the units label at the midpoint of and just below the scale line
  text(x=min(c(floor(pc[1]+6), floor(pc[1]+5))), 
       y=pc[3] + 0.3*(pc[4] - pc[3]),
       label=units_label, adj=c(0.1, yadj))
}


#For the whole BIOT
pdf("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/RFB_BRT_Models_BIOT.pdf")
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
plot(ChagosEEZ)
plot(Rasters, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend = F, useRaster= T, add = T)
plot(ChagosBanks, add = T)
myScalebar("100 km")
dev.off()

tiff("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/RFB_BRT_Models_BIOT.tiff")
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
plot(ChagosEEZ)
plot(Rasters, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend = F, useRaster= T, add = T)
plot(ChagosBanks, add = T)
myScalebar("100 km")
dev.off()

