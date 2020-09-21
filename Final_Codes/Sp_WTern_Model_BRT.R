rm (list = ls())

setwd("C:/Users/ASUS/Documents/Master_CMEE/")


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

###########################################################
################ LETS DO SOME PREDICTIONS WITH BRT! #######
###########################################################



#Load Red-footed Bobby BRTs

load("Chagos/Paper/RData_BRTModels/SpeciesBRT_WTern.rda")



model.data <- read.csv("Chagos/Paper/Data_Birds_Separate_Final/Species_Data.csv")

model.data <- subset(model.data, model.data$Bird_Recording_Activity == "Transect")
model.data <- subset(model.data, model.data$Year != 2017)

int.null.deviance = Model_WTern2$self.statistics$mean.null
int.residual.deviance = Model_WTern2$cv.statistics$deviance.mean
int.dev = (int.residual.deviance/int.null.deviance) * 100


#First we plot the data from the models


WTern_Contribution <- Model_WTern2$contributions


WTern_Cont <- WTern_Contribution$rel.inf
names(WTern_Cont) <- WTern_Contribution$var
WTern_Cont <- WTern_Cont[1:5]
names(WTern_Cont) <- c("DC", "CHL", "Slope", "SLA", "SST")

Model_WTern2$cv.statistics

pdf("Chagos/Paper/Final_Graphs/BRT_Contributions/WTern_ModelContribution.pdf")
x <- barplot(WTern_Cont, col = c("darkgreen"), 
             ylim=c(0,50), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2)  
y<-as.matrix(WTern_Cont)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()


tiff("Chagos/Paper/Final_Graphs/BRT_Contributions/WTern_ModelContribution.tiff")
x <- barplot(WTern_Cont, col = c("darkgreen"), 
             ylim=c(0,50), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2)  
y<-as.matrix(WTern_Cont)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()



names(model.data[,c(20, 21, 22, 24, 25)])

WTern_Contribution

WTern_1 <- plot.gbm(Model_WTern2,i.var= 5,return.grid=T)
WTern_2 <- plot.gbm(Model_WTern2,i.var= 2,return.grid=T)
WTern_3 <- plot.gbm(Model_WTern2,i.var= 1,return.grid=T)
WTern_4 <- plot.gbm(Model_WTern2,i.var= 4,return.grid=T)
WTern_5 <- plot.gbm(Model_WTern2,i.var= 3,return.grid=T)


gbm.plot(Model_WTern2, plot.layout = c(3,2), write.title = F, smooth = TRUE, common.scale = F)


WTern_Contribution

WTern_DC<- 
  ggplot(WTern_1, aes(x=Dist_Coast)) + 
  geom_line(aes(y = y),size = 0.5, color = "darkgreen") + theme_classic() +
  labs(y="", x = "Distance to Coast (km)")  +
  annotate("text", x= 75, y= 0.5 , label= "Cont.: 41.1%", angle = 0,
           size = 8) +
  theme(axis.text.x = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, face = "plain"))

WTern_CHL <- 
  ggplot(WTern_2, aes(x=Chl_Climonth)) + 
  geom_line(aes(y = y), size = 0.5, color = "darkgreen") + theme_classic() +
  labs(y="", x = "Chlorophyll-a (mg/m3)") +
  annotate("text", x= 0.3, y= 0.15 , label= "Cont.: 23.5%", angle = 0,
           size = 8) +
  theme(axis.text.x = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, face = "plain")) 

WTern_SLA <- 
  ggplot(WTern_3, aes(x=SLA_Climonth)) + 
  geom_line(aes(y = y), size = 0.5,  color = "darkgreen") + theme_classic() +
  labs(y="", x = "Sea Level Anomalies (m)") +
  annotate("text", x= 0.10, y= 0.35 , label= "Cont.: 10.8%", angle = 0,
           size = 8) +
  theme(axis.text.x = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, face = "plain")) 

WTern_Slope <- 
  ggplot(WTern_4, aes(x=Slope)) + 
  geom_line(aes(y = y), size = 0.5,  color = "darkgreen") + theme_classic() +
  labs(y="", x = "Slope (m)") +
  annotate("text", x= 20, y= 0.175 , label= "Cont.: 13.8%", angle = 0,
           size = 8) +
  theme(axis.text.x = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, face = "plain")) 

WTern_SST <- 
  ggplot(WTern_5, aes(x=SST_Climonth)) + 
  geom_line(aes(y = y), size = 0.5,  color = "darkgreen") + theme_classic() + 
  labs(y="", x = "Sea Surface Temperature (°C)") +
  annotate("text", x= 29.1, y= 0.2 , label= "Cont.: 10.8%", angle = 0,
           size = 8) +
  theme(axis.text.x = element_text(color = "black", size = 20, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 20, angle = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 20, angle = 0, face = "plain")) 


ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/WTern_Model_DC.pdf", WTern_DC)
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/WTern_Model_CHL.pdf", WTern_CHL)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/WTern_Model_SLA.pdf", WTern_SLA)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/WTern_Model_Slope.pdf", WTern_Slope)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/WTern_Model_SST.pdf", WTern_SST)  


ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/WTern_Model_DC.tiff", WTern_DC)
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/WTern_Model_CHL.tiff", WTern_CHL)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/WTern_Model_SLA.tiff", WTern_SLA)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/WTern_Model_Slope.tiff", WTern_Slope)  
ggsave("Chagos/Paper/Final_Graphs/BRT_DependencePlots/WTern_Model_SST.tiff", WTern_SST)  



#now lets plot

rasterextend <- raster("~/Master_CMEE/Chagos/Variables/Chlorophyll/Clima-Month/Rasters/A20023052016335.L3m_MC_CHL_chlor_a_4km.grd")



setwd("Chagos/Paper/Covariates/")

get_list_pred <- list.files(pattern=".csv")

Dist_Data <- read.csv("~/Master_CMEE/Chagos/Variables/New_Variables/Dist_Coast_YN_Rats.csv", sep = ";")


Model_WTern2$contributions
#rep this on the different ranges of covariates

rangeSlope <- range(model.data$Slope)
rangeCHL <- range(model.data$Chl_Climonth)
rangeSST <- range(model.data$SST_Climonth)
rangeSLA <- range(model.data$SLA_Climonth)
rangeDC <- range(model.data$Dist_Coast)

#DNIValues <- c(0.43, -0.098, -0.074, 0.066, 0.161, 0.341) #DNI in the order of the predictors appeareance in the loop
#ONIValues <- c(2.5, -0.7, -0.6, 2.2, 1.7, 1.0)


Model_WTern_Rat$contributions


file <- get_list_pred[1]

#i <- 0 #index controlled by the loop



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
  
  
  Values_pred <- predict.gbm(Model_WTern2, Predictors, 
                             n.trees=Model_WTern2$gbm.call$best.trees, 
                             type="response")
  
  #new values from predictions
  New_Values <- data.frame(Predictors$LongDec, Predictors$LatDec, Values_pred)
  names(New_Values) <- c("LongDec", "LatDec", "Predictions")
  
  
  #geting maps of fitted values
  Values_fit <- data.frame(New_Values$LongDec, New_Values$LatDec, New_Values$Predictions)
  
  New_Raster_Val <- rasterFromXYZ(Values_fit)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  writeRaster(ras2_resized,paste("../Graphs_BRT_MAPS/Raster_BRT_SP_Models/WTern_BRT_Fitted", 
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
File_Raster <- list.files("../Graphs_BRT_MAPS/Raster_BRT_SP_Models/", full.names = T, recursive=TRUE, pattern = "WTern")

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
color_sequence <- round(seq(0,5,length.out=color_levels+1),1)

pdf("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/WTern_BRT_Models.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5), useRaster= T)
plot(ChagosEEZ, add=TRUE)
plot(ChagosBanks, add = T)
dev.off()



tiff("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/WTern_BRT_Models.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5), useRaster= T)
plot(ChagosEEZ, add=TRUE)
plot(ChagosBanks, add = T)
dev.off()




# #For the whole BIOT
# pdf("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/WTern_BRT_Models_BIOT.pdf")
# par(mar=c(0,0,0,0), oma=c(0,0,0,0))
# plot(ChagosEEZ)
# plot(Rasters, col = my_palette(n=color_levels),
#      breaks=color_sequence,
#      xlab = "Longitude", ylab = "Latitude",
#      legend = F, useRaster= T, add = T)
# plot(ChagosBanks, add = T)
# dev.off()
# 
# tiff("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/WTern_BRT_Models_BIOT.tiff")
# par(mar=c(0,0,0,0), oma=c(0,0,0,0))
# plot(ChagosEEZ)
# plot(Rasters, col = my_palette(n=color_levels),
#      breaks=color_sequence,
#      xlab = "Longitude", ylab = "Latitude",
#      legend = F, useRaster= T, add = T)
# plot(ChagosBanks, add = T)
# dev.off()
