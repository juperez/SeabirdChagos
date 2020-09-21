###this is Boosted Regression Trees with the final models
# using presence of rats and distance to coast
# groupng guilds and not by species
# 

# for Boobies, and all species that has rats in either islands with and without rats, as Distance to Coast
# and Distane to Colonies with rats are correlated, we are going to use the approach of modelling with P/A of rats
#

#sudo apt-get install r-cran-segmented used in the terminal to install packages directly
rm(list = ls())



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



setwd("C:/Users/ASUS/Documents/Master_CMEE")

source("Elith_el_all_BRT/brt.functions.R") #Sourcing BRT functions from Elith et al

######################################################################################################### 


##################### SHEARWATERs


######################################################################################################### 



# Shearwater

model.dataShearwater <- read.csv("Chagos/Variables/Data_Birds_Separate_Final/F_Procellaridae_Data.csv", sep = ";")
areas <- read.table("Chagos/Variables/Data_Birds_Separate_Final/Areas.txt", header = T)
model.dataShearwater <- cbind(model.dataShearwater, areas)

model.dataShearwater <- subset(model.dataShearwater, model.dataShearwater$Bird_Recording_Activity == "Transect")

names(model.dataShearwater)

Shear_Rats <- model.dataShearwater[,c(1,22,16,28,8,37,54)]
Shear_No_Rats <- model.dataShearwater[,c(1,22,16,28,8,40,55)]

names(Shear_Rats)

model.dataShearwater$Dist_Island


#Shear_Rats$Shear_Total[is.na(Shear_Rats$Shear_Total)]<- 0 
#Shear_No_Rats$Shear_Total[is.na(Shear_No_Rats$Shear_Total)]<- 0 

#Shear_Rats$Rat_Group <- as.factor(Shear_Rats$Rat_Group)
#Shear_No_Rats$Rat_Group <- as.factor(Shear_No_Rats$Rat_Group)

#Shear_Rats <- subset(Shear_Rats, Shear_Rats$Wedge_Tailed_Shearwater > 0)

#Shear_No_Rats <- subset(Shear_No_Rats, Shear_No_Rats$Wedge_Tailed_Shearwater > 0)



names(Shear_No_Rats)

Model_Shearwater_Rat <- gbm.step(data = Shear_Rats, 
                                 gbm.x = 2:7, #this is for the covariates
                                 gbm.y = 1, #this is the column of the responae
                                 family = "poisson", #this is the family, in the case of the example is bernoulli as it is P/A data
                                 tree.complexity = 3, 
                                 prev.stratify = FALSE,
                                 n.trees = 1000,
                                 learning.rate = 0.00001,
                                 bag.fraction = 0.5,
                                 max.trees = 1000000,
                                 n.folds = 10,
                                 plot.main = TRUE)

#gbm.plot(Model_Shearwater_Rat, n.plots = 6, plot.layout = c(3,2), write.title = F, smooth = TRUE, common.scale = F)
#savePlot(filename = "/home/cme006/Documents/Chagos/Models/Final_Models/ML/Shearwater_All_Rats.tiff", type = "tiff")


#gbm.plot(Model_Shearwater_Rat, variable.no = 4, n.plots = 1, plot.layout = c(1,1), write.title = F, smooth = TRUE, common.scale = F)
#savePlot(filename = "/home/cme006/Documents/Chagos/Models/Final_Models/ML/Shearwater_Group_Rats.tiff", type = "tiff")

Model_Shearwater_No_Rat <- gbm.step(data = Shear_No_Rats, 
                                    gbm.x = 2:7, #this is for the covariates
                                    gbm.y = 1, #this is the column of the responae
                                    family = "poisson", #this is the family, in the case of the example is bernoulli as it is P/A data
                                    tree.complexity = 3, 
                                    prev.stratify = FALSE,
                                    n.trees = 1000,
                                    learning.rate = 0.00001,
                                    bag.fraction = 0.5,
                                    max.trees = 1000000,
                                    n.folds = 10,
                                    plot.main = TRUE)

#gbm.plot(Model_Shearwater_No_Rat, n.plots = 6, plot.layout = c(3,2), write.title = F, smooth = TRUE, common.scale = F)
#savePlot(filename = "/home/cme006/Documents/Chagos/Models/Final_Models/ML/Shearwater_All_No_Rats.tiff", type = "tiff")

#gbm.plot(Model_Shearwater_No_Rat, variable.no = 4, n.plots = 1, plot.layout = c(1,1), write.title = F, smooth = TRUE, common.scale = F)
#savePlot(filename = "/home/cme006/Documents/Chagos/Models/Final_Models/ML/Shearwater_Group_No_Rats.tiff", type = "tiff")

Model_Shearwater_No_Rat$self.statistics
Model_Shearwater_Rat$self.statistics

Shear_Rat_Cont <- Model_Shearwater_Rat$contributions
write.csv(Shear_Rat_Cont, "Chagos/Paper/Graphs_BRT_Presence/ProcellaridaeR_Contribution.csv", row.names=FALSE)

Shear_NoRat_Cont <- Model_Shearwater_No_Rat$contributions
write.csv(Shear_NoRat_Cont, "Chagos/Paper/Graphs_BRT_Presence/ProcellaridaeNR_Contribution.csv", row.names=FALSE)





Shearwater_Rats <- plot.gbm(Model_Shearwater_Rat,i.var= 5,return.grid=T)
Range_Shearwater_Rats <- ((range(Shearwater_Rats[,1])[2])+(range(Shearwater_Rats[,1])[1]))/2
lin.Shearwater_Rats <- lm(y~Dist_Coast_R,data=Shearwater_Rats)
#seg.Shearwater_Rats<-segmented(lin.Shearwater_Rats,seg.Z=~Dist_Coast_R,psi=Range_Shearwater_Rats)
#tiff("/home/cme006/Documents/Chagos/Models/Final_Models/ML/Shearwater_Rats.tiff")

seg.Shearwater_Rats <- segmented.lm(lin.Shearwater_Rats,seg.Z=~Dist_Coast_R,
                                 psi=c(78.2867269,155.8265439), control=seg.control(stop.if.error=FALSE,n.boot=0, it.max=20))

plot(seg.Shearwater_Rats,conf.level=0.95,shade=T,rug=F,
     ylab="Effect on Tern Shearwater",xlab="Distance to Coast Rats", col = "red",
     main = paste("Break Point =",round(seg.Shearwater_Rats$psi[2], digits = 2)))  
#dev.off()

Model_Shearwater_No_Rat$contributions


summary(seg.Shearwater_Rats)
slope(seg.Shearwater_Rats)
davies.test(lin.Shearwater_Rats, seg.Z=~Dist_Coast_R)
confint.segmented(seg.Shearwater_Rats)


Shearwater_No_Rats <- plot.gbm(Model_Shearwater_No_Rat,i.var= 5,return.grid=T)
Range_Shearwater_No_Rats <- ((range(Shearwater_No_Rats[,1])[2])+(range(Shearwater_No_Rats[,1])[1]))/2
lin.Shearwater_No_Rats <- lm(y~Dist_Coast_NR,data=Shearwater_No_Rats)
#seg.Shearwater_No_Rats<-segmented(lin.Shearwater_No_Rats,seg.Z=~Dist_Coast_NR,psi=Range_Shearwater_No_Rats)
#tiff("/home/cme006/Documents/Chagos/Models/Final_Models/ML/Shearwater_No_Rats.tiff")

seg.Shearwater_No_Rats <- segmented.lm(lin.Shearwater_No_Rats,seg.Z=~Dist_Coast_NR,
                                    psi=c(78.2867269,155.8265439), control=seg.control(stop.if.error=FALSE,n.boot=0, it.max=20))

plot(seg.Shearwater_No_Rats,conf.level=0.95,shade=T,rug=F,
      ylab="Effect on Tern Shearwater",xlab="Distance to Coast No Rats", col = "red",
      main = paste("Break Point =",round(seg.Shearwater_No_Rats$psi[2], digits = 2)))  
#dev.off()


# for calculateinf the CI interval 1) sum and substract se from the fitted values
augmented_Shearwater_rats <- augment(seg.Shearwater_Rats)
augmented_Shearwater_rats$upperR <- augmented_Shearwater_rats$.fitted + augmented_Shearwater_rats$.se.fit
augmented_Shearwater_rats$lowerR <- augmented_Shearwater_rats$.fitted - augmented_Shearwater_rats$.se.fit
colnames(augmented_Shearwater_rats)[7] <- "fitted_R"


write.csv(augmented_Shearwater_rats, "Chagos/Paper/Graphs_BRT_Presence/ProcellaridaeR.csv", row.names=FALSE)


augmented_Shearwater_no_rats <- augment(seg.Shearwater_No_Rats)
augmented_Shearwater_no_rats$upperNR <- augmented_Shearwater_no_rats$.fitted + augmented_Shearwater_no_rats$.se.fit
augmented_Shearwater_no_rats$lowerNR <- augmented_Shearwater_no_rats$.fitted - augmented_Shearwater_no_rats$.se.fit
colnames(augmented_Shearwater_no_rats)[7] <- "fitted_NR"


write.csv(augmented_Shearwater_no_rats, "Chagos/Paper/Graphs_BRT_Presence/ProcellaridaeNR.csv", row.names=FALSE)


augmented_Shearwater_rats <- read.csv("Chagos/Paper/Graphs_BRT_Presence/ProcellaridaeR.csv")
augmented_Shearwater_no_rats  <- read.csv("Chagos/Paper/Graphs_BRT_Presence/ProcellaridaeNR.csv")

data_aumengted_Shearwater <- data.frame(augmented_Shearwater_rats, augmented_Shearwater_no_rats)

names(data_aumengted_Shearwater)

#create the graphics of interactions with rat-free and rat-infested islands

cols <- c("Infested Islands"="white","Rat-free Islands"="blue4")

Shearwater_Dist_Lines <- ggplot(data_aumengted_Shearwater, aes(x=Dist_Coast_NR)) +
  
  #doing first line for no rats islands
  geom_line(aes(y = upperNR), alpha = "0") + 
  geom_line(aes(y = lowerNR), alpha = 0) +
  geom_ribbon(data=data_aumengted_Shearwater, 
              aes(ymin=lowerNR,ymax=upperNR), fill="blue4", alpha="1") +
  geom_line(aes(y = fitted_NR, color = "Rat-free Islands")) +
  
  #Second line for no rats islands
  geom_line(aes(y = upperR), alpha = "0") + 
  geom_line(aes(y = lowerR), alpha = 0) +
  geom_ribbon(data=data_aumengted_Shearwater, 
              aes(ymin=lowerR,ymax=upperR), fill="white", alpha="0") +
  geom_line(aes(y = fitted_R,  color = "Infested Islands")) +
  
  #including a vertical lines for rat-free breaking points
  geom_segment(aes(x = 82.9 , y = 0.49, xend = 82.9, yend = .545),
               color = "blue4", linetype="dashed") +
  annotate("text", x= 82.9, y=0.55, label= "82.9 km", angle = 90,
           size = 5, fontface = 'italic') + 
  
  #Customize
  theme_classic() +
  theme(panel.grid.minor = element_blank(),  aspect.ratio=1/1, legend.position = "none",
        axis.text.y = element_text(face="bold", size = 12, color = "black", angle = 90,hjust = 0.75),
        axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.title.x=element_text(size=14,face="bold")) +
  labs(x = "Distance to nearest island", y = NULL)+
  scale_y_continuous(expand = c(0, 0), limits=c(0.49,0.56)) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,350)) +
  scale_colour_manual(name="Distance to",values=cols, guide = guide_legend(fill = NULL,colour = NULL))


Shearwater_Dist_Lines

ggsave("Chagos/Paper/Graphs_BRT_Presence/Procellaridae_Dist_Lines.pdf", Shearwater_Dist_Lines)  
ggsave("Chagos/Paper/Graphs_BRT_Presence/Procellaridae_Dist_Lines.tiff", Shearwater_Dist_Lines)  


#create graphics that showd CI of distance to coast to rat free and rat infested islands 

CI_Shearwater_Rats <- data.frame(confint.segmented(seg.Shearwater_Rats))
CI_Shearwater_Rats$Num <- 1:length(CI_Shearwater_Rats[,1])
Y_Rats <- c(as.integer(0.0),as.integer(0.0),as.integer(0.0),as.integer(0.0))
CI_Shearwater_Rats <- rbind(CI_Shearwater_Rats,Y_Rats)
CI_Shearwater_Rats$Status <- rep("Infested Islands", nrow(CI_Shearwater_Rats))
colnames(CI_Shearwater_Rats) <- c("Estimated", "Lower", "Upper", "Num","Status")


CI_Shearwater_No_Rats <- data.frame(confint.segmented(seg.Shearwater_No_Rats))
CI_Shearwater_No_Rats$Num <- 1:length(CI_Shearwater_No_Rats[,1])
N_Rats <- c(as.integer(0.0),as.integer(0.0),as.integer(0.0),as.integer(0.0))
CI_Shearwater_No_Rats <- rbind(CI_Shearwater_No_Rats,N_Rats)
CI_Shearwater_No_Rats$Status <- rep("Free Islands", nrow(CI_Shearwater_No_Rats))
colnames(CI_Shearwater_No_Rats) <- c("Estimated", "Lower", "Upper", "Num","Status")

Set_CI <- rbind(CI_Shearwater_Rats, CI_Shearwater_No_Rats)


write.csv(Set_CI, "Chagos/Paper/Graphs_BRT_Presence/ProcellaridaeCI.csv", row.names=FALSE)


Set_CI <- read.csv("Chagos/Paper/Graphs_BRT_Presence/ProcellaridaeCI.csv")

pd <- position_dodge(0.1)


Shearwater_CI_Dist <- ggplot(Set_CI, aes(x=Num, y=Estimated, group=Status, color=Status)) + 
  geom_line() +
  geom_point()+
  scale_y_continuous(expand = c(0, 0), limits=c(0,150))+
  theme_classic()+ theme(legend.justification=c(1,0),
                        legend.position=c(1,0)) +
  scale_color_manual(values=c('#00008B','#FF0000'))+
  geom_hline(yintercept=50, linetype="dashed", 
            color = "black", size=1)+
  geom_hline(yintercept=130, color = "black", size=1)

ggsave("/Chagos/Paper/Graphs_BRT_Presence/Procellaridae_CI_Dist.pdf", Shearwater_CI_Dist)
ggsave("/Chagos/Paper/Graphs_BRT_Presence/Procellaridae_CI_Dist.tiff", Shearwater_CI_Dist)



# Shearwater_CI_Dist <- ggplot(Set_CI, aes(x=Status, y = Estimated, group = Status)) +
#   #draws the means
#   geom_point(position=pd) +
#   #draws the CI error bars
#   geom_errorbar(data=Set_CI, aes(ymin=Lower, ymax=Upper, color=c("blue", "red")), width=.3, position=pd) + 
#   theme_classic() +
#   theme(panel.grid.minor = element_blank(),  aspect.ratio=1/1, legend.position = "none") +
#   labs(x = "Island Status", y = "Estimated break point for Shearwaters")
# 
# ggsave("/Chagos/Paper/Graphs_BRT_Presence/Shearwater_CI_Dist.pdf", Shearwater_CI_Dist)
# ggsave("/Chagos/Paper/Graphs_BRT_Presence/Shearwater_CI_Dist.tiff", Shearwater_CI_Dist)





###########################################################
################ LETS DO SOME PREDICTIONS WITH BRT! #######
###########################################################


################# PREDICTIONS WITHOUT RATS

rasterextend <- raster("Chagos/Variables/Chlorophyll/Clima-Month/Rasters/A20023052016335.L3m_MC_CHL_chlor_a_4km.grd")

setwd("D:/Chagos/Paper/Covariates/")

get_list_pred <- list.files(pattern=".csv")
Dist_Data <- read.csv("/Chagos/Variables/New_Variables/Dist_Coast_YN_Rats.csv", sep = ";")



#rep this on the different ranges of covariates
rangeSST <- range(model.dataShearwater$SST_Climonth)
rangeSLA <- range(model.dataShearwater$SLA_Climonth)
rangeCHL <- range(model.dataShearwater$Chl_Climonth)
rangeSlope <- range(model.dataShearwater$Slope)


file <- get_list_pred[1]

nrow(Predictors)

for(file in get_list_pred){
  
  Predictors <- read.csv(file)
  
  Predictors <- data.frame(Predictors$LongDec, Predictors$LatDec, 
                           Predictors$Chl_Climonth, Predictors$SST_Climonth, 
                           Predictors$Slope ,Predictors$SLA_Climonth, 
                           Dist_Data$Dist_Coast, Dist_Data$Dist_Coast,
                           Dist_Data$Area,Dist_Data$Area, Dist_Data$Rats)

  names(Predictors) <- c("LongDec", "LatDec","Chl_Climonth",
                         "SST_Climonth", "Slope", "SLA_Climonth", 
                         "Dist_Coast_NR", "Dist_Coast_R",
                         "Area_NR", "Area_R","Rats")
  
  Predictors <- subset(Predictors, 
                       Predictors$Chl_Climonth > rangeCHL[1] & Predictors$Chl_Climonth < rangeCHL[2])
  names(Predictors) <- c("LongDec", "LatDec","Chl_Climonth",
                         "SST_Climonth", "Slope", "SLA_Climonth", 
                         "Dist_Coast_NR", "Dist_Coast_R",
                         "Area_NR", "Area_R","Rats")
  
  
  Predictors <- subset(Predictors, 
                       Predictors$SST_Climonth > rangeSST[1] & Predictors$SST_Climonth < rangeSST[2])
  names(Predictors) <- c("LongDec", "LatDec","Chl_Climonth",
                         "SST_Climonth", "Slope", "SLA_Climonth", 
                         "Dist_Coast_NR", "Dist_Coast_R",
                         "Area_NR", "Area_R","Rats")
  
  Predictors <- subset(Predictors, 
                       Predictors$SLA_Climonth > rangeSLA[1] & Predictors$SLA_Climonth < rangeSLA[2])
  names(Predictors) <- c("LongDec", "LatDec","Chl_Climonth",
                         "SST_Climonth", "Slope", "SLA_Climonth", 
                         "Dist_Coast_NR", "Dist_Coast_R",
                         "Area_NR", "Area_R","Rats")
  
  nrow(Predictors)
  
  Predictors$Year <- rep(2017, nrow(Predictors))
  
  Values_predNR <- predict.gbm(Model_Shearwater_No_Rat, Predictors, 
                               n.trees=Model_Shearwater_No_Rat$gbm.call$best.trees, 
                               type="response")
  
  Values_predR <- predict.gbm(Model_Shearwater_Rat, Predictors, 
                              n.trees=Model_Shearwater_Rat$gbm.call$best.trees, 
                              type="response")
  
  Difference_NR_R <- Values_predNR - Values_predR
  
  
  #for Rats free islands
  New_ValuesNR <- data.frame(Predictors$LongDec, Predictors$LatDec, Values_predNR)
  names(New_ValuesNR) <- c("LongDec", "LatDec", "Predictions")
  
  
  #for Rats infestation
  New_ValuesR <- data.frame(Predictors$LongDec, Predictors$LatDec, Values_predR, Predictors$Rats)
  #New_ValuesR <- subset(New_ValuesR, New_ValuesR$Predictors.Rats > 0)
  names(New_ValuesR) <- c("LongDec", "LatDec", "Predictions", "Rats")  
  
  
  New_Values <- data.frame(Predictors$LongDec, Predictors$LatDec, Difference_NR_R, Predictors$Rats)
  #New_Values <- subset(New_Values, New_Values$Predictors.Rats > 0)
  names(New_Values) <- c("LongDec", "LatDec", "Predictions", "Rats")   
  
  #here we ensure that there is no NA in the predictios and turn them into 0  
  #for(num in 1:length(New_Values$Predictions)){
  # if(New_Values$Predictions[num]>0 && is.na(New_Values$Predictions[num]) == FALSE){
  #   New_Values$Predictions[num] <- log(New_Values$Predictions[num])
  #  }
  #   else{New_Values$Predictions[num] <- 0}
  # }
  
  
  #geting maps of fitted values for rat free islandas
  Values_fit_NR <- data.frame(New_ValuesNR$LongDec, New_ValuesNR$LatDec, New_ValuesNR$Predictions)
  
  New_Raster_Val <- rasterFromXYZ(Values_fit_NR)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  writeRaster(ras2_resized,paste("../Graphs_BRT_MAPS/Rasters_BRT_Family/Procellariade_BRT_NR_Fitted", 
                                 file_path_sans_ext(file),sep = ""), overwrite = T)
  #plot(ras2_resized)
  
  #geting maps of fitted values for rat free islandas
  Values_fit_R <- data.frame(New_ValuesR$LongDec, New_ValuesR$LatDec, New_ValuesR$Predictions)
  New_Raster_Val <- rasterFromXYZ(Values_fit_R)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  writeRaster(ras2_resized,paste("../Graphs_BRT_MAPS/Rasters_BRT_Family/Procellariade_BRT_R_Fitted", 
                                 file_path_sans_ext(file),sep = ""), overwrite = T)
  
  
  #geting maps of fitted values
  Values_fit <- data.frame(New_Values$LongDec, New_Values$LatDec, New_Values$Predictions)
  New_Raster_Val <- rasterFromXYZ(Values_fit)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  writeRaster(ras2_resized,paste("../Graphs_BRT_MAPS/Rasters_BRT_Family/Procellariade_BRT_Dif_Fitted", 
                                 file_path_sans_ext(file),sep = ""), overwrite = T)
  
}



################################################################
################################################################

#GET THE COLORS OF THE MAPS
my_palette <- colorRampPalette(c("blue", "cyan", "yellow", "red"))(n = 100)

#function to get the mean of the raster but without including 0s
#meanIgnoringZeroes <- function(x) {
#  mean(x[x!=0],na.rm=T)
#}

#Function to get the means but ignoring the NA in each pixel, it is better
meanIgnoringna <- function(x) {
  mean(x,na.rm=T)
}


#coordinates
e_coord <- extent(67.5,76, -11,-2)
e_chagos <- extent(70.5,73, -7.5,-5)

#chagos maps
ChagosMap <- shapefile("D:/Chagos/Maps_Project/Chagos_Land.shp")
proj4string(ChagosMap) <- CRS("+proj=longlat +ellps=WGS84")

ChagosEEZ <- shapefile("D:/Chagos/Maps_Project/ChagosEEZ_Reprojected.shp")
proj4string(ChagosEEZ) <- CRS("+proj=longlat +ellps=WGS84")

ChagosBanks <- shapefile("/Chagos/Mapping/Chagos_v6.shp")
proj4string(ChagosBanks) <- CRS("+proj=longlat +ellps=WGS84")

Raster_NA <- raster("D:/Chagos/Paper/Maps/Final_Maps_BRT/NA_MAP.grd")


#get all the files from the fitted values for RAT FREE ISLANDS
#get all the rasters you need for rat-free islands
File_Raster <- list.files("../Graphs_BRT_MAPS/Rasters_BRT_Family/", full.names = T, recursive=TRUE, pattern = "Procellariade_BRT_NR_Fitted")

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
color_sequence <- seq(0.8,2.2,length.out=color_levels+1)

pdf("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Procellaridae_BRT_NR.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()

tiff("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Procellaridae_BRT_NR.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()

#get all the files from the fitted values for RAT INFESTED ISLANDS
#get all the rasters you need for rat-free islands
File_Raster <- list.files("../Graphs_BRT_MAPS/Rasters_BRT_Family/", full.names = T, recursive=TRUE, pattern = "Procellariade_BRT_R_Fitted")

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
color_sequence <- seq(0.8,2.2,length.out=color_levels+1)

pdf("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Procellaridae_BRT_R.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()

tiff("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Procellaridae_BRT_R.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()

######### Difference

#get all the files from the fitted values for RAT FREE ISLANDS
#get all the rasters you need for rat-free islands
File_Raster <- list.files("../Graphs_BRT_MAPS/Rasters_BRT_Family/", full.names = T, recursive=TRUE, pattern = "Procellariade_BRT_Dif_Fitted")

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
color_sequence <- seq(-0.8,0.8,length.out=color_levels+1)

pdf("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Procellaridae_BRT_Dif.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()

tiff("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Procellaridae_BRT_Dif.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()


#get que 95% upper quantile for the hotspot after rat erradication

raster_total_cropq <- raster_total_crop
values(raster_total_cropq) <- as.numeric(values(raster_total_cropq) >= quantile(raster_total_crop, 0.925))

#values(raster_total_cropq) <- as.numeric(values(raster_total_cropq) >= quantile(raster_total_crop, 0.975)) #upper 95% quantile
#values(raster_total_cropq) <- as.numeric(values(raster_total_cropq) >= quantile(raster_total_crop, 0.875)) #upper 85% quantile


pdf("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Procellaridae_BRT_Dif_Quantile.pdf")
plot(raster_total_cropq,col=c("lightblue", "darkred"),
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=2), legend = F)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = 0.8, legend = F, add = T)
dev.off()


tiff("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Procellaridae_BRT_Dif_Quantile.tiff")
plot(raster_total_cropq,col=c("lightblue", "darkred"),
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=2), legend = F)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = 0.8, legend = F, add = T)
dev.off()

