###this is Boosted Regression Trees with the final models
# using presence of rats and distance to coast
# Y variables are speices RFB, WTS, BN, LN, WT and ST

# This script was made usin the approch suggested by reviewers

dev.off()
#sudo apt-get install r-cran-segmented used in the terminal to install packages directly


rm(list = ls())

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


######################################################################################################### 
##################### Red-footed Booby
######################################################################################################### 

# 
# For brown noddy, distance to coast is same as distance to colony as they might be correlated
model.data <- read.csv("Chagos/Paper/Data_Birds_Separate_Final/Species_Data.csv")

names(model.data)

model.data <- subset(model.data, model.data$Bird_Recording_Activity == "Transect")
model.data <- subset(model.data, model.data$Year != 2017)




Model_RFB_Rat <- gbm.step(data = model.data, 
                              gbm.x = c(20, 21, 24, 22, 30, 37), #this is for the covariates
                              gbm.y = 3, #this is the column of the responae
                              family = "poisson", #this is the family, in the case of the example is bernoulli as it is P/A data
                              tree.complexity = 4, 
                              prev.stratify = FALSE,
                              n.trees = 1000,
                              learning.rate = 0.0001,
                              bag.fraction = 0.5,
                              #step.size = 25,
                              max.trees = 1000000,
                              n.folds = 10,
                              plot.main = TRUE)

Model_RFB_No_Rat <- gbm.step(data = model.data, 
                                 gbm.x = c(20, 21, 24, 22, 33, 37), #this is for the covariates
                                 gbm.y = 3, #this is the column of the responae
                                 family = "poisson", #this is the family, in the case of the example is bernoulli as it is P/A data
                                 tree.complexity = 4, 
                                 prev.stratify = FALSE,
                                 n.trees = 1000,
                                 learning.rate = 0.0001,
                                 bag.fraction = 0.5,
                                 #step.size = 25,
                                 max.trees = 1000000,
                                 n.folds = 10,
                                 plot.main = TRUE)

save(Model_RFB_Rat, Model_RFB_No_Rat, file = "Chagos/Paper/RData_BRTModels/RFB_BRT.rda")


#return interactions between variables
interactionsRFB_Rats <- gbm.interactions(Model_RFB_Rat)
interactionsRFB_NRats <- gbm.interactions(Model_RFB_No_Rat)

#no significant interactions wer found
#in both cases areas with increase slope and SLA have more effects on seabirds
#particularly areas 

interactionsRFB_Rats$rank.list 
interactionsRFB_NRats$rank.list

#now we analize the deviance

#importatnt, simplifing this model mean that ONI in NR scenario
# is not importnat, but in a R scenario, it was still informative

gbm.plot(Model_RFB_Rat, plot.layout = c(4,2), write.title = F, smooth = TRUE, common.scale = F)

#deviance rat free

gbm.plot(Model_RFB_No_Rat, plot.layout = c(4,2), write.title = F, smooth = T, common.scale = F)


#######################################################################

#Loading models


load(file = "Chagos/Paper/RData_BRTModels/RFB_BRT.rda") #Rats model
load(file ="Chagos/Paper/RData_BRTModels/SpeciesBRT_RFB.rda") # Distance model


Model_RFB2$cv.statistics$deviance.mean #residual
Model_RFB2$self.statistics$mean.null #null
Model_RFB2$cv.statistics$correlation.mean # CV


Model_RFB_Rat$cv.statistics$deviance.mean #residual
Model_RFB_Rat$self.statistics$mean.null #null
Model_RFB_Rat$cv.statistics$correlation.mean # CV

Model_RFB_No_Rat$cv.statistics$deviance.mean #residual
Model_RFB_No_Rat$self.statistics$mean.null #null
Model_RFB_No_Rat$cv.statistics$correlation.mean # CV


Model_RFB2$contributions
Model_RFB_Rat$contributions
Model_RFB_No_Rat$contributions

#saving contributions for later plots
RFB_Rat_Cont <- Model_RFB_Rat$contributions
write.csv(RFB_Rat_Cont, "Chagos/Paper/BRTData_revision/RFB_Rats_Contribution.csv", row.names=FALSE)

RFB_NoRat_Cont <- Model_RFB_No_Rat$contributions
write.csv(RFB_NoRat_Cont, "Chagos/Paper/BRTData_revision/RFB_NoRats_Contribution.csv", row.names=FALSE)



#############
#obtaining the thresholds


RFB_Model <- plot.gbm(Model_RFB2,i.var= 5,return.grid=T)
Range_RFB_Rats <- ((range(RFB_Model[,1])[2])+(range(RFB_Model[,1])[1]))/2
lin.RFB_Model <- lm(y~Dist_Coast,data=RFB_Model)

seg.RFB_Model <- segmented.lm(lin.RFB_Model,seg.Z=~Dist_Coast, #here we erased PSI
                             control=seg.control(stop.if.error=FALSE,n.boot=0, it.max=20))

confint.segmented(seg.RFB_Model)

plot(seg.RFB_Model)


RFB_Rats <- plot.gbm(Model_RFB_Rat,i.var= 5,return.grid=T)
#plot(RFB_Rats)
Range_RFB_Rats <- ((range(RFB_Rats[,1])[2])+(range(RFB_Rats[,1])[1]))/2
lin.RFB_Rats <- lm(y~Dist_Coast_R,data=RFB_Rats)

quantile(RFB_Rats$Dist_Coast_R)

seg.RFB_Rats <- segmented.lm(lin.RFB_Rats,seg.Z=~Dist_Coast_R, #here we erased PSI
                                 control=seg.control(stop.if.error=FALSE,n.boot=0, it.max=20))

confint.segmented(seg.RFB_Rats)



RFB_No_Rats <- plot.gbm(Model_RFB_No_Rat,i.var= 5,return.grid=T)
Range_RFB_No_Rats <- ((range(RFB_No_Rats[,1])[2])+(range(RFB_No_Rats[,1])[1]))/2
lin.RFB_No_Rats <- lm(y~Dist_Coast_NR,data=RFB_No_Rats)

#here in psi we use to control where we want to get the breakpoint
seg.RFB_No_Rats <- segmented.lm(lin.RFB_No_Rats,seg.Z=~Dist_Coast_NR,
                                control=seg.control(stop.if.error=FALSE,n.boot=0, it.max=20))

confint.segmented(seg.RFB_No_Rats)

quantile(RFB_No_Rats$Dist_Coast_NR)

plot(seg.RFB_No_Rats,conf.level=0.95,shade=T,rug=F,
     ylab="Effect on Tern Shearwater",xlab="Distance to Coast No Rats", col = "red",
     main = paste("Break Point =",round(seg.RFB_No_Rats$psi[2], digits = 2)))  
#dev.off()

plot(seg.RFB_Rats,conf.level=0.95,shade=T,rug=F,
     ylab="Effect on Tern Shearwater",xlab="Distance to Coast No Rats", col = "red",
     main = paste("Break Point =",round(seg.RFB_Rats$psi[2], digits = 2)))  
#dev.off()

quantile(RFB_Rats$Dist_Coast_R)

#Original Model

augmented_RFB <- augment(seg.RFB_Model)
augmented_RFB$upperM <- augmented_RFB$.fitted + augmented_RFB$.se.fit
augmented_RFB$lowerM <- augmented_RFB$.fitted - augmented_RFB$.se.fit
colnames(augmented_RFB)[5] <- "fitted_M"


# for calculateinf the CI interval 1) sum and substract se from the fitted values
augmented_RFB_rats <- augment(seg.RFB_Rats)
augmented_RFB_rats$upperR <- augmented_RFB_rats$.fitted + augmented_RFB_rats$.se.fit
augmented_RFB_rats$lowerR <- augmented_RFB_rats$.fitted - augmented_RFB_rats$.se.fit
colnames(augmented_RFB_rats)[5] <- "fitted_R"


#write.csv(augmented_RFB_rats, "Chagos/Paper/Graphs_BRT_Presence/LaridaeR.csv", row.names=FALSE)

augmented_RFB_no_rats <- augment(seg.RFB_No_Rats)
augmented_RFB_no_rats$upperNR <- augmented_RFB_no_rats$.fitted + augmented_RFB_no_rats$.se.fit
augmented_RFB_no_rats$lowerNR <- augmented_RFB_no_rats$.fitted - augmented_RFB_no_rats$.se.fit
colnames(augmented_RFB_no_rats)[5] <- "fitted_NR"

#write.csv(augmented_RFB_no_rats, "Chagos/Paper/Graphs_BRT_Presence/LaridaeNR.csv", row.names=FALSE)


#augmented_RFB_rats <- read.csv("Chagos/Paper/Graphs_BRT_Presence/LaridaeR.csv")
#augmented_RFB_no_rats  <- read.csv("Chagos/Paper/Graphs_BRT_Presence/LaridaeNR.csv")


data_aumengted_RFB <- data.frame(augmented_RFB, augmented_RFB_rats, augmented_RFB_no_rats)

head(data_aumengted_RFB)

#create the graphics of interactions with rat-free and rat-infested islands

cols <- c("Infested Islands"="red3","Rat-free Islands"="blue4", "Coast"="darkgreen")

names(data_aumengted_RFB)

max(data_aumengted_RFB$fitted_NR) / max(data_aumengted_RFB$fitted_R)


data_aumengted_RFB$fitted_R

CIrats <- confint.segmented(seg.RFB_Rats)
CInorats <- confint.segmented(seg.RFB_No_Rats)

CIrats$Dist_Coast_R[2]

CInorats$Dist_Coast_NR[2]

RFB_Dist_Lines <- 
  ggplot(data_aumengted_RFB, aes(x= Dist_Coast_NR)) +
  
  #doing first line for no rats islands
  geom_line(aes(y = upperNR), alpha = 0) + 
  geom_line(aes(y = lowerNR), alpha = 0) +
  geom_ribbon(data=data_aumengted_RFB, 
              aes(ymin=lowerNR, ymax=upperNR), fill="blue4", alpha=0.3) +
  geom_line(aes(y = fitted_NR, color = "Rat-free Islands")) +
  
  #Second line for rats islands
  geom_line(aes(y = upperR), alpha = 0) + 
  geom_line(aes(y = lowerR), alpha = 0) +
  geom_ribbon(data=data_aumengted_RFB, 
              aes(ymin=lowerR, ymax=upperR), fill="red3", alpha= 0.3) +
  geom_line(aes(y = fitted_R,  color = "Infested Islands")) +
  
  #Third line for distribution model
  geom_line(aes(y = upperM), alpha = 0) + 
  geom_line(aes(y = lowerM), alpha = 0) +
  geom_ribbon(data=data_aumengted_RFB, 
              aes(ymin=lowerM, ymax=upperM), fill="darkgreen", alpha= 0.3) +
  geom_line(aes(y = fitted_M,  color = "Coast")) +
  
  #including a vertical lines for rat-free breaking points
  geom_segment(aes(x = 37.7831 , y = 1, xend = 37.7831, yend = 1.6),
               color = "blue4", linetype="dashed") +
  annotate("text", x= 37.7831, y=1.7, label= "37.8 km", angle = 90,
           size = 4, fontface = 'italic') + 
  
  # Including a vertical line for rat-infested breaking point
  geom_segment(aes(x = 60.01, y = 1, xend = 60.01, yend = 1.6),
               color = "red3", linetype="dashed") +
  annotate("text", x=60.01, y=1.7, label= "60.0 km", angle = 90,
           size = 4, fontface = c('italic')) + 
  
  
  #Customize
  theme_classic() +
  theme(panel.grid.minor = element_blank(),  aspect.ratio=1/1, 
        legend.position = c(0.8, 0.7), 
        legend.title=element_text(size=12), 
        legend.text=element_text(size=10),
        axis.text.y = element_text(face="bold", size = 12, color = "black", angle = 90,hjust = 0.5),
        axis.text.x = element_text(face="bold", size = 12, color = "black"),
        axis.title.x=element_text(size=14,face="bold")) +
  labs(x = "Distance to nearest island", y = NULL)+
  scale_y_continuous(expand = c(0, 0), limits=c(1,1.9)) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,150)) +
  scale_colour_manual(name="Models",values=cols, guide = guide_legend(fill = NULL,colour = NULL))

RFB_Dist_Lines
dev.off(f)

ggsave("Chagos/Paper/Graphs_BRT_Presence/RFB_Dist_Lines.pdf", RFB_Dist_Lines)  

ggsave("Chagos/Paper/Graphs_BRT_Presence/RFB_Dist_Lines.tiff", RFB_Dist_Lines)  

#create graphics that showd CI of distance to coast to rat free and rat infested islands 

CI_RFB <- confint.segmented(seg.RFB_Model)
CI_RFB_Rats <- confint.segmented(seg.RFB_Rats)
CI_RFB_No_Rats <- confint.segmented(seg.RFB_No_Rats)

Set_CI <- rbind(CI_RFB$Dist_Coast, CI_RFB_Rats$Dist_Coast_R, CI_RFB_No_Rats$Dist_Coast_NR)
Islans_Status <- c("Distribution", "Rat-Invaded", "Rat-free")
Set_CI <- data.frame(Set_CI, Islans_Status)
colnames(Set_CI) <- c("Estimated", "Lower", "Upper", "Status")

pd <- position_dodge(1)

RFB_CI_Dist <- ggplot(Set_CI, aes(x=Status, y = Estimated, group = Status)) +
  #draws the means
  #geom_point(position=pd) +
  
  #draws the CI error bars
  geom_errorbar(data=Set_CI, aes(ymin=Lower, ymax=Upper, 
                                 color=c("darkgreen","darkblue", "darkred")), 
                width=.3,size = 1, position=pd) + 
  scale_color_manual(values = c('darkred', 'darkgreen', 'darkblue')) +
  theme_classic() +
  theme(aspect.ratio=1/1, legend.position = "none") +
  labs(x = "Models", y = "Estimated break-point Regression") 
#geom_hline(yintercept=67.5, color = "red", linetype = 4) # for the threshold
#ggsave("/home/cme006/Documents/Chagos/Graphs_Paper/Final_Graphps/Graphics_BRT/Boobies_CI_Dist.pdf", Boobies_CI_Dist)

RFB_CI_Dist

ggsave("Chagos/Paper/Graphs_BRT_Presence/RFB_CI_Dist.pdf", RFB_CI_Dist)
ggsave("Chagos/Paper/Graphs_BRT_Presence/RFB_CI_Dist.tiff", RFB_CI_Dist)


###########################################################
################ LETS DO SOME PREDICTIONS WITH BRT! #######
###########################################################


################# PREDICTIONS WITHOUT RATS

rasterextend <- raster("~/Master_CMEE/Chagos/Variables/Chlorophyll/Clima-Month/Rasters/A20023052016335.L3m_MC_CHL_chlor_a_4km.grd")

setwd("Chagos/Paper/Covariates/")

get_list_pred <- list.files(pattern=".csv")
Dist_Data <- read.csv("~/Master_CMEE/Chagos/Variables/New_Variables/Dist_Coast_YN_Rats.csv", sep = ";")


Model_RFB_No_Rat$contributions
#rep this on the different ranges of covariates
rangeSST <- range(model.data$SST_Climonth)
rangeSLA <- range(model.data$SLA_Climonth)
rangeCHL <- range(model.data$Chl_Climonth)
rangeSlope <- range(model.data$Slope)

Model_RFB_Rat$contributions

file <- get_list_pred[1]

nrow(Predictors)

i <- 0 #index controlled by the loop

DMIValues <- c(0.43, -0.098, -0.074, 0.066, 0.161, 0.341) #DNI in the order of the predictors appeareance in the loop



for(file in get_list_pred){
  
  Predictors <- read.csv(file)
  
  names(Predictors)
  
  #we repear discost and area just to include 
  #what would happend (the effect) on rats and withour rats
  Predictors <- data.frame(Predictors$LongDec, Predictors$LatDec, 
                           Predictors$SST_Climonth, Predictors$SLA_Climonth, 
                           Predictors$Chl_Climonth, Predictors$Slope, 
                           Dist_Data$Dist_Coast, Dist_Data$Dist_Coast, 
                           Dist_Data$Area, Dist_Data$Area,Dist_Data$Rats)
  
  names(Predictors) <- c("LongDec", "LatDec","SST_Climonth",
                         "SLA_Climonth", "Chl_Climonth", "Slope", 
                         "Dist_Coast_R", "Dist_Coast_NR",  
                         "Area_R","Area_NR", "Rats")
  
  Predictors <- subset(Predictors, 
                       Predictors$SST_Climonth > rangeSST[1] & Predictors$SST_Climonth < rangeSST[2])
  names(Predictors) <- c("LongDec", "LatDec","SST_Climonth",
                         "SLA_Climonth", "Chl_Climonth", "Slope",  
                         "Dist_Coast_R", "Dist_Coast_NR",  
                         "Area_R","Area_NR", "Rats")
  
  Predictors <- subset(Predictors, 
                       Predictors$SLA_Climonth > rangeSLA[1] & Predictors$SLA_Climonth < rangeSLA[2])
  names(Predictors) <- c("LongDec", "LatDec","SST_Climonth",
                         "SLA_Climonth", "Chl_Climonth", "Slope",   
                         "Dist_Coast_R", "Dist_Coast_NR",  
                         "Area_R","Area_NR", "Rats")
  
  Predictors <- subset(Predictors, 
                       Predictors$Chl_Climonth > rangeCHL[1] & Predictors$Chl_Climonth < rangeCHL[2])
  names(Predictors) <- c("LongDec", "LatDec","SST_Climonth",
                         "SLA_Climonth", "Chl_Climonth", "Slope",   
                         "Dist_Coast_R", "Dist_Coast_NR",  
                         "Area_R","Area_NR", "Rats")
  
  Predictors <- subset(Predictors, 
                       Predictors > rangeSlope[1] & Predictors$Slope < rangeSlope[2])
  names(Predictors) <- c("LongDec", "LatDec","SST_Climonth",
                         "SLA_Climonth", "Chl_Climonth", "Slope",   
                         "Dist_Coast_R", "Dist_Coast_NR",  
                         "Area_R","Area_NR", "Rats")
  
  
  i <- i + 1
  
  Predictors$DMI <- rep(DMIValues[i], nrow(Predictors))
  
  
  Values_predNR <- predict.gbm(Model_RFB_No_Rat, Predictors, 
                               n.trees=Model_RFB_No_Rat$gbm.call$best.trees, 
                               type="response")
  
  Values_predR <- predict.gbm(Model_RFB_Rat, Predictors, 
                              n.trees=Model_RFB_Rat$gbm.call$best.trees, 
                              type="response")
  
  Difference_NR_R <- Values_predNR - Values_predR
  
  #for Rats free islands
  New_ValuesNR <- data.frame(Predictors$LongDec, Predictors$LatDec, Values_predNR)
  names(New_ValuesNR) <- c("LongDec", "LatDec", "Predictions")
  
  
  #for Rats infestation
  New_ValuesR <- data.frame(Predictors$LongDec, Predictors$LatDec, Values_predR, Predictors$Rats)
  names(New_ValuesR) <- c("LongDec", "LatDec", "Predictions", "Rats")
  
  
  #for rat difference
  New_Values <- data.frame(Predictors$LongDec, Predictors$LatDec, Difference_NR_R, Predictors$Rats)
  names(New_Values) <- c("LongDec", "LatDec", "Predictions", "Rats") 
  
  
  
  #geting maps of fitted values for rat free islandas
  Values_fit_NR <- data.frame(New_ValuesNR$LongDec, New_ValuesNR$LatDec, New_ValuesNR$Predictions)
  
  New_Raster_Val <- rasterFromXYZ(Values_fit_NR)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  writeRaster(ras2_resized,paste("../Graphs_BRT_MAPS/Rasters_BRT_Sp/RFB_BRT_NR_Fitted", 
                                 file_path_sans_ext(file),sep = ""), overwrite = T)
  #plot(ras2_resized)
  
  #geting maps of fitted values for rat free islandas
  Values_fit_R <- data.frame(New_ValuesR$LongDec, New_ValuesR$LatDec, New_ValuesR$Predictions)
  New_Raster_Val <- rasterFromXYZ(Values_fit_R)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  writeRaster(ras2_resized,paste("../Graphs_BRT_MAPS/Rasters_BRT_Sp/RFB_BRT_R_Fitted", 
                                 file_path_sans_ext(file),sep = ""), overwrite = T)
  
  
  #geting maps of fitted values
  Values_fit <- data.frame(New_Values$LongDec, New_Values$LatDec, New_Values$Predictions)
  New_Raster_Val <- rasterFromXYZ(Values_fit)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  writeRaster(ras2_resized,paste("../Graphs_BRT_MAPS/Rasters_BRT_Sp/RFB_BRT_Dif_Fitted", 
                                 file_path_sans_ext(file),sep = ""), overwrite = T)
  
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
e_chagos <- extent(70.5,73, -7.5,-5)

#chagos maps
ChagosMap <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Maps_Project/Chagos_Land.shp")
proj4string(ChagosMap) <- CRS("+proj=longlat +ellps=WGS84")

ChagosEEZ <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Maps_Project/ChagosEEZ_Reprojected.shp")
proj4string(ChagosEEZ) <- CRS("+proj=longlat +ellps=WGS84")

ChagosBanks <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Mapping/Chagos_v6.shp")
proj4string(ChagosBanks) <- CRS("+proj=longlat +ellps=WGS84")

Raster_NA <- raster("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT/NA_MAP.grd")


#get all the files from the fitted values for RAT FREE ISLANDS
#get all the rasters you need for rat-free islands
File_Raster <- list.files("../Graphs_BRT_MAPS/Rasters_BRT_Sp/", full.names = T, recursive=TRUE, pattern = "RFB_BRT_NR")

#stack all the files
Stack_Files <- stack(File_Raster)

#this is for overlaying various raster and do operations. I created a function for avoid 0s in the overlay
Rasters <- overlay(Stack_Files, fun = meanIgnoringna) 
proj4string(Rasters) <- CRS("+proj=longlat +ellps=WGS84") #this is for reprojecting into the CRS
Rasters <- mask(Rasters, ChagosEEZ) #here I mask the the map into the BIOT Shape
Rasters <- crop(Rasters,e_coord) #finally here I crop into the chagos extent
raster_total_crop <- crop(Rasters,e_chagos)

writeRaster(raster_total_crop, "~/Master_CMEE/Chagos/Paper/Raster Stacks/RFB_NR", overwrite = T)


valuesRFBNR <- mean(values(raster_total_crop), na.rm = T)
max(values(raster_total_crop), na.rm = T)
min(values(raster_total_crop), na.rm = T)


my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(raster_total_crop, min), cellStats(raster_total_crop, max))))
color_sequence <- round(seq(0,20,length.out=color_levels+1), 1)

pdf("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/Chagos_RFB_BRT_NR.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

tiff("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/Chagos_RFB_BRT_NR.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

#get all the files from the fitted values for RAT INFESTED ISLANDS
#get all the rasters you need for rat-free islands
File_Raster <- list.files("../Graphs_BRT_MAPS/Rasters_BRT_Sp/", full.names = T, recursive=TRUE, pattern = "RFB_BRT_R")

#stack all the files
Stack_Files <- stack(File_Raster)

#this is for overlaying various raster and do operations. I created a function for avoid 0s in the overlay
Rasters <- overlay(Stack_Files, fun = meanIgnoringna) 
proj4string(Rasters) <- CRS("+proj=longlat +ellps=WGS84") #this is for reprojecting into the CRS
Rasters <- mask(Rasters, ChagosEEZ) #here I mask the the map into the BIOT Shape
Rasters <- crop(Rasters,e_coord) #finally here I crop into the chagos extent
raster_total_crop <- crop(Rasters,e_chagos)

writeRaster(raster_total_crop, "~/Master_CMEE/Chagos/Paper/Raster Stacks/RFB_R", overwrite = T)


valuesRFBR <- mean(values(raster_total_crop), na.rm = T)
max(values(raster_total_crop), na.rm = T)
min(values(raster_total_crop), na.rm = T)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 10
max_absolute_value <- max(abs(c(cellStats(raster_total_crop, min), cellStats(raster_total_crop, max))))
color_sequence <- round(seq(0,20,length.out=color_levels+1), 1) #similar values as other response

pdf("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/Chagos_RFB_BRT_R.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

tiff("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/Chagos_RFB_BRT_R.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

######### Difference

#get all the files from the fitted values for RAT FREE ISLANDS
#get all the rasters you need for rat-free islands
File_Raster <- list.files("../Graphs_BRT_MAPS/Rasters_BRT_Sp/", full.names = T, recursive=TRUE, pattern = "RFB_BRT_Dif")

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
color_sequence <- round(seq(-6,6,length.out=color_levels), 1)

pdf("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/Chagos_RFB_BRT_Dif.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks= color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


tiff("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/Chagos_RFB_BRT_Dif.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5))
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


#get que 95% upper quantile for the hotspot after rat erradication

raster_total_cropq <- raster_total_crop
values(raster_total_cropq) <- as.numeric(values(raster_total_cropq) >= quantile(raster_total_crop, 0.925))

values(raster_total_cropq) > 0
Datatest <- as.numeric(values(raster_total_cropq) >= quantile(raster_total_crop, 0.925))

#values(raster_total_cropq) <- as.numeric(values(raster_total_cropq) >= quantile(raster_total_crop, 0.975)) #upper 95% quantile
#values(raster_total_cropq) <- as.numeric(values(raster_total_cropq) >= quantile(raster_total_crop, 0.875)) #upper 85% quantile


pdf("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/Chagos_RFB_BRT_Dif_Quantile.pdf")
plot(raster_total_cropq,col=c("lightblue", "darkred"),
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5), legend = F)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


tiff("~/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT_SP/Chagos_RFB_BRT_Dif_Quantile.tiff")
plot(raster_total_cropq,col=c("lightblue", "darkred"),
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.5), legend = F)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

