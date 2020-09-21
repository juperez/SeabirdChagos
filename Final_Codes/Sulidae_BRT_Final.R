###this is Boosted Regression Trees with the final models
# using presence of rats and distance to coast
# groupng guilds and not by species
# 

# for Boobies, and all species that has rats in either islands with and without rats, as Distance to Coast
# and Distane to Colonies with rats are correlated, we are going to use the approach of modelling with P/A of rats
#


#sudo apt-get install r-cran-segmented used in the terminal to install packages directly
rm(list = ls())

setwd("~/Master_CMEE/")


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


###################################################################################################

######################### BOOBIES MODEL

###################################################################################################


model.data <- read.csv("Chagos/Variables/Data_Birds_Separate_Final/F_Sulidae_Data.csv")
areas <- read.table("Chagos/Variables/Data_Birds_Separate_Final/Areas.txt", header = T)
head(areas)
model.data <- cbind(model.data, areas)
 
model.data <- subset(model.data, model.data$Bird_Recording_Activity == "Transect")

head(model.data)
names(model.data)

levels(model.data$Island_R)

Boob_Dist_Coast_R <- model.data[,c(1,15,28,8,37,54)] #Rats - we include area of closes rats island

Boob_Dist_Coast_NR <- model.data[,c(1,15,28,8,40,55)] #No Rats  - we include area of closes rats free island


names(Boob_Dist_Coast_R)
nrow(Boob_Dist_Coast_NR)

#Boob_Dist_Coast_R <- subset(Boob_Dist_Coast_R, Boob_Dist_Coast_R$Dist_Coast_R <100)

#Boob_Dist_Coast_NR <- subset(Boob_Dist_Coast_NR, Boob_Dist_Coast_NR$Dist_Coast_NR <100)




names(Boob_Dist_Coast_R)

Model_Boobies_Rats <- gbm.step(data = Boob_Dist_Coast_R, 
                               gbm.x = 2:6, #this is for the covariates
                               gbm.y = 1, #this is the column of the responae
                               family = "poisson", #this is the family, in the case of the example is bernoulli as it is P/A data
                               tree.complexity = 6, 
                               prev.stratify = FALSE,
                               n.trees = 1000,
                               learning.rate = 0.0001,
                               bag.fraction = .5,
                               max.trees = 1000000,
                               n.folds = 10,
                               plot.main = TRUE)


Model_Boobies_No_Rats <- gbm.step(data = Boob_Dist_Coast_NR, 
                                  gbm.x = 2:6, #this is for the covariates
                                  gbm.y = 1, #this is the column of the responae
                                  family = "poisson", #this is the family, in the case of the example is bernoulli as it is P/A data
                                  tree.complexity = 3, 
                                  prev.stratify = FALSE,
                                  n.trees = 1000,
                                  learning.rate = 0.0001,
                                  bag.fraction = .5,
                                  max.trees = 1000000,
                                  n.folds = 10,
                                  plot.main = TRUE)

#gbm.plot(Model_Boobies_No_Rats, variable.no = 4, n.plots = 1, plot.layout = c(1,1), write.title = F, smooth = TRUE, common.scale = F)
#gbm.plot(Model_Boobies_Rats, variable.no = 4, n.plots = 1, plot.layout = c(1,1), write.title = F, smooth = TRUE, common.scale = F)


################################### checking code from tom
Bob_Rat_Cont <- Model_Boobies_Rats$contributions
write.csv(Bob_Rat_Cont, "Chagos/Paper/Graphs_BRT_Presence/SulidaeNR_Contribution.csv", row.names=FALSE)

Bob_NoRat_Cont <- Model_Boobies_No_Rats$contributions
write.csv(Bob_NoRat_Cont, "Chagos/Paper/Graphs_BRT_Presence/SulidaeR_Contribution.csv", row.names=FALSE)

Boobies_Rats <- plot.gbm(Model_Boobies_Rats,i.var= 4,return.grid=T)
Range_Boobies_Rats <- ((range(Boobies_Rats[,1])[2])+(range(Boobies_Rats[,1])[1]))/2
lin.Boobies_Rats <- lm(y~Dist_Coast_R,data=Boobies_Rats)
seg.Boobies_Rats <- segmented(lin.Boobies_Rats,seg.Z=~Dist_Coast_R,
                            psi=Range_Boobies_Rats)
seg.Boobies_Rats <- segmented.lm(lin.Boobies_Rats,seg.Z=~Dist_Coast_R,
                              psi=NA, control=seg.control(stop.if.error=FALSE,n.boot=0, it.max=20))

seg.Boobies_Rats <- segmented.lm(lin.Boobies_Rats,seg.Z=~Dist_Coast_R,
                                 psi=c(78.0021030,155.4202340), control=seg.control(stop.if.error=FALSE,n.boot=0, it.max=20))

quantile(Boobies_Rats$Dist_Coast_R)

plot(seg.Boobies_Rats)

plot(seg.Boobies_Rats,conf.level=0.95,shade=T,rug=F,
     ylab="Effect on Red Footed Bobby",xlab="Distance to Coast Rats", col = "red",
     main = paste("Break Point =",round(seg.Boobies_Rats$psi[2], digits = 2)))  


# summary(seg.Boobies_Rats)
# slope(seg.Boobies_Rats)
# davies.test(lin.Boobies_Rats, seg.Z=~Dist_Coast_R)
# confint.segmented(seg.Boobies_Rats)

Model_Boobies_No_Rats$contributions
Boobies_No_Rats <- plot.gbm(Model_Boobies_No_Rats,i.var= 4,return.grid=T)
Range_Boobies_No_Rats <- ((range(Boobies_No_Rats[,1])[2])+(range(Boobies_No_Rats[,1])[1]))/2
lin.Boobies_No_Rats <- glm(y~Dist_Coast_NR,data=Boobies_No_Rats)
#seg.Boobies_No_Rats<-segmented(lin.Boobies_No_Rats,seg.Z=~Dist_Coast_NR,psi=Range_Boobies_No_Rats)

seg.Boobies_No_Rats <- segmented.lm(lin.Boobies_No_Rats,seg.Z=~Dist_Coast_NR,
                                 psi=NA, control=seg.control(stop.if.error=FALSE,n.boot=0, it.max=20))

seg.Boobies_No_Rats <- segmented.lm(lin.Boobies_No_Rats,seg.Z=~Dist_Coast_NR,
                                    psi=c(78.2867269,155.8265439), control=seg.control(stop.if.error=FALSE,n.boot=0, it.max=20))

quantile(Boobies_No_Rats$Dist_Coast_NR)
plot(seg.Boobies_No_Rats)

# plot(seg.Boobies_No_Rats,conf.level=0.95,shade=T,rug=F,
#      ylab="Effect on Red Footed Bobby",xlab="Distance to Coast No Rats", col = "red",
#      main = paste("Break Point =",round(seg.Boobies_No_Rats$psi[2], digits = 2))) 
# 
# 
# summary(seg.Boobies_No_Rats)
# slope(seg.Boobies_No_Rats)
# davies.test(lin.Boobies_No_Rats, seg.Z=~Dist_Coast_NR)
# confint.segmented(seg.Boobies_No_Rats)



# for calculateinf the CI interval 1) sum and substract se from the fitted values
augmented_Boobies_rats <- augment(seg.Boobies_Rats)
augmented_Boobies_rats$upperR <- augmented_Boobies_rats$.fitted + augmented_Boobies_rats$.se.fit
augmented_Boobies_rats$lowerR <- augmented_Boobies_rats$.fitted - augmented_Boobies_rats$.se.fit
colnames(augmented_Boobies_rats)[8] <- "fitted_R"

write.csv(augmented_Boobies_rats, "Chagos/Paper/Graphs_BRT_Presence/SulidaeR.csv", row.names=FALSE)

augmented_Boobies_no_rats <- augment(seg.Boobies_No_Rats)
augmented_Boobies_no_rats$upperNR <- augmented_Boobies_no_rats$.fitted + augmented_Boobies_no_rats$.se.fit
augmented_Boobies_no_rats$lowerNR <- augmented_Boobies_no_rats$.fitted - augmented_Boobies_no_rats$.se.fit
colnames(augmented_Boobies_no_rats)[8] <- "fitted_NR"

head(augmented_Boobies_no_rats)
write.csv(augmented_Boobies_no_rats, "Chagos/Paper/Graphs_BRT_Presence/SulidaeNR.csv", row.names=FALSE)



augmented_Boobies_rats <- read.csv("Chagos/Paper/Graphs_BRT_Presence/SulidaeR.csv") 

augmented_Boobies_no_rats <- read.csv("Chagos/Paper/Graphs_BRT_Presence/SulidaeNR.csv")


data_aumengted_Boobies <- data.frame(augmented_Boobies_rats, augmented_Boobies_no_rats)

#create the graphics of interactions with rat-free and rat-infested islands

cols <- c("Infested Islands"="red3","Rat-free Islands"="blue4")

mean(max(data_aumengted_Boobies$upperR),max(data_aumengted_Boobies$lowerR)) / mean(max(data_aumengted_Boobies$upperNR), max(data_aumengted_Boobies$lowerNR))

dat1 <- mean(c(1.77, 1.72))
dat2 <- mean(c(2.29, 2.09))

dat2/dat1
max(data_aumengted_Boobies$upperR) / max(data_aumengted_Boobies$upperNR)
Boobies_Dist_Lines <- ggplot(data_aumengted_Boobies, aes(x=Dist_Coast_NR)) +
  
  #doing first line for no rats islands
  geom_line(aes(y = upperNR), alpha = "0") + 
  geom_line(aes(y = lowerNR), alpha = 0) +
  geom_ribbon(data=data_aumengted_Boobies, 
              aes(ymin=lowerNR,ymax=upperNR), fill="blue4", alpha="1") +
  geom_line(aes(y = fitted_NR, color = "Rat-free Islands")) +
  
  #Second line for rats islands
  geom_line(aes(y = upperR), alpha = "0") + 
  geom_line(aes(y = lowerR), alpha = 0) +
  geom_ribbon(data=data_aumengted_Boobies, 
              aes(ymin=lowerR,ymax=upperR), fill="red3", alpha="1") +
  geom_line(aes(y = fitted_R,  color = "Infested Islands")) +
  
  #including a vertical lines for rat-free breaking points
  geom_segment(aes(x = 16.51 , y = 1, xend = 16.51, yend = 2.04),
               color = "blue4", linetype="dashed") +
  annotate("text", x= 16, y=2.2, label= "16.51 km", angle = 90,
           size = 5, fontface = 'italic') + 
  
  # Including a vertical line for rat-infested breaking point
  geom_segment(aes(x = 60.64 , y = 1, xend = 60.64, yend = 2.04),
               color = "red3", linetype="dashed") +
  annotate("text", x=60.64, y=2.2, label= "60.64 km", angle = 90,
           size = 5, fontface = c('italic')) + 
  
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
  scale_y_continuous(expand = c(0, 0), limits=c(1,2.4)) +
  scale_x_continuous(expand = c(0, 0), limits=c(0,350)) +
  scale_colour_manual(name="Distance to",values=cols, guide = guide_legend(fill = NULL,colour = NULL))


Boobies_Dist_Lines

ggsave("Chagos/Paper/Graphs_BRT_Presence/Sulidae_Dist_Lines.pdf", Boobies_Dist_Lines)  

ggsave("Chagos/Paper/Graphs_BRT_Presence/Sulidae_Dist_Lines.tiff", Boobies_Dist_Lines)  



#create graphics that showd CI of distance to coast to rat free and rat infested islands 

CI_Boobies_Rats <- data.frame(confint.segmented(seg.Boobies_Rats))
CI_Boobies_Rats$Num <- 1:length(CI_Boobies_Rats[,1])
Y_Rats <- c(as.integer(0.0),as.integer(0.0),as.integer(0.0),as.integer(0.0))
CI_Boobies_Rats <- rbind(CI_Boobies_Rats,Y_Rats)
CI_Boobies_Rats$Status <- rep("Infested Islands", nrow(CI_Boobies_Rats))
colnames(CI_Boobies_Rats) <- c("Estimated", "Lower", "Upper", "Num","Status")


CI_Boobies_No_Rats <- data.frame(confint.segmented(seg.Boobies_No_Rats))
CI_Boobies_No_Rats$Num <- 1:length(CI_Boobies_No_Rats[,1])
N_Rats <- c(as.integer(0.0),as.integer(0.0),as.integer(0.0),as.integer(0.0))
CI_Boobies_No_Rats <- rbind(CI_Boobies_No_Rats,N_Rats)
CI_Boobies_No_Rats$Status <- rep("Rat-free Islands", nrow(CI_Boobies_No_Rats))
colnames(CI_Boobies_No_Rats) <- c("Estimated", "Lower", "Upper", "Num","Status")

Set_CI <- rbind(CI_Boobies_Rats, CI_Boobies_No_Rats)


write.csv(Set_CI, "Chagos/Paper/Graphs_BRT_Presence/SulidaeCI.csv", row.names=FALSE)


Set_CI <- read.csv("Chagos/Paper/Graphs_BRT_Presence/SulidaeCI.csv")

pd <- position_dodge(0.1)


Sulidae_CI_Dist <- ggplot(Set_CI, aes(x=Num, y=Estimated, group=Status, color=Status)) + 
  geom_line() +
  geom_point()+
  scale_y_continuous(expand = c(0, 0), limits=c(0,350))+
  theme_classic()+ theme(legend.justification=c(1,0),
                         legend.position=c(0.6,0.7)) +
  scale_color_manual(values=c('#FF0000','#00008B'))+
  geom_hline(yintercept=108, color = "black", size=1)+
  geom_errorbar(aes(ymin=Lower, ymax=Upper), colour="black", width=.1)

ggsave("/Chagos/Paper/Graphs_BRT_Presence/Sulidae_CI_Dist.pdf", Sulidae_CI_Dist)
ggsave("/Chagos/Paper/Graphs_BRT_Presence/Sulidae_CI_Dist.tiff", Sulidae_CI_Dist)



###########################################################
################ LETS DO SOME PREDICTIONS WITH BRT! #######
###########################################################


################# PREDICTIONS WITHOUT RATS

rasterextend <- raster("/Chagos/Variables/Chlorophyll/Clima-Month/Rasters/A20023052016335.L3m_MC_CHL_chlor_a_4km.grd")

setwd("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Covariates/")

get_list_pred <- list.files(pattern=".csv")
Dist_Data <- read.csv("/Chagos/Variables/New_Variables/Dist_Coast_YN_Rats.csv", sep = ";")

names(Dist_Data)

names(Dist_Data)



#rep this on the different ranges of covariates
rangeSlope <- range(model.data$Slope)
rangeSST <- range(model.data$SST_Climonth)
rangeSLA <- range(model.data$SLA_Climonth)

#file <- get_list_pred[1]

Model_Boobies_No_Rats$contributions

file <- get_list_pred[1]

for(file in get_list_pred){
  
  Predictors <- read.csv(file)
  
  Predictors <- data.frame(Predictors$LongDec, Predictors$LatDec, 
                           Predictors$Slope, Predictors$SST_Climonth, 
                           Predictors$SLA_Climonth, Dist_Data$Dist_Coast, 
                           Dist_Data$Dist_Coast, Dist_Data$Area,
                           Dist_Data$Area, Dist_Data$Rats) #now is different, run the effects not considering rats infestation
  names(Predictors) <- c("LongDec", "LatDec",
                         "Slope", "SST_Climonth", 
                         "SLA_Climonth", "Dist_Coast_R", 
                         "Dist_Coast_NR",  
                         "Area_R","Area_NR", "Rats")
  
  Predictors <- subset(Predictors, 
                       Predictors$Slope > rangeSlope[1] & Predictors$Slope < rangeSlope[2])
  names(Predictors) <- c("LongDec", "LatDec",
                         "Slope", "SST_Climonth", 
                         "SLA_Climonth", "Dist_Coast_R", 
                         "Dist_Coast_NR",  
                         "Area_R","Area_NR", "Rats")
  
  Predictors <- subset(Predictors, 
                       Predictors$SLA_Climonth > rangeSLA[1] & Predictors$SLA_Climonth < rangeSLA[2])
  names(Predictors) <- c("LongDec", "LatDec",
                         "Slope", "SST_Climonth", 
                         "SLA_Climonth", "Dist_Coast_R", 
                         "Dist_Coast_NR",  
                         "Area_R","Area_NR", "Rats")
  Predictors$Year <- rep(2017,nrow(Predictors))
  Values_predNR <- predict.gbm(Model_Boobies_No_Rats, Predictors, 
                               n.trees=Model_Boobies_No_Rats$gbm.call$best.trees, 
                               type="response")
  
  Values_predR <- predict.gbm(Model_Boobies_Rats, Predictors, 
                              n.trees=Model_Boobies_Rats$gbm.call$best.trees, 
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
  
  
  #geting maps of fitted values for rat free islandas
  Values_fit_NR <- data.frame(New_ValuesNR$LongDec, New_ValuesNR$LatDec, New_ValuesNR$Predictions)
  
  New_Raster_Val <- rasterFromXYZ(Values_fit_NR)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84")
  writeRaster(ras2_resized,paste("../Graphs_BRT_MAPS/Rasters_BRT_Family/Sulidae_NR_Fitted", 
                                 file_path_sans_ext(file),sep = ""), overwrite = T)
  
  #geting maps of fitted values for rat free islandas
  Values_fit_R <- data.frame(New_ValuesR$LongDec, New_ValuesR$LatDec, New_ValuesR$Predictions)
  New_Raster_Val <- rasterFromXYZ(Values_fit_R)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  writeRaster(ras2_resized,paste("../Graphs_BRT_MAPS/Rasters_BRT_Family/Sulidae_BRT_R_Fitted", 
                                 file_path_sans_ext(file),sep = ""), overwrite = T)
  
  
  #geting maps of fitted values
  Values_fit <- data.frame(New_Values$LongDec, New_Values$LatDec, New_Values$Predictions)
  New_Raster_Val <- rasterFromXYZ(Values_fit)
  ras2_resized <- resample(New_Raster_Val, rasterextend)
  proj4string(ras2_resized) <- CRS("+proj=longlat +ellps=WGS84") 
  writeRaster(ras2_resized,paste("../Graphs_BRT_MAPS/Rasters_BRT_Family/Sulidae_BRT_Dif_Fitted", 
                                 file_path_sans_ext(file),sep = ""), overwrite = T)
  
}



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
ChagosMap <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Maps_Project/Chagos_Land.shp")
proj4string(ChagosMap) <- CRS("+proj=longlat +ellps=WGS84")

ChagosEEZ <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Maps_Project/ChagosEEZ_Reprojected.shp")
proj4string(ChagosEEZ) <- CRS("+proj=longlat +ellps=WGS84")

ChagosBanks <- shapefile("/Users/ASUS/Documents/Master_CMEE/Chagos/Mapping/Chagos_v6.shp")
proj4string(ChagosBanks) <- CRS("+proj=longlat +ellps=WGS84")

Raster_NA <- raster("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT/NA_MAP.grd")



#get all the files from the fitted values for RAT FREE ISLANDS
#get all the rasters you need for rat-free islands
File_Raster <- list.files("../Graphs_BRT_MAPS/Rasters_BRT_Family/", full.names = T, recursive=TRUE, pattern = "Sulidae_NR_Fitted")

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
color_sequence <- seq(0,30,length.out=color_levels+1)

pdf("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Sulidae_BRT_NR.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=2))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()

tiff("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Sulidae_BRT_NR.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=2))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()

#get all the files from the fitted values for RAT INFESTED ISLANDS
#get all the rasters you need for rat-free islands
File_Raster <- list.files("../Graphs_BRT_MAPS/Rasters_BRT_Family/", full.names = T, recursive=TRUE, pattern = "Sulidae_BRT_R_Fitted")

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
color_sequence <- seq(0,30,length.out=color_levels+1)

pdf("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Sulidae_BRT_R.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=2))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()

tiff("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Sulidae_BRT_R.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=2))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()

######### Difference

#get all the files from the fitted values for RAT FREE ISLANDS
#get all the rasters you need for rat-free islands
File_Raster <- list.files("../Graphs_BRT_MAPS/Rasters_BRT_Family/", full.names = T, recursive=TRUE, pattern = "Sulidae_BRT_Dif_Fitted")

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
color_sequence <- seq(-15,15,length.out=color_levels+1)

pdf("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Sulidae_BRT_Dif.pdf")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=2))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()

tiff("/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Sulidae_BRT_Dif.tiff")
plot(raster_total_crop, col = my_palette(n=color_levels),
     breaks=color_sequence,
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=2))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = .8, legend = F, add = T)
dev.off()

#get que 95% upper quantile for the hotspot after rat erradication

quantile(raster_total_crop, c(.875))

raster_total_cropq <- raster_total_crop
values(raster_total_cropq) <- as.numeric(values(raster_total_cropq) >= quantile(raster_total_crop, 0.925))

#values(raster_total_cropq) <- as.numeric(values(raster_total_cropq) >= quantile(raster_total_crop, 0.975)) #upper 95% quantile
#values(raster_total_cropq) <- as.numeric(values(raster_total_cropq) >= quantile(raster_total_crop, 0.875)) #upper 85% quantile



pdf("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Sulidae_BRT_Dif_Quantile.pdf")
plot(raster_total_cropq,col=c("lightblue", "darkred"),
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=2), legend = F)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = 0.8, legend = F, add = T)
dev.off()


tiff("/Users/ASUS/Documents/Master_CMEE/Chagos/Paper/Maps/Final_Maps_BRT/Chagos_Sulidae_BRT_Dif_Quantile.tiff")
plot(raster_total_cropq,col=c("lightblue", "darkred"),
     xlab = "Longitude", ylab = "Latitude",
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=2), legend = F)
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
plot(Raster_NA, col = "#ffffff", alpha = 0.8, legend = F, add = T)
dev.off()

