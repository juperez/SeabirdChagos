
# This BRT model is for all the species with distance to coast. 

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
library(tidyr)
library(stats)


############ FIRST PART IS FOR MODELLING BRt #############################


#graph per year

Famdata <- read.csv("Paper/Data_Birds_Separate_Final/Seabirds_Sp_Fam.csv")

Famdata <- subset(Famdata, Famdata$Bird_Recording_Activity == "Transect")

FamdataYear <- table(Famdata$Year)*0.3

Values <- data.frame(Famdata$Sulidae, Famdata$Laridae, Famdata$Procellaridae,
                     Famdata$Oceanitidae, Famdata$Hydrobatidae, Famdata$Phaethontidae,
                     Famdata$Fregatidae, Famdata$Year)

Sulidae <- tapply(Famdata$Sulidae, Famdata$Year, FUN=sum) 
Laridae <- tapply(Famdata$Laridae, Famdata$Year, FUN=sum) 
Procellaridae <- tapply(Famdata$Procellaridae, Famdata$Year, FUN=sum) 
Oceanitidae <- tapply(Famdata$Oceanitidae, Famdata$Year, FUN=sum)
Hydrobatidae <- tapply(Famdata$Hydrobatidae, Famdata$Year, FUN=sum)
Phaethontidae <- tapply(Famdata$Phaethontidae, Famdata$Year, FUN=sum)
Fregatidae <- tapply(Famdata$Fregatidae, Famdata$Year, FUN=sum)


Total <- data.frame(Sulidae, Laridae, Procellaridae, Oceanitidae, 
                    Hydrobatidae, Phaethontidae, Fregatidae)

TotalCountY <- rowSums(Total)

Total$Year <- c(2012:2016)

Total$km <- FamdataYear

Total$Yearkm <- paste(Total$Year,":", Total$km, " km")

Total_long <- gather(Total, Family, Count, Sulidae, Laridae, Procellaridae, Oceanitidae, 
                     Hydrobatidae, Phaethontidae, Fregatidae)

Total_long$Order <- rep(1:5, 7)

#aggregate counts per family 
totals <- aggregate(Count ~ Family, Total_long, sum)




StackYear <- 
  ggplot(data=Total_long) + geom_bar(stat="identity") +
  aes(x=reorder(Family, -Count, sum), y= Count, 
      label = Count, fill = as.factor(Yearkm)) +
  theme_classic()+
  geom_text(aes(Family, Count, label = Count, fill = NULL), 
            vjust = -0.5, data = totals, size = 5) +
  theme(legend.justification=c(0,1), legend.position=c(.7,.9), 
        legend.text=element_text(size=15), legend.title = element_text(size = 20),
        axis.text.x = element_text(face="bold", size = 13, color = "black"),
        axis.text.y = element_text(face="bold", size = 16, color = "black", angle = 90, hjust = 0.5),
        axis.title.x=element_text(size=18,face="bold"),
        axis.title.y=element_text(size=18,face="bold")) +
  labs(x = "Seabirds Families", y = "Number of Individuals") +
  scale_fill_manual(values = c("#440154FF", "#404688FF", "#287C8EFF","#27AD81FF",
                               "#8FD744FF", "#FDE725FF"),name= "Year") +
  ylim(0, 5000)

StackYear

ggsave("Paper/Final_Graphs/StackYearFam.tiff", StackYear, dpi = 300, width = 10, height = 5)
ggsave("Paper/Final_Graphs/StackYearFam.pdf", StackYear, dpi = 300, width = 10, height = 5)



#graph per specie

#########stackplot for family

Values <- data.frame(model.data$Red_footed_Booby, model.data$Brown_Noddy, model.data$White_Tern,
                     model.data$Wedge_Tailed_Shearwater, model.data$Year)
names(Values) <- c("Red_footed_Booby", "Brown_Noddy", "White_Tern", "Wedge_Tailed_Shearwater", "Year")


Red_footed_Booby <- tapply(Values$Red_footed_Booby, Values$Year, FUN=sum) 
Brown_Noddy <- tapply(Values$Brown_Noddy, Values$Year, FUN=sum) 
White_Tern <- tapply(Values$White_Tern, Values$Year, FUN=sum) 
Wedge_Tailed_Shearwater <- tapply(Values$Wedge_Tailed_Shearwater, Values$Year, FUN=sum) 


ValuesStack <- data.frame(Red_footed_Booby, Brown_Noddy, White_Tern, Wedge_Tailed_Shearwater)


ValuesStack$Year <- c(2012:2016)


data_long <- gather(ValuesStack, Specie, Count, Red_footed_Booby, 
                    Brown_Noddy, White_Tern, Wedge_Tailed_Shearwater)

data_long$Order <- c(rep(1,5), rep(2,5), rep(3,5), rep(4,5))

YearCount <- table(Values$Year)

Y2012 <- subset(data_long, data_long$Year == 2012)
Y2012$Prop <- round(Y2012$Count/YearCount[1], digits = 1)

Y2013 <- subset(data_long, data_long$Year == 2013)
Y2013$Prop <- round(Y2013$Count/YearCount[2], digits = 1)

Y2014 <- subset(data_long, data_long$Year == 2014)
Y2014$Prop <- round(Y2014$Count/YearCount[3], digits = 1)

Y2015 <- subset(data_long, data_long$Year == 2015)
Y2015$Prop <- round(Y2015$Count/YearCount[4], digits = 1)

Y2016 <- subset(data_long, data_long$Year == 2016)
Y2016$Prop <- round(Y2016$Count/YearCount[5], digits = 1)



data_long <- rbind(Y2012, Y2013, Y2014, Y2015, Y2016)



stacksp <- 
  ggplot(data=data_long, aes(x=Year, y= Prop, fill=as.factor(Order))) +
  geom_bar(stat="identity", position= position_dodge())+
  geom_text(aes(label=Prop), vjust=-0.5, color="black",
            position = position_dodge(0.86), size=5)+
  scale_x_continuous("Year", breaks = 2012:2016) +
  theme_classic()+
  theme(legend.justification=c(0,1), legend.position=c(.6,.9), 
        legend.text=element_text(size=15), legend.title = element_text(size = 20),
        axis.text.x = element_text(face="bold", size = 16, color = "black"),
        axis.text.y = element_text(face="bold", size = 16, color = "black", angle = 90),
        axis.title.x=element_text(size=18,face="bold"),
        axis.title.y=element_text(size=18,face="bold")) +
  scale_fill_manual(values = c("#C59900", "orangered4", "rosybrown4", "#95A900"),name= "Species", 
                    labels = c("Red-footed booby", "Brown noddy", "White tern", "Wedge-tailed shearwater")) + 
  ylab("Individuals per sample")

stacksp



ggsave("Chagos/Paper/Final_Graphs/StackSpecies.tiff", stacksp, dpi = 300, width = 10, height = 5)
ggsave("Chagos/Paper/Final_Graphs/StackSpecies.pdf", stacksp, dpi = 300, width = 10, height = 5)


##############################################################
#################### BRT MODELS
##############################################################

model.data <- read.csv("Chagos/Paper/Data_Birds_Separate_Final/Species_Data.csv")

model.data <- subset(model.data, model.data$Bird_Recording_Activity == "Transect")
model.data <- subset(model.data, model.data$Year != 2017)


######################################################################################################### 
##################### Red-footed Booby
######################################################################################################### 

# 
# For brown noddy, distance to coast is same as distance to colony as they might be correlated

names(model.data)


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


Names <- c("Year", "Bath", "Slope", "Chl_Climonth", "SST_Climonth", "NPP_Climonth", "SLA_Climonth", 
           "Dist_Coast", "Area_Island", "ONI", "DMI"  )
pairs(model.data[, c(13, 19, 20, 21, 22, 23, 24,25, 28, 36, 37)],
      lower.panel = panel.cor,
      cex.labels=1.3,
      labels=Names, main = "Correlation of Variables")


names(model.data[,c(13, 19, 20, 21, 22, 23, 24,25, 28, 36, 37)])


#Model with all variables included ONI and DMI

Model_com <- c()  
Com_factors <- combn(c(1, 2,   3,  4, 5,  6,   7, 8,  9, 10, 11), m = 5)
Com_factors <- combn(c(13, 19, 20, 21, 22, 23, 24, 25, 36, 37), m = 6) #this is to get the number of the combinations
formulas <- c()
name_var <- c("Year", "Bath", "Slope", "Chl_Climonth", "SST_Climonth", "NPP_Climonth", "SLA_Climonth", 
              "Dist_Coast", "Area_Island", "ONI", "DMI"  )

ncol(Com_factors)
names(model.data)

#correlations: Year 3 vs SLA 24, Year 3 vs ONI 36, Year 3 vs DMI 37, 
              #Bath 19 vs DC 25, CHL 21 vs NPP 23, ONI 36 vs DMI 37


library(plyr)
# j <- 1
# 
# Com_factors2 <- c()
# 
# #lets avoid highly correlated variables
# 
# for(j in 1:ncol(Com_factors)){
#   
#   
#   variables <- Com_factors[,j]
#   
#   the13 <- '13' %in% variables
#   the24 <- '24' %in% variables
#   the36 <- '36' %in% variables
#   the37 <- '37' %in% variables
#   the19 <- '19' %in% variables
#   the25 <- '25' %in% variables
#   the21 <- '21' %in% variables
#   the23 <- '23' %in% variables
#  
# 
#   if(the13 == TRUE & the24 == TRUE) {
#   next
#   }
#   if(the13 == TRUE & the36 == TRUE) {
#     next
#   }
#   if(the13 == TRUE & the37 == TRUE) {
#     next
#   }
#   if(the19 == TRUE & the25 == TRUE) {
#     next
#   }
#   if(the21 == TRUE & the23 == TRUE) {
#     next
#   }
#   if(the36 == TRUE & the37 == TRUE) {
#     next
#   }
# 
# Com_factors2 <- cbind(Com_factors2, Com_factors[,j])
#   
# }
# 
# 
# 
# #, "Year"
# 
# all_var <- c()
# 
# all_aic <- c()
# 
# estats_models <- c()
# 
# i <- 1
# 
# for(i in 1:ncol(Com_factors2)){
# 
# Model_RFB<- gbm.step(data = model.data, 
#                           gbm.x = Com_factors2[, i], #this is for the covariates
#                           gbm.y = 3, #this is the column of the responae
#                           family = "poisson", #this is the family, in the case of the example is bernoulli as it is P/A data
#                           tree.complexity = 4, 
#                           prev.stratify = FALSE,
#                           n.trees = 1000,
#                           learning.rate = 0.0001,
#                           bag.fraction = 0.5,
#                           #step.size = 25,
#                           max.trees = 1000000,
#                           n.folds = 10,
#                           plot.main = TRUE)
# 
# nul_dev <- Model_RFB$self.statistics$mean.null #null deviance
# res_dev <- Model_RFB$cv.statistics$deviance.mean #residual deviance
# TDE <- Model_RFB$cv.statistics$deviance.mean/Model_RFB$self.statistics$mean.null #percentage of deviance explained
# cv <- Model_RFB$cv.statistics$correlation.mean #correlation coefficient cv
# 
# models_results <- data.frame(nul_dev, res_dev, TDE, cv)
# 
# estats_models <- rbind(estats_models, models_results)
# 
# print(paste("Lopp num: ", i))
#   
# }


#estats_models

# length(estats_models$nul_dev)
# Com_factors2[, 2]
# names(model.data[,c(13, 19, 20, 21, 22, 37)])
# 
# Model_RFBFull <- gbm.step(data = model.data, 
#                      gbm.x = c(13, 19, 20, 21, 22, 23, 24, 25, 36, 37), #this is for the covariates
#                      gbm.y = 3, #this is the column of the responae
#                      family = "poisson", #this is the family, in the case of the example is bernoulli as it is P/A data
#                      tree.complexity = 4, 
#                      prev.stratify = FALSE,
#                      n.trees = 1000,
#                      learning.rate = 0.0001,
#                      bag.fraction = 0.55,
#                      #step.size = 25,
#                      max.trees = 1000000,
#                      n.folds = 10,
#                      plot.main = TRUE)

#Model_RFBFull$contributions


gbm.plot(Model_RFBFull, plot.layout = c(5,2), write.title = F, smooth = TRUE, common.scale = F)

names(model.data)

Model_RFB$contributions
#Model Simplification and improvement

#after all tries, I decided to go explaining with a better bag fraction in 0.5

Model_RFBFull <- gbm.step(data = model.data, 
                      gbm.x = c(20, 21, 22, 23, 24, 25, 36, 37), #this is for the covariates
                      gbm.y = 3, #this is the column of the responae
                      family = "poisson", #this is the family, in the case of the example is bernoulli as it is P/A data
                      tree.complexity = 4, 
                      prev.stratify = FALSE,
                      n.trees = 1000,
                      learning.rate = 0.0001,
                      bag.fraction = 0.55,
                      #step.size = 25,
                      max.trees = 1000000,
                      n.folds = 10,
                      plot.main = TRUE)

gbm.plot(Model_RFBFull, plot.layout = c(3,3), write.title = F, smooth = TRUE, common.scale = F)

names(model.data)

Model_RFB2 <- gbm.step(data = model.data, 
                     gbm.x = c(20, 21, 22, 24, 25, 37), #this is for the covariates
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



gbm.plot(Model_RFB2, plot.layout = c(3,2), write.title = F, smooth = TRUE, common.scale = F)


Model_RFB2$contributions

#return interactions between variables
interactionsRFB_Rats <- gbm.interactions(Model_RFB)

interactionsRFB_Rats$rank.list
#deviance rat presence

gbm.plot(Model_RFB2, plot.layout = c(3,2), write.title = F, smooth = TRUE, common.scale = F)

RFB_Cont <- Model_RFB$contributions



#### BROWN NODDY

names(model.data)

Model_BrownNoddy <- gbm.step(data = model.data, 
                         gbm.x = c(20, 21, 22, 23, 24, 25, 36, 37), #this is for the covariates
                         gbm.y = 5, #this is the column of the responae
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


#Select Contributions
BN_Cont <- Model_BrownNoddy$contributions

names(model.data)

Model_BrownNoddy2 <- gbm.step(data = model.data, 
                             gbm.x = c(20, 21, 22, 24, 25), #this is for the covariates
                             gbm.y = 5, #this is the column of the responae
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


Model_BrownNoddy2$cv.statistics
Model_BrownNoddy2$contributions
#return interactions between variables
interactionsBrownNoddy <- gbm.interactions(Model_BrownNoddy)

interactionsBrownNoddy$rank.list
#deviance rat presence

gbm.plot(Model_BrownNoddy, plot.layout = c(3,3), write.title = F, smooth = TRUE, common.scale = F)
gbm.plot(Model_BrownNoddy2, plot.layout = c(3,2), write.title = F, smooth = TRUE, common.scale = F)



BN_Cont <- Model_BrownNoddy$contributions

Model_BrownNoddy$cv.statistics
Model_BrownNoddy2$cv.statistics


#Wedgetailed Shearwater


Model_WTShearwater <- gbm.step(data = model.data, 
                              gbm.x = c(20, 21, 22, 23, 24, 25, 36, 37), #this is for the covariates
                              gbm.y = 2, #this is the column of the responae
                              family = "poisson", #this is the family, in the case of the example is bernoulli as it is P/A data
                              tree.complexity = 4, 
                              prev.stratify = FALSE,
                              n.trees = 1000,
                              learning.rate = 0.0001,
                              bag.fraction = 0.3,
                              #step.size = 25,
                              max.trees = 1000000,
                              n.folds = 10,
                              plot.main = TRUE)


WTS_Cont <- Model_WTShearwater$contributions
names(model.data[,c(19, 20, 21, 22, 24,25, 37)])
names(model.data)
Model_WTShearwater$cv.statistics

Model_WTShearwater2 <- gbm.step(data = model.data, 
                              gbm.x = c(20, 21, 22, 25, 37), #this is for the covariates
                              gbm.y = 2, #this is the column of the responae
                              family = "poisson", #this is the family, in the case of the example is bernoulli as it is P/A data
                              tree.complexity = 1, 
                              prev.stratify = FALSE,
                              n.trees = 1000,
                              learning.rate = 0.0001,
                              bag.fraction = 0.3,
                              #step.size = 25,
                              max.trees = 100000,
                              n.folds = 10,
                              plot.main = TRUE)

Model_WTShearwater2$contributions

Model_WTShearwater2$cv.statistics$deviance.mean

Model_WTShearwater2$self.statistics$mean.null

#return interactions between variables
interactionsWTShearwater<- gbm.interactions(Model_WTShearwater)

interactionsWTShearwater$rank.list
#deviance rat presence

gbm.plot(Model_WTShearwater, plot.layout = c(3,3), write.title = F, smooth = TRUE, common.scale = F)
gbm.plot(Model_WTShearwater2, plot.layout = c(3,2), write.title = F, smooth = TRUE, common.scale = F)

WTS_Cont <- Model_WTShearwater$contributions
WTS_Cont <- Model_WTShearwater2$contributions


#White Tern

names(model.data[,c(13, 20, 21, 22, 24,25,26, 36, 37)])

Model_WTern <- gbm.step(data = model.data, 
                            gbm.x = c(20, 21, 22, 24,25, 36, 37), #this is for the covariates
                            gbm.y = 4, #this is the column of the responae
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

WTern_Cont <- Model_WTern$contributions



Model_WTern2 <- gbm.step(data = model.data, 
                        gbm.x = c(20, 21, 22, 24, 25), #this is for the covariates
                        gbm.y = 4, #this is the column of the responae
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

WTern_Cont2 <- Model_WTern2$contributions


#return interactions between variables
interactionsWTern<- gbm.interactions(Model_WTern)

interactionsWTern$rank.list
#deviance rat presence


gbm.plot(Model_WTern2, plot.layout = c(3,3), write.title = F, smooth = TRUE, common.scale = F)

WTern_Cont <- Model_WTern$contributions


#saving models separately

save(Model_RFB2, file = "Chagos/Paper/RData_BRTModels/SpeciesBRT_RFB.rda")

save(Model_BrownNoddy2, file = "Chagos/Paper/RData_BRTModels/SpeciesBRT_BN.rda")

save(Model_WTern2, file = "Chagos/Paper/RData_BRTModels/SpeciesBRT_WTern.rda")

save(Model_WTShearwater2, file = "Chagos/Paper/RData_BRTModels/SpeciesBRT_WTS.rda")

