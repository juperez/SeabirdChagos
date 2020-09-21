#this code is for getting the map with all abundances

library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(ggplot2)


setwd("/Users/ASUS/Documents/Master_CMEE/Chagos/")

ChagosMap  <- readOGR(dsn="Chagos/Maps_Project",layer="Chagos_Land") #the land cover
proj4string(ChagosMap) <- CRS("+proj=longlat +ellps=WGS84")

ChagosEEZ <- readOGR(dsn = "Chagos/Maps_Project", layer = "ChagosEEZ_Reprojected") #the EEZ cover
proj4string(ChagosEEZ) <- CRS("+proj=longlat +ellps=WGS84")

ChagosBanks <- readOGR(dsn = "Chagos/Mapping", layer = "Chagos_v6") # the banks cover
proj4string(ChagosBanks) <- CRS("+proj=longlat +ellps=WGS84")





# for ggplot
map <- fortify(ChagosEEZ)
maps2 <- fortify(ChagosBanks)

names(Sulidae)

#Sulidae

dSulidae<- read.csv("Chagos/Variables/Data_Birds_Separate_Final/F_Sulidae_Data.csv", sep = ";")

Graph_data_Sulidae <- subset(dSulidae, dSulidae$Sulidae > 0)

gg_Sulidae <- ggplot()
gg_Sulidae  <- gg_Sulidae  + geom_map(data=map, map=map, aes(x=long, y=lat, map_id=id, group=group),
                                      fill="#ffffff", color="#0e0e0e", size=0.15) + 
  theme_classic() +
  geom_map(data=maps2, map=maps2, aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_point(data=Graph_data_Sulidae, aes(x=LongDec, y=LatDec, size=Sulidae), color="blue") +
  theme(legend.position="bottom", legend.box = "horizontal", aspect.ratio=1/1) +
  xlab("Longitude") + 
  ylab("Latitude")

gg_Sulidae

#Laridae

dLariae<- read.csv("Chagos/Variables/Data_Birds_Separate_Final/F_Laridae_Data.csv", sep = ";")

Graph_data_Laridae <- subset(dLariae, dLariae$Laridae > 0)
nrow(dLariae)
nrow(Graph_data_Laridae)

gg_Laridae <- ggplot()
gg_Laridae  <- gg_Laridae  + geom_map(data=map, map=map, aes(x=long, y=lat, map_id=id, group=group),
                                      fill="#ffffff", color="#0e0e0e", size=0.15) + 
  theme_classic() +
  geom_map(data=maps2, map=maps2, aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_point(data=Graph_data_Laridae, aes(x=LongDec, y=LatDec, size=Laridae), color="blue") +
  theme(legend.position="bottom", legend.box = "horizontal", aspect.ratio=1/1) +
  xlab("Longitude") + 
  ylab("Latitude")

gg_Laridae


#Procellaridae

nrow(Graph_data_Procellaridae)

dProcellaridae <- read.csv("Chagos/Variables/Data_Birds_Separate_Final/F_Procellaridae_Data.csv", sep = ";")

Graph_data_Procellaridae <- subset(dProcellaridae, dProcellaridae$Procellaridae > 0)

gg_Procellaridae <- ggplot()
gg_Procellaridae  <- gg_Procellaridae  + geom_map(data=map, map=map, aes(x=long, y=lat, map_id=id, group=group),
                                      fill="#ffffff", color="#0e0e0e", size=0.15) + 
  theme_classic() +
  geom_map(data=maps2, map=maps2, aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_point(data=Graph_data_Procellaridae, aes(x=LongDec, y=LatDec, size=Procellaridae), color="blue") +
  theme(legend.position="bottom", legend.box = "horizontal", aspect.ratio=1/1) +
  xlab("Longitude") + 
  ylab("Latitude")


gg_Procellaridae


#Phaethontidae

dPhaethontidae <- read.csv("Chagos/Variables/Data_Birds_Separate_Final/F_Phaethontidae_Data.csv", sep = ";")

nrow(Graph_data_Phaethontidae)
Graph_data_Phaethontidae <- subset(dPhaethontidae, dPhaethontidae$Phaethontidae > 0)

gg_Phaethontidae <- ggplot()
gg_Phaethontidae  <- gg_Phaethontidae  + geom_map(data=map, map=map, aes(x=long, y=lat, map_id=id, group=group),
                                                  fill="#ffffff", color="#0e0e0e", size=0.15) + 
  theme_classic() +
  geom_map(data=maps2, map=maps2, aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_point(data=Graph_data_Phaethontidae, aes(x=LongDec, y=LatDec, size=Phaethontidae), color="blue") +
  theme(legend.position="bottom", legend.box = "horizontal", aspect.ratio=1/1) +
  xlab("Longitude") + 
  ylab("Latitude")


gg_Phaethontidae


#Fregatidae

dFregatidae <- read.csv("Chagos/Variables/Data_Birds_Separate_Final/F_Fregatidae_Data.csv", sep = ";")

Graph_data_Fregatidae <- subset(dFregatidae, dFregatidae$Fregatidae > 0)
nrow(Graph_data_Fregatidae)
gg_Fregatidae <- ggplot()
gg_Fregatidae  <- gg_Fregatidae  + geom_map(data=map, map=map, aes(x=long, y=lat, map_id=id, group=group),
                                                  fill="#ffffff", color="#0e0e0e", size=0.15) + 
  theme_classic() +
  geom_map(data=maps2, map=maps2, aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_point(data=Graph_data_Fregatidae, aes(x=LongDec, y=LatDec, size=Fregatidae), color="blue") +
  theme(legend.position="bottom", legend.box = "horizontal", aspect.ratio=1/1) +
  xlab("Longitude") + 
  ylab("Latitude")


gg_Fregatidae

#Hydrobatidae

dHydrobatidae <- read.csv("Chagos/Variables/Data_Birds_Separate_Final/F_Hydrobatidae_Data.csv", sep = ";")

Graph_data_Hydrobatidae <- subset(dHydrobatidae, dHydrobatidae$Hydrobatidae > 0)

gg_Hydrobatidae <- ggplot()
gg_Hydrobatidae  <- gg_Hydrobatidae  + geom_map(data=map, map=map, aes(x=long, y=lat, map_id=id, group=group),
                                            fill="#ffffff", color="#0e0e0e", size=0.15) + 
  theme_classic() +
  geom_map(data=maps2, map=maps2, aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_point(data=Graph_data_Hydrobatidae, aes(x=LongDec, y=LatDec, size=Hydrobatidae), color="blue") +
  theme(legend.position="bottom", legend.box = "horizontal", aspect.ratio=1/1) +
  xlab("Longitude") + 
  ylab("Latitude")


gg_Hydrobatidae



#Oceanitidae

dOceanitidae <- read.csv("Chagos/Variables/Data_Birds_Separate_Final/F_Oceanitidae_Data.csv", sep = ";")

Graph_data_Oceanitidae <- subset(dOceanitidae, dOceanitidae$Oceanitidae > 0)

gg_Oceanitidae <- ggplot()
gg_Oceanitidae  <- gg_Oceanitidae  + geom_map(data=map, map=map, aes(x=long, y=lat, map_id=id, group=group),
                                                fill="#ffffff", color="#0e0e0e", size=0.15) + 
  theme_classic() +
  geom_map(data=maps2, map=maps2, aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_point(data=Graph_data_Oceanitidae, aes(x=LongDec, y=LatDec, size=Oceanitidae), color="blue") +
  theme(legend.position="bottom", legend.box = "horizontal", aspect.ratio=1/1) +
  xlab("Longitude") + 
  ylab("Latitude")


gg_Oceanitidae



plots <- c("gg_Sulidae","gg_Laridae", 
           "gg_Procellaridae", "gg_Phaethontidae" , 
           "gg_Fregatidae", "gg_Hydrobatidae", "gg_Oceanitidae")

library(grid)
library(gridExtra)
install.packages("ggpubr")
library(ggpubr)

ggarrange(gg_Sulidae,gg_Laridae, 
          gg_Procellaridae, gg_Phaethontidae , 
          gg_Fregatidae, gg_Hydrobatidae, gg_Oceanitidae,
          ncol = 2, nrow = 4)

the_plots <- list(gg_Sulidae,gg_Laridae, 
                  gg_Procellaridae, gg_Phaethontidae , 
                  gg_Fregatidae, gg_Hydrobatidae, gg_Oceanitidae)

the_graphs <- mget(plots)

invisible(mapply(ggsave, file=paste0("Chagos/Paper/Final_Graphs/Plot-", names(the_graphs), ".pdf"), plot=the_graphs))

invisible(mapply(ggsave, file=paste0("Chagos/Paper/Final_Graphs/Plot-", names(the_graphs), ".tiff"), plot=the_graphs))

