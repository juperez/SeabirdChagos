install.packages("rasterVis")


library(raster)
library(rgdal)
library(grDevices)
library(ggplot2)
library(rasterVis)

my_palette <- colorRampPalette(c("blue", "cyan", "yellow", "red"))(n = 1000)
###This is for the rasters of all covariates

setwd("/Users/ASUS/Documents/Master_CMEE/Chagos/")

#first the extent of a raster sample
rasterextend <- raster("Variables/Chlorophyll/Clima-Month/Rasters/A20023052016335.L3m_MC_CHL_chlor_a_4km.grd")

ChagosMap <- shapefile("Maps_Project/Chagos_Land.shp")
proj4string(ChagosMap) <- CRS("+proj=longlat +ellps=WGS84")

ChagosEEZ <- shapefile("Maps_Project/ChagosEEZ_Reprojected.shp")
proj4string(ChagosEEZ) <- CRS("+proj=longlat +ellps=WGS84")

ChagosBanks <- shapefile("Mapping/Chagos_v6.shp")
proj4string(ChagosBanks) <- CRS("+proj=longlat +ellps=WGS84")

#the extent
e_coord <- extent(67,76, -11,-2) 

# 1) Fix covariates

# Bathymetry
raster_bathimetry <- raster("Maps_Project/GEBCO2014_65.0_-12.0_78.0_-2.0_30Sec_ESRIASCII.asc") #rasterFromXYZ function work for 2D files in order to rasterize
proj4string(raster_bathimetry) <- CRS("+proj=longlat +ellps=WGS84") #get the projection
plot(raster_bathimetry) 

res(raster_bathimetry)
raster_bathimetry <- mask(raster_bathimetry, ChagosEEZ)
raster_bathimetry <- crop(raster_bathimetry,e_coord)


my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 20
max_absolute_value <- max(abs(c(cellStats(raster_bathimetry, min), cellStats(raster_bathimetry, max))))
color_sequence <- seq(0,-6000,length.out=color_levels+1)

pdf("Paper/Final_Graphs/Maps/Bath.pdf")
plot(raster_bathimetry, col = my_palette(n=color_levels),  main = "Depth (m)", xlab = "Longitude", ylab = "Latitude", 
     xlim = c(67.9,76),legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.25))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.9,add = TRUE)
dev.off()

tiff("Paper/Final_Graphs/Maps/Bath.tiff")
plot(raster_bathimetry, col = my_palette(n=color_levels),  main = "Depth (m)", xlab = "Longitude", ylab = "Latitude", 
     xlim = c(67.9,76),legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.25))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.9,add = TRUE)
dev.off()


# Slope

raster_slope <- raster("Maps_Project/Rasters/Slope_ASCII_30sec")
proj4string(raster_slope) <- CRS("+proj=longlat +ellps=WGS84") #get the projection

res(raster_slope)

raster_slope <- mask(raster_slope, ChagosEEZ)
raster_slope <- crop(raster_slope,e_coord)


my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 20
max_absolute_value <- max(abs(c(cellStats(raster_slope, min), cellStats(raster_slope, max))))
color_sequence <- seq(0,100,length.out=color_levels+1)

pdf("Paper/Final_Graphs/Maps/Slope.pdf")
plot(raster_slope, col = my_palette(n=color_levels),  main = "Slope (%)", xlab = "Longitude", ylab = "Latitude",
     xlim = c(67.9,76),legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.25))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


tiff("Paper/Final_Graphs/Maps/Slope.tiff")
plot(raster_slope, col = my_palette(n=color_levels),  main = "Slope (%)", xlab = "Longitude", ylab = "Latitude",
     xlim = c(67.9,76),legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.25))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()


# 2)  Environmental Variables

# Chl

Files_CHL <- list.files(path ="Variables/Chlorophyll/Clima-Month/Rasters/", pattern = ".grd", full.names = T)
CHL_Climonth <- stack(Files_CHL)
CHL_Climonth_mean <- overlay(CHL_Climonth, fun = mean)
CHL_Climonth_mean <- mask(CHL_Climonth_mean, ChagosEEZ)
CHL_Climonth_mean <- crop(CHL_Climonth_mean,e_coord)
proj4string(CHL_Climonth_mean) <- CRS("+proj=longlat +ellps=WGS84") #get the projection

res(CHL_Climonth)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 20
max_absolute_value <- max(abs(c(cellStats(CHL_Climonth_mean, min), cellStats(CHL_Climonth_mean, max))))
color_sequence <- seq(0,0.5,length.out=color_levels+1)

pdf("Paper/Final_Graphs/Maps/Chlorophyl.pdf")
plot(CHL_Climonth_mean, col = my_palette(n=color_levels), main = expression("Chlorophyll-a" ~ (mg / m^{3})),
     xlab = "Longitude", ylab = "Latitude", xlim = c(67.9,76),
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.25))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

tiff("Paper/Final_Graphs/Maps/Chlorophyl.tiff")
plot(CHL_Climonth_mean, col = my_palette(n=color_levels), main = expression("Chlorophyll-a" ~ (mg / m^{3})),
     xlab = "Longitude", ylab = "Latitude", xlim = c(67.9,76),
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.25))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

# SST
Files_SST <- list.files(path ="Variables/SST/Clima-Month/Rasters/", pattern = ".grd", full.names = T)
SST_Climonth <- stack(Files_SST)
SST_Climonth_mean <- overlay(SST_Climonth, fun = mean)
SST_Climonth_mean <- mask(SST_Climonth_mean, ChagosEEZ)
SST_Climonth_mean <- crop(SST_Climonth_mean,e_coord)
proj4string(SST_Climonth_mean) <- CRS("+proj=longlat +ellps=WGS84") #get the projection

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 20
max_absolute_value <- max(abs(c(cellStats(SST_Climonth_mean, min), cellStats(SST_Climonth_mean, max))))
color_sequence <- seq(28.5,30,length.out=color_levels+1)

pdf("Paper/Final_Graphs/Maps/SST.pdf")
plot(SST_Climonth_mean, col = my_palette(n=color_levels), main = expression("Sea Surface Temperature " (degree~C)), 
     xlab = "Longitude", ylab = "Latitude", xlim = c(67.9,76),
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.25))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

tiff("Paper/Final_Graphs/Maps/SST.tiff")
plot(SST_Climonth_mean, col = my_palette(n=color_levels), main = expression("Sea Surface Temperature " (degree~C)), 
     xlab = "Longitude", ylab = "Latitude", xlim = c(67.9,76),
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.25))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

# SLA

Files_SLA <- list.files(path ="Variables/SLA/Clima-Month", pattern = ".grd", full.names = T)
SLA_Climonth <- stack(Files_SLA)
SLA_Climonth <- resample(SLA_Climonth, rasterextend)
SLA_Climonth_mean <- overlay(SLA_Climonth, fun = mean)
SLA_Climonth_mean <- mask(SLA_Climonth_mean, ChagosEEZ)
SLA_Climonth_mean <- crop(SLA_Climonth_mean,e_coord)
proj4string(SLA_Climonth_mean) <- CRS("+proj=longlat +ellps=WGS84") #get the projection

res(SLA_Climonth)

my_palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
color_levels <- 20
max_absolute_value <- max(abs(c(cellStats(SLA_Climonth_mean, min), cellStats(SLA_Climonth_mean, max))))
color_sequence <- seq(0.04,0.09,length.out=color_levels+1)

pdf("Paper/Final_Graphs/Maps/SLA.pdf")
plot(SLA_Climonth_mean, col = my_palette(n=color_levels), main = "Sea-level Anomalies (m)",
     xlab = "Longitude", ylab = "Latitude", xlim = c(67.9,76),
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.25))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()

tiff("Paper/Final_Graphs/Maps/SLA.tiff")
plot(SLA_Climonth_mean, col = my_palette(n=color_levels), main = "Sea-level Anomalies (m)",
     xlab = "Longitude", ylab = "Latitude", xlim = c(67.9,76),
     legend.shrink=1, legend.width=1, axis.args=list(cex.axis=1.25))
plot(ChagosEEZ,add=TRUE)
plot(ChagosBanks, alpha=0.01,add = TRUE)
dev.off()