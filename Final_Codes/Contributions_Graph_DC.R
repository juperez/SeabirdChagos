#this script is for ploting the contributions of the maps for each models



BirdsDev <- read.csv("Chagos/Paper/Final_Graphs/BRT_Contributions/Deviance_Models.csv", sep = ";")

BirdsDev <- data.frame(Vector_Sulidae, Vector_Laridae, Vector_Procellaridae)
names(BirdsDev) <- c("Sulidae", "Laridae", "Procellaridae")
rownames(BirdsDev) <- c("Rat-Infested Model", "Rat-Free Model")

BirdsDevRI <- BirdsDev[which(BirdsDev$Status=="Rat-Invaded"),]
BirdsDevRF <- BirdsDev[which(BirdsDev$Status=="Rat-Free"),]
BirdsDevSt   <- cbind(BirdsDevRI[,3], BirdsDevRF[,3])
colnames(BirdsDevSt) <- c("Rat-Invaded", "Rat-Free")
rownames(BirdsDevSt) <- BirdsDevRI$Family



pdf("Chagos/Paper/Final_Graphs/BRT_Contributions/BRTDEviance.pdf")
Bat_Dev <- barplot(t(BirdsDevSt), beside=T, ylab="Explained Deviance (%)",cex.axis = 1.5, 
                   cex.names=2, cex.lab = 1.5, las=1, ylim=c(0,1), col=c("red","darkblue"))
box(bty="l")
legend(5.5,1,c("Rat-Invaded", "Rat-Free"), fill = c("red", "darkblue"),
       cex = 2, border = c("red", "darkblue"), bty = "n")

dev.off()

tiff("Chagos/Paper/Final_Graphs/BRT_Contributions/BRTDEviance.tiff")
Bat_Dev <- barplot(t(BirdsDevSt), beside=T, ylab="Explained Deviance (%)",cex.axis = 1.5, 
        cex.names=2, cex.lab = 1.5, las=1, ylim=c(0,1), col=c("red","darkblue"))
box(bty="l")
legend(5.5,1,c("Rat-Invaded", "Rat-Free"), fill = c("red", "darkblue"),
       cex = 2, border = c("red", "darkblue"), bty = "n")
dev.off()



############ SULIDAE

setwd("C:/Users/ASUS/Documents/Master_CMEE/")

Sul_R_Cont <- read.csv("Chagos/Paper/Graphs_BRT_Presence/SulidaeR_Contribution.csv")
Sul_R_Cont <- subset(Sul_R_Cont, Sul_R_Cont$rel.inf > 5)
Sul_NR_Cont <- read.csv("Chagos/Paper/Graphs_BRT_Presence/SulidaeNR_Contribution.csv")
Sul_NR_Cont <- subset(Sul_NR_Cont, Sul_NR_Cont$rel.inf > 5)

data_Sul_R <- Sul_R_Cont$rel.inf
names(data_Sul_R) <- c("Distance", "Slope", "SLA", "Area")

pdf("Chagos/Paper/Final_Graphs/BRT_Contributions/Sulidae_R_Cont_DC.pdf")
x <- barplot(data_Sul_R, col = c("red3"), 
        ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2)  
y<-as.matrix(data_Sul_R)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()

tiff("Chagos/Paper/Final_Graphs/BRT_Contributions/Sulidae_R_Cont_DC.tiff")
x <- barplot(data_Sul_R, col = c("red3"), 
             ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2) 
y<-as.matrix(data_Sul_R)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()

data_Sul_NR <- Sul_NR_Cont$rel.inf
names(data_Sul_NR) <- c("Distance", "Slope", "SLA", "Area", "Year")

pdf("Chagos/Paper/Final_Graphs/BRT_Contributions/Sulidae_NR_Cont_DC.pdf")
x <- barplot(data_Sul_NR, col = c("blue4"), 
             ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2) 
y<-as.matrix(data_Sul_NR)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()

tiff("Chagos/Paper/Final_Graphs/BRT_Contributions/Sulidae_NR_Cont_DC.tiff")
x <- barplot(data_Sul_NR, col = c("blue4"), 
             ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2)   
y<-as.matrix(data_Sul_NR)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()



############ LARIDAE


Lar_R_Cont <- read.csv("Chagos/Paper/Graphs_BRT_Presence/LaridaeR_Contribution.csv")
Lar_R_Cont <- subset(Lar_R_Cont, Lar_R_Cont$rel.inf > 5)
Lar_NR_Cont <- read.csv("Chagos/Paper/Graphs_BRT_Presence/LaridaeNR_Contribution.csv")
Lar_NR_Cont <- subset(Lar_NR_Cont, Lar_NR_Cont$rel.inf > 5)

data_Lar_R <- Lar_R_Cont$rel.inf
names(data_Lar_R) <- c("Distance", "CHL", "SST", "SLA", "Area")

pdf("Chagos/Paper/Final_Graphs/BRT_Contributions/Laridae_R_Cont_DC.pdf")
x <- barplot(data_Lar_R, col = c("red3"), 
             ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2) 
y<-as.matrix(data_Lar_R)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()

tiff("Chagos/Paper/Final_Graphs/BRT_Contributions/Laridae_R_Cont_DC.tiff")
x <- barplot(data_Lar_R, col = c("red3"), 
             ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2)  
y<-as.matrix(data_Lar_R)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()

data_Lar_NR <- Lar_NR_Cont$rel.inf
names(data_Lar_NR) <- c("Distance", "SST", "CHL", "SLA", "Area")

pdf("Chagos/Paper/Final_Graphs/BRT_Contributions/Laridae_NR_Cont_DC.pdf")
x <- barplot(data_Lar_NR, col = c("blue4"), 
             ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2)   
y<-as.matrix(data_Lar_NR)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()

tiff("Chagos/Paper/Final_Graphs/BRT_Contributions/Laridae_NR_Cont_DC.tiff")
x <- barplot(data_Lar_NR, col = c("blue4"), 
             ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2)  
y<-as.matrix(data_Lar_NR)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()


############ PROCELLARIDAE

Pro_R_Cont <- read.csv("Chagos/Paper/Graphs_BRT_Presence/ProcellaridaeR_Contribution.csv")
Pro_R_Cont <- subset(Pro_R_Cont, Pro_R_Cont$rel.inf > 5)
Pro_NR_Cont <- read.csv("Chagos/Paper/Graphs_BRT_Presence/ProcellaridaeNR_Contribution.csv")
Pro_NR_Cont <- subset(Pro_NR_Cont, Pro_NR_Cont$rel.inf > 5)

data_Pro_R <- Pro_R_Cont$rel.inf
names(data_Pro_R) <- c("SST", "CHL", "Area")

pdf("Chagos/Paper/Final_Graphs/BRT_Contributions/Procellaridae_R_Cont_DC.pdf")
x <- barplot(data_Pro_R, col = c("red3"), 
             ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2) 
y<-as.matrix(data_Pro_R)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()

tiff("Chagos/Paper/Final_Graphs/BRT_Contributions/Procellaridae_R_Cont_DC.tiff")
x <- barplot(data_Pro_R, col = c("red3"), 
             ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2) 
y<-as.matrix(data_Pro_R)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()

data_Pro_NR <- Pro_NR_Cont$rel.inf
names(data_Pro_NR) <- c("SST", "Distance", "CHL")

pdf("Chagos/Paper/Final_Graphs/BRT_Contributions/Procellaridae_NR_Cont_DC.pdf")
x <- barplot(data_Pro_NR, col = c("blue4"), 
             ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2) 
y<-as.matrix(data_Pro_NR)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()

tiff("Chagos/Paper/Final_Graphs/BRT_Contributions/Procellaridae_NR_Cont_DC.tiff")
x <- barplot(data_Pro_NR, col = c("blue4"), 
             ylim=c(0,80), cex.axis = 1.5, cex.names = 1.5, font = 2, font.lab = 2) 
y<-as.matrix(data_Pro_NR)
text(x,y+5,labels=paste(as.character(round(y, 1)), "%"), cex = 1.5)
dev.off()

