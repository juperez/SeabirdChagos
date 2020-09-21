library(ggplot2)
library(scales)


data_dev <- read.csv("/Chagos/Paper/Deviance_GAM.csv", sep = ";")

as.numeric(percent(data_dev$Deviance))
names(data_dev)
Total_Abundance_Dev <- subset(data_dev, data_dev$Type == "TO")
# Change barplot fill colors by groups
Total_Abundance_Dev <- Total_Abundance_Dev[order(Total_Abundance_Dev$Number), ]

dev_resp_TO <- ggplot(Total_Abundance_Dev, aes(x = reorder(Temporal.Scale, - Deviance), y = Deviance, fill = Temporal.Scale)) + 
  geom_bar(stat="identity", width=1, position=position_dodge(width=.7)) + 
  geom_text(aes(label=Deviance), position=position_dodge(width=.8), vjust=0, hjust = -0.3, size = 3.5) +
  scale_fill_brewer(palette = "Set1") + theme_classic(base_size = 10, base_family = "") +
  xlab("Temporal Scale") + 
  ylab("Explained deviance (%)")+
  scale_y_continuous(limits=c(0,60)) +
  scale_fill_manual(values=c("#000000", "#000099", "#CC0000", "#336633", "#cc9900")) +
  coord_flip() +

  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"), legend.position = "botton",
        axis.text.y = element_text(hjust=1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(5, 5, 5, 5)) 
 

dev_resp_TO

ggsave("Chagos/Paper/Final_Graphs/Deviance/devianceTo.tiff", dev_resp_TO, dpi = 120)
ggsave("Chagos/Paper/Final_Graphs/Deviance/devianceTo.pdf", dev_resp_TO)


#################################################


Boobies_Abundance_Dev <- subset(data_dev, data_dev$Type == "BO")
# Change barplot fill colors by groups

dev_resp_BO <- ggplot(Boobies_Abundance_Dev, aes(reorder(Temporal.Scale, - Deviance), Deviance, fill = Temporal.Scale)) + 
  geom_bar(stat="identity", width=1, position=position_dodge(width=.7)) + 
  geom_text(aes(label=Deviance), position=position_dodge(width=.8), vjust=0, hjust = -0.3, size = 3.5) +
  scale_fill_brewer(palette = "Set1") + theme_classic(base_size = 10, base_family = "") +
  xlab("Temporal Scale") + 
  ylab("Explained deviance (%)")+
  scale_y_continuous(limits=c(0,60)) +
  scale_fill_manual(values=c("#000000", "#000099", "#CC0000", "#336633", "#cc9900")) +
  coord_flip() +
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"), legend.position = "botton",
        axis.text.y = element_text(hjust=1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(5, 5, 5, 5)) 


dev_resp_BO

ggsave("Chagos/Paper/Final_Graphs/Deviance/devianceBo.tiff", dev_resp_BO, dpi = 120)
ggsave("Chagos/Paper/Final_Graphs/Deviance/devianceBo.pdf", dev_resp_BO)

#################################################


Noddies_Abundance_Dev <- subset(data_dev, data_dev$Type == "NO")
# Change barplot fill colors by groups

dev_resp_NO <- ggplot(Noddies_Abundance_Dev, aes(reorder(Temporal.Scale, - Deviance), Deviance, fill = Temporal.Scale)) + 
  geom_bar(stat="identity", width=1, position=position_dodge(width=.7)) + 
  geom_text(aes(label=Deviance), position=position_dodge(width=.8), vjust=0, hjust = -0.3, size = 3.5) +
  scale_fill_brewer(palette = "Set1") + theme_classic(base_size = 10, base_family = "") +
  xlab("Temporal Scale") + 
  ylab("Explained deviance (%)")+
  scale_y_continuous(limits=c(0,60)) +
  scale_fill_manual(values=c("#000000", "#000099", "#CC0000", "#336633", "#cc9900")) +
  coord_flip() +
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"), legend.position = "botton",
        axis.text.y = element_text(hjust=1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(5, 5, 5, 5)) 


dev_resp_NO

ggsave("Chagos/Paper/Final_Graphs/Deviance/devianceNo.tiff", dev_resp_NO, dpi = 120)
ggsave("Chagos/Paper/Final_Graphs/Deviance/devianceNo.pdf", dev_resp_NO)

#################################################


Shear_Abundance_Dev <- subset(data_dev, data_dev$Type == "SH")
# Change barplot fill colors by groups

dev_resp_SH <- ggplot(Shear_Abundance_Dev, aes(reorder(Temporal.Scale, - Deviance), Deviance, fill = Temporal.Scale)) + 
  geom_bar(stat="identity", width=1, position=position_dodge(width=.7)) + 
  geom_text(aes(label=Deviance), position=position_dodge(width=.8), vjust=0, hjust = -0.3, size = 3.5) +
  scale_fill_brewer(palette = "Set1") + theme_classic(base_size = 10, base_family = "") +
  xlab("Temporal Scale") + 
  ylab("Explained deviance (%)")+
  scale_y_continuous(limits=c(0,60)) +
  scale_fill_manual(values=c("#000000", "#000099", "#CC0000", "#336633", "#cc9900")) +
  coord_flip() +
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"), legend.position = "botton",
        axis.text.y = element_text(hjust=1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(5, 5, 5, 5)) 


dev_resp_SH

ggsave("Chagos/Paper/Final_Graphs/Deviance/devianceSh.tiff", dev_resp_SH, dpi = 120)
ggsave("Chagos/Paper/Final_Graphs/Deviance/devianceSh.pdf", dev_resp_SH)

#################################################


Tern_Abundance_Dev <- subset(data_dev, data_dev$Type == "TE")
# Change barplot fill colors by groups

dev_resp_TE <- ggplot(Tern_Abundance_Dev, aes(reorder(Temporal.Scale, - Deviance), Deviance, fill = Temporal.Scale)) + 
  geom_bar(stat="identity", width=1, position=position_dodge(width=.7)) + 
  geom_text(aes(label=Deviance), position=position_dodge(width=.8), vjust=0, hjust = -0.3, size = 3.5) +
  scale_fill_brewer(palette = "Set1") + theme_classic(base_size = 10, base_family = "") +
  xlab("Temporal Scale") + 
  ylab("Explained deviance (%)")+
  scale_y_continuous(limits=c(0,60)) +
  scale_fill_manual(values=c("#000000", "#000099", "#CC0000", "#336633", "#cc9900")) +
  coord_flip() +
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"), legend.position = "botton",
        axis.text.y = element_text(hjust=1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(5, 5, 5, 5)) 


dev_resp_TE

ggsave("Chagos/Paper/Final_Graphs/Deviance/devianceTe.tiff", dev_resp_TE, dpi = 120)
ggsave("Chagos/Paper/Final_Graphs/Deviance/devianceTe.pdf", dev_resp_TE)


