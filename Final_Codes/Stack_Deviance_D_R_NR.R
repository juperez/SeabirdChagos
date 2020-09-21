
#rm(list = ls())


setwd("~/Master_CMEE/Chagos")

getwd()

library(ggplot2)



data_cont <- read.csv("Paper/Graph_HD_Revision/BRT_MODELS_Deviance.csv")

cont <- ggplot(data=data_cont, aes(x= Specie, y=Contribution, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Contribution), vjust=-1, color="black",
            position = position_dodge(0.9), size=2)+
  scale_fill_brewer(palette="Paired")+  ylim(0, 50) +
  scale_fill_manual(values=c('darkgreen','darkblue', 'darkred')) +
  scale_x_discrete(limits = c("RFB", "BN", "WT", "WTS")) +
  theme_classic() + 
  theme(legend.position="bottom", panel.grid.minor = element_blank(), 
                          legend.title=element_text(size=13), 
                          legend.text=element_text(size=12)) +
  labs(x = "Species Modeled", y = "Contribution %")+
  theme(axis.text.x = element_text(color = "black", size = 12, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12,  hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 12,  hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12,  hjust = .5, vjust = .5, face = "plain"))

ggsave("Paper/Graph_HD_Revision/DC_Contribution.pdf", cont)  

ggsave("Paper/Graph_HD_Revision/DC_Contribution.tiff", cont) 

