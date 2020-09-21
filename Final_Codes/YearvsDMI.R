setwd("/Users/ASUS/Documents/Master_CMEE/Chagos/")

library(ggplot2)


year <- c(2012, 2013, 2014, 2015, 2016, 2017)
ONI <- c(-0.1, -0.25, -0.05, 0.666666667, 2.2, 0.3)
DMI <- c(0.324, 0.309, 0.075, 0.062, 0.066, 0.703)
Sulidae <- c(0.321962776, 0.099423005, -0.068967228, -0.148293214, -0.138554954, -0.074667155)
Suup <- c(0.414568585, 0.156483171, -0.007486125, -0.096913452, -0.085720297, 0.050725415)
Sulow <- c(0.229356968, 0.04236284,  -0.13044833, -0.199672976, -0.191389611, -0.200059726)
Procellariidae <- c(0.499588875, -0.895648317, -1.594481707, -1.147881889, 0.444151135, 2.73258796)
Proup <- c(0.69513494, -0.669954464, -1.312504342, -0.967946363, 0.612913838, 3.297904925)
Prolow <- c(0.30404281, -1.12134217, -1.876459071, -1.327817415, 0.275388431, 2.167270996)


dataoni <- data.frame(year, ONI, DMI, 
                      Sulidae, Suup, Sulow, 
                      Procellariidae, Proup, Prolow)

#get Procellariidae graph

ylim.prim <- c(range(dataoni$ONI)[1], range(dataoni$ONI)[2])   # in this example, ONI
ylim.sec <- c(range(dataoni$Procellariidae)[1], range(dataoni$Procellariidae)[2])    # in this example, Sulidae

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

ONIPRO <- ggplot(dataoni, aes(year, ONI)) +
  geom_line() +
  geom_line(aes(y = a + Procellariidae*b), color = "red", size = 2) +
  scale_y_continuous("Oceanic Niño Index", sec.axis = sec_axis(~ (. - a)/b, name = "Sulidae")) +
  scale_x_continuous("Yeare", breaks = 2012:2017) +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red"),  
        # Remove panel background
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
  ) + 
  ggtitle("ONI vs Procellariidae response") 

ggsave("Paper/Final_Graphs/ONIvsProcellariidae.pdf", ONIPRO, dpi = 300, width = 10, height = 5)
ggsave("Paper/Final_Graphs/ONIvsProcellariidae.tiff", ONIPRO, dpi = 300, width = 10, height = 5)


#######################################################################################################

#sulidae graph

Sulidae_YearSmooth <- 
  ggplot(dataoni, aes(x=year)) +
  
  #doing first line for no rats islands
  geom_line(aes(y =  Suup), linetype = "dashed") + 
  geom_line(aes(y = Sulow), linetype = "dashed") +
  geom_ribbon(data=dataoni, 
              aes(ymin=Sulow,ymax=Suup), fill="grey94") +
  geom_line(aes(y = Sulidae)) + 
  
  #Customize
  theme_classic() +
  theme(panel.grid.minor = element_blank(),  aspect.ratio=1/2, legend.position = c(0.8, 0.7),
        axis.text.x = element_text(face="bold", size = 16, color = "black"),
        axis.text.y = element_text(face="bold", size = 16, color = "black"),
        axis.title.x=element_text(size=18,face="bold"),
        axis.title.y=element_text(size=15,face="bold")) +
  labs(x = "Year", y = "Effect over Sulidae")


ggsave("Paper/Final_Graphs/Sulidae_Response.pdf", Sulidae_YearSmooth, dpi = 300, width = 10, height = 5)
ggsave("Paper/Final_Graphs/Sulidae_Response.tiff", Sulidae_YearSmooth, dpi = 300, width = 10, height = 5)



#Procellariidae graph

Proce_YearSmooth <- 
  ggplot(dataoni, aes(x=year)) +
  
  #doing first line for no rats islands
  geom_line(aes(y =  Proup), linetype = "dashed") + 
  geom_line(aes(y = Prolow), linetype = "dashed") +
  geom_ribbon(data=dataoni, 
              aes(ymin=Prolow,ymax=Proup), fill="grey94") + 
  geom_line(aes(y = Procellariidae)) + 
  
  #Customize
  theme_classic() +
  theme(panel.grid.minor = element_blank(), aspect.ratio=1/2, legend.position = c(0.8, 0.7),
        axis.text.x = element_text(face="bold", size = 16, color = "black"),
        axis.text.y = element_text(face="bold", size = 16, color = "black"),
        axis.title.x=element_text(size=18,face="bold"),
        axis.title.y=element_text(size=15,face="bold")) +
  labs(x = "Year", y = "Effect over Procellariidae")


ggsave("Paper/Final_Graphs/Procellariidae_Response.pdf", Proce_YearSmooth, dpi = 300, width = 10, height = 5)
ggsave("Paper/Final_Graphs/Procellariidae_Response.tiff", Proce_YearSmooth, dpi = 300, width = 10, height = 5)



############ DMI Graph

DMI <- 
  ggplot(dataoni, aes(year, DMI)) +
  geom_line(color="darkolivegreen", size = 3) + 
  coord_cartesian(ylim = c(0,0.8)) + #coord_fixed(ratio = 2) + 
  scale_x_continuous("Year", breaks = 2012:2017) +
  scale_y_continuous(name ="Dipole Mode Index") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),  aspect.ratio=1/2,
        axis.text.x = element_text(face="bold", size = 16, color = "black"),
        axis.text.y = element_text(face="bold", size = 16, color = "black"),
        axis.title.x=element_text(size=18,face="bold"),
        axis.title.y=element_text(size=15,face="bold")) 

ggsave("Paper/Final_Graphs/DMI.pdf", DMI, dpi = 300, width = 10, height = 5)
ggsave("Paper/Final_Graphs/DMI.tiff", DMI, dpi = 300, width = 10, height = 5)



# ONI and DMI

#get ONI - DMI graph
ylim.prim <- c(range(dataoni$ONI)[1], range(dataoni$ONI)[2])   # in this example, ONI
ylim.sec <- c(range(dataoni$DMI)[1], range(dataoni$DMI)[2])    # in this example, Sulidae

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

ONIDMI <- ggplot(dataoni, aes(year, ONI)) +
  geom_col() +
  geom_line(aes(y = a + DMI*b), color = "red", size = 2) +
  scale_y_continuous("Oceanic Niño Index", sec.axis = sec_axis(~ (. - a)/b, name = "Dipole Model Index")) +
  scale_x_continuous("Year", breaks = 2012:2017) +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red"),  
        # Remove panel background
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
  ) + 
  ggtitle("ONI vs DMI") 

ggsave("Paper/Final_Graphs/ONIvsDMI.pdf", ONIDMI, dpi = 300, width = 10, height = 5)
ggsave("Paper/Final_Graphs/ONIvsDMI.tiff", ONIDMI, dpi = 300, width = 10, height = 5)


##############################
names(dataoni)
b + geom_point() + geom_smooth(method = lm)


# Extend the regression lines: fullrange
b + geom_point(aes(color = cyl, shape = cyl)) + 
  geom_smooth(aes(color = cyl), method = lm, se = FALSE, fullrange = TRUE) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))




year <- c(2012, 2013, 2014, 2015, 2016, 2017, 2012, 2013, 2014, 2015, 2016, 2017)
ONI <- c(-0.1, -0.25, -0.05, 0.666666667, 2.2, 0.3, -0.1, -0.25, -0.05, 0.666666667, 2.2, 0.3)
DMI <- c(0.324, 0.309, 0.075, 0.062, 0.066, 0.703, 0.324, 0.309, 0.075, 0.062, 0.066, 0.703)
Resp <- c(0.321962776, 0.099423005, -0.068967228, -0.148293214, -0.138554954, -0.074667155, 0.499588875, -0.895648317, -1.594481707, -1.147881889, 0.444151135, 2.73258796)
Seabird <- c(rep("Sulidae", 6), rep("Procellariidae", 6))


data_long <- data.frame(year, ONI, DMI, Resp, Seabird)


b <- ggplot(data_long, aes(x = DMI, y = Resp))+ 
  geom_point(aes(color = Seabird, shape = Seabird)) + 
  geom_smooth(aes(color = Seabird), method = lm, se = FALSE, fullrange = TRUE) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_classic()


ggsave("Paper/Final_Graphs/Corr.pdf", b)
ggsave("Paper/Final_Graphs/Corr.tiff", b)
