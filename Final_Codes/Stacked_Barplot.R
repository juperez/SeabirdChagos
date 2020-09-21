# this code is for a stacked bar plot ovr family abundance
setwd("C:/Users/ASUS/Documents/Master_CMEE/Chagos/")

library(tidyr)
library(stats)
library(ggplot2)

Sulidae <- read.csv("Variables/Data_Birds_Separate_Final/F_Sulidae_Data.csv")
Laridae <- read.csv("Variables/Data_Birds_Separate_Final/F_Laridae_Data.csv", sep = ";")
Procellaridae <- read.csv("Variables/Data_Birds_Separate_Final/F_Procellaridae_Data.csv", sep = ";")
Oceanitidae <- read.csv("Variables/Data_Birds_Separate_Final/F_Oceanitidae_Data.csv", sep = ";")
Hydrobatidae <- read.csv("Variables/Data_Birds_Separate_Final/F_Hydrobatidae_Data.csv", sep = ";")
Phaethontidae <- read.csv("Variables/Data_Birds_Separate_Final/F_Phaethontidae_Data.csv", sep = ";")
Fregatidae <- read.csv("Variables/Data_Birds_Separate_Final/F_Fregatidae_Data.csv", sep = ";")


Values <- data.frame(Sulidae$Sulidae, Laridae$Laridae, Procellaridae$Procellaridae,
                     Oceanitidae$Oceanitidae, Hydrobatidae$Hydrobatidae, Phaethontidae$Ph,
                     Fregatidae$Fregatidae, Fregatidae$Year)

Sulidae <- tapply(Sulidae$Sulidae, Sulidae$Year, FUN=sum) 
Laridae <- tapply(Laridae$Laridae, Laridae$Year, FUN=sum) 
Procellaridae <- tapply(Procellaridae$Procellaridae, Procellaridae$Year, FUN=sum) 
Oceanitidae <- tapply(Oceanitidae$Oceanitidae, Oceanitidae$Year, FUN=sum)
Hydrobatidae <- tapply(Hydrobatidae$Hydrobatidae, Hydrobatidae$Year, FUN=sum)
Phaethontidae <- tapply(Phaethontidae$Phaethontidae, Phaethontidae$Year, FUN=sum)
Fregatidae <- tapply(Fregatidae$Fregatidae, Fregatidae$Year, FUN=sum)


Total <- data.frame(Sulidae, Laridae, Procellaridae, Oceanitidae, 
                    Hydrobatidae, Phaethontidae, Fregatidae)

TotalCountY <- rowSums(Total)

Total$Year <- c(2012:2017)


Total_long <- gather(Total, Family, Count, Sulidae, Laridae, Procellaridae, Oceanitidae, 
                     Hydrobatidae, Phaethontidae, Fregatidae)

Total_long$Order <- rep(1:6, 7)

#aggregate counts per family 
totals <- aggregate(Count ~ Family, Total_long, sum)


StackYear <- 
 ggplot(data=Total_long) + geom_bar(stat="identity") +
  aes(x=reorder(Family, -Count, sum), y= Count, 
        label = Count, fill = as.factor(Year)) +
  theme_classic()+
  geom_text(aes(Family, Count, label = Count, fill = NULL), 
            vjust = -0.5, data = totals, size = 5) +
  theme(legend.justification=c(0,1), legend.position=c(.8,.9), 
        legend.text=element_text(size=15), legend.title = element_text(size = 20),
        axis.text.x = element_text(face="bold", size = 13, color = "black"),
        axis.text.y = element_text(face="bold", size = 16, color = "black", angle = 90, hjust = 0.5),
        axis.title.x=element_text(size=18,face="bold"),
        axis.title.y=element_text(size=18,face="bold")) +
    labs(x = "Seabirds Families", y = "Number of Individuals") +
    scale_fill_manual(values = c("#440154FF", "#404688FF", "#287C8EFF","#27AD81FF",
                                 "#8FD744FF", "#FDE725FF"),name= "Year") +
  ylim(0, 10000)



#stack_obs <-  ggplot(data=Total_Counts) +  geom_bar(stat = "identity") +  
   #           aes(x = reorder(class, -value, sum), y = value, label = value, fill = Year) +
      #        theme(legend.position=c(1,0))+
      #        theme_classic(base_size = 17) +
       #       geom_text(aes(class, total + 350, label = total, fill = NULL), data = totals)+
        #      labs(x = "Seabirds Families", y = "Counts per Year")+
         #     theme(legend.position= c(0.8,0.5), legend.text=element_text(size=20),
                  #  legend.key.size = unit(2, "cm"))


ggsave("Paper/Final_Graphs/StackYear.tiff", StackYear, dpi = 300, width = 10, height = 5)
ggsave("Paper/Final_Graphs/StackYear.pdf", StackYear, dpi = 300, width = 10, height = 5)

library(RColorBrewer)

display.brewer.all()

names(Total_Counts) <- c("class","Year","value")

totals <- Total_Counts %>%
  group_by(class) %>%
  summarize(total = sum(value))

ggplot(data=Total_Counts) +  geom_bar(stat = "identity") +  
  aes(x = -reorder(class, -value, sum), y = value, label = value, fill = year) +
  theme_classic() +
  geom_text(aes(class, total + 350, label = total, fill = NULL), data = totals)


#########stackplot for family

Values <- data.frame(Sulidae$Sulidae, Laridae$Laridae, Procellaridae$Procellaridae,
                     Sulidae$Year)
names(Values) <- c("Sulidae", "Laridae", "Procellariidae", "Year")


Sulidae <- tapply(Values$Sulidae, Values$Year, FUN=sum) 
Laridae <- tapply(Values$Laridae, Values$Year, FUN=sum) 
Procellariidae <- tapply(Values$Procellariidae, Values$Year, FUN=sum) 


ValuesStack <- data.frame(Sulidae, Laridae, Procellariidae)


ValuesStack$Year <- c(2012:2017)


data_long <- gather(ValuesStack, Family, Count, Sulidae, 
                    Laridae, Procellariidae)

data_long$Order <- c(rep(2,6), rep(1,6), rep(3,6))

TotalCountY #this is for total count of observations without non selected families

YearCount <- table(Sulidae$Year)

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

Y2017 <- subset(data_long, data_long$Year == 2017)
Y2017$Prop <- round(Y2017$Count/YearCount[6], digits = 1)


data_long <- rbind(Y2012, Y2013, Y2014, Y2015, Y2016, Y2017)



stackfamilies <- 
  ggplot(data=data_long, aes(x=Year, y= Prop, fill=as.factor(Order))) +
  geom_bar(stat="identity", position= position_dodge())+
  geom_text(aes(label=Prop), vjust=-0.5, color="black",
            position = position_dodge(0.86), size=5)+
  scale_x_continuous("Year", breaks = 2012:2017) +
  theme_classic()+
  theme(legend.justification=c(0,1), legend.position=c(.85,.9), 
        legend.text=element_text(size=15), legend.title = element_text(size = 20),
        axis.text.x = element_text(face="bold", size = 16, color = "black"),
        axis.text.y = element_text(face="bold", size = 16, color = "black", angle = 90),
        axis.title.x=element_text(size=18,face="bold"),
        axis.title.y=element_text(size=18,face="bold")) +
  scale_fill_manual(values = c("chartreuse4", "orangered4", "rosybrown4"),name= "Family", 
                    labels = c("Laridae", "Sulidae", "Procellariidae")) + 
  ylab("Individuals per sample")
  

ggsave("Paper/Final_Graphs/StackFamilies.tiff", stackfamilies, dpi = 300, width = 10, height = 5)
ggsave("Paper/Final_Graphs/StackFamilies.pdf", stackfamilies, dpi = 300, width = 10, height = 5)
