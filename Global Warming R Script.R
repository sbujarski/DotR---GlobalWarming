#Data on the Rocks
#May 2017
#Global Warming


#REQUIRED PACKAGES
library(xlsx) #package to import xls files directly
library(pastecs) #summary stats function stat.desc
library(ggplot2)
library(scales) #for percent axis
library(plyr) #data wrangling
library(dplyr) #data wrangling 2
library(tidyr) #data wrangling 3
library(lubridate) #data manipulation with date

#CUSTOM FUNCTIONS
Sp.Desc <- function(data)
{
  data.num <- data.frame(Dummy=rep(NA,dim(data)[1])) #Make a dummy data.frame
  data.cat <- data.frame(Dummy=rep(NA,dim(data)[1]))
  #separate categorical from numerical data
  for(i in 1:dim(data)[2]){
    if(!is.na(stat.desc(data[i])["mean",])){#if R can compute a mean then add to data.num
      data.num <- cbind(data.num, data[i])
    }
    else{
      data.cat <- cbind(data.cat, data[i])
    }
  }
  #Delete dummy variable
  data.num$Dummy <- NULL
  data.cat$Dummy <- NULL
  
  #Print Numerical results
  if(dim(data.num)[2]>0) {
    print(t(stat.desc(data.num))[,-c(2,3,6,7,11,14)])
    cat(noquote(""), sep="\n\n")
  }
  
  #Print categorical results
  if(dim(data.cat)[2]>0) {
    for(j in 1:dim(data.cat)[2]){
      cat(noquote(names(data.cat[j])))
      print(table(data.cat[j]))
    }
  }
}

DotR.Theme <- function(axis.text.size=16, axis.title.size=16, title.size=20, legend.position="none")
{
  theme(panel.grid.major = element_line(colour="grey90"), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"),
        axis.title.x=element_text(colour = "black", size=axis.title.size), axis.title.y=element_text(colour = "black", size=axis.title.size),
        axis.text.x=element_text(colour = "black", size=axis.text.size), axis.text.y=element_text(colour = "black", size=axis.text.size),
        plot.title=element_text(colour = "black",size=title.size, face="bold", hjust=.5),
        axis.ticks=element_line(colour="black"), legend.position = legend.position, legend.key=element_blank())
}



#DATA IMPORT----
NOAA <- read.xlsx("C:/Users/sbuja/Documents/Data on the Rocks/Global Warming/NOAA-1916-2016.xlsx", sheetName="Sheet1")
NOAA$Anomoly.F <- NOAA$Anomoly.C * (9/5)
NOAA$Source <- "NOAA"
Sp.Desc(NOAA)
write.csv(NOAA, "NOAA.csv", row.names=F)

NASA <- read.xlsx("C:/Users/sbuja/Documents/Data on the Rocks/Global Warming/NASA.xlsx", sheetName="Sheet1")
NASA$Anomoly.F <- NASA$Anomoly.C * (9/5)
NASA$Source <- "NASA"
Sp.Desc(NASA)
write.csv(NASA, "NASA.csv", row.names=F)


#Merge data
GlobalWarming <- rbind(NOAA, NASA)
Sp.Desc(GlobalWarming)
View(GlobalWarming)

#GRAPHING DATA----
colours <- c("#009ae1", "#003d72", "#FF351F")#NOAA-light blue from logo, NASA-deep blue from logo, CO2-red

GlobalWarming.plot <- ggplot(GlobalWarming, aes(x=Year, y=Anomoly.F, colour=Source)) +
  geom_hline(yintercept=0, colour="grey", linetype="11", size=2) +
  geom_point(size=2, alpha=.4) +
  stat_smooth(method="loess", se=F, size=2, span=.3) + 
  scale_colour_manual(values=colours) +
  annotate("text", label="20th Century Average", x=1999, y=-.1, colour="grey", size=6, fontface="bold", hjust=.5, vjust=1) + 
  annotate("text", label="National Oceanic and\nAtmospheric Administration", x=1936, y=.9, colour=colours[1], size=6, fontface="bold", hjust=0, vjust=0) + 
  annotate("text", label="NASA", x=2005, y=0.3, colour=colours[2], size=6, fontface="bold", hjust=.5, vjust=0) + 
  scale_x_continuous("Year", limits=c(1916,2020), breaks=c(seq(1920,2016,20),2016), expand = c(0,0)) +
  scale_y_continuous("Global Temperature Anomaly in ?F", limits=c(-1,2), breaks=seq(-1,2,0.5), expand = c(0,0)) +
  ggtitle("Global Warming over 100 Years") +
  DotR.Theme()
GlobalWarming.plot

ggsave(GlobalWarming.plot, filename="GlobalWarming.plot.png", width = 8, height=7, dpi=500)
