library(Cairo)
library("XML")
library(RColorBrewer)
library(ks)
library(maptools)
library(ggmap)
library(ggplot2)
library(mapproj)

setwd("/Users/pbiecek/camtasia/GitHub/Eseje/Droga/Podstawowki/")

dane <- read.table("Wwa_2012.csv",sep=";",h=T,dec=",",fileEncoding="UTF-8")
load("res2.Rdata")
dane2 <- cbind(dane, res2)

kolory <- brewer.pal(6,"RdYlGn")
kolor <- floor(6*rank(dane[,6])/244)+1
rozmiar <- sqrt(dane[,5])

newproj <- "+ellps=GRS80"
location <- c(20.8, 52.1, 21.3, 52.4)

portland = get_map(location = location, source = "stamen", maptype = "watercolor", color="bw")
str(portland)
CairoPDF("podstawowkiStamenWater.pdf",12,12)
ggmap(portland,darken = c(0.65, "white")) +
  geom_point(aes(y = res2[,1], x = res2[,2]), data=dane2, 
             colour="black", size=rozmiar/2+1)+
  geom_point(aes(y = res2[,1], x = res2[,2]), data=dane2, 
             colour=kolory[kolor], size=rozmiar/2)
dev.off()

portland = get_map(location = location, source = "google", maptype = "roadmap", color="bw")
str(portland)
CairoPDF("podstawowkiGoogle.pdf",12,12)
ggmap(portland,darken = c(0.65, "white")) +
  geom_point(aes(y = res2[,1], x = res2[,2]), data=dane2, 
             colour="black", size=rozmiar/2+1)+
  geom_point(aes(y = res2[,1], x = res2[,2]), data=dane2, 
             colour=kolory[kolor], size=rozmiar/2)
  
dev.off()


portland = get_map(location = location, source = "stamen", maptype = "toner", color="bw")



