setwd("c:/_Przemek_/GitHub/Eseje/Droga/KolejkaDoLekarza/data/") 
setwd("/Users/pbiecek/camtasia/GitHub/Eseje/Droga/KolejkaDoLekarza/data/") 

library(RODBC)

library("kknn")
library("lattice")
library(rworldmap)
library(maptools)
library(gpclib)
library(maptools)   
library(RColorBrewer) 
library(classInt)
gpclibPermit()
library(rgeos)  
require(maptools)

file.name <- "07_AOS_30062013.xls"
file.names <- list.files(pattern="xls")
wojewodztwa <- c("Dolnoslaskie", "Kujawsko-Pomorskie", "Lubelskie", "Lubuskie", "Lodzkie", "Malopolskie", "Mazowieckie", "Opolskie", "Podkarpackie", "Podlaskie", "Pomorskie", "Slaskie", "Swietokrzyskie", "Warminsko - Mazurskie", "Wielkopolskie", "Zachodniopomorskie")
dats <- list()

for (file.name in file.names) {
  excel.connect <- odbcConnectExcel(file.name)
  dats[[file.name]] <- sqlFetch(excel.connect, "Zestawienie")
  dats[[file.name]] <- data.frame(dats[[file.name]], woj=file.name)
  odbcClose(excel.connect)
}

wszystkieDane <- do.call(rbind, dats)

miasta <- sapply(strsplit(as.character(wszystkieDane[,5]), split="\n"), '[', 1)
miastawoj <- paste(miasta, ", ", wojewodztwa[as.numeric(wszystkieDane[,10])], sep="")

tocheck <- unique(miastawoj)

pary <- strsplit(tocheck, split=", ")
wspolrzedne <- list()
for (i in 1190:length(tocheck)) {
  wspolrzedne[[i]] <- SmarterPoland::getGoogleMapsAddress(city=pary[[i]][1], street=pary[[i]][2])
  cat(i, tocheck[i], "\n")
}


save(wszystkieDane, file="wszystkieDane.rda")
save(wspolrzedne,file="wspolrzedne.rda")

# wczytaj uprzednio zapisane dane
load("wszystkieDane.rda")
load("wspolrzedne.rda")
load("miastawoj.rda")
load("pary.rda")
load("tocheck.rda")


#
# skad sa dane?
df <- data.frame(sapply(wspolrzedne, `[`, 1), 
                 sapply(wspolrzedne, `[`, 2), 
                 sapply(pary, `[`, 1),
                 sapply(pary, `[`, 2))
colnames(df) <- c("szerokosc", "dlugosc", "miasto", "wojewodztwo")
rownames(df) <- tocheck

plot(df[,2], df[,1], pch=19, cex=0.2)

#
# tylko wybrany typ poradni

grupa <- "PORADNIA ALERGOLOGICZNA"

wybraneDane <- wszystkieDane[which(wszystkieDane[,1] == grupa & wszystkieDane[,2] == "przypadek stabilny"), ]
adres <- as.character(wybraneDane[,5])
wybraneMiasta <- sapply(strsplit(adres, split="\n"), '[', 1)
wybraneMiastawoj <- paste(wybraneMiasta, ", ", wojewodztwa[as.numeric(wybraneDane[,10])], sep="")

load("wybraneDane.rda")
load("wybraneMiasta.rda")
load("wybraneMiastawoj.rda")

wybraneWspolrzedne <- df[wybraneMiastawoj,]
plot(wybraneWspolrzedne[,2], wybraneWspolrzedne[,1], pch=19, cex=0.2)

wybraneDane[,c(6,8)]

#
# podejscie 1
# sredni czas czekania w okolicy za pomoca koloru

newproj <- "+proj=utm +zone=55 +south +ellps=GRS80 +units=m"
newproj <- "+ellps=GRS80"
shape0 <- readShapeSpatial("/Users/pbiecek/camtasia/Dropbox/__SmarterPoland__/_Mapy_/POL_adm0", proj4string = CRS(newproj))
shape1 <- readShapeSpatial("/Users/pbiecek/camtasia/Dropbox/__SmarterPoland__/_Mapy_/POL_adm1", proj4string = CRS(newproj),repair=TRUE,force_ring=T,verbose=TRUE) 

# siatka punktow
x = seq(14,24.5,0.02)
y = seq(49,55,0.02)
grid <- expand.grid(dlugosc = x, szerokosc=y)

pdf("mapaSredniCzas.pdf",12,11)
par(mar=c(0,0,0,0))

rownames(wybraneWspolrzedne) = NULL
wybraneDF <- data.frame(y=wybraneDane[,8], wybraneWspolrzedne[,2:1])
srednieCzasy <- kknn(y~., wybraneDF, grid, k=5)
srednieCzasyLista <- list(x = x, y = y, z = matrix(srednieCzasy$fitted.values, length(x), length(y)))
tx = readWKT("POLYGON ((12.9 48.9, 12.9 55.1, 25.1 55.1, 25.1 48.9, 12.9 48.9))")
d = gDifference(tx,shape0)
lv <- seq(0,max(srednieCzasy$fitted.values) + 1,length.out=101)
    
filled.contour2(srednieCzasyLista$x, srednieCzasyLista$y, srednieCzasyLista$z, col = rev(heat.colors(length(lv) - 1)), levels=lv)
plot(d,col='white',add=TRUE)
plot(shape1, border="grey50", lwd=1, add=T)
plot(shape0, border="black", lwd=1, add=T)  
points(wybraneDF[,2] + rnorm(nrow(wybraneDF))/30, wybraneDF[,3] + rnorm(nrow(wybraneDF))/30, pch='x', cex=0.5)

dev.off()

pdf("skalaKol.pdf", 10, 4)
plot(1:length(lv), rep(1,length(lv)), col=rev(heat.colors(length(lv) - 1)), pch=15, yaxt="n", cex=3, bty="n", xlab="",ylab="", xaxt="n")
axis(1, (0:7)*13.5, paste(0:7,"msc."))
dev.off()

#
# podejscie 2
# sredni czas czekania w okolicy za pomoca wielkosci punktu

pdf("mapaSredniPunkty.pdf",12,11)
par(mar=c(0,0,0,0))
x = seq(14,24.5,0.15)
y = seq(49,55,0.1)
grid <- expand.grid(dlugosc = x, szerokosc=y)
srednieCzasy <- kknn(y~., wybraneDF, grid, k=5)
srednieCzasyLista <- list(x = x, y = y, z = matrix(srednieCzasy$fitted.values, length(x), length(y)))

plot(grid[,1], grid[,2], pch=19, cex=as.vector(srednieCzasyLista$z)/100, xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
plot(d,col='white',add=TRUE)
plot(shape1, border="grey50", lwd=1, add=T)
plot(shape0, border="black", lwd=1, add=T)  

dev.off()

#
# podejscie 3
# odleglosc od miejsca za pomoca dlugosci odcinkow

polskieMiasta <- world.cities[world.cities$country.etc == 'Poland',]

lekarz <- wybraneWspolrzedne[wybraneDane[,8] < 14,]

najblizszyLekarz <- t(apply(polskieMiasta[,c(4,5)], 1, function(x) {
  ind <- which.min((lekarz[,1] - x[1])^2*2 + (lekarz[,2] - x[2])^2)
  c(lekarz[ind,1], lekarz[ind,2])
}))

pdf("mapaOdleglosci.pdf",12,11)

par(mar=c(0,0,0,0))
plot(polskieMiasta[,5], polskieMiasta[,4], pch=19, cex=(polskieMiasta[,3]/200000)^0.5, col="#ff0000",
     xaxt="n", yaxt="n", bty="n", xlab="", ylab="", type="n")
for (i in 1:nrow(polskieMiasta)) {
  lines(c(polskieMiasta[i,5], najblizszyLekarz[i,2]),
        c(polskieMiasta[i,4], najblizszyLekarz[i,1]),
        col="#000000bb", lwd=1)
}
points(polskieMiasta[,5], polskieMiasta[,4], pch=19, cex=(polskieMiasta[,3]/100000)^0.5, col="#ff0000aa")
points(najblizszyLekarz[,2], najblizszyLekarz[,1], pch='x', cex=0.8)
plot(shape1, border="#55555522", lwd=1, add=T)
plot(shape0, border="#55555522", lwd=1, add=T)  

dev.off()



library(maptools)
library(ggmap)
library(ggplot2)
library(mapproj)
location <- c(min(x), min(y), max(x), max(y))
location <- c(18.2, 49.8, 19.7, 50.9)

odcinki <- data.frame(x=polskieMiasta[,5], y=polskieMiasta[,4], xe=najblizszyLekarz[,2], ye = najblizszyLekarz[,1])
  
portland = get_map(location = location, source = "google", color="bw", maptype="roadmap")

pdf("mapaTlo.pdf",12,11)
ggmap(portland,darken = c(0.95, "white")) +theme_nothing() + xlim(location[c(1,3)]) + ylim(location[c(2,4)]) +
  geom_point(aes(y = lat, x = long, size=2*sqrt(pop/100000)), data=polskieMiasta, 
             colour="#ff0000aa") +
  geom_segment(aes(x=x, xend=xe, y=y, yend=ye), data=odcinki)+
  geom_point(aes(y = szerokosc, x = dlugosc), data=wybraneWspolrzedne, 
             colour="#000000aa", pch='x')
  
dev.off()  



#
# podejscie 4
# odleglosc od miejsca za pomoca dlugosci odcinkow na regularnej siatce
x = seq(14,24.5,0.1)
y = seq(49,55,0.1)
grid <- expand.grid(dlugosc = x, szerokosc=y)

lekarz <- wybraneWspolrzedne[wybraneDane[,8] < 14,]
najblizszyLekarz <- t(apply(grid[,2:1], 1, function(x) {
  ind <- which.min((lekarz[,1] - x[1])^2*2 + (lekarz[,2] - x[2])^2)
  c(lekarz[ind,1], lekarz[ind,2])
}))

pdf("mapaOdleglosciSiatka.pdf",12,11)
par(mar=c(0,0,0,0))
plot(grid[,1], grid[,2], pch=19, cex=0.1, col="#ff000055",
     xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
for (i in 1:nrow(grid)) {
  lines(c(grid[i,1], najblizszyLekarz[i,2]),
        c(grid[i,2], najblizszyLekarz[i,1]),
        col="#00000055")
}
plot(d,col='white',add=TRUE)
plot(shape1, border="grey50", lwd=1, add=T)
plot(shape0, border="black", lwd=1, add=T)  

dev.off()

#
# podejscie 5
najblizszyLekarzOdleglosc <- apply(grid[,2:1], 1, function(x) {
  min(sqrt((lekarz[,1] - x[1])^2*2 + (lekarz[,2] - x[2])^2), na.rm=TRUE)
})
srednieCzasyLista <- list(x = x, y = y)
najblizszyLekarzOdlegloscMatrix <- matrix(najblizszyLekarzOdleglosc, length(srednieCzasyLista$x), length(srednieCzasyLista$y))

lv <- seq(0,2 ,length.out=101)
wybraneDF2 <- wybraneDF[wybraneDane[,8] < 14,]

pdf("mapaOdleglosciKolory.pdf",12,11)
par(mar=c(0,0,0,0))
filled.contour2(srednieCzasyLista$x, srednieCzasyLista$y, najblizszyLekarzOdlegloscMatrix, col = rev(heat.colors(length(lv) - 1)), levels=lv)
plot(d,col='white',add=TRUE)
plot(shape1, border="grey50", lwd=1, add=T)
plot(shape0, border="black", lwd=1, add=T)  
points(wybraneDF[,2] + rnorm(nrow(wybraneDF))/30, wybraneDF[,3] + rnorm(nrow(wybraneDF))/30, pch='x', cex=0.7, col="#77777777")
points(wybraneDF2[,2] + rnorm(nrow(wybraneDF2))/30, wybraneDF2[,3] + rnorm(nrow(wybraneDF2))/30, pch='x', cex=0.7, col="black")

dev.off()








#
# Funkajc pomocnicza
# zmodyfikowana by latwiej bylo rysowac odpowiednia skale kolorow

filled.contour2 <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
                                                       length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
          ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
          levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
          col = color.palette(length(levels) - 1), plot.title, plot.axes, 
          key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
          axes = TRUE, frame.plot = axes, ...) 
{
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  .filled.contour(x, y, z, levels, col)
  if (frame.plot) 
    box()
  invisible()
}

