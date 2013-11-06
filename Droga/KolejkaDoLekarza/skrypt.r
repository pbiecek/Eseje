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
miasta <- sapply(strsplit(as.character(wszystkieDane[,5]), split="\n"), '[', 1)
miastawoj <- paste(miasta, ", ", wojewodztwa[as.numeric(wszystkieDane[,10])], sep="")



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

wybraneDane <- wszystkieDane[wszystkieDane[,1] == grupa & wszystkieDane[,2] == "przypadek stabilny", ]
wybraneMiasta <- sapply(strsplit(as.character(wybraneDane[,5]), split="\n"), '[', 1)
wybraneMiastawoj <- paste(wybraneMiasta, ", ", wojewodztwa[as.numeric(wybraneDane[,10])], sep="")

wybraneWspolrzedne <- df[wybraneMiastawoj,]
plot(wybraneWspolrzedne[,2], wybraneWspolrzedne[,1], pch=19, cex=0.2, col="#00000022")

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

wybraneDF <- data.frame(y=wybraneDane[,8], wybraneWspolrzedne[,2:1])
srednieCzasy <- kknn(y~., wybraneDF, grid, k=5)
srednieCzasyLista <- list(x = x, y = y, z = matrix(srednieCzasy$fitted.values, length(x), length(y)))
tx = readWKT("POLYGON ((12.9 48.9, 12.9 55.1, 25.1 55.1, 25.1 48.9, 12.9 48.9))")
d = gDifference(tx,shape0)
lv <- seq(0,max(srednieCzasy$fitted.values) + 1,length.out=101)
    
filled.contour2(srednieCzasyLista$x, srednieCzasyLista$y, srednieCzasyLista$z,  frame.plot = FALSE, plot.axes={
  plot(d,col='white',add=TRUE)
  plot(shape1, border="grey50", lwd=1, add=T)
  plot(shape0, border="black", lwd=2, add=T)  
  points(wybraneDF[,2] + rnorm(nrow(wybraneDF))/30, wybraneDF[,3] + rnorm(nrow(wybraneDF))/30, pch=10, cex=0.2)
}, col = rev(heat.colors(length(lv) - 1)), levels=lv)
title(main=grupa)



#
# podejscie 2
# sredni czas czekania w okolicy za pomoca wielkosci punktu

x = seq(14,24.5,0.1)
y = seq(49,55,0.1)
grid <- expand.grid(dlugosc = x, szerokosc=y)
srednieCzasy <- kknn(y~., wybraneDF, grid, k=15)
srednieCzasyLista <- list(x = x, y = y, z = matrix(srednieCzasy$fitted.values, length(x), length(y)))

plot(grid[,1], grid[,2], pch=19, cex=as.vector(srednieCzasyLista$z)/100, xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
plot(d,col='white',add=TRUE)
plot(shape1, border="grey50", lwd=1, add=T)
plot(shape0, border="black", lwd=2, add=T)  


#
# podejscie 3
# odleglosc od miejsca za pomoca dlugosci odcinkow

polskieMiasta <- world.cities[world.cities$country.etc == 'Poland',]

lekarz <- wybraneWspolrzedne[wybraneDane[,8] < 14,]

najblizszyLekarz <- t(apply(polskieMiasta[,c(4,5)], 1, function(x) {
  ind <- which.min((lekarz[,1] - x[1])^2 + (lekarz[,2] - x[2])^2)
  c(lekarz[ind,1], lekarz[ind,2])
}))

par(mar=c(0,0,0,0))
plot(polskieMiasta[,5], polskieMiasta[,4], pch=19, cex=(polskieMiasta[,3]/200000)^0.3, col="#ff000055",
     xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
for (i in 1:nrow(polskieMiasta)) {
  lines(c(polskieMiasta[i,5], najblizszyLekarz[i,2]),
        c(polskieMiasta[i,4], najblizszyLekarz[i,1]),
        col="#00000055")
}

#
# podejscie 4
# odleglosc od miejsca za pomoca dlugosci odcinkow na regularnej siatce
x = seq(14,24.5,0.1)
y = seq(49,55,0.1)
grid <- expand.grid(dlugosc = x, szerokosc=y)

lekarz <- wybraneWspolrzedne[wybraneDane[,8] < 14,]
najblizszyLekarz <- t(apply(grid[,2:1], 1, function(x) {
  ind <- which.min((lekarz[,1] - x[1])^2 + (lekarz[,2] - x[2])^2)
  c(lekarz[ind,1], lekarz[ind,2])
}))

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
plot(shape0, border="black", lwd=2, add=T)  

#
# podejscie 5
najblizszyLekarzOdleglosc <- apply(grid[,2:1], 1, function(x) {
  min(sqrt((lekarz[,1] - x[1])^2 + (lekarz[,2] - x[2])^2))
})
najblizszyLekarzOdlegloscMatrix <- matrix(najblizszyLekarzOdleglosc, length(srednieCzasyLista$x), length(srednieCzasyLista$y))

lv <- seq(0,max(najblizszyLekarzOdleglosc) + 1,length.out=101)

filled.contour2(srednieCzasyLista$x, srednieCzasyLista$y, najblizszyLekarzOdlegloscMatrix,  frame.plot = FALSE, plot.axes={
  plot(d,col='white',add=TRUE)
  plot(shape1, border="grey50", lwd=1, add=T)
  plot(shape0, border="black", lwd=2, add=T)  
  points(wybraneDF[,2] + rnorm(nrow(wybraneDF))/30, wybraneDF[,3] + rnorm(nrow(wybraneDF))/30, pch=10, cex=0.2)
}, col = rev(heat.colors(length(lv) - 1)), levels=lv)
title(main=grupa)










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
  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.orig))
  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
  layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
  par(las = las)
  mar <- mar.orig
  mar[4L] <- mar[2L]
  mar[2L] <- 1
  par(mar = mar)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
              yaxs = "i")
  rect(0, levels[-length(levels)], 1, levels[-1L], col = col,border=NA)
  if (missing(key.axes)) {
    if (axes) 
      axis(4)
  }
  else key.axes
  box()
  if (!missing(key.title)) 
    key.title
  mar <- mar.orig
  mar[4L] <- 1
  par(mar = mar)
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  .filled.contour(x, y, z, levels, col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
}

