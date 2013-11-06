setwd("c:/_Przemek_/GitHub/Eseje/Droga/KolejkaDoLekarza/data/") 

library(RODBC)
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


plot(sapply(wspolrzedne, `[`, 2), sapply(wspolrzedne, `[`, 1), pch=19, cex=0.2)
text(sapply(wspolrzedne, `[`, 2), sapply(wspolrzedne, `[`, 1), tocheck)

df <- data.frame(sapply(wspolrzedne, `[`, 1), 
                 sapply(wspolrzedne, `[`, 2), 
                 sapply(pary, `[`, 1),
                 sapply(pary, `[`, 2))
colnames(df) <- c("szerokosc", "dlugosc", "miasto", "wojewodztwo")
rownames(df) <- tocheck

all <- df[miastawoj,]

plot(all[,2]+rnorm(nrow(all))/100, all[,1]+rnorm(nrow(all))/100, pch=19, cex=0.2, col="#00000022")


plot(1+as.vector(table(dats3[,1])), 1+tapply(dats3[,8], dats3[,1], mean, na.rm=TRUE), log="xy", pch=19, cex=0.5)

grupa <- "PORADNIA KARDIOLOGICZNA"

dats4 <- dats3[dats3[,1] == grupa,]
all4 <- all[dats3[,1] == grupa,]

plot(all4[,2]+rnorm(nrow(all4))/100, all4[,1]+rnorm(nrow(all4))/100, pch=19, cex=0.2, col="#00000022")


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

newproj <- "+proj=utm +zone=55 +south +ellps=GRS80 +units=m"
newproj <- "+ellps=GRS80"
shape0 <- readShapeSpatial("POL_adm0", proj4string = CRS(newproj))
shape1 <- readShapeSpatial("POL_adm1", proj4string = CRS(newproj),repair=TRUE,force_ring=T,verbose=TRUE) 

x = seq(14,24.5,0.02)
y = seq(49,55,0.02)

grid <- expand.grid(dlugosc = x, szerokosc=y)

grupa <- "PORADNIA KARDIOLOGICZNA"

grupy <- names(which(table(dats3[,1])>100))

for (k in c(5,10,20)) {
  for (grupa in grupy) {
    dats4 <- dats3[dats3[,1] == grupa,]
    all4 <- all[dats3[,1] == grupa,]
    
    ddf <- data.frame(y=dats4[,8], all4[,2:1])
    
    res <- kknn(y~., ddf, grid, k=k)
    
    resG <- list(x = x, y = y, z = matrix(res$fitted.values, length(x), length(y)))
    tx = readWKT("POLYGON ((12.9 48.9, 12.9 55.1, 25.1 55.1, 25.1 48.9, 12.9 48.9))")
    d = gDifference(tx,shape0)
    
    lv <- seq(0,max(res$fitted.values) + 1,length.out=101)
    
    png(paste(grupa,k,".png",sep=""),900,700)
    
    windowsFonts(Calibri=windowsFont("Palatino Linotype"))
    par(family="Calibri")
    
    filled.contour2(resG$x, resG$y, resG$z,  frame.plot = FALSE, plot.axes={
      plot(d,col='white',add=TRUE)
      plot(shape1, border="grey50", lwd=1, add=T)
      plot(shape0, border="black", lwd=2, add=T)  
      points(ddf[,2] + rnorm(nrow(ddf))/30,ddf[,3] + rnorm(nrow(ddf))/30, pch=10, cex=0.2)
    }, col = rev(heat.colors(length(lv) - 1)), levels=lv)
    title(main=grupa)
    
    dev.off()
  }
}




grupy <- names(which(table(dats3[,1]) > 500))


for (grupa in grupy) {
  dats4 <- dats3[dats3[,1] == grupa,]
  all4 <- all[dats3[,1] == grupa,]
  
  ddf <- data.frame(y=dats4[,8], all4[,2:1])
  
  res <- kknn(y~., ddf, grid, k=12)
  
  resG <- list(x = x, y = y, z = matrix(res$fitted.values, length(x), length(y)))
  tx = readWKT("POLYGON ((12.9 48.9, 12.9 55.1, 25.1 55.1, 25.1 48.9, 12.9 48.9))")
  d = gDifference(tx,shape0)
  
  lv <- seq(0,max(res$fitted.values) + 1,length.out=101)
  
  png(paste(grupa,".png",sep=""),900,700)
  
  windowsFonts(Calibri=windowsFont("Palatino Linotype"))
  par(family="Calibri")
  
  filled.contour2(resG$x, resG$y, resG$z,  frame.plot = FALSE, plot.axes={
    plot(d,col='white',add=TRUE)
    plot(shape1, border="grey50", lwd=1, add=T)
    plot(shape0, border="black", lwd=2, add=T)  
    points(ddf[,2] + rnorm(nrow(ddf))/30,ddf[,3] + rnorm(nrow(ddf))/30, pch=10, cex=0.2)
  }, col = rev(heat.colors(length(lv) - 1)), levels=lv)
  title(main=grupa)
  
  dev.off()
}





















































miasta <- sapply(strsplit(as.character(dat[,5]),"\n"),`[`, 1)

smiasta <- unique(miasta)
res <- list()
for (i in 1:length(smiasta)) {
  res[[i]] <- SmarterPoland::getGoogleMapsAddress(city=smiasta[i], street="mazowieckie")
  cat(i, smiasta[i], "\n")
}
  

plot(sapply(res, `[`, 2), sapply(res, `[`, 1))
text(sapply(res, `[`, 2), sapply(res, `[`, 1), smiasta)



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

