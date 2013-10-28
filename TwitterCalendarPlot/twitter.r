#
library(Cairo)
library(twitteR)
library(UsingR)

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
consumerKey <- ""
consumerSecret <- ""
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake()
registerTwitterOAuth(twitCred)

zz <- searchTwitter("JerzyBuzek")
twListToDF(zz)

zlapane <- twListToDF(zlapane)


write.table(file=paste(adresy[i],".csv",sep=""),zlapane)
       cat("dim", dim(zlapane), "\n")
   }
   a <- getCurRateLimitInfo()
   cat("zostalo ",a$getRemainingHits(), "\n")
   if (a$getRemainingHits() < 30) {
        ile <- round(as.numeric(a$getResetTime()) - as.numeric(Sys.time()))
        cat("Czekam ", ile," sekund \n")
        Sys.sleep(ile)
   }
}



#
# kalendarz
#

##############################################################################
 #                        Calendar Heatmap                                    #
 #                                by                                          #
 #                         Paul Bleicher                                      #
 # an R version of a graphic from:                                            #
 # http://stat-computing.org/dataexpo/2009/posters/wicklin-allison.pdf        #
 #  requires lattice, chron, grid packages                                    #
 ############################################################################## 

## calendarHeat: An R function to display time-series data as a calendar heatmap 
## Copyright 2009 Humedica. All rights reserved.

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You can find a copy of the GNU General Public License, Version 2 at:
## http://www.gnu.org/licenses/gpl-2.0.html


calendarHeat <- function(dates, 
                         values, 
                         ncolors=10, 
                         color="r2b", 
                         varname="Values",startYR="2009",prawyMar = 0,
                         date.form = "%Y-%m-%d", ...) {
require(lattice)
require(grid)
require(chron)
if (class(dates) == "character" | class(dates) == "factor" ) {
  dates <- strptime(dates, date.form)
        }
caldat <- data.frame(value = values, dates = dates)
min.date <- as.Date(paste(format(min(dates), "%Y"),
                    "-1-1",sep = ""))
max.date <- as.Date(paste(format(max(dates), "%Y"),
                     "-12-31", sep = ""))
dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))

# Merge moves data by one day, avoid
caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
dates <- as.Date(dates) 
caldat$value[match(dates, caldat$date.seq)] <- values

caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
yrs <- as.character(unique(caldat$yr))
d.loc <- as.numeric()                        
for (m in min(yrs):max(yrs)) {
  d.subset <- which(caldat$yr == m)  
  sub.seq <- seq(1,length(d.subset))
  d.loc <- c(d.loc, sub.seq)
  }  
caldat <- cbind(caldat, seq=d.loc)

#color styles
r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue                                                                               
r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")   #red to green
w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")   #white to blue
            
assign("col.sty", get(color))
calendar.pal <- colorRampPalette((col.sty), space = "Lab")
def.theme <- lattice.getOption("default.theme")
cal.theme <-
   function() {  
  theme <-
  list(
    strip.background = list(col = "transparent"),
    strip.border = list(col = "transparent"),
    axis.line = list(col="transparent"),
    par.strip.text=list(cex=0.5))
    }
lattice.options(default.theme = cal.theme)
yrs <- (unique(caldat$yr))
nyr <- length(yrs)
ttt <- tapply(caldat$value, caldat$yr,sum,na.rm=T)
dodaj <- paste(names(ttt), paste("(",ttt,")",sep=""), collapse=", ")

theme.novpadding <-
   list(layout.heights =
        list(top.padding = 0,
 	    main.key.padding = 0,
 	    key.axis.padding = 0,
 	    axis.xlab.padding = 0,
 	    xlab.key.padding = 0,
 	    key.sub.padding = 0,
 	    bottom.padding = 0),
        layout.widths =
        list(left.padding = 0,
 	    key.ylab.padding = 0,
 	    ylab.axis.padding = 0,
 	    axis.key.padding = 0,
 	    right.padding = prawyMar))

print(cal.plot <- levelplot(value~woty*dotw | yr,par.settings = theme.novpadding, data=caldat,
   as.table=TRUE,
   aspect=.12, 
 layout = c(1, nyr), # %% 7
   between = list(x=0, y=c(0.3,0.3)),
   strip=TRUE,
   cex.main=0.5,
   main = paste("Liczba ?wierkni?? ", varname, ", ", dodaj,sep = ""),
   scales = list(
     x = list(
               at= c(seq(2.9, 52, by=4.42)),
               labels = c("Sty","Lut","Mar","Kwi","Maj","Cze","Lip","Sie","Wrz","Paz","Lis","Gru"),
               alternating = c(1, rep(0, (nyr-1))),
               tck=0,
               cex = 0.7),
     y=list(
          at = c(0, 1, 2, 3, 4, 5, 6),
          labels = c("Niedziela", "Poniedzialek", "Wtorek", "Sroda", "Czwartek",
                      "Piatek", "Sobota"),
          alternating = 1,
          cex = 0.6,
          tck=0)),
   xlim =c(0.4, 54.6),
   ylim=c(6.6,-0.6),
   cuts= ncolors - 1,
   col.regions = (calendar.pal(ncolors)),
   xlab="" ,
   ylab="",
   colorkey= list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
   subscripts=TRUE
    ) )
panel.locs <- trellis.currentLayout()
for (row in 1:nrow(panel.locs)) {
    for (column in 1:ncol(panel.locs))  {
    if (panel.locs[row, column] > 0)
{
    trellis.focus("panel", row = row, column = column,
                  highlight = FALSE)
xyetc <- trellis.panelArgs()
subs <- caldat[xyetc$subscripts,]
dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
y.start <- dates.fsubs$dotw[1]
y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
dates.len <- nrow(dates.fsubs)
adj.start <- dates.fsubs$woty[1]

for (k in 0:6) {
 if (k < y.start) {
    x.start <- adj.start + 0.5
    } else {
    x.start <- adj.start - 0.5
      }
  if (k > y.end) {
     x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
    } else {
     x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
      }
    grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), 
     default.units = "native", gp=gpar(col = "grey", lwd = 1))
     }
if (adj.start <  2) {
 grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), 
      default.units = "native", gp=gpar(col = "grey", lwd = 1))
 grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
      gp=gpar(col = "grey", lwd = 1))
 grid.lines(x = c(x.finis, x.finis), 
      y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
      gp=gpar(col = "grey", lwd = 1))
 if (dates.fsubs$dotw[dates.len] != 6) {
 grid.lines(x = c(x.finis + 1, x.finis + 1), 
      y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
      gp=gpar(col = "grey", lwd = 1))
      }
 grid.lines(x = c(x.finis, x.finis), 
      y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
      gp=gpar(col = "grey", lwd = 1))
      }
for (n in 1:51) {
  grid.lines(x = c(n + 1.5, n + 1.5), 
    y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
        }
x.start <- adj.start - 0.5

if (y.start > 0) {
  grid.lines(x = c(x.start, x.start + 1),
    y = c(y.start - 0.5, y.start -  0.5), default.units = "native",
    gp=gpar(col = "black", lwd = 1.75))
  grid.lines(x = c(x.start + 1, x.start + 1),
    y = c(y.start - 0.5 , -0.5), default.units = "native",
    gp=gpar(col = "black", lwd = 1.75))
  grid.lines(x = c(x.start, x.start),
    y = c(y.start - 0.5, 6.5), default.units = "native",
    gp=gpar(col = "black", lwd = 1.75))
 if (y.end < 6  ) {
  grid.lines(x = c(x.start + 1, x.finis + 1),
   y = c(-0.5, -0.5), default.units = "native",
   gp=gpar(col = "black", lwd = 1.75))
  grid.lines(x = c(x.start, x.finis),
   y = c(6.5, 6.5), default.units = "native",
   gp=gpar(col = "black", lwd = 1.75))
   } else {
      grid.lines(x = c(x.start + 1, x.finis),
       y = c(-0.5, -0.5), default.units = "native",
       gp=gpar(col = "black", lwd = 1.75))
      grid.lines(x = c(x.start, x.finis),
       y = c(6.5, 6.5), default.units = "native",
       gp=gpar(col = "black", lwd = 1.75))
       }
       } else {
           grid.lines(x = c(x.start, x.start),
            y = c( - 0.5, 6.5), default.units = "native",
            gp=gpar(col = "black", lwd = 1.75))
           }

 if (y.start == 0 ) {
  if (y.end < 6  ) {
  grid.lines(x = c(x.start, x.finis + 1),
   y = c(-0.5, -0.5), default.units = "native",
   gp=gpar(col = "black", lwd = 1.75))
  grid.lines(x = c(x.start, x.finis),
   y = c(6.5, 6.5), default.units = "native",
   gp=gpar(col = "black", lwd = 1.75))
   } else {
      grid.lines(x = c(x.start + 1, x.finis),
       y = c(-0.5, -0.5), default.units = "native",
       gp=gpar(col = "black", lwd = 1.75))
      grid.lines(x = c(x.start, x.finis),
       y = c(6.5, 6.5), default.units = "native",
       gp=gpar(col = "black", lwd = 1.75))
       }
       }
for (j in 1:12)  {
   last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
   x.last.m <- dates.fsubs$woty[last.month] + 0.5
   y.last.m <- dates.fsubs$dotw[last.month] + 0.5
   grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
     default.units = "native", gp=gpar(col = "black", lwd = 1.75))
   if ((y.last.m) < 6) {
      grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
     grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
   } else {
      grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
       default.units = "native", gp=gpar(col = "black", lwd = 1.75))
    }
 }
 }
 }
trellis.unfocus()
} 
lattice.options(default.theme = def.theme)
}






vp <- function (x, ..., skala=rep(1,length(args)), orientation = "vertical", bw = "nrd0", names = NULL, pars = NULL) {
    args <- list(x, ...)
    namedargs <- if (!is.null(attributes(args)$names)) 
        attributes(args)$names != ""
    else rep(FALSE, length = length(args))
    pars <- c(args[namedargs], pars)
    groups <- if (is.list(x)) 
        x
    else args[!namedargs]
    if (0 == (n <- length(groups))) 
        stop("invalid first argument")
    if (length(class(groups))) 
        groups <- unclass(groups)
    if (!missing(names)) 
        attr(groups, "names") <- names
    else {
        if (is.null(attr(groups, "names"))) 
            attr(groups, "names") <- 1:n
        names <- attr(groups, "names")
    }
    xvals <- matrix(0, nrow = 512, ncol = n)
    yvals <- matrix(0, nrow = 512, ncol = n)
    center <- 1:n
    for (i in 1:n) {
      groups[[i]] <- na.omit(groups[[i]])
      if (length(groups[[i]]>0)) {
          tmp.dens <- density(groups[[i]], bw = bw)
          xvals[, i] <- tmp.dens$x
          yvals.needtoscale <- tmp.dens$y
          yvals.scaled <- 7/16 * yvals.needtoscale/max(yvals.needtoscale)
          yvals[, i] <- yvals.scaled
          }
    }
    if (orientation == "vertical") {
        xrange <- c(1/2, n + 1/2)
        yrange <- range(xvals)
    }
    else {
        xrange <- range(xvals)
        yrange <- c(min(yvals), max(yvals))
    }
#    plot.new()
    plot.window(xlim = c(0.5,7.5), ylim = c(0,24),las=1, yaxt="n")
    for (i in 1:n) {
      if (length(groups[[i]]>0)) {
        yvals[yvals[, i] < 0.001,i] = NA
        vlnplt(xvals[, i], yvals[, i]*skala[i], center[i], 
          bordercolor = "black", bgcolor = "black", orientation = orientation, col="grey90")
        points(rep(center[i], length(groups[[i]])), groups[[i]], cex=0.1,pch=19)
      }
    }
}



#
library(ggplot2)
require(xts)

narysujHM <- function(x, startYR=as.POSIXct("2010-01-01", format="%Y-%m-%d"),...) {
  tab <- read.table(paste(x,".csv",sep=""), fill=T)[-1,5:6]
  daty <- apply(tab, 1, function(x) paste(as.character(x[1]), as.character(x[2])))
  daty <- as.POSIXct(daty, format="%Y-%m-%d %H:%M:%S")
  tab <- tab[daty > startYR,]
  daty <- daty[daty > startYR]
  ts=xts(rep(1,times=length(daty)),daty)
  ts.sum=apply.daily(ts,sum) 
  ts.sum.df=data.frame(date=index(ts.sum), coredata(ts.sum))

  colnames(ts.sum.df)=c('date','sum')
  ts.sum.df <- na.omit(ts.sum.df)
  calendarHeat(ts.sum.df$date, ts.sum.df$sum, varname=paste("@",x,sep=""),ncolors=max(ts.sum.df$sum),...)
}


narysujMiPolityka <- function(x,startYR=as.POSIXct("2010-01-01", format="%Y-%m-%d")) {
  tab <- read.table(paste(x,".csv",sep=""), fill=T)[-1,5:6]
  daty <- apply(tab, 1, function(x) paste(as.character(x[1]), as.character(x[2])))
  daty <- as.POSIXct(daty, format="%Y-%m-%d %H:%M:%S")
  tab$month=sapply(daty, function(x) {p=as.POSIXlt(x);p$mon})
  tab$hour=sapply(daty, function(x) {p=as.POSIXlt(x);p$hour + p$min/60})
  tab$wday=sapply(daty, function(x) {p=as.POSIXlt(x);p$wday})
  tab <- na.omit(tab)
  tab <- tab[daty > startYR,]
  daty <- daty[daty > startYR]
  
  if (length(daty) == 0 ) return(0)
  
  CairoPNG(paste(x,".png",sep=""),750,300)                                           
  par(mar=c(2,40.5,3,2))
  plot.new()
  narysujHM(x,color="r2b",prawyMar=30)
  
  vp(tab$hour[tab$wday==0],
        tab$hour[tab$wday==1],
        tab$hour[tab$wday==2],
        tab$hour[tab$wday==3],
        tab$hour[tab$wday==4],
        tab$hour[tab$wday==5],
        tab$hour[tab$wday==6],
       skala = prop.table(table(tab$wday))*6,
      col="grey90", bw=0.3)
  axis(4,at=c(0,6,12,18,24),las=1, cex.axis=0.7)
  axis(1, at = 1:7, labels = c("N","P","W","S","C","P","S"), cex.axis=0.7)
  abline(h=seq(0,24,2),col="grey",lty=3)
  dev.off()
  CairoSVG(paste(x,".svg",sep=""),9,4)
  par(mar=c(2,34,3,2))
  plot.new()
  narysujHM(x,color="r2b",prawyMar=30)
  
  vp(tab$hour[tab$wday==0],
        tab$hour[tab$wday==1],
        tab$hour[tab$wday==2],
        tab$hour[tab$wday==3],
        tab$hour[tab$wday==4],
        tab$hour[tab$wday==5],
        tab$hour[tab$wday==6],
       skala = prop.table(table(tab$wday))*6,
      col="grey90", bw=0.3)
  axis(4,at=c(0,6,12,18,24),las=1, cex.axis=0.7)
  axis(1, at = 1:7, labels = c("N","P","W","S","C","P","S"), cex.axis=0.7)
  abline(h=seq(0,24,2),col="grey",lty=3)
  dev.off()
}



narysujMiPolityka("BarbaraKudrycka")

kogo <- sapply(strsplit(list.files(pattern="csv$"),split="\\."), `[[`, 1)
sapply(kogo, narysujMiPolityka)


zlapane <- userTimeline("Dziennikarz", n=3200)
zlapane <- twListToDF(zlapane)
write.table(file=paste("Dziennikarz.csv",sep=""),zlapane)


library(tm)

kogo <- sapply(strsplit(list.files(pattern="csv$"),split="\\."), `[[`, 1)

x = kogo[1]
opis <- function(x) {
  tekst <- read.table(paste(x,".csv",sep=""),fill=T)[-1,2]
  actaSlownik <- Corpus(DataframeSource(data.frame(tekst)))
  actaSlownik <- tm_map(actaSlownik, removePunctuation)
  actaSlownik <- tm_map(actaSlownik, tolower)
  tdm <- TermDocumentMatrix(actaSlownik)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  c(length(v), sum(v))
}
tab <- sapply(kogo, opis)


# kontrola dziennikarza
  tekst <- read.table("TROJKAdaMalecki.csv",fill=T)[-1,2]
  actaSlownik <- Corpus(DataframeSource(data.frame(tekst)))
  actaSlownik <- tm_map(actaSlownik, removePunctuation)
  actaSlownik <- tm_map(actaSlownik, tolower)
  tdm <- TermDocumentMatrix(actaSlownik)
  m <- as.matrix(tdm)
  ttt <- slownik2[rownames(m),]

  liniaD1 <- sapply((1:28)*100, function(k) {
    tmpk <- rowSums(m[,1:k])
    c(sum(tmpk>0), sum(tmpk), sum(m[!is.na(ttt),1:k]), sum(!is.na(ttt[tmpk>0])), length(unique(ttt[tmpk>0]))-1)
    })


  tekst <- read.table("Dziennikarz.csv",fill=T)[-1,2]
  actaSlownik <- Corpus(DataframeSource(data.frame(tekst)))
  actaSlownik <- tm_map(actaSlownik, removePunctuation)
  actaSlownik <- tm_map(actaSlownik, tolower)
  tdm <- TermDocumentMatrix(actaSlownik)
  m <- as.matrix(tdm)
  ttt <- slownik2[rownames(m),]

  liniaD2 <- sapply((1:30)*100, function(k) {
    tmpk <- rowSums(m[,1:k])
    c(sum(tmpk>0), sum(tmpk), sum(m[!is.na(ttt),1:k]), sum(!is.na(ttt[tmpk>0])), length(unique(ttt[tmpk>0]))-1)
    })



plot(tab[2,], tab[1,], pch=19)

  
  

slownik <- read.table("morfologik.txt", header=F, sep="\t", stringsAsFactors=FALSE)
slownik <- slownik[!duplicated(slownik[,1]),]
rownames(slownik) <- slownik[,1]
slownik2 <- slownik[,2,drop=F]
    
opis <- function(x) {
  tekst <- read.table(paste(x,".csv",sep=""),fill=T)[-1,2]
  actaSlownik <- Corpus(DataframeSource(data.frame(tekst)))
  actaSlownik <- tm_map(actaSlownik, removePunctuation)
  actaSlownik <- tm_map(actaSlownik, tolower)
  tdm <- TermDocumentMatrix(actaSlownik)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  ttt <- slownik2[rownames(m),]
  cat(x,"\n")
  
  length(rownames(m)[!is.na(ttt)]) # roznych slow zmapowanych
  
  c(length(v), sum(v), sum(m[!is.na(ttt),]), sum(!is.na(ttt)), length(unique(ttt))-1)
}

tab2 <- matrix(0,length(kogo),5)
for (i in 1:length(kogo)) {
  tab2[i,] <- opis(kogo[i])
  write(tab2, file="akt.wynik.txt")
}


tab2 <- rbind(tab2[-12,],tab2[12,,drop=F])
kogo <- c(kogo[-12],kogo[12])

#
# RYS 1
#
CairoPNG("twitter5liczb.png", 700, 700)
pairs(tab2[-c(95,97),], pch=19,las=1)
dev.off()


#
# RYS 2
CairoPNG("twitter2c.png", 700, 700)
plot(tab2[-c(95,97),3], tab2[-c(95,97),5],pch=19, xlab="Liczba rozpoznanych polskich s??w", ylab="Liczba r??nych polskich rdzeni",
        ylim=c(2000,4000), xlim=c(5000,16000),las=1)
lines(liniaD1[3,], liniaD1[5,],col="blue",type="l",pch=19,cex=0.5)
lines(liniaD2[3,], liniaD2[5,],col="red",type="l",pch=19,cex=0.5)
in1 <- c(76, 48, 57, 93, 20)
in2 <- c(55, 50, 10, 95)
par(xpd=NA)
text(tab2[in1,3], tab2[in1,5], kogo[in1],adj=c(1,0))
text(tab2[in2,3], tab2[in2,5], kogo[in2],adj=c(0,1))

text(liniaD2[3,18], liniaD2[5,18]+100, "@Dziennikarz", srt=40, col="red")
text(liniaD1[3,23], liniaD1[5,23]+100, "@TROJKAdaMalecki", srt=40, col="blue")
par(xpd=T)
dev.off()


#
CairoPNG("twitter2a.png", 700, 700)
plot(tab2[-c(95,97),5], tab2[-c(95,97),4],pch=19, ylab="Liczba rozpoznanych r??nych polskich s??w", xlab="Liczba r??nych polskich rdzeni",
        xlim=c(2000,3500), ylim=c(3000,5500),las=1)
lines(liniaD1[5,], liniaD1[4,],col="blue",type="l",pch=19,cex=0.5)
lines(liniaD2[5,], liniaD2[4,],col="red",type="l",pch=19,cex=0.5)



in1 <- c(57,20)
in2 <- c(96, 76, 10, 64, 50, 58)
par(xpd=NA)
text(tab2[in1,5], tab2[in1,4], kogo[in1],adj=c(0,1))
text(tab2[in2,5], tab2[in2,4], kogo[in2],adj=c(1,0))

text(liniaD2[5,16], liniaD2[4,16]+100, "@Dziennikarz", srt=40, col="red")
text(liniaD1[5,26], liniaD1[4,26]+100, "@TROJKAdaMalecki", srt=40, col="blue")
par(xpd=T)
dev.off()


plot(tab2[-c(95,97),4]/tab2[-c(95,97),2], tab2[-c(95,97),3]/tab2[-c(95,97),2],pch=19, ylab="Rozpoznanych r??nych s??w", xlab="R??nych rdzeni",las=1)
text(liniaD2[4,16]/liniaD2[2,16], liniaD2[3,16]/liniaD2[2,16], "@Dziennikarz", srt=40, col="red")
text(liniaD1[4,26]/liniaD1[2,16], liniaD1[3,26]/liniaD1[2,16], "@TROJKAdaMalecki", srt=40, col="blue")



#
# RYS 3
#
CairoPNG("twitter2b.png", 700, 700)
plot(tab2[-c(95,97),2], tab2[-c(95,97),1],pch=19, xlab="Liczba wyraz?w", ylab="Liczba r??nych wyraz?w",
        xlim=c(7000,20000), ylim=c(3000,8000),las=1)
lines(liniaD1[2,], liniaD1[1,],col="blue",type="l",pch=19,cex=0.5)
lines(liniaD2[2,], liniaD2[1,],col="red",type="l",pch=19,cex=0.5)
par(xpd=NA)
text(liniaD2[2,16], liniaD2[1,16]+100, "@Dziennikarz", srt=40, col="red")
text(liniaD1[2,19], liniaD1[1,19]+100, "@TROJKAdaMalecki", srt=30, col="blue")

in1 <- c(9,50,76,22)
in2 <- c(87,3,20,69,93,57,46,65,94,92,16,48)
text(tab2[in1,2], tab2[in1,1], kogo[in1],adj=c(0,1))
text(tab2[in2,2], tab2[in2,1], kogo[in2],adj=c(1,0))
par(xpd=T)

dev.off()

#
# Rzs 4
#      
zakim <- function(x) {
  tekst <- as.character(read.table(paste(x,".csv",sep=""),fill=T)[-1,2])
  c(length(grep(tekst, pattern="PO")), length(grep(tekst, pattern="PiS")), length(grep(tekst, pattern="acta",ignore.case=TRUE)), length(tekst))
}
tab5 <- sapply(kogo, zakim)

CairoPNG("twitterPOPiS.png", 700, 700)
plot(tab5[1,]/tab5[4,], tab5[2,]/tab5[4,],xlab="frakcja wiadomo?ci z wyrazem PO",ylab="frakcja wiadomo?ci z wyrazem PiS",log="xy",xlim=c(0.003,0.3),ylim=c(0.001,0.2),cex=1.5, pch=19, col="grey")
abline(0,1)

iii1 <- unlist(sapply(adresy[1:54], function(x) grep(kogo, pattern=x)))
iii2 <- unlist(sapply(adresy[55:88], function(x) grep(kogo, pattern=x)))

points(tab5[1,iii1]/tab5[4,iii1], tab5[2,iii1]/tab5[4,iii1],col="blue",pch=19,cex=1.5)
points(tab5[1,iii2]/tab5[4,iii2], tab5[2,iii2]/tab5[4,iii2],col="orange",pch=19,cex=1.5)

ind3 <- c(76,6,84,19,86,32,25,79,77,44   ,11)
ind4 <- c(23,62,63,41,85,61,10,40,53,     9,47)
ind3a <- c(3,54)
text(tab5[1,ind3]/tab5[4,ind3], tab5[2,ind3]/tab5[4,ind3],kogo[ind3],adj=c(0,1.3))
text(tab5[1,ind4]/tab5[4,ind4], tab5[2,ind4]/tab5[4,ind4],kogo[ind4],adj=c(1,-0.3))
text(tab5[1,ind3a]/tab5[4,ind3a], tab5[2,ind3a]/tab5[4,ind3a],kogo[ind3a],adj=c(0,-0.3))
dev.off()

identify(tab5[1,]/tab5[4,], tab5[2,]/tab5[4,],1:97)

#
#
#
colnames(tab5) = paste("@",colnames(tab5),sep="")

CairoPNG("twitterActa.png", 600, 500)
par(mar=c(5,10,3,3))
a <- barplot(sort(tab5[3,])[68:96],horiz=T,las=1, main="# wyst?pie? s?owa ACTA")
abline(v=seq(10,100,10),col="grey",lty=3)
par(xpd=NA)
text(sort(tab5[3,])[68:96],a,sort(tab5[3,])[68:96],adj=c(-0.5,0.5))
par(xpd=F)
dev.off()


