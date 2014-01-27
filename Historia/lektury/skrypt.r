dat <- read.table("pisaze3.csv", sep=",",h=F,fill=TRUE, 
                  encoding="UTF8", fileEncoding="UTF8")
dat <- dat[which(dat[,5] > 1750),]
dat <- dat[order(gsub(as.character(dat[,7]),pattern=" ", replacement="")),]
x1 <- as.numeric(as.character(dat[,2]))
x2 <- as.numeric(as.character(dat[,3]))
x3 <- as.numeric(as.character(dat[,5]))

dat[,4] <- gsub(as.character(dat[,4]), pattern="^ ", replacement="")


pdf("pisaze8.pdf",18,13)
par(mar=c(2,2,2,19))
plot(x2,seq_along(x1), xlim=c(1700,2005), pch=19, cex=0.2, yaxt="n", ylab="", bty="n", type="n", xaxt="n", xlab="")
par(xpd=NA)
for(i in seq_along(x1))
  lines(c(x1[i], 2020),i*c(1,1), lty=2, col="grey85")
par(xpd=FALSE)
rect(x1,seq_along(x1),x2,seq_along(x2)+0.8, border="grey90", col="grey90")
for(i in seq_along(x1))
  for(j in seq(x1[i]+10, x2[i], 20))
    rect(j,i,min(x2[i],j+10),i+0.8, border="grey93", col="grey93")
for(i in seq_along(x1))
  for(j in seq(x1[i]+10, x2[i], 10))
    lines(j*c(1,1), i+c(0,0.8), col="white", lwd=2.5)
points(x3,seq_along(x3)+0.4, pch=19, col="red", cex=2)
axis(1, seq(1700,2020, 10), cex.axis=0.8, line = -1.5)
axis(3, seq(1700,2020, 10), cex.axis=0.8, line = -0.7)
axis(4,
     seq_along(x1)+0.3,
     paste0(dat[,4], " (", dat[,5], ")"), 
     las=1, cex.axis=1.05,
     col="white")
text(x1 - 2,
     seq_along(x3)+0.4,
     paste0(dat[,1], " (", dat[,2],"-", ifelse(dat[,3] < 2015, dat[,3], ""),")"), 
     adj=c(1,0.5), cex=0.9, col="grey40")
dev.off()

