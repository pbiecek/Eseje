x <- c(2318,2427,2644,2968,3081,3316)
y <- c(1850,1900,2000,2100,2600,2780)

library(RColorBrewer)
library(Cairo)

kol <- brewer.pal(3, "Set1")

pdf("nauczyciele.pdf", 10, 10)
matplot(2005:2010, cbind(x,y,x-y), ylim=c(0,4000), type="o", pch=19,las=1,xlab="", ylab="",
        lty=1,lwd=5,cex=1.2, col=c("steelblue3","blue3","green4"), bty="n", yaxt="n",
        cex.axis=1)
abline(h=seq(0,3500,500),col="grey",lty=3)
axis(2, seq(0,3500,500), las=1,cex.axis=1)
par(xpd=NA)
text(2005:2010,x+150,x,cex=1.2)
text(2005:2010,y+150,y,cex=1.2)
text(2005:2010,x-y+150,x-y,cex=1.2)
par(xpd=F)
legend("top", c("Średnia pensja krajowa", "Średnia pensja nauczycieli", "Różnica pomiędzy średnimi"), 
       ncol=3, col=c("steelblue3","blue3","green4"), bty="n", lwd=5, cex=0.9)
dev.off()





matplot(2005:2010, cbind(x,y), ylim=c(1000,4500), type="l", pch=19,las=1,xlab="", ylab="",
        lty=1,lwd=5,cex=1.5, col=c("steelblue3","blue3"), yaxt="n", bty="n",col.tick="white")
legend("top", c("Średnia pensja nauczycieli", "Średnia pensja krajowa"), ncol=2, col=c("steelblue3","blue3"), bty="n", lwd=5)
par(xpd=NA)
text(2005:2010,x+150,x,cex=0.8)
text(2005:2010,y+150,y,cex=0.8)
par(xpd=F)
