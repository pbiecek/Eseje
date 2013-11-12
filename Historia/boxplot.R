setwd("/Users/pbiecek/camtasia/GitHub/Eseje/Historia/")

library(UsingR)
library(Hmisc)
library(hdrcde)
library(beanplot)

pdf("boxplot.pdf", 15,7)
library(Hmisc)
library(hdrcde)
x <- c(rnorm(100,-2,3),
       rnorm(50,5,3),
       rnorm(50,6,1))
par(mfrow=c(1,5))
simple.violinplot(x, ylim=c(-12,15))
bpplot(x, ylim=c(-12,15))
boxplot(x, ylim=c(-12,15))
beanplot(x, ylim=c(-12,15))
hdr.boxplot(x, ylim=c(-12,15))
dev.off()





pdf("anscombe.pdf",10,7)

data(anscombe)
par(mfrow=c(2,2), mar=c(2,2,1,1))
for (i in 1:4) {
  plot(anscombe[,i],anscombe[,4+i],xlab="",ylab="",pch=18, xlim=c(4,20),ylim=c(5,13), cex=2.5, las=1)
  abline(lm(anscombe[,4+i]~anscombe[,i]), lwd=2, col='grey', lty=2)

}

dev.off()
