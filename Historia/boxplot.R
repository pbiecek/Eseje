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



library(ggplot2)
library(Cairo)
CairoPDF("Bertin.pdf", 14, 6)
grid.newpage()
p1 <- ggplot(aes(x = Sepal.Length, y = Sepal.Width, col=Species), data=iris) + geom_point(size=5) + theme_bw() + theme(legend.position="top")
p2 <- ggplot(aes(x = Sepal.Length, y = Sepal.Width, shape=Species), data=iris) + geom_point(size=5) + theme_bw() + theme(legend.position="top")
p3 <- ggplot(aes(x = Sepal.Length, y = Sepal.Width, size=Species), data=iris) + geom_point() + theme_bw() + theme(legend.position="top")

pushViewport(viewport(layout = grid.layout(1, 3)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p3, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
dev.off()


