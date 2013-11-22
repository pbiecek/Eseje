library(reshape2)
library(ggplot2)

mat <- matrix(0, 500, 250)
colnames(mat) <- 1:250
rownames(mat) <- 1:500
mmat <- melt(mat)[,1:2]

h = 0
kols <- apply(mmat, 1, function(x) hcl(h, x[1]/2.5, x[2]/2.5, fixup=FALSE))
plot(mmat[,1]/2.5, mmat[,2]/2.5, col=kols, pch=20, las=1, xlab="Natężenie", ylab="Jasność", bty="n", main="Barwa = 0")

h = 90
kols <- apply(mmat, 1, function(x) hcl(h, x[1]/2.5, x[2]/2.5, fixup=FALSE))
plot(mmat[,1]/2.5, mmat[,2]/2.5, col=kols, pch=20, las=1, xlab="Natężenie", ylab="Jasność", bty="n", main="Barwa = 90")

h = 180
kols <- apply(mmat, 1, function(x) hcl(h, x[1]/2.5, x[2]/2.5, fixup=FALSE))
plot(mmat[,1]/2.5, mmat[,2]/2.5, col=kols, pch=20, las=1, xlab="Natężenie", ylab="Jasność", bty="n", main="Barwa = 180")

h = 270
kols <- apply(mmat, 1, function(x) hcl(h, x[1]/2.5, x[2]/2.5, fixup=FALSE))
plot(mmat[,1]/2.5, mmat[,2]/2.5, col=kols, pch=20, las=1, xlab="Natężenie", ylab="Jasność", bty="n", main="Barwa = 270")


mat <- matrix(0, 360, 250)
colnames(mat) <- 1:250
rownames(mat) <- 1:360
mmat <- melt(mat)[,1:2]

kols <- apply(mmat, 1, function(x) hcl(x[1], x[2]/2, 50, fixup=FALSE))
plot(mmat[,1], mmat[,2]/2.5, col=kols, pch=20, las=1, xlab="Natężenie", ylab="Intensywność", bty="n", main="")

kols <- apply(mmat, 1, function(x) hcl(x[1], x[2]/1.5, 50, fixup=FALSE))
ggplot(aes(x=mmat[,1], y=mmat[,2]/1.5), data=mmat) + geom_point(color=I(kols)) + coord_polar() + theme_bw() + xlab("Barwa") + 
  ylab("Natężenie") + scale_x_continuous(breaks = seq(0,360,30))








mat <- matrix(0, 360, 250)
colnames(mat) <- 1:250
rownames(mat) <- 1:360
mmat <- melt(mat)[,1:2]

kols <- apply(mmat, 1, function(x) hcl(x[1], x[2]/1.5, 20, fixup=FALSE))
p1 <- ggplot(aes(x=mmat[,1], y=mmat[,2]/1.5), data=mmat) + geom_point(color=I(kols)) + coord_polar() + theme_bw() + xlab("Barwa") + 
  ylab("Natężenie") + scale_x_continuous(breaks = seq(0,360,30)) + ggtitle("Jasność = 20") 

kols <- apply(mmat, 1, function(x) hcl(x[1], x[2]/1.5, 50, fixup=FALSE))
p2 <- ggplot(aes(x=mmat[,1], y=mmat[,2]/1.5), data=mmat) + geom_point(color=I(kols)) + coord_polar() + theme_bw() + xlab("Barwa") + 
  ylab("Natężenie") + scale_x_continuous(breaks = seq(0,360,30)) + ggtitle("Jasność = 50") 

kols <- apply(mmat, 1, function(x) hcl(x[1], x[2]/1.5, 80, fixup=FALSE))
p3 <- ggplot(aes(x=mmat[,1], y=mmat[,2]/1.5), data=mmat) + geom_point(color=I(kols)) + coord_polar() + theme_bw() + xlab("Barwa") + 
  ylab("Natężenie") + scale_x_continuous(breaks = seq(0,360,30)) + ggtitle("Jasność = 80") 


mat <- matrix(0, 500, 250)
colnames(mat) <- 1:250
rownames(mat) <- 1:500
mmat2 <- melt(mat)[,1:2]

h = 10
kols <- apply(mmat2, 1, function(x) hcl(h, x[1]/2.5, x[2]/2.5, fixup=FALSE))
p4 <- ggplot(aes(x=mmat2[,1]/2.5, y=mmat2[,2]/2.5), data=mmat2) + geom_point(color=I(kols)) +
  xlab("Natężenie") + ylab("Jasność") + ggtitle("Barwa = 10") +  theme_bw() 

h = 130
kols <- apply(mmat2, 1, function(x) hcl(h, x[1]/2.5, x[2]/2.5, fixup=FALSE))
p5 <- ggplot(aes(x=mmat2[,1]/2.5, y=mmat2[,2]/2.5), data=mmat2) + geom_point(color=I(kols)) +
  xlab("Natężenie") + ylab("Jasność") + ggtitle("Barwa = 130") +  theme_bw() 

h = 260
kols <- apply(mmat2, 1, function(x) hcl(h, x[1]/2.5, x[2]/2.5, fixup=FALSE))
p6 <- ggplot(aes(x=mmat2[,1]/2.5, y=mmat2[,2]/2.5), data=mmat2) + geom_point(color=I(kols)) +
  xlab("Natężenie") + ylab("Jasność") + ggtitle("Barwa = 260") +  theme_bw() 


setwd("/Users/pbiecek/camtasia/GitHub/Eseje/Kolory/")

pdf("skalaHCL.pdf", 15, 10)
grid.newpage()
print(p1, vp=viewport(0.16,0.25,0.33, 0.5))
print(p2, vp=viewport(0.16+0.33,0.25,0.33, 0.5))
print(p3, vp=viewport(0.16+0.66,0.25,0.33, 0.5))

print(p4, vp=viewport(0.16,0.75,0.33, 0.5))
print(p5, vp=viewport(0.16+0.33,0.75,0.33, 0.5))
print(p6, vp=viewport(0.16+0.66,0.75,0.33, 0.5))
dev.off()
