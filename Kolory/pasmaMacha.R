# 
# Przyklady do rozdzialu o kolorach
#

#
# Przyklad: Pasma Macha
#

ncols <- 8

stt <- function(x) (x-min(x))/diff(range(x))

cols1 <- grey(stt(((ncols:1)/ncols)))
cols2 <- grey(stt(((ncols:1)/ncols)^0.35))

pdf("bands8.pdf",10,3)
par(mar=c(0,0,0,0))
plot(c(1,ncols+1)/ncols, c(0,2), type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
for (i in 1:ncols) {
  rect(i/ncols, 0, (i+1)/ncols, 0.98, col=cols1[i], border=NA)
  rect(i/ncols, 1, (i+1)/ncols, 1.98, col=cols2[i], border=NA)
}
rect(1/ncols, 0, 1 + 1/ncols, 0.98, col=NA, border="black")
rect(1/ncols, 1, 1 + 1/ncols, 1.98, col=NA, border="black")
dev.off()


