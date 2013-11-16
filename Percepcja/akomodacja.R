x <- seq(4,60,4)
y <- c(20,16,# 4 8
       14,12.5,#16
       11,10,#24
       9,8,#32
       6.8,5.5,#40
       4,2.5,#48
       1.5,1.2,#56
       1)

pdf("akomodacja.pdf",7,8)

plot(x,y, las=1, ylim=c(0,21),xlim=c(0,62), xaxs="i",yaxs="i",
     xlab="wiek", ylab="typowa zdolność skupiania (D)",
     type="o", pch=19,bty="n")
abline(h=seq(0,20,2.5), lty=3, col="#00000077")
abline(v=seq(0,60,5), lty=3, col="#00000077")

dev.off()


# zdrodlo:
# http://www.visioncareeducation.com/article.aspx?article=102546&a=cls

