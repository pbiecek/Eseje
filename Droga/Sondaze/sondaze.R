partie  <- c("PO", "PIS", "RP", "PSL","SLD")
kolory <- c("orange", "blue3", "gold", "green3", "red3")

smgkrc <- read.table("smgkrc.txt", sep=";", row.names=1)
colnames(smgkrc) <- partie

daty <- as.Date(rownames(smgkrc), "%d/%m/%Y")
smgkrc$daty <- daty

library(ggplot2)
library(reshape2)
smgkrc2 <- melt(smgkrc[,c(1,2,6)], id.vars=c(3))

ggplot(aes(x=daty, y=value, col=variable), data= smgkrc2) + geom_point() + geom_smooth() + theme_bw()

