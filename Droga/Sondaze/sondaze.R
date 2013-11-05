library(ggplot2)
library(reshape2)

partie  <- c("PO", "PIS", "RP", "PSL","SLD")
kolory <- c("orange", "blue3", "gold", "green3", "red3")
smgkrc <- read.table("smgkrc.txt", sep=";", row.names=1)
colnames(smgkrc) <- partie


pdf("zmiana.pdf",12,5)
delta <- round(smgkrc[1,1:5] - smgkrc[2,1:5])
napis <- paste0(round(smgkrc[1,1:5]),"%", 
    ifelse(abs(delta) < 1, "(bez zmian)",
    paste0(ifelse(delta < 0, " (spadek o ", " (wzrost o "), abs(delta),"%)")))
pp <- barplot(as.vector(as.matrix(smgkrc[1,1:5])), horiz=TRUE, las=1, xlim=c(0,50), col=as.matrix(kolory), border=kolory)
text( as.matrix(smgkrc[1,1:5]) + 2, pp , napis, adj=c(0,1))
axis(2, at=pp, partie, las=1, col.ticks="white")
dev.off()


daty <- as.Date(rownames(smgkrc), "%d/%m/%Y")
smgkrc$daty <- daty

smgkrc2 <- melt(smgkrc[,c(1,2,6)], id.vars=c(3))
smgkrc2$UI90 <- smgkrc2$value + 1.64*sqrt(smgkrc2$value*(100-smgkrc2$value)/500)
smgkrc2$LI90 <- smgkrc2$value - 1.64*sqrt(smgkrc2$value*(100-smgkrc2$value)/500)

pdf("poparcie2lata.pdf", 12, 8)
ggplot(aes(x=daty, y=value, col=variable), data= smgkrc2) + geom_point(size=5) + geom_line()+ theme_bw() + theme(legend.position="none")+ ylab("poparcie dla partii") + xlab("")+scale_color_manual(values=c("orange", "blue3"))+ scale_x_date(expand = 0.01*c(1, 1)) + ylim(16,50.3)+scale_fill_manual(values=c("orange", "blue3"))
dev.off()


pdf("poparcie2lataWstega.pdf", 12, 8)
ggplot(aes(x=daty, y=value, col=variable), data= smgkrc2) + geom_point(size=5) + geom_line()+ theme_bw()  + geom_ribbon(aes(ymin = UI90, ymax=LI90, fill=variable), alpha=I(0.3), col="#ffffff00")+ theme(legend.position="none")+ ylab("poparcie dla partii") + xlab("")+scale_color_manual(values=c("orange", "blue3"))+ scale_x_date(expand = 0.01*c(1, 1)) + ylim(16,50.3)+scale_fill_manual(values=c("orange", "blue3"))
dev.off()


pdf("poparcie2lataSlupki.pdf", 12, 6)
ggplot(aes(x=daty, y=value, col=variable), data= smgkrc2) + theme_bw()  + geom_errorbar(aes(ymin = UI90, ymax=LI90, color=variable), width=10)+ theme(legend.position="none")+ ylab("poparcie dla partii") + xlab("")+scale_color_manual(values=c("orange", "blue3"))+ scale_x_date(expand = 0.01*c(1, 1)) + ylim(16,50.3)
#geom_point(size=5) + 
dev.off()


pdf("poparcie2lataSlupkiBlade.pdf", 12, 8)
ggplot(aes(x=daty, y=value, col=variable), data= smgkrc2) + geom_point(size=5, alpha=0.5) + theme_bw()  + geom_errorbar(aes(ymin = UI90, ymax=LI90, color=variable), alpha=0.5, width=10)+ theme(legend.position="none") + ylab("poparcie dla partii") + xlab("")+scale_color_manual(values=c("orange", "blue3"))+ scale_x_date(expand = 0.01*c(1, 1)) + ylim(16,50.3)
dev.off()


pdf("poparcie2lataSlupkiTrend32.pdf", 12, 8)
ggplot(aes(x=daty, y=value, col=variable), data= smgkrc2) + geom_smooth(se=FALSE, size=3, span=0.32) + theme_bw()  + geom_errorbar(aes(ymin = UI90, ymax=LI90, color=variable), alpha=0.5, width=10)+ theme(legend.position="none") + ylab("poparcie dla partii") + xlab("")+scale_color_manual(values=c("orange", "blue3"))+ scale_x_date(expand = 0.01*c(1, 1)) + ylim(16,50.3)
#geom_point(size=5, alpha=0.5) + 
dev.off()

pdf("poparcie2lataSlupkiTrend100.pdf", 12, 8)
ggplot(aes(x=daty, y=value, col=variable), data= smgkrc2)  + geom_smooth(se=FALSE, size=3, span=51, degree=1) + theme_bw()  + geom_errorbar(aes(ymin = UI90, ymax=LI90, color=variable), alpha=0.5, width=10)+ theme(legend.position="none") + ylab("poparcie dla partii") + xlab("")+scale_color_manual(values=c("orange", "blue3"))+ scale_x_date(expand = 0.01*c(1, 1)) + ylim(16,50.3)
#+ geom_point(size=5, alpha=0.5)
dev.off()

pdf("poparcie2lataSlupkiTrend1.pdf", 12, 8)
ggplot(aes(x=daty, y=value, col=variable), data= smgkrc2) +geom_smooth(se=FALSE, size=3, span=0.1) + theme_bw()  + geom_errorbar(aes(ymin = UI90, ymax=LI90, color=variable), alpha=0.5, width=10)+ theme(legend.position="none") + ylab("poparcie dla partii") + xlab("")+scale_color_manual(values=c("orange", "blue3"))+ scale_x_date(expand = 0.01*c(1, 1))+ ylim(16,50.3)
# geom_point(size=5, alpha=0.5) + 
dev.off()


sapply(seq(0.1,0.5,0.02), function(x) {
	mod <- loess(value~as.numeric(daty), data = smgkrc2[smgkrc2$variable == "PO",], span=x)
	c(x, mean(mod$fitted <= smgkrc2$UI90[smgkrc2$variable == "PO"] & mod$fitted >= smgkrc2$LI90[smgkrc2$variable == "PO"]))
})*0.5 + 
sapply(seq(0.1,0.5,0.02), function(x) {
	mod <- loess(value~as.numeric(daty), data = smgkrc2[smgkrc2$variable == "PIS",], span=x)
	c(x, mean(mod$fitted <= smgkrc2$UI90[smgkrc2$variable == "PIS"] & mod$fitted >= smgkrc2$LI90[smgkrc2$variable == "PIS"]))
})*0.5




cutoffs <- seq(0.01,.35,0.01)
for (co in cutoffs) {
	ncols <- cbind(smgkrc2$value + qnorm(co)*sqrt(smgkrc2$value*(100-smgkrc2$value)/500),
	      smgkrc2$value - qnorm(co)*sqrt(smgkrc2$value*(100-smgkrc2$value)/500))
	colnames(ncols) = paste0(c("UI", "LI"), 100*co)
	smgkrc2 <- cbind(smgkrc2, ncols)
}


pl <- ggplot(aes(x=daty, y=value, col=variable), data= smgkrc2) +theme_bw()  + ylab("poparcie dla partii") + xlab("")+scale_color_manual(values=c("orange", "blue3"))
for (co in cutoffs) {
 pl <- pl +   
    geom_ribbon(aes_string(ymin = paste0("UI", 100*co), ymax=paste0("LI", 100*co),
    fill="variable"), alpha=I(0.03), col="#ffffff00")
}
pdf("poparcie2lataRozmazane.pdf", 12, 8)
pl+ theme(legend.position="none") + ylab("poparcie dla partii") + xlab("")+scale_fill_manual(values=c("orange", "blue3"))+ scale_x_date(expand = 0.01*c(1, 1)) + ylim(16,50)
dev.off()




smgkrc2 <- melt(smgkrc[1:10,c(1,2,6)], id.vars=c(3))

