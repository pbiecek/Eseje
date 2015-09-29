# symbole

pdf("symbole.pdf",7,7, useDingbats=FALSE)
plot(1:10,rep(1,10), pch=1:10, cex=2)
dev.off()
