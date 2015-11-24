# Scale_fill_brewer

library(ggplot2)
library(latticeExtra)
library(lattice)
library(gridExtra)
df <- data.frame(a=factor(1:8))

pl1 <- list()
pl2 <- list()
pl3 <- list()
for (i in 1:8) {
  pl1[[i]] <- ggplot(df, aes(x=a,fill=a)) +
    geom_bar() +
    scale_fill_brewer(type="qual",palette = i) +
    theme(axis.title.x = element_text(size=0, color="black"),
          axis.title.y = element_text(size=0, color="black"),
          axis.text.y = element_text(size=0, color="black"),
          axis.text.x = element_text(size=0, color="black"),
          line = element_blank(), rect = element_blank(), #text = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "lines"), complete = TRUE,
          legend.position="none") +
    ggtitle(paste("type = qual, palette =",i,""))
  pl2[[i]] <- pl1[[i]] + scale_fill_brewer(type="div",palette = i) +
    ggtitle(paste("type = div, palette =",i,""))
  pl3[[i]] <- pl1[[i]] + scale_fill_brewer(type="seq",palette = i) +
    ggtitle(paste("type = seq, palette =",i,""))
}

pl <- c(pl1, pl2, pl3)
pl4 <- sapply(1:8, function(i) list(pl1[[i]], pl2[[i]], pl3[[i]]))
pl$ncol=1
pl$nrow =24
pl4$ncol=3
pl4$nrow =8

pl1$ncol=1
pl1$nrow =8
pl2$ncol=1
pl2$nrow =8
pl3$ncol=1
pl3$nrow =8
do.call(grid.arrange, pl1)
do.call(grid.arrange, pl2)
do.call(grid.arrange, pl3)

pdf("palette_fill.pdf",2,14)
do.call(grid.arrange, pl)
dev.off()

pdf("palette_fill3.pdf",7,5)
do.call(grid.arrange, pl4)
dev.off()
