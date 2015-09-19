load("~/Dropbox/_Ksiazki_/_jakPokazywacDane_/eseje/matura/matura2011.rda")

dfP <- data.frame(sredniaP=rowSums(maturaPolski[,5:ncol(maturaPolski)], na.rm=TRUE), 
                  obs=maturaPolski[,"id_obserwacji"])
dfM <- data.frame(sredniaM=rowSums(maturaMatematyka[,5:ncol(maturaMatematyka)], na.rm=TRUE), 
                  obs=maturaMatematyka[,"id_obserwacji"])
dfMP <- merge(dfM, dfP)

tab <- table(matematyka = cut(rank(dfMP$sredniaM)/nrow(dfMP), c(0,0.25,0.5,0.75,1), labels = c("matematyka <25%", "matematyka 25%-50%", "matematyka 50%-75%", "matematyka >75%")),
             polski = cut(rank(dfMP$sredniaP)/nrow(dfMP), c(0,0.25,0.5,0.75,1), labels = c("polski <25%", "polski 25%-50%", "polski 50%-75%", "polski >75%")))

pl <- ggplotit:::ggplotit.table(tab, "div") + 
  ylab("") + theme(legend.position="top") +
  ggtitle("Wyniki matura 2011")

pl 

ggsave(pl, file="mozaika2011.pdf", width = 8, height = 8)
