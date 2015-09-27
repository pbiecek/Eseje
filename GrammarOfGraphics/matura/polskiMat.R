load("~/Dropbox/_Ksiazki_/_jakPokazywacDane_/eseje/matura/matura2015.rda")

dfP <- data.frame(sredniaP=rowSums(maturaPolski[,5:ncol(maturaPolski)], na.rm=TRUE), 
                  obs=maturaPolski[,"id_obserwacji"])
dfM <- data.frame(sredniaM=rowSums(maturaMatematyka[,5:ncol(maturaMatematyka)], na.rm=TRUE), 
                  obs=maturaMatematyka[,"id_obserwacji"])
dfMP <- merge(dfM, dfP)

tab <- table(matematyka = cut(rank(dfMP$sredniaM)/nrow(dfMP), c(0,0.25,0.5,0.75,1), labels = c("matematyka <25%", "matematyka 25%-50%", "matematyka 50%-75%", "matematyka >75%")),
             polski = cut(rank(dfMP$sredniaP)/nrow(dfMP), c(0,0.25,0.5,0.75,1), labels = c("polski <25%", "polski 25%-50%", "polski 50%-75%", "polski >75%")))

pl <- ggplotit:::ggplotit.table(tab, "div") + 
  ylab("") + theme(legend.position="top") +
  ggtitle("Wyniki matura 2015")

pl 

# a2c0d34c2ff4d1752e838ed057d21741
ggsave(pl, file="mozaika2015.pdf", width = 8, height = 8)



# all years
load("~/Dropbox/_Ksiazki_/_jakPokazywacDane_/eseje/matura/matura2015.rda")

lata <- lapply(c("matura2010.rda",  "matura2011.rda",
                 "matura2012.rda",  "matura2013.rda",
                 "matura2014.rda", "matura2015.rda"),
               function(x) {
                 load(x)
                 dfP <- data.frame(sredniaP=rowSums(maturaPolski[,5:ncol(maturaPolski)], na.rm=TRUE), 
                                   obs=maturaPolski[,"id_obserwacji"])
                 dfM <- data.frame(sredniaM=rowSums(maturaMatematyka[,5:ncol(maturaMatematyka)], na.rm=TRUE), 
                                   obs=maturaMatematyka[,"id_obserwacji"])
                 dfMP <- merge(dfM, dfP)
                 dfMP$rok = substr(x, 7,10)
                 dfMP
               })
df <- do.call(rbind, lata)
df <- df[,-1]
colnames(df) = c("podstawowy.matematyka", "podstawowy.j.polski","rok")
maturaExam <- df

save(maturaExam, file="maturaExam.rda", compress = "xz", compression_level = 9)
