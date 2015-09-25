setwd("~/Dropbox/_Ksiazki_/_jakPokazywacDane_/eseje/matura/")

lata <- lapply(c("matura2010.rda",  "matura2011.rda",
         "matura2012.rda",  "matura2013.rda",
         "matura2014.rda", "matura2015.rda"),
       function(x) {
         load(x)
         dfP <- data.frame(sredniaP=rowSums(maturaPolski[,5:ncol(maturaPolski)], na.rm=TRUE), 
                    obs=maturaPolski[,"rok"], 
                    przedmiot="j. polski podstawowy")
         dfM <- data.frame(sredniaP=rowSums(maturaMatematyka[,5:ncol(maturaMatematyka)], na.rm=TRUE), 
                    obs=maturaMatematyka[,"rok"], 
                    przedmiot="matematyka podstawowy")
         rbind(dfP, dfM)
       })
df <- do.call(rbind, lata)


ll <- theme_bw()
ll$legend.position = "none"

pl <- ggplot(df, aes(sredniaP, fill=przedmiot)) +
  geom_histogram(binwidth=1, color="white", size=0.1) + facet_grid(rok~przedmiot, scales = "free") +
  xlab("Liczba punktów na maturze") + ggtitle("Wyniki matur poziom podstawowy") +
  scale_fill_manual(values=c("red3","blue3")) +
  ll + ylab("")

# 62400e7e6a0b300dd17c0fb29e3655ab
pl 

archivist::aread("pbiecek/graphGallery/62400e7e6a0b300dd17c0fb29e3655ab")

ggsave(pl, file="calaMacierz.pdf", width = 8, height = 10)




#
# Ze zmienną matura
#

ggplot(matura, aes(punkty, fill=przedmiot)) +
  geom_histogram(binwidth=1, color="white", size=0.1) + facet_grid(rok~przedmiot, scales = "free_x") +
  xlab("Liczba punktów na maturze") + ggtitle("Wyniki matur poziom podstawowy") +
  scale_fill_manual(values=c("red3","blue3")) +
  ll + ylab("")

