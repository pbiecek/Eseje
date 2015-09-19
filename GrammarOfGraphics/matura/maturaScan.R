setwd("~/Dropbox/_Ksiazki_/_jakPokazywacDane_/eseje/matura/")

lata <- lapply(c("matura2010.rda",  "matura2011.rda",
         "matura2012.rda",  "matura2013.rda",
         "matura2014.rda",  "matura2015.rda"),
       function(x) {
         load(x)
         data.frame(sredniaP=rowSums(maturaPolski[,5:ncol(maturaPolski)], na.rm=TRUE), 
                           obs=maturaPolski[,"rok"])
       })
df <- do.call(rbind, lata)


pl <- ggplot(df, aes(sredniaP)) +
  geom_histogram(binwidth=1) + facet_wrap(~rok)

pl 

ggsave(pl, file="gestoscPolski.pdf", width = 8, height = 8)



pl <- ggplot(df, aes(sredniaP/74, colour=factor(rok))) +
  stat_ecdf() + scale_color_brewer(type="qual", palette = 2, name="Rok matury") + 
  theme_bw() + scale_y_continuous(label=percent, name="Procent osób o wyniku gorszym") +
  scale_x_continuous(label=percent, name="Procent maksymalnej liczby punktów", limits=c(0,1)) +
  ggtitle("Matury z j. polskiego poziom podstawowy") 

pl 

ggsave(pl, file="dystrybuantaPolski.pdf", width = 8, height = 8)
