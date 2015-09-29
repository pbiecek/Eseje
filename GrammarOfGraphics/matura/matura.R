# http://smarterpoland.pl/index.php/2013/03/czy-polonisci-sa-mniej-obiektywni-a-matematyki-jest-za-malo-w-liceum/
# http://i.imgur.com/ZR4jdWt.png

library(ZPD)
src = polacz()
gh2011 = pobierz_wyniki_egzaminu(src, 'egzamin gimnazjalny', 'humanistyczna', 2011, TRUE)
matura <- pobierz_wyniki_egzaminu(src, rodzajEgzaminu = "matura", 
                                  czescEgzaminu = "j. polski podstawowa", 2014, TRUE)
as.data.frame(pobierz_testy(src))

matura <- collect(matura)

matura <- list()
for (i in 2010:2015) {
  maturaPolski <- collect(pobierz_wyniki_egzaminu(src, rodzajEgzaminu = "matura", 
                                          czescEgzaminu = "j. polski podstawowa", i, TRUE))
  maturaMatematyka <- collect(pobierz_wyniki_egzaminu(src, rodzajEgzaminu = "matura", 
                                          czescEgzaminu = "matematyka podstawowa", i, TRUE))
  save(maturaPolski, maturaMatematyka, file=paste0("matura",i,".rda"))
  matura[[i - 2009]] <- list(maturaPolski, maturaMatematyka)
}


szkoly <- collect(pobierz_szkoly(src))

save(szkoly, file="szkoly.rda")

suma <- rowSums(matura[,5:35], na.rm=TRUE)
hist(suma,100)

# kor
ro <- 5
mpol <- matura[[ro]][[1]]
dfP <- data.frame(sredniaP=rowSums(mpol[,5:ncol(mpol)], na.rm=TRUE), 
                 obs=mpol[,"id_obserwacji"])
mmat <- matura[[ro]][[2]]
dfM <- data.frame(sredniaM=rowSums(mmat[,5:ncol(mmat)], na.rm=TRUE), 
                 obs=mmat[,"id_obserwacji"])
dfMP <- merge(dfM, dfP)
ggplot(dfMP, aes(sredniaM, sredniaP)) + geom_jitter(alpha=0.1)
ggplot(dfMP, aes(cut(sredniaM/50 + sredniaP/74, breaks = 30),
                 sredniaM/50 - sredniaP/74)) + geom_boxplot(alpha=0.1)
ggplot(dfMP, aes(cut(sredniaM/50 + sredniaP/74, breaks = 30),
                 rank(sredniaM)/304436 - rank(sredniaP)/304436)) + 
  geom_boxplot(alpha=0.1)
       
table(cut(rank(dfMP$sredniaM)/304436, c(0,0.25,0.5,0.75,1)),
      cut(rank(dfMP$sredniaP)/304436, c(0,0.25,0.5,0.75,1)))

tab <- table(matematyka = cut(rank(dfMP$sredniaM)/304436, c(0,0.25,0.5,0.75,1), labels = c("matematyka <25%", "matematyka 25%-50%", "matematyka 50%-75%", "matematyka >75%")),
             polski = cut(rank(dfMP$sredniaP)/304436, c(0,0.25,0.5,0.75,1), labels = c("polski <25%", "polski 25%-50%", "polski 50%-75%", "polski >75%")))

ggplotit:::ggplotit.table(tab, "div") + ylab("") + theme(legend.position="top")

ggplotit(tab)  

   
ggplot(dfM, aes(x=sredniaM)) + stat_ecdf()


ro <- 6
mpol <- matura[[ro]][[2]]
df <- data.frame(srednia=rowSums(mpol[,5:ncol(mpol)], na.rm=TRUE), 
                 mpol[,"id_szkoly"])
dfs <- merge(szkoly, df)

ggplot(dfs, aes(x=srednia, fill=typ_szkoly)) +
  geom_histogram(binwidth=1) + facet_wrap(~typ_szkoly, scales = "free_y")+
  theme(legend.position="none") + ggtitle(ro+2009)

ggplot(dfs, aes(x=srednia)) +
  geom_histogram(binwidth=1)+
  theme(legend.position="none") + 
  ggtitle(ro+2009) +
  geom_vline(xintercept=stats::quantile(dfs$srednia, probs=c(0.1, 0.5, 0.9)), color="red")


allYears <- lapply(matura, function(x) {
  mpol <- x[[1]]
  df1 <- data.frame(srednia=rowSums(mpol[,5:ncol(mpol)], na.rm=TRUE), 
                    mpol[,"rok"], subj = "j. polski podstawowy")
  mmat <- x[[2]]
  df2 <- data.frame(srednia=rowSums(mmat[,5:ncol(mmat)], na.rm=TRUE), 
                    mmat[,"rok"], subj = "matematyka podstawowy")
  rbind(df1, df2)
})

allYearsDF <- do.call(rbind, allYears)

ggplot(allYearsDF, aes(x=srednia, fill=factor(rok))) +
  geom_histogram(binwidth=0.5)+
  theme(legend.position="none") + 
  facet_grid(rok~subj, scales = "free_y")



hist(rowSums(matura[[6]][[1]][,5:67], na.rm=TRUE),100)


