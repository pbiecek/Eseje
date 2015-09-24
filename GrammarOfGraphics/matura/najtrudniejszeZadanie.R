colMeans(maturaMatematyka[,5:ncol(maturaMatematyka)]>0, na.rm=TRUE)

colMeans(maturaMatematyka[,5:ncol(maturaMatematyka)]>0, na.rm=TRUE) -> sc
sc[order(names(sc))][35:68]

punkty <- c(rep(1, 25), rep(2, 6), rep(4, 2), 5)
barplot(sc[order(names(sc))][35:68] , horiz = TRUE, las=1, col=punkty)





apply(maturaMatematyka[,5:ncol(maturaMatematyka)], 2, function(x) length(unique(x))) 

maturaMatematyka[,"k_9394"]

library(scales)

df <- data.frame(zadanie = factor(paste("Zadanie", 1:34), levels=paste("Zadanie", 1:34)),
           procent = sc[order(names(sc))][35:68], 
           Max.punktow.za.zadanie = factor(punkty))
# 21df17c8dabda20740a6db7f9cb4a93f
pl <- ggplot(df, aes(x=zadanie, y=procent, fill=Max.punktow.za.zadanie)) +
  geom_bar(stat="identity") +
  coord_flip() + theme_bw() + scale_y_continuous(label=percent, limits=c(0,1)) +
  ylab("Procent odpowiedzi ocenionych na >0 punktów") + xlab("") + 
  scale_fill_brewer(type="div", palette = 4) +theme(legend.position="bottom") +
  ggtitle("Matematyka, poziom podstawowy, 2015")
