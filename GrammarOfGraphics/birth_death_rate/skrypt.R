# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)

rates <- read.xlsx("data.xlsx",1)[-1,]
rates[,2] <- as.numeric(as.character(rates[,2]))
rates[,3] <- as.numeric(as.character(rates[,3]))

colnames(rates) <- c("country","Birth_rate","Death_rate")

library(ggplot2)

pl <- ggplot(rates, aes(x=Birth_rate, y=Death_rate, 
                  color=country=="Poland",
                  size=country=="Poland")) +
  geom_point() + coord_fixed() +
  geom_abline(xintercept=0,slope=1) + 
  scale_color_manual(values=c("black","red"))+
  theme(legend.position="none")

pl

ggsave(plot = pl, filename="rates.png", width = 15, height = 10)

