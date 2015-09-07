# dane z WHO
# http://apps.who.int/gho/data/node.main.GNI107?lang=en

library(openxlsx)

rates <- read.xlsx("gini_Indicator.xlsx",1)[-1,]

gini <- t(rates[rates[,1] == "Poland",-(1:4)])
plot(as.numeric(rownames(gini)), gini)

rates <- read.xlsx("GINIcnt.xls",1)[-1,]
for (i in 2:7)
  rates[,i] <- as.numeric(as.character(rates[,i]))

colnames(rates) <- c("country","Birth_rate","Death_rate", "Population", "Population.under.15", "Population.over.60","Population.med.age", "Population.urban")
