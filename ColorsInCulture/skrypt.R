library(openxlsx)

dane <- read.xlsx("Colours & Cultures 2.xlsx",1)

sort(table(tolower(unlist(dane[,c(2,12,6,13,11,105,)]))))
