library(Diagnoza)

table(osoby$województwo, osoby$ap3)

t(apply(round(prop.table(table(osoby$województwo, osoby$ap3),1)*1000), 1, cumsum))
