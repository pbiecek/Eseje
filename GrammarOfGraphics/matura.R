# http://smarterpoland.pl/index.php/2013/03/czy-polonisci-sa-mniej-obiektywni-a-matematyki-jest-za-malo-w-liceum/
# http://i.imgur.com/ZR4jdWt.png

library(ZPD)
src = polacz()
gh2011 = pobierz_wyniki_egzaminu(src, 'egzamin gimnazjalny', 'humanistyczna', 2011, TRUE)
matura <- pobierz_wyniki_egzaminu(src, rodzajEgzaminu = "matura", czescEgzaminu = "j. polski podstawowa", 2014, TRUE)
as.data.frame(pobierz_testy(src))

matura <- collect(matura)

suma <- rowSums(matura[,5:35], na.rm=TRUE)
hist(suma,100)


summary(matura$id_szkoly)
