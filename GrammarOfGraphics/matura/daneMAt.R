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

df[,1:3] %>%
  group_by(rok, przedmiot, sredniaP) %>%
  summarise(n=n()) %>%
  group_by(rok, przedmiot) %>%
  mutate(n=round(100*n/sum(n),3)) %>%
  spread(rok, n) -> tabi
  
library(xtable)
xtable(tabi)

library(dplyr)
library(tidyr)

res1 <- df %>% 
  group_by(rok, przedmiot) %>%
  dplyr::summarise(srednia = round(mean(sredniaP),1)) %>%
  mutate(rok = paste0("rok_", rok))

res1 %>% 
  spread(rok, srednia) %>%
  as.data.frame

res1
