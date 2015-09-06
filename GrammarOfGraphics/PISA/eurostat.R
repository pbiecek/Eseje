library(SmarterPoland) 
library(dplyr) 
library(ggplot2) 

milk <- getEurostatRCV("apro_mk_pobta")

EUcc <- c('AL', 'AD', 'AM', 'AT', 'BY', 'BE', 'BA', 'BG', 'CH', 'CY', 'CZ', 'DE',
          'DK', 'EE', 'ES', 'FO', 'FI', 'FR', 'GB', 'GE', 'GI', 'GR', 'HU', 'HR',
          'IE', 'IS', 'IT', 'LT', 'LU', 'LV', 'MC', 'MK', 'MT', 'NO', 'NL', 'PL',
          'PT', 'RO', 'RU', 'SE', 'SI', 'SK', 'SM', 'TR', 'UA', 'VA')

cowMilk <- milk %>%
  filter(milkitem == "PRO" & prodmilk == "MC001") %>%
  filter(geo %in% EUcc)

ggplot(cowMilk, aes(x=time, y=value, fill=geo == "PL")) +
  geom_bar(stat="identity") + coord_flip()

cowMilk2 <- cowMilk %>%
  mutate(isPL = factor(geo == "PL")) %>%
  group_by(isPL, time) %>%
  summarise(total = sum(value, na.rm=TRUE))

ggplot(cowMilk2, 
       aes(x=time, y=total, fill=isPL)) +
  geom_bar(stat="identity", position="fill") + 
  coord_cartesian(ylim=c(0.85,1))

sort(tapply(cowMilk$value, cowMilk$geo, sum, na.rm=TRUE))

boxplot(cowMilk$value~cowMilk$geo)
