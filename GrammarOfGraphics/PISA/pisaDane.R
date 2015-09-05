library(PISA2012lite)
library(dplyr)

pisa <- student2012 %>%
  select(CNT, ST04Q01, PV1MATH, PV1READ, W_FSTUWT) %>%
  filter(CNT %in% c("Austria", "Belgium", "Bulgaria", "Switzerland",
                    "Czech Republic", "Germany", "Denmark", "Spain", "Estonia",             
                    "Finland", "France", "United Kingdom", "Greece", "Croatia", 
                    "Hungary", "Ireland", "Iceland", "Italy", "Liechtenstein", "Lithuania",              
                    "Luxembourg", "Latvia", "Netherlands", "Norway", "Poland", "Portugal",              
                    "Romania", "Russian Federation", "Serbia", "Slovak Republic", "Slovenia",              
                    "Sweden")) %>%
  group_by(CNT, ST04Q01) %>%
  summarize(math = weighted.mean(PV1MATH, W_FSTUWT),
            read = weighted.mean(PV1READ, W_FSTUWT))


ggplot(pisa, aes(math, read)) + geom_point()
