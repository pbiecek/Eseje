# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)
library(SmarterPoland)

head(countries)

# density
ggplot(ratesSmall, aes(x=Birth_rate)) +
  stat_density() + 
  geom_rug() + 
  theme_bw() 
