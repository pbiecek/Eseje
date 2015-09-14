# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)
library(SmarterPoland)

head(countries)


# boxplot
plB <- ggplot(countries, aes(x = continent, y = birth.rate)) +
  geom_boxplot(fill="grey", coef = 3) +
  theme_bw()

ggsave(plB, filename = "geomBoxplot.pdf", width = 7, height = 7, useDingbats=FALSE)




