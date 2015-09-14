# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)
library(SmarterPoland)

head(countries)

# dotplot
plD <- ggplot(countries, aes(x = continent, y = birth.rate)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.7) +
  theme_bw()

ggsave(plD, filename = "geomDotplot.pdf", width = 7, height = 7, useDingbats=FALSE)



# dotplot
plP <- ggplot(countries, aes(x = birth.rate, y =death.rate)) +
  geom_point() +
  theme_bw()

ggsave(plP, filename = "geomPoint.pdf", width = 7, height = 7, useDingbats=FALSE)



# jitter
plJ <- ggplot(countries, aes(x = continent, y =birth.rate)) +
  geom_jitter(position = position_jitter(width = .2)) +
  theme_bw()

ggsave(plJ, filename = "geomJitter.pdf", width = 7, height = 7, useDingbats=FALSE)




