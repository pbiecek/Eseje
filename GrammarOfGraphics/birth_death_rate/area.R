# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)
library(SmarterPoland)

head(countries)

# density
plD <- ggplot(countries, aes(x = birth.rate, fill = continent)) +
  geom_density(alpha=0.5) +
  theme_bw()

ggsave(plD, filename = "geomDensity.pdf", width = 7, height = 7, useDingbats=FALSE)


# vioplot
plV <- ggplot(countries, aes(x = continent, y = birth.rate)) +
  geom_violin(alpha=0.5) +
  theme_bw()

ggsave(plV, filename = "geomViolin.pdf", width = 7, height = 7, useDingbats=FALSE)



