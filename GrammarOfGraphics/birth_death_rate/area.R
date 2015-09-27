# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)
library(SmarterPoland)

head(countries)

# density
plD <- ggplot(countries, aes(x = birth.rate, fill = continent)) +
  geom_density(alpha=0.5) +
  theme_bw() + theme(legend.position=c(0.85,0.85))

ggsave(plD, filename = "geomDensity.pdf", width = 7, height = 7, useDingbats=FALSE)


# vioplot
plV <- ggplot(countries, aes(x = continent, y = birth.rate, fill=continent)) +
  geom_violin() +
  theme_bw() + theme(legend.position="none")

ggsave(plV, filename = "geomViolin.pdf", width = 7, height = 7, useDingbats=FALSE)



# ribbon
plR <- ggplot(countries[order(countries$birth.rate),], aes(x = birth.rate, ymin = 0, ymax = death.rate)) +
  geom_ribbon(fill="black") +
  theme_bw()

ggsave(plR, filename = "geomRibbon.pdf", width = 7, height = 7, useDingbats=FALSE)


