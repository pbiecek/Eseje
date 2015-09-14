# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)
library(SmarterPoland)

head(countries)


# path
plP <- ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  geom_path(fill="black") +
  theme_bw()

ggsave(plP, filename = "geomPath.pdf", width = 7, height = 7, useDingbats=FALSE)


# line
plL <- ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  geom_line(fill="black") +
  theme_bw()

ggsave(plL, filename = "geomLine.pdf", width = 7, height = 7, useDingbats=FALSE)


