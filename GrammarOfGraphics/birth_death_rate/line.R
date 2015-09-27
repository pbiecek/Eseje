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



# smooth
plS <- ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  geom_smooth() +
  theme_bw()

ggsave(plS, filename = "geomSmooth.pdf", width = 7, height = 7, useDingbats=FALSE)





# smooth + kolor
plS <- ggplot() +
  geom_smooth(data=countries, aes(x = birth.rate, y = death.rate, color=continent), method="loess", span=1, se=FALSE, size=3) +
  theme_bw()
plS


plS <- ggplot() +
  geom_smooth(data=countries, aes(x = population, y = death.rate, color=continent), method="loess", span=1, se=FALSE, size=3) +
  geom_point(data=countries, aes(x = population, y = death.rate, color=continent), method="loess", span=1, se=FALSE, size=3) +
  scale_x_log10() +
  theme_bw()
plS



ggplot() +
  geom_smooth(data=maturaExam, aes(x=podstawowy.matematyka,
                             y=podstawowy.j.polski,
                             color=rok), se=FALSE,
              method="loess", span=1)


