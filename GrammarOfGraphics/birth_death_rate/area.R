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
ndf <- countries %>%
  group_by(continent) %>%
  summarise(birth.rate = weighted.mean(birth.rate, population, na.rm=TRUE),
            death.rate = weighted.mean(death.rate, population, na.rm=TRUE))

plR <- ggplot() + 
  geom_ribbon(data=ndf, aes(x=continent, ymax=birth.rate, y=birth.rate, ymin=0, group=1), fill="green3") +
  geom_ribbon(data=ndf, aes(x=continent, ymax=death.rate, y=death.rate, ymin=0, group=1), fill="red3") +
  theme_bw() + xlab("") + ylab("birth.rate / death.rate")

ggsave(plR, filename = "geomRibbon.pdf", width = 7, height = 7, useDingbats=FALSE)


