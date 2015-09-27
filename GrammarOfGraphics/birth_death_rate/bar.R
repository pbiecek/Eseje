# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(dplyr)
library(SmarterPoland)

continents <- 
  countries %>%
  group_by(continent) %>%
  summarise(birth.rate = mean(birth.rate, na.rm=TRUE),
            death.rate = mean(death.rate, na.rm=TRUE),
            population = sum(population))
  

# bar
plB <- ggplot(continents, aes(x = continent, y = birth.rate)) +
  geom_bar(stat = "identity") +
  theme_bw()

ggsave(plB, filename = "geomBar.pdf", width = 7, height = 7, useDingbats=FALSE)


continents2 <- continents %>%
  mutate(cum = cumsum(population)-population)

barPP <- ggplot() +
  geom_rect(data=continents2, aes(xmin=cum, ymax=0, xmax=cum+population, ymin=birth.rate, fill=continent)) +
  geom_text(data=continents2, aes(label=continent, x=cum+population/2, y=birth.rate), vjust=-0.1) + 
  theme_bw() + theme(legend.position="none") +
  xlab("population") + ylab("birth.rate")

ggsave(barPP, filename = "geomRectDW.pdf", width = 7, height = 7, useDingbats=FALSE)





# rect
plR <- ggplot(continents, aes(xmin = death.rate, ymin = birth.rate, xmax = death.rate+1, ymax = birth.rate+1)) +
  geom_rect() +
  theme_bw()

ggsave(plR, filename = "geomRect.pdf", width = 7, height = 7, useDingbats=FALSE)

