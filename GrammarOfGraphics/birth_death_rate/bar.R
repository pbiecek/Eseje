# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(dplyr)
library(SmarterPoland)

continents <- 
  countries %>%
  group_by(continent) %>%
  summarise(birth.rate = weighted.mean(birth.rate, w = population, na.rm=TRUE),
            death.rate = weighted.mean(death.rate, w = population, na.rm=TRUE),
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



plRR3 <- ggplot() +
  geom_rect(data=continents, aes(xmin = as.numeric(factor(continent))-0.1, 
                                 ymin = 0, 
                                 xmax = as.numeric(factor(continent))+0.2, 
                                 ymax = birth.rate),
            fill="green3") +
  geom_rect(data=continents, aes(xmin = as.numeric(factor(continent))+0.21, 
                                 ymin = birth.rate - death.rate, 
                                 xmax = as.numeric(factor(continent))+0.51, 
                                 ymax = birth.rate),
            fill="red3") +
  geom_text(data=continents, aes(x = as.numeric(factor(continent))+0.21, 
                                 y = birth.rate + 1, 
                                 label = continent)) + 
  geom_hline(yintercept=0) + ylab("birth.rate - death.rate") + xlab("") +
  geom_rug(data=continents, aes(x = as.numeric(factor(continent))+0.21, 
                                y = birth.rate - death.rate), sides="l") +
  theme_bw() + theme(axis.text.x = element_text(color="white"),
                     axis.ticks.x = element_line(color="white"))



ggsave(plRR3, filename = "geomRect.pdf", width = 7, height = 7, useDingbats=FALSE)

