# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(dplyr)
library(SmarterPoland)

continents <- 
  countries %>%
  group_by(continent) %>%
  summarise(birth.rate = mean(birth.rate, na.rm=TRUE),
            death.rate = mean(death.rate, na.rm=TRUE))
  

# bar
plB <- ggplot(continents, aes(x = continent, y = birth.rate)) +
  geom_bar(stat = "identity") +
  theme_bw()

ggsave(plB, filename = "geomBar.pdf", width = 7, height = 7, useDingbats=FALSE)

