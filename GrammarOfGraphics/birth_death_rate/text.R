# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(dplyr)
library(SmarterPoland)

continents <- 
  countries %>%
  group_by(continent) %>%
  summarise(birth.rate = mean(birth.rate, na.rm=TRUE),
            death.rate = mean(death.rate, na.rm=TRUE))
  

# text
plT <- ggplot(continents, aes(x = birth.rate, y = death.rate, label = continent)) +
  geom_text(alpha=1) +
  theme_bw() + xlim(8,35)

ggsave(plT, filename = "geomText.pdf", width = 7, height = 7, useDingbats=FALSE)

