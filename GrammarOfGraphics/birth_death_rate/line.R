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
  geom_point(color="white", alpha=0) +
  geom_smooth(se=FALSE, size=3, color="black") +
  theme_bw()

ggsave(plS, filename = "geomSmooth.pdf", width = 7, height = 7, useDingbats=FALSE)




# arrow
library(grid)
countries$country <- reorder(countries$country, countries$birth.rate, mean)
ar <- ggplot() +
  geom_segment(data=countries[countries$continent == "Europe",], aes(x = country, xend = country, 
                                  y = birth.rate, yend=death.rate),
               arrow = arrow(length = unit(0.1,"cm"))) +
  theme_bw() + coord_flip() + theme(legend.position="none") + 
  ylab("<---- more births                more deaths ---->") + xlab("") 

ggsave(ar, filename = "geomLine1.pdf", width = 7, height = 7, useDingbats=FALSE)



ar2 <- ggplot() +
  geom_segment(data=countries[countries$continent == "Europe",], aes(x = country, xend = country, 
                                                                     y = birth.rate, yend=death.rate,
                                                                     color=death.rate > birth.rate),
               arrow = arrow(length = unit(0.1,"cm"))) +
  theme_bw() + coord_flip() + theme(legend.position="top") + 
  ylab("<---- more births                more deaths ---->") + xlab("") +
  scale_color_manual(values = c("green3", "red3"), labels=c("More births than deaths","More deaths than births"), name="")

ggsave(ar2, filename = "geomLine2.pdf", width = 7, height = 7, useDingbats=FALSE)



ar3 <- ggplot() +
  geom_segment(data=countries[countries$continent == "Europe",], aes(x = country, xend = country, 
                                                                     y = birth.rate, yend=death.rate,
                                                                     size=population),
               arrow = arrow(length = unit(0.1,"cm"), type="closed")) +
  theme_bw() + coord_flip() + theme(legend.position="none") + 
  ylab("<---- more births                more deaths ---->") + xlab("") 

ggsave(ar3, filename = "geomLine3.pdf", width = 7, height = 7, useDingbats=FALSE)


ar4 <- ggplot() +
  geom_segment(data=countries[countries$continent == "Europe",], aes(x = country, xend = country, 
                                                                     y = birth.rate, yend=death.rate,
                                                                     linetype=birth.rate > death.rate),
               arrow = arrow(length = unit(0.1,"cm"), type="closed")) +
  theme_bw() + coord_flip() + theme(legend.position="top") + 
  ylab("<---- more births                more deaths ---->") + xlab("") +
  scale_linetype_manual(values = c(1,2), labels=c("More births than deaths","More deaths than births"), name="")

ggsave(ar4, filename = "geomLine4.pdf", width = 7, height = 7, useDingbats=FALSE)







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


