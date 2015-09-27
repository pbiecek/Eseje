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




# error bary

conts <- countries %>% 
  group_by(continent) %>%
  summarise(bmin = min(birth.rate, na.rm=TRUE),
            bmax = max(birth.rate, na.rm=TRUE),
            bmea = weighted.mean(birth.rate, w = population, na.rm=TRUE),
            dmin = min(death.rate, na.rm=TRUE),
            dmax = max(death.rate, na.rm=TRUE),
            dmea = weighted.mean(death.rate, w = population, na.rm=TRUE),
            population = sum(population, na.rm=TRUE)
  )

plError <- ggplot(conts, aes(x = bmea, y = dmea, 
                  ymin = dmin, ymax = dmax,
                  xmin = bmin, xmax = bmax,
                  color=continent))+
 geom_point() + 
  geom_errorbar(width=0.5) +
  geom_errorbarh(width=0.5) + 
  theme_bw() + xlab("birth.rate") + ylab("death.rate") +
  theme(legend.position="none")

ggsave(plError, filename = "geomError.pdf", width = 7, height = 7, useDingbats=FALSE)

