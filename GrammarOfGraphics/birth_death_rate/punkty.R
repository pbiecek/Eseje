# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)
library(SmarterPoland)

head(countries)

# dotplot
plD <- ggplot(countries, aes(x = continent, y = birth.rate)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.7) +
  theme_bw()

ggsave(plD, filename = "geomDotplot.pdf", width = 7, height = 7, useDingbats=FALSE)



# dotplot
plP <- ggplot(countries, aes(x = birth.rate, y =death.rate)) +
  geom_point() +
  theme_bw()

ggsave(plP, filename = "geomPoint.pdf", width = 7, height = 7, useDingbats=FALSE)



# jitter
plJ <- ggplot(countries, aes(x = continent, y =birth.rate)) +
  geom_jitter(position = position_jitter(width = .2)) +
  theme_bw()

ggsave(plJ, filename = "geomJitter.pdf", width = 7, height = 7, useDingbats=FALSE)



# różne mapownia

plP <- ggplot() +
  geom_point(data=countries, aes(x = birth.rate, y =death.rate, shape=continent), size=4) +
  theme_bw() +
  scale_shape_manual(values=c("F","A","S","E","O")) +
  theme(legend.position=c(0.9,0.17))
plP
ggsave(plP, filename = "geomPointShape.pdf", width = 7, height = 7, useDingbats=FALSE)


plC <- ggplot() +
  geom_point(data=countries, aes(x = birth.rate, y =death.rate, color=continent), size=4, shape=19) +
  theme_bw() + scale_color_brewer(type = "qual", palette=6)
plC
ggsave(plC, filename = "geomPointColor.pdf", width = 7, height = 7, useDingbats=FALSE)



library(scales)
plS <- ggplot() +
  geom_point(data=countries, aes(x = birth.rate, y =death.rate, size=population)) +
  scale_size_continuous(trans="sqrt", label=comma, limits=c(0,1500000)) +
  theme_bw() + theme(legend.position="none")

ggsave(plS, filename = "geomPointSize.pdf", width = 7, height = 7, useDingbats=FALSE)






