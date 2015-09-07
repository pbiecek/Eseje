# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)

rates <- read.xlsx("data.xlsx",1)[-1,]
for (i in 2:7)
  rates[,i] <- as.numeric(as.character(rates[,i]))

colnames(rates) <- c("country","Birth_rate","Death_rate", "Population", "Population.under.15", "Population.over.60","Population.med.age", "Population.urban")

ratesMerged <- merge(rates, countrycode_data[,c("country.name", "continent")],
      by.x = "country", by.y = "country.name")

ggplot(ratesMerged, aes(x=Birth_rate, y=Death_rate, color=continent)) +
  geom_density2d(h=c(10,10), color="grey") +
  geom_point() + coord_fixed() +
  geom_abline(xintercept=0,slope=1) + 
  geom_point(data=rates[136,], color="red", size=6) + 
  theme_bw() 

ggplot(ratesMerged, aes(x=Population.under.15, y=Population.over.60, color=continent)) +
  geom_density2d(h=c(10,10), color="grey") +
  geom_point() + coord_fixed() +
  geom_abline(xintercept=0,slope=1) + 
  geom_point(data=rates[136,], color="red", size=6) + 
  theme_bw() 


library(ggplot2)
library(MASS)

pl <- ggplot(rates, aes(x=Birth_rate, y=Death_rate)) +
  geom_density2d(h=c(10,10), color="grey") +
  geom_point() + coord_fixed() +
  geom_abline(xintercept=0,slope=1) + 
  geom_point(data=rates[136,], color="red", size=6) + 
  theme_bw() 

pl

ggsave(plot = pl, filename="rates.png", width = 15, height = 10)


#
# drugi wykres ze współrzędnymi
#

library(maps) 
library(dplyr) 
data(world.cities)
capitals <- world.cities %>%
  filter(capital == 1)

setdiff(capitals$country.etc,rates[,1])
setdiff(rates[,1],capitals$country.etc)

ratesGeo <- merge(capitals, rates, by.x = "country.etc", by.y = "country")

#
# mapy
#

# World map, using geom_path instead of geom_polygon
world <- map_data("world")
worldmap <- ggplot(data=world, aes(x=long, y=lat, group=group)) +
  geom_path(color="grey") +
  scale_y_continuous(breaks=(-2:2) * 30) +
  scale_x_continuous(breaks=(-4:4) * 45) +
  coord_fixed() + theme_bw() + 
  geom_point(data=ratesGeo, aes(x=long, y=lat, group=country.etc,
                           size=Birth_rate-Death_rate,
                           color=Birth_rate-Death_rate)) +
  scale_color_gradient2(low="red", mid="black", high = "blue")+
  theme(legend.position="none")

ggsave(plot = worldmap, filename="rates_map.png", width = 15, height = 10)

worldmapG <- worldmap + coord_map("gilbert",orientation=c(90,0,0))

ggsave(plot = worldmapG, filename="rates_mapG.png", width = 15, height = 10)


worldmap <- ggplot(data=world, aes(x=long, y=lat, group=group)) +
  geom_polygon(color="white", fill="grey") +
  scale_y_continuous(breaks=(-2:2) * 30, limits=c(-60,85)) +
  scale_x_continuous(breaks=(-4:4) * 45) +
  coord_fixed() + theme_bw() + 
  geom_point(data=ratesGeo, aes(x=long, y=lat, group=country.etc,
                                size=Birth_rate-Death_rate,
                                color=Birth_rate-Death_rate)) +
  scale_color_gradient2(low="red", mid="black", high = "blue")+
  theme(legend.position="none")
worldmapG <- worldmap + coord_map("gilbert",orientation=c(90,0,0))
worldmapG

ggsave(plot = worldmap, filename="rates_map2.png", width = 15, height = 10)
ggsave(plot = worldmapG, filename="rates_mapG2.png", width = 15, height = 10)


#
# jeszcze jedno podejscie

worldmap <- ggplot(data=world, aes(x=long, y=lat, group=group)) +
  geom_polygon(color="white", fill="lightgrey") +
  scale_y_continuous(breaks=(-2:2) * 30, limits=c(-60,85)) +
  scale_x_continuous(breaks=(-4:4) * 45) +
  coord_fixed() + theme_bw() + 
  geom_point(data=ratesGeo, aes(x=long, y=lat, group=country.etc,
                                size=Birth_rate-Death_rate)) +
  theme(legend.position="top")
worldmapG <- worldmap + coord_map("gilbert",orientation=c(90,0,0))
worldmapG

ggsave(plot = worldmap, filename="rates_map3.png", width = 15, height = 10)
ggsave(plot = worldmapG, filename="rates_mapG3.png", width = 15, height = 10)
