# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)
library(SmarterPoland)

rates <- read.xlsx("data.xlsx",1)[-1,]
for (i in 2:7)
  rates[,i] <- as.numeric(as.character(rates[,i]))

colnames(rates) <- c("country","Birth_rate","Death_rate", "Population", "Population.under.15", "Population.over.60","Population.med.age", "Population.urban")


# density
ggplot(ratesSmall, aes(x=Birth_rate)) +
  stat_density() + 
  geom_rug() + 
  theme_bw() 


ggplot(ratesSmall, aes(x=Birth_rate)) +
  geom_histogram(binwidth=1) + 
  geom_rug() + 
  theme_bw() 


ggplot(ratesSmall, aes(x=continent, y=Birth_rate)) +
  geom_boxplot() + 
  geom_rug(sides = "lr") + 
  theme_bw() 

ggplot(ratesSmall, aes(x=continent, y=Birth_rate)) +
  geom_violin() + 
  geom_rug(sides = "lr") + 
  theme_bw() 


ggplot(ratesSmall, aes(x=continent, y=Birth_rate)) +
  stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max,
               colour = "red") + 
  geom_rug(sides = "lr") + 
  theme_bw() 

ggplot(ratesSmall, aes(x=continent, y=Birth_rate)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar",
               colour = "red", width = 0.3) + 
  geom_rug(sides = "lr") + 
  theme_bw() 


pp1 <- ggplot(ratesSmall, aes(x=continent, y=Birth_rate)) +
  geom_violin(fill="grey") + 
  stat_summary(fun.data = "median_hilow", geom = "crossbar",
               colour = "red", width = 0.5, conf.int=0.5) + 
  geom_rug(sides = "r") + 
  geom_point(position= position_jitter(width = .05, height = 0), size=2) + 
  theme_bw() 

ggsave(pp1, filename = "ppP5.pdf", width = 8, height = 8, useDingbats=FALSE)


ggsave(pp1, filename = "pp1.pdf", width = 6, height = 6, useDingbats=FALSE)


countries <- ratesSmall
colnames(countries) <- c("country", "birth.rate", "death.rate", "population", "continent")
  
save(countries, file="countries.rda")
  

library(countrycode)


ratesMerged <- merge(rates, countrycode_data[,c("country.name", "continent")],
      by.x = "country", by.y = "country.name")

ratesSmall <- ratesMerged[,c(1,2,3,4,9)]

ratesMergedEurope <- ratesMerged[ratesMerged$continent == "Europe",]


pl <- ggplot(ratesMergedEurope, aes(x=Birth_rate, y=Death_rate, label=country)) +
  geom_point() + 
  geom_text(vjust=-.5, size=4) + 
  coord_fixed(xlim = c(6,16), ylim = c(6,16)) +
  theme_bw() 

plP <- ggplot(ratesMergedEurope, aes(x=Birth_rate, y=Death_rate, label=country)) +
  geom_point() + 
  coord_fixed(xlim = c(6,16), ylim = c(6,16)) +
  theme_bw() 

plT <- ggplot(ratesMergedEurope, aes(x=Birth_rate, y=Death_rate, label=country)) +
  geom_text(vjust=-.5, size=4) + 
  coord_fixed(xlim = c(6,16), ylim = c(6,16)) +
  theme_bw() 

ggsave(pl, filename = "europaBirthDeath.pdf", width = 9, height = 5)
ggsave(plP, filename = "europaBirthDeathPunkty.pdf", width = 9, height = 5)
ggsave(plT, filename = "europaBirthDeathText.pdf", width = 9, height = 5)

plI <- ggplot(ratesMerged, aes(x=Birth_rate, y=Death_rate)) +
  geom_point() + coord_fixed() +
  theme_bw() 

ggsave(plI, filename = "europaBirthDeathI.pdf", width = 9, height = 5)

plCont <- ggplot(ratesMerged, aes(x=Birth_rate, y=Death_rate)) +
  coord_fixed() +
  geom_density2d(h=c(10,10), color="grey") +
  theme_bw() 

ggsave(plCont, filename = "europaBirthDeathCont.pdf", width = 9, height = 5)

plC <- ggplot(ratesMerged, aes(x=Birth_rate, y=Death_rate)) +
  geom_point() + coord_fixed() +
  geom_density2d(h=c(10,10), color="grey") +
  theme_bw() 

ggsave(plC, filename = "europaBirthDeathC.pdf", width = 9, height = 5)


plII <- ggplot(ratesMerged, aes(x=Birth_rate, y=Death_rate, color=continent, shape=continent)) +
  geom_point() + coord_fixed() +
  theme_bw() 

ggsave(plII, filename = "europaBirthDeathII.pdf", width = 9, height = 5)

ratesMerged$PopulationCat <- cut(ratesMerged$Population, c(1,10^3,10^4,10^5, 10^6, 10^7), labels = c("< 1M", "< 10M","< 100 M", "< 1 B", "> 1 B"))
plIII <- ggplot(ratesMerged, aes(x=Birth_rate, y=Death_rate, color=continent, 
                                 shape=PopulationCat, size=PopulationCat)) +
  geom_point() + coord_fixed() +
  theme_bw() 

ggsave(plIII, filename = "europaBirthDeathIII.pdf", width = 9, height = 5)









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


ratesRug <- ggplot(rates, aes(x=Birth_rate, y=Death_rate)) +
  geom_rug() + coord_fixed() +
  theme_bw() 

ratesBoth <- ggplot(rates, aes(x=Birth_rate, y=Death_rate)) +
  geom_rug() + coord_fixed() +
  geom_point() + 
  theme_bw() 

ggsave(plot = ratesRug, filename="ratesRug.pdf", width = 15, height = 10)
ggsave(plot = ratesBoth, filename="ratesBoth.pdf", width = 15, height = 10)


pl <- ggplot(rates, aes(x=Birth_rate, y=Death_rate)) +
  geom_density2d(h=c(10,10), color="grey") +
  geom_point() + coord_fixed() +
  geom_abline(xintercept=0,slope=1) + 
  geom_point(data=rates[136,], color="red", size=6) + 
  theme_bw() 

pl

ggsave(plot = pl, filename="rates.png", width = 15, height = 10)

ggsave(plot = pl, filename="rates.pdf", width = 15, height = 10, useDingbats=FALSE)

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


library(archivist)
setLocalRepo("~/GitHub/Eseje/arepo/")
# df400101cb7709b64dba60c007379cea
saveToRepo(ratesMergedEurope)


tmp <- aread("pbiecek/Eseje/arepo/df400101cb7709b64dba60c007379cea")
tmp2 <- tmp[,c(1:4,9)]
rownames(tmp2) <- tmp2[,1]

library(xtable)
xtable(tmp2[,-1])

