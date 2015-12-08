# dane z WHO
# http://apps.who.int/gho/data/view.main.CBDR2040

library(openxlsx)
library(SmarterPoland)
library(maps)



library(rworldmap)
library(ggplot2)
map.world <- map_data(map="world")
countries[177,"country"] = "UK"
countries[135,"country"] = "Russia"
countries[76,"country"] = "Iran"

map2 <- merge(map.world, countries, by.x="region", by.y="country")
map3 <- map2#[map2$continent %in% c("Europe", "Africa"),]
map3 <- map3[order(map3$order),]

gg <- ggplot()
gg <- gg + theme(legend.position="none")
gg <- gg + geom_map(data=map3, map=map3, color="grey", aes(map_id=region, x=long, y=lat, fill=birth.rate))
gg <- gg + scale_fill_gradient(low = "yellow", high = "blue4", guide = "colourbar")

pdf("coords_map1.pdf", width = 7, height = 7)
gg+ coord_map("mollweide") + theme_bw() + ylim(10,70)+ xlim(-25,50) +
  theme(text=element_text(size=20))+theme_map()
dev.off()

pdf("coords_map2.pdf", width = 7, height = 7)
gg+ coord_map() + theme_bw() + ylim(10,70) + xlim(-25,50) +
  theme(text=element_text(size=20))+theme_map()
dev.off()






# dotplot
plD <- ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  geom_point(size=2) +
  geom_smooth(size=2, se=F) + 
  theme_bw() + theme(legend.position="none")

pdf("coords_cartesian.pdf", width = 7, height = 7)
plD+
  theme(text=element_text(size=20))
dev.off()

pdf("coords_fixed.pdf", width = 7, height = 7)
plD + coord_fixed()+
  theme(text=element_text(size=20))
dev.off()

pdf("coords_polar.pdf", width = 7, height = 7)
plD + coord_polar()+
  theme(text=element_text(size=20))
dev.off()

pdf("coords_trans.pdf", width = 7, height = 7)
plD + coord_trans(xtrans = "log2", ytrans = "sqrt")+
  theme(text=element_text(size=20))
dev.off()

pdf("coords_flip.pdf", width = 7, height = 7)
plD + coord_flip()+
  theme(text=element_text(size=20))
dev.off()


