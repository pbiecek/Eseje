library(ggplot2)
library(maps)

wc <- world.cities[world.cities$name %in% c("Warsaw","Berlin","Wroclaw", "Vienna"),][c(2,4,5,6),]

states <- map_data("world")

pl1 <- ggplot(states, aes(x=long, y=lat, group = group)) + 
#  geom_polygon(size=3, fill="#00000050", color="white") +
  geom_point(data=wc, aes(group=1), size=5,color="black") +
  geom_text(data=wc, aes(group=1, label = name), size=5,color="black", vjust = -1) +
  coord_map(xlim=c(10,25),ylim=c(47,55), projection = "cylequalarea", lat0=50) + 
  theme_bw()

pl2 <- ggplot(states, aes(x=long, y=lat, group = group)) + 
  geom_polygon(size=3, fill="#00000050", color="white") +
  geom_point(data=wc, aes(group=1), size=5,color="black") +
  geom_text(data=wc, aes(group=1, label = name), size=5,color="black", vjust = -1) +
  coord_map(xlim=c(10,25),ylim=c(47,55), projection = "cylequalarea", lat0=50) + 
  theme_bw()


# Zapisujemy
## 59e5c70228245b28facc8997ba187cd0
## 816048bc988aebc8f8f27e975c285dc9

ggsave(pl1, filename = "miasta0.pdf", width=7, height=7)
ggsave(pl2, filename = "miasta1.pdf", width=7, height=7)

