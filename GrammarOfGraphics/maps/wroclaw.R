library(ggplot2)
library(maps)

data(world.cities)

head(world.cities)

wc <- world.cities[world.cities$name %in% c("Warsaw","Berlin","Prague","Wroclaw", "Vienna"),][c(2,4,5,6,7),]

wc <- world.cities[world.cities$name %in% c("Warsaw","Berlin","Wroclaw", "Vienna"),][c(2,4,5,6),]

states <- map_data("world")

ggplot(states, aes(x=long, y=lat, group = group)) + 
#  geom_polygon(size=3, fill="#00000050", color="white") +
  geom_point(data=wc, aes(group=1), size=5,color="black") +
  geom_text(data=wc, aes(group=1, label = name), size=5,color="black", vjust = -1) +
  coord_map(xlim=c(10,25),ylim=c(47,55), projection = "cylequalarea", lat0=50) + 
  theme_bw()

ggplot(states, aes(x=long, y=lat, group = group)) + 
  geom_polygon(size=3, fill="#00000050", color="white") +
  geom_point(data=wc, aes(group=1), size=5,color="black") +
  geom_text(data=wc, aes(group=1, label = name), size=5,color="black", vjust = -1) +
  coord_map(xlim=c(10,25),ylim=c(47,55), projection = "cylequalarea", lat0=50) + 
  theme_bw()

