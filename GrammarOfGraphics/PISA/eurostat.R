library(SmarterPoland) 
library(dplyr) 
library(ggplot2) 

milk <- getEurostatRCV("apro_mk_pobta")

EUcc <- c(
  'AT', 'BE', 'BG', 'CY', 'CZ', 'DK', 'EE', 'FI', 'FR', 'DE', 'GR', 'HU', 'IE', 'IT',
  'LV', 'LT', 'LU', 'MT', 'NL', 'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE', 'GB', 'UK', 'EL')

cowMilk <- milk %>%
  filter(milkitem == "PRO" & prodmilk == "MC001") %>%
  filter(geo %in% EUcc)

ggplot(cowMilk, aes(x=time, y=value, fill=geo == "PL")) +
  geom_bar(stat="identity") + coord_flip()






# apples
library(ggplot2)
library(grid)
library(rworldmap)
library(maptools) 
gpclibPermit() 

jablko <- getEurostatRCV("tag00036")

jablko2014 <- milk %>%
  filter(milkitem == "PRO" & prodmilk == "MC001" & time=="2014") %>%
  filter(geo %in% EUcc)

jablko2014 <- jablko %>% 
  filter(time==2014, geo %in% EUcc)
jablko2014$value[is.na(jablko2014$value)] = 0

ggplot(jablkoEU, aes(x=time, y=value, group=geo, col=geo)) + 
  geom_line()

worldMap <- getMap()

# Member States of the European Union
indEU <- which(worldMap$ISO_A2 %in% EUcc)
indEU <- seq_along(worldMap$ISO_A2)

europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region = as.character(worldMap$NAME[i])
  df$iso2 = as.character(worldMap$ISO_A2[i])
  vv <- jablko2014[jablko2014$geo == as.character(worldMap$ISO_A2[i]),"value"]
  if (as.character(worldMap$ISO_A2[i]) == "GB") {
    vv <- jablko2014[jablko2014$geo == "UK","value"]
  }
  if (as.character(worldMap$ISO_A2[i]) == "GR") {
    vv <- jablko2014[jablko2014$geo == "EL","value"]
  }
  if (length(vv) != 1) vv = NA
  df$value = vv
  colnames(df) <- list("long", "lat", "region", "iso2", "value")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

pl1 <- ggplot() + geom_polygon(data = europeCoords, 
                        aes(x = long, y = lat, group = factor(region), fill = value),
                        colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71), projection = "mollweide")

pl1

ggsave(filename = "appleMap.pdf", pl1, width = 7, height = 7)

