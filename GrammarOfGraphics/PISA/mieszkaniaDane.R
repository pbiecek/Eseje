library(PBImisc)
library(ggplot2)
library(lubridate)

head(apartments)

ggplot(apartments, aes(x=surface, y=m2.price)) +
  geom_point() + 
  geom_smooth() +
  scale_x_sqrt() +
  scale_y_sqrt()

ggplot(apartments, aes(x=surface, y=m2.price, color=type)) +
  geom_point() + 
  geom_smooth(se=FALSE) +
  scale_x_sqrt() +
  scale_y_sqrt()

apartments$date <- ymd(paste0(apartments$year, "-",
                         ifelse(apartments$month < 10, "0",""),
                         apartments$month,
                         "-01"))

ggplot(apartments, aes(x=date, y=m2.price)) +
  geom_point(position="jitter") + 
  geom_smooth(se=FALSE) +
  scale_y_sqrt()


ggplot(apartments, aes(x=factor(n.rooms), fill=factor(year))) +
  geom_bar(se=FALSE)

ggplot(apartments, aes(x=factor(n.rooms), fill=factor(year))) +
  geom_bar(se=FALSE, position="fill")

ggplot(apartments, aes(x=factor(condition), fill=factor(year))) +
  geom_bar(se=FALSE)

ggplot(apartments, aes(fill=cut(n.rooms,c(0,2,4,10)), x=factor(district))) +
  geom_bar(se=FALSE) +
  coord_flip()
