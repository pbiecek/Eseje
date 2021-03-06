library(dplyr)
library(tidyr)
library(ggplot2)

df <- iris %>%
  mutate(gr = cut(Sepal.Length, seq(4,8,0.5))) %>%
  group_by(gr) %>%
  summarise(min = min(Petal.Length),
            max = max(Petal.Length))

# range
pl1 <- ggplot(df, aes(x=gr, ymin=min, ymax=max)) + 
  geom_linerange() + theme_bw()

df2 <- df %>%
  gather(min_max, value, min, max)
  
# points
pl2 <- ggplot(df2, aes(x=gr, y=value)) + 
  geom_point() + theme_bw()

# line
pl3 <- ggplot(df2, aes(x=gr, y=value, group=min_max)) + 
  geom_line() + theme_bw()

# wszystkie
pl4 <- ggplot(df2, aes(x=gr, y=value, group=min_max)) + 
  geom_line(color="red") + 
  geom_linerange(data=df, aes(x=gr, ymin=min, ymax=max, y=max, group=1), color="grey") + 
  geom_point(color="blue", size=5) + 
  theme_bw()


#
# save

ggsave("stats_range.png", pl1, width = 7, height = 7)
ggsave("stats_point.png", pl2, width = 7, height = 7)
ggsave("stats_line.png", pl3, width = 7, height = 7)
ggsave("stats_all.png", pl4, width = 7, height = 7)

